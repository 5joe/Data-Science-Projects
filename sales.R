# 1.0 LIBRARIES
library(readr)
library(tidyverse)

library(tidyquant)

library(plotly)

library(rsample)
library(parsnip)


sales_pipeline <- read.csv(file = "00_dataset/sales_pipeline.csv", header = TRUE, sep = ",")

sales_teams <- read.csv(file = "00_dataset/sales_teams.csv", header = TRUE, sep = ",")

products_tbl <- read.csv(file = "00_dataset/products.csv", header = TRUE, sep = ",")



# 2.0 PREPROCESS DATA

train_tbl <- sales_pipeline %>% 
    
    filter(deal_stage != "Engaging") %>% 
    
    filter(deal_stage != "Prospecting") %>% 
    
    mutate(product = case_when(
        product == "GTXPro" ~ "GTX Pro",
        TRUE ~ product
    )) %>% 
    left_join(sales_teams, by = "sales_agent") %>%
    left_join(products_tbl, by = "product") %>% 
    
    separate(col = product,
             into = str_c("model_", 1:3),
             sep = " ",
             remove = FALSE,
             fill = "right")


# xgboost model
# 3.0 CREATE MODEL

train_tbl <- train_tbl %>% 
    select(c(model_1, model_2, close_value, sales_agent)) %>% #serie = model_1
    select(close_value, everything())

train_tbl

set.seed(1234)
model_xgboost <- boost_tree(mode = "regression", ### I need to research on the creation of this regression model
                            mtry = 30,
                            learn_rate = 0.25,
                            tree_depth = 7) %>%
        set_engine(engine = "xgboost") %>%
        fit(close_value ~ ., data = train_tbl)


# 3.1 TEST MODEL ----

model_xgboost %>%
    predict(new_data = train_tbl %>% select(-close_value))

# 3.2 Save Model ----

write_rds(model_xgboost, file = "00_models/model_xgboost.rds")
read_rds("00_models/model_xgboost.rds")
 

# 4.0 MODULARIZE PREPROCESSING CODE ---
 #4.1 separate_product() ---
data <- sales_pipeline

separate_product <- function(data, keep_product_column = TRUE, append = TRUE) {
    if ("deal_stage" %in% colnames(data)) { 
        
        data <- data %>% 
            filter(!deal_stage %in% c("Engaging", "Prospecting"))
        
    } else {
        
        warning("'deal_stage' column not found. Skipping filtering for 'Engaging' and 'Prospecting'.")
        
    }
    
    
    if (!append) {
        
        data <- data %>% select(product)
        
    }
    
    output_tbl <- data %>%
        mutate(product = case_when(
            product == "GTXPro" ~ "GTX Pro",
            TRUE ~ product
        )) %>%
        separate(
            col = product,
            into = str_c("model_", 1:3),
            sep = " ",
            remove = FALSE,
            fill = "right"
        )
    
    if (!keep_product_column) {
        
        output_tbl <- output_tbl %>% select(-product)
        
    }
    
    return(output_tbl)
}


sales_pipeline %>% separate_product() ##Why is it that when keep_product_column = TRUE, AND append = FALSE sales_agent isnt printed


# 4.2 TEST FUNCTIONS ----
sales_pipeline %>% 
    separate_product(keep_product_column = FALSE)

sales_pipeline %>% 
    separate_product(append = FALSE, keep_product_column = FALSE)


# 4.3 SAVE FUNCTIONS ----

dump("separate_product",
     file = "00_scripts/01_separate_product.R")

  
# 5.0 USER INPUT & PREDICTION ----

# 5.1 Inputs ----
product <- "GTX Basic"
sales_agent <- "Zane Levy"
model_1 <- "GTX"
model_2 <- "Basic"

# 5.2 Make Prediction ---
train_tbl

new_sales_tbl <- tibble(
    product = product,
    sales_agent = sales_agent,
    model_1 = model_1,
    model_2 = model_2
) %>% 
    separate_product()

new_sales_tbl %>% 
    predict(model_xgboost, new_data = .)

# 6.0 MODULARIZE NEW SALES PREDICTION ---

# 6.1 generate_new_sales() Function ---

generate_new_sales <- function(product, model_1, model_2, sales_agent, .ml_model){
    
    new_sales_tbl <- tibble(
        product = product,
        sales_agent = sales_agent,
        model_1 = model_1,
        model_2 = model_2
    ) %>% 
        separate_product()
    
    predict(.ml_model, new_data = new_sales_tbl) %>%
        bind_cols(new_sales_tbl) %>%
        rename(close_value = .pred)
    
}

new_sales_tbl <- generate_new_sales(
    product = "GTK 500",
    sales_agent = "Moses Frase",
    model_1 = "GTK",
    model_2 = "500",
    .ml_model = model_xgboost
    
)

sales_pipeline %>% 
    separate_product() %>% 
    bind_rows(new_sales_tbl) %>% 
    tail()

# 6.2 Test ----

new_sales_tbl <- generate_new_sales(
    product = "MG Special",
    sales_agent = "Maureen Marcano",
    model_1 = "MG",
    model_2 = "Special",
    .ml_model = model_xgboost
)

new_sales_tbl
  

# 7.0 output table ----

format_table <- function(new_sales_tbl){
    new_sales_tbl %>% 
        mutate(close_value = scales::dollar(close_value, accuracy = 1)) %>% 
        gather(key = "Sales Product Info", value = "value", -product, factor_key = TRUE) %>% 
        spread(key = product, value = value)
}

new_sales_tbl %>% format_table()




# 8.1 bind_sales_prediction() function ----

### PREPROCESS DATA FOR THE PLOT

bind_sales_prediction <- function(sales_pipeline, new_sales_tbl){
    
    sales_pipeline %>% 
        separate_product() %>% 
        mutate(estimate = "Actual") %>% 
        bind_rows(
            new_sales_tbl %>% mutate(estimate = "Prediction")
            ) %>% 
        select(estimate, product, sales_agent, model_1, model_2, close_value)
        
    
}

bind_sales_prediction(sales_pipeline, new_sales_tbl) %>% tail()


## 8.2 working on my own plot for this case

sales_pipeline %>% 
    bind_sales_prediction(new_sales_tbl)
ggplot()



# 8.2 plot_sales_prediction() function ----

### This Particular plot is promising and I want to use this on the final piece


plot_sales_prediction <- function(data, interactive = TRUE){
    
    # Summarize data to have one point per product for `Actual` values
    summarized_actual <- data %>%
        filter(estimate == "Actual") %>% # Filter for Actual values
        group_by(product) %>%
        summarize(
            avg_close_value = mean(close_value, na.rm = TRUE),
            estimate = "Actual", # Add estimate column
            label_text = str_glue("Avg Close Value (Actual): {scales::dollar(avg_close_value, accuracy = 1)}
                                   Product: {product}")
        ) %>%
        ungroup()
    
    # Prepare Prediction data with custom label_text
    prediction_data <- data %>%
        filter(estimate == "Prediction") %>% # Filter for Prediction values
        mutate(
            label_text = str_glue("Close Value (Prediction): {scales::dollar(close_value, accuracy = 1)}
                                   Product: {product}
                                   Sales Agent: {sales_agent}")
        )
    
    # Combine Actual and Prediction data
    combined_data <- bind_rows(summarized_actual, prediction_data)
    
    # Update the plot to use combined data
    g <- combined_data %>%
        ggplot(aes(product, avg_close_value, color = estimate)) +
        
        geom_point(aes(y = ifelse(estimate == "Actual", avg_close_value, close_value), 
                       text = label_text), size = 4, alpha = 0.7) +
        
        coord_flip() +
        
        scale_y_log10(labels = scales::dollar_format(accuracy = 1)) +
        
        scale_color_manual(values = c("Actual" = "steelblue", "Prediction" = "red"))+
        
        theme_tq() +
        
        theme(strip.text.x = element_text(margin = margin(5, 5, 5, 5))) +
        labs(title = "Comparing Actual vs Predicted Sales by Product", 
             x = "", 
             y = "Close Value")
    
    if(interactive) {
        return(ggplotly(g, tooltip = "text"))
    } else {
        return(g)
    }
}

bind_sales_prediction(sales_pipeline, new_sales_tbl) %>% 
    plot_sales_prediction(interactive = TRUE)


####The plots as functions 
# Function 1: Deal Stage Analysis (Bar Plot)


deal_stage_plot <- function(data) {
    p <- ggplot(data, aes(x = deal_stage)) +
        geom_bar(fill = "skyblue") +
        labs(title = "Deal Stage Distribution", x = "Deal Stage", y = "Number of Deals") +
        theme_minimal()
    
    ggplotly(p)
}

# Function 2: Sales Over Time (Line Plot)
sales_over_time_plot <- function(data) {
    # Ensure close_date is in Date format
    data$close_date <- as.Date(data$close_date)
    
    # Aggregate close_value by close_date
    sales_over_time <- aggregate(close_value ~ close_date, data, sum)
    
    p <- ggplot(sales_over_time, aes(x = close_date, y = close_value)) +
        geom_line(color = "steelblue", size = 1) +
        labs(title = "Total Sales Over Time", x = "Close Date", y = "Total Close Value") +
        theme_minimal()
    
    ggplotly(p)
}

# Function 3: Regional Performance (Bar Plot by Region and Deal Stage)
regional_performance_plot <- function(data) {
    p <- ggplot(data, aes(x = regional_office, fill = deal_stage)) +
        geom_bar(position = "dodge") +
        labs(title = "Regional Performance by Deal Stage", x = "Regional Office", y = "Number of Deals") +
        theme_minimal() +
        scale_fill_manual(values = c("Won" = "skyblue", "Lost" = "steelblue"))
    
    ggplotly(p)
}

# Function 4: Sales Agent Performance (Bar Plot)
sales_agent_performance_plot <- function(data) {
    # Aggregate close_value by sales_agent
    agent_performance <- aggregate(close_value ~ sales_agent, data, sum)
    
    p <- ggplot(agent_performance, aes(x = reorder(sales_agent, -close_value), y = close_value)) +
        geom_bar(stat = "identity", fill = "lightblue") +
        labs(title = "Sales Agent Performance", x = "Sales Agent", y = "Total Close Value") +
        theme_minimal() +
        coord_flip()
    
    ggplotly(p)
}


##Might not this particular plot

# Function 5: Sales Price vs. Close Value (Scatter Plot)
sales_price_vs_close_value_plot <- function(data) {
    p <- ggplot(data, aes(x = sales_price, y = close_value)) +
        geom_point(color = "darkorange", alpha = 0.6) +
        labs(title = "Sales Price vs. Close Value", x = "Sales Price", y = "Close Value") +
        theme_minimal()
    
    ggplotly(p)
}


deal_stage_plot(data)
sales_over_time_plot(data)
regional_performance_plot(data)
sales_agent_performance_plot(data)
sales_price_vs_close_value_plot(data)

# End of file

# Save Functions 

dump(c("deal_stage_plot", "sales_over_time_plot", "regional_performance_plot", "sales_agent_performance_plot", "sales_price_vs_close_value_plot", "format_table", "bind_sales_prediction", "plot_sales_prediction", "generate_new_sales"), 
     file = "00_scripts/plot_functions.R")



