deal_stage_plot <-
function(data) {
    p <- ggplot(data, aes(x = deal_stage)) +
        geom_bar(fill = "skyblue") +
        labs(title = "Deal Stage Distribution", x = "Deal Stage", y = "Number of Deals") +
        theme_minimal()
    
    ggplotly(p)
}
sales_over_time_plot <-
function(data) {
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
regional_performance_plot <-
function(data) {
    p <- ggplot(data, aes(x = regional_office, fill = deal_stage)) +
        geom_bar(position = "dodge") +
        labs(title = "Regional Performance by Deal Stage", x = "Regional Office", y = "Number of Deals") +
        theme_minimal() +
        scale_fill_manual(values = c("Won" = "skyblue", "Lost" = "steelblue"))
    
    ggplotly(p)
}
sales_agent_performance_plot <-
function(data) {
    # Aggregate close_value by sales_agent
    agent_performance <- aggregate(close_value ~ sales_agent, data, sum)
    
    p <- ggplot(agent_performance, aes(x = reorder(sales_agent, -close_value), y = close_value)) +
        geom_bar(stat = "identity", fill = "lightblue") +
        labs(title = "Sales Agent Performance", x = "Sales Agent", y = "Total Close Value") +
        theme_minimal() +
        coord_flip()
    
    ggplotly(p)
}
sales_price_vs_close_value_plot <-
function(data) {
    p <- ggplot(data, aes(x = sales_price, y = close_value)) +
        geom_point(color = "darkorange", alpha = 0.6) +
        labs(title = "Sales Price vs. Close Value", x = "Sales Price", y = "Close Value") +
        theme_minimal()
    
    ggplotly(p)
}
format_table <-
function(new_sales_tbl){
    new_sales_tbl %>% 
        mutate(close_value = scales::dollar(close_value, accuracy = 1)) %>% 
        gather(key = "Sales Product Info", value = "value", -product, factor_key = TRUE) %>% 
        spread(key = product, value = value)
}
bind_sales_prediction <-
function(sales_pipeline, new_sales_tbl){
    
    sales_pipeline %>% 
        separate_product() %>% 
        mutate(estimate = "Actual") %>% 
        bind_rows(
            new_sales_tbl %>% mutate(estimate = "Prediction")
            ) %>% 
        select(estimate, product, sales_agent, model_1, model_2, close_value)
        
    
}
plot_sales_prediction <-
function(data, interactive = TRUE){
    
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
generate_new_sales <-
function(product, model_1, model_2, sales_agent, .ml_model){
    
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
