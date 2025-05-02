separate_product <-
function(data, keep_product_column = TRUE, append = TRUE) {
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
