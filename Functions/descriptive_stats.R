descriptive_stats <-
function(data, ...){
  desc <- data %>% dplyr::select(...) %>% 
    describe() %>% 
    dplyr::select(-vars, -trimmed, -mad, -range, -se) %>% 
    dplyr::rename("Observations" = n,
                  "Mean" = mean,
                  "Median" = median,
                  "Min" = min,
                  "Max" = max,
                  "Standard Deviation" = sd,
                  "Skewness" = skew,
                  "Kurtosis" = kurtosis,
                  
    )
  }
