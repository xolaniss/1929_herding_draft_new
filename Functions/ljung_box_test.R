ljungbox_test <-
function(data, lag = 10, fitdf = 0){
  Ljungbox_test <- map(data.frame(data), Box.test, lag = lag, type = c("Ljung-Box"), fitdf = fitdf)
  Ljungbox_statistic <- map_df(Ljungbox_test, ~.x[1]$statistic) %>% round(2)
  Ljungbox_pvalue <- map_df(Ljungbox_test, ~.x[3]$p.value) %>% round(2)
  Ljungbox_pvalue <- pivot_longer(Ljungbox_pvalue, names_to = "Variable", values_to = "P value", cols = everything()) 
  Ljungbox_tbl <- cbind(Ljungbox_pvalue, Ljungbox_statistic) %>% relocate(`X-squared`, .before = `P value`)
  Ljungbox_tbl
}
