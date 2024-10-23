jb_test <-
function(data){
  jb_test <- map(data.frame(data), jarque.bera.test)
  jb_statistic <- map_df(jb_test, ~.x[1]$statistic) %>% round(2)
  jb_pvalue <- map_df(jb_test, ~.x[3]$p.value) %>% round(2)
  jb_pvalue <- pivot_longer(jb_pvalue, names_to = "Variable", values_to = "P value", cols = everything())
  jb_tbl <- cbind(jb_pvalue, jb_statistic) %>% relocate(`X-squared`, .before = `P value`)
  jb_tbl 
}
