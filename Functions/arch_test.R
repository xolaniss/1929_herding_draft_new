arch_test <-
function(data, lags) {
  arch_test <- map(data.frame(data), archtest, lags) 
  arch_statistic <- map_df(arch_test, ~.x[1]$statistic) %>% round(2)
  arch_pvalue <- map_df(arch_test, ~.x[4]$p.value) %>% round(2)
  arch_pvalue <- pivot_longer(arch_pvalue, names_to = "Variable", values_to = "P value", cols = everything()) 
  arch_tbl <- cbind(arch_pvalue, arch_statistic) %>% rename("Statistic" = statistic) %>% relocate(Statistic, .before = `P value`)
  arch_tbl
}
