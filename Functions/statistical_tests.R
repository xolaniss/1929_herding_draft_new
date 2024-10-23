additional_tests <- function(variable = variable){
  jb_result <- jarque.bera.test(variable) %>% tidy()
  adf_result <- adf.test(variable) %>% tidy() %>% dplyr::select(-alternative)
  pp_result <- pp.test(variable) %>% tidy() %>% dplyr::select(-alternative)
  result <- rbind(jb_result, adf_result, pp_result) %>% 
    dplyr::select(method, statistic, p.value) %>% 
    dplyr::rename("Method" = method,
                  "Statistic" = statistic,
                  "P value" = p.value)
}

additional_tests_multiple_variable <- function(variable_names_list){
  tests <- map_dfr(names(variable_names_list), ~additional_tests(variable_names_list[[.x]]))
  tests
}
