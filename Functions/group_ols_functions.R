ols_nest_full_prep <-
  function(data){
    data %>%
      dplyr::relocate(Date, .after = "Category") %>% 
      group_by(Category) %>% 
      nest()
  }

ols_tidy_group_models <-
function(nested_data, formula = formula){
  nested_data %>% 
    mutate(models = map(data, ~coeftest(lm(formula, data = .), 
                                        vcov = NeweyWest))) %>% 
    mutate(models_coef = map(models, ~tidy(.)))
}

ols_pretty_full_results <-
  function(data_fitted_models){
    data_fitted_models %>% 
      unnest(cols = models_coef, names_repair = "universal") %>% 
      dplyr::select(Category, term, estimate, p.value) %>% 
      mutate(
        stars = ifelse(p.value < 0.001, "***", ifelse(p.value < 0.01, "**", ifelse(p.value < 0.05, "*", "")))
      ) %>% 
      mutate(across(2, ~strtrim(., 8))) %>%
      mutate(across(3, ~strtrim(., 3))) %>% 
      dplyr::select(Category,  term, estimate, p.value, stars) %>%
      mutate(Estimate = paste0(estimate, stars)) %>%
      dplyr::select(-estimate, -p.value, -stars) %>% 
      pivot_longer(-c(Category, term)) %>%
      spread(key = term, value = value) %>%
      dplyr::select(-name)
  }

ols_group_full_workflow <-
  function(data, formula = formula) {
    data %>% 
      ols_nest_full_prep() %>% 
      ols_tidy_group_models(formula) %>% 
      ols_pretty_full_results()
  }


