dummy_dummy_nest_full_prep <-
  function(data){
    data %>%
      dplyr::relocate(Date, .after = "Category") %>% 
      group_by(Category) %>% 
      nest()
  }

dummy_dummy_tidy_group_models <-
  function(nested_data, formula = formula){
    nested_data %>% 
      mutate(models = map(data, ~coeftest(lm(formula, data = .), 
                                          vcov = NeweyWest))) %>% 
      mutate(models_coef = map(models, ~tidy(.)))
  }

dummy_dummy_pretty_results <- 
  function(tidy_models){
    tidy_models %>% 
      unnest(cols = models_coef, names_repair = "universal") %>% 
      dplyr::select(Category, term, estimate, p.value) %>% 
      mutate(
        stars = ifelse(p.value < 0.001, "***", ifelse(p.value < 0.01, "**", ifelse(p.value < 0.05, "*", "")))
      ) %>% 
      mutate(
        term = str_replace_all(term, "party_dummy", "Democrat"),
        term = str_replace_all(term, "volatility", "Volatility"),
        term = stri_replace_all_fixed(term, "(Intercept)", "Republican"),
      ) %>% 
      dplyr::select(Category, term, estimate, p.value, stars) %>% 
      rename(
        "Party" = term,
        "Estimate" = estimate,
        "p-value" = p.value
      ) %>% 
      arrange(Category, Party) %>% 
      mutate(across(2, ~strtrim(., 6))) %>% 
      mutate(Estimate = paste0(Estimate, stars)) %>%
      dplyr::select(-`p-value`, -stars) %>% 
      pivot_longer(-c(Category, Party)) %>% 
      spread(key = Party, value = value) %>% 
      dplyr::select(-name) %>% 
      ungroup()
  }

dummy_dummy_full_workflow <- function(formula = formula, data){
  data %>%
    dummy_dummy_nest_full_prep() %>% 
    dummy_dummy_tidy_group_models(formula = formula) %>% 
    dummy_dummy_pretty_results()
}