modelsummary_pdf <- function(models = models,
                              coef_map = NULL, 
                              stars = TRUE, 
                              decimals = 2,
                              vcov =  NULL,
                              shape = term + statistic ~ model,
                              variables_omit = "AIC|BIC|RMSE|Log",
                              align = "lccccccc"
) {
  modelsummary(
    models,
    output = "flextable",
    stars = stars,
    fmt = decimals,
    vcov =  vcov,
    gof_omit = variables_omit ,
    coef_map = coef_map,
    shape = shape,
    align = align
  ) %>%
    hline_bottom(part = "footer", border = fp_border(color = "black")) %>% 
    align_text_col(align = "left", header = TRUE, footer = TRUE)
}
