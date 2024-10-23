csad <-
function (data, market_return) {
  rowSums(abs(data[, -1] - as.numeric(market_return)), na.rm = TRUE) / nrow(data)
}
cssd <-
function (data, market_return) {
  sqrt(rowSums((data[, -1] - as.numeric(market_return)) ^ 2, na.rm = TRUE) / nrow(data - 1))
}


cross_deviations_tbl <-
function(data){
  # Market return
  market_return_matrix <- 
    rowMeans(data[, -1], na.rm = TRUE )
  
  # CSAD and CSSD
  csad_matrix <- csad(data = data, 
                      market_return = market_return_matrix)
  # cssd_matrix <- cssd(data = data[, -1], 
  #                     market_return = market_return_matrix)
  
  # Combined_results
  results_tbl <- 
    bind_cols(
      data %>% dplyr::select(Date),
      market_return_matrix,
      csad_matrix,
      # cssd_matrix 
    ) %>% 
    rename("Mkt" = ...2,  
           "CSAD" = ...3,
           # "CSSD" = ...4
           ) 
  
  # Result
  results_tbl
}
group_cross_deviations_tbl <- 
  function(group_data){
    group_data %>% 
      nest(.by = Crisis) %>% 
      mutate(deviations = map(data, ~cross_deviations_tbl(data = .))) %>% 
      unnest(cols = deviations) %>% 
      dplyr::select(-data)
  }
results_gg <- function(cross_deviations_tbl, variables_color = 2){
  cross_deviations_tbl %>% 
  pivot() %>% 
  mutate(Series = dplyr::recode(Series,
                                  # "CSSD" = "CSSD[t]",
                                  "CSAD" = "CSAD[t]",
                                  "Mkt" = "R[mt]")) %>% 
  fx_recode_plot(variables_color = variables_color)
}
group_results_gg <-  
  function (data, 
            plotname = "%", 
            variables_color = 5
  ) {
    data %>% 
      pivot_longer(-c(Crisis, Date), names_to = "Series", values_to = "Value") %>% 
      mutate(Series = dplyr::recode(Series,
                                    "CSAD" = "CSAD",
                                    "Mkt" = "Mkt Returns")) %>% 
      ggplot(
        aes(x = Date, y = Value, color = Crisis)
      ) +
      geom_line() +
      facet_grid(Series ~ Crisis, 
                 labeller = label_parsed,
                 scale = "free",
                 space = "fixed"
                 ) +
      theme_bw() +
      theme(
        legend.position = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
      ) +
      theme(
        text = element_text(size = 9),
        strip.background = element_rect(colour = "white", fill = "white"),
        axis.text.x = element_text(angle = 90),
        axis.title = element_text(size = 9),
        plot.tag = element_text(size = 9),
        legend.position = "none"
      ) +
      labs(x = "", y = plotname) +
      scale_color_manual(values = pnw_palette("Shuksan2", variables_color))  +
      scale_x_date(date_labels = "%b-%Y " )
  }
