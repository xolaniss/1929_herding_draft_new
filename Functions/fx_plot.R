fx_plot <-
function (data, plotname = " ", variables_color = 12) {
  ggplot(
    pivot_longer(data,-Date, names_to = "Series", values_to = "Value"),
    aes(x = Date, y = Value, col = Series)
  ) +
    geom_line() +
    facet_wrap (. ~ Series, scale = "free") +
    theme_bw() +
    theme(
      legend.position = "none",
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank()
    ) +
    theme(
      text = element_text(size = 8),
      strip.background = element_rect(colour = "white", fill = "white"),
      axis.text.x = element_text(angle = 90),
      axis.title = element_text(size = 8),
      plot.tag = element_text(size = 8)
    ) +
    labs(x = "", y = plotname) +
    scale_color_manual(values = pnw_palette("Shuksan2", variables_color))
}

pivot <- function(data){
  result <- 
  pivot_longer(data,-Date, names_to = "Series", values_to = "Value") 
  result
}

fx_recode_plot <-
  function (data, 
            plotname = " ", 
            variables_color = 12, 
            ncol = NULL,
            nrow = NULL
            ) {
    
    crisis_tbl = tibble(
      "recession_start" = c(as.POSIXct("1929-01-01"), 
                            as.POSIXct("1997-01-01"),
                            as.POSIXct("2007-01-01"),
                            as.POSIXct("2020-01-01")
      ),
      "recession_end" = c(as.POSIXct("1939-12-31"), 
                          as.POSIXct("2003-12-31"),
                          as.POSIXct("2009-12-31"),
                          as.POSIXct("2020-12-31")
      )
    )    
    
    ggplot(
      data,
      aes(x = Date, y = Value, color = Series)
    ) +
      # geom_rect(
      #   data = crisis_tbl,
      #   inherit.aes = F,
      #   aes(xmin=recession_start, xmax=recession_end, ymin=-Inf, ymax=Inf), 
      #   alpha=0.5, 
      #   fill = "grey70"
      # )  +
      geom_line() +
      facet_wrap (. ~ Series, scale = "free", ncol = ncol, nrow = nrow, labeller = label_parsed) +
      theme_bw() +
      theme(
        legend.position = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
      ) +
      theme(
        text = element_text(size = 8),
        strip.background = element_rect(colour = "white", fill = "white"),
        axis.text.x = element_text(angle = 90),
        axis.title = element_text(size = 8),
        plot.tag = element_text(size = 8),
        legend.position = "none"
      ) +
      labs(x = "", y = plotname) +
      scale_color_manual(values = pnw_palette("Shuksan2", variables_color))
  
}

fx_recode_group_plot <-
  function (data, 
            plotname = " ", 
            variables_color = 12, 
            ncol = NULL,
            nrow = NULL, 
            color = Category,
            group = Category) {
    ggplot(
      data,
      aes(x = Date, y = Value, color = {{ color }}, group = {{ group }})
    ) +
      geom_line() +
      facet_wrap (. ~ Series, scale = "free", labeller = label_parsed, ncol = ncol, nrow = nrow) +
      theme_bw() +
      theme(
        legend.position = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
      ) +
      theme(
        text = element_text(size = 8),
        strip.background = element_rect(colour = "white", fill = "white"),
        axis.text.x = element_text(angle = 90),
        axis.title = element_text(size = 8),
        plot.tag = element_text(size = 8),
        legend.position = "bottom"
      ) +
      labs(x = "", y = plotname, color = NULL) +
      scale_color_manual(values = pnw_palette("Shuksan2", variables_color))
}


fx_nopivot_plot <-
  function (data, plotname = " ", variables_color = 12, scale = "fixed") {
    ggplot(
      data = data,
      aes(x = Date, y = Value, fill = Series)
    ) +
      geom_area() +
      facet_wrap (. ~ Series, scale = scale, labeller = label_parsed) +
      theme_bw() +
      theme(
        legend.position = "none",
        legend.key.height= unit(0.5, 'cm'),
        legend.key.width= unit(0.5, 'cm'),
        legend.text = element_text(size=6),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
      ) +
      theme(
        text = element_text(size = 8),
        strip.background = element_rect(colour = "white", fill = "white"),
        axis.text.x = element_text(angle = 90),
        axis.title = element_text(size = 8),
        plot.tag = element_text(size = 8)
      ) +
      labs(x = "", y = plotname) +
      scale_fill_manual(name = "", values = pnw_palette("Shuksan2", variables_color))
  }
