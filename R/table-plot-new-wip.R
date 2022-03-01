#' table plot
#'
#' Private helper to record and tabulate confidence intervals. Called internally by forest_constructor
#' @inheritParams plot_forest
#' @param plotdata dataframe constructed by forest_constructor
#' @param madata dataframe constructed by forest_constructor
#' @param r,l parameters corresponding to the plot margin for the confidence intervals
#' @keywords internal
table_plot2 <-
  function(plotdata,
           r = 5.5,
           l = 5.5,
           tbl_titles = NULL,
           text_size,
           y_limit,
           y_breaks,
           y_lines
  ) {

    nchar2 <-
      function(x) {
        unlist(sapply(strsplit(x, "\n"), function(x)
          max(nchar(x, keepNA = FALSE))))
      }
    area_per_column <-
      cumsum(c(1, apply(rbind(tbl_titles, plotdata$label), 2, function(x)
        max(round(max(nchar2(
          x
        )) / 100, 2),  0.03))))

    x_limit <- range(area_per_column)

    table <- ggplot(data = plotdata, aes(y = ID, x = 1)) +
      geom_text(
        aes(x = 1, y = ID, label = label),
        size = text_size * 0.8,
        hjust = 0,
        vjust = 0
      ) +
      geom_hline(yintercept = y_lines) +
      scale_y_continuous(name = y_lab,
                         breaks = y_breaks,
                         labels = NULL) +
      coord_cartesian(xlim = x_limit,
                      ylim = y_limit,
                      expand = F) +
      theme_bw() +
      theme(
        text = element_text(size = 1 / 0.352777778 * text_size, lineheight = .25),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none",
        panel.border = element_blank(),
        axis.text.x = element_text(colour = "white"),
        axis.text.y = element_blank(),
        axis.ticks.x = element_line(colour = "white"),
        axis.ticks.y = element_blank(),
        axis.line.x = element_line(colour = "white"),
        axis.line.y = element_blank(),
        plot.margin = margin(
          t = 5.5,
          r = r,
          b = 5.5,
          l = l,
          unit = "pt"
        )
      ) +
      labs(x = "", y = "") #+
      # ggtitle(lab_title$value)


    # aligned
    # p + table + patchwork::plot_layout(widths = c(plot_width, 12-plot_width))
    return(table)

  }
