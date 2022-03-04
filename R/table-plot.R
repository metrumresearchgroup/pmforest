#' table plot
#'
#' Private helper to record and tabulate confidence intervals. Called internally by forest_constructor
#' @inheritParams plot_forest
#' @param plotdata dataframe constructed by forest_constructor
#' @param madata dataframe constructed by forest_constructor
#' @param r,l parameters corresponding to the plot margin for the confidence intervals
#' @keywords internal
table_plot <-
  function(tbl,
           summary_label,
           ID,
           r = 5.5,
           l = 5.5,
           tbl_titles = NULL,
           plotdata,
           text_size,
           y_limit,
           y_breaks,
           y_lines
           ) {
    # all columns and column names are stacked to a vector
    df_to_vector <- function(df) {
      v <- vector("character", 0)
      for (i in 1:ncol(df))
        v <- c(v, as.vector(df[, i]))
      v
    }
    if (!is.data.frame(tbl)){
      tbl <- data.frame(tbl)
    }

    tbl <-
      data.frame(lapply(tbl, as.character), stringsAsFactors = FALSE)
    if (is.null(tbl_titles)) {
      tbl_titles <- names(tbl)
    }
    v <- df_to_vector(tbl)

    # For study labels with newlines in it, the width of the column is now set according to longest line and not the whole label
    nchar2 <-
      function(x) {
        unlist(sapply(strsplit(x, "\n"), function(x)
          max(nchar(x, keepNA = FALSE))))
      }
    area_per_column <-
      cumsum(c(1, apply(rbind(tbl_titles, tbl), 2, function(x)
        max(round(max(nchar2(
          x
        )) / 100, 2),  0.03))))

    x_values <- area_per_column[1:ncol(tbl)]
    x_limit <- c(1, 1.5) #range(area_per_column)

    lab <- data.frame(
      y = rep(ID, ncol(tbl)),
      x = rep(x_values,
              each = length(ID)),
      value = v,
      stringsAsFactors = FALSE
    )

    lab <- lab %>% arrange(desc(y))
    lab <- lab %>% mutate(
      diff = c(NA, apply(lab[-c(2:3)] , 2 , diff ))
    )

    # Table title - only needed if adding via geom_text
    lab_title <-
      data.frame(
        y = rep(max(plotdata$ID) + text_size - 1, times = length(tbl_titles)),
        x = x_values,
        value = tbl_titles
      )

    # # Add extra space to y_limit depending on number of lines in table title
    num_lines <- stringr::str_count(tbl_titles, "\n") + 1
    if(num_lines>=4){stop("`CI_label` must be less than 4 lines")}
    if(num_lines > 1 & num_lines < 4 ){
      titles <- strsplit(tbl_titles, "\n", fixed=T)[[1]]
      title <- NULL
      for(title.i in titles){
        title <- bquote(atop(.(title)), atop(.(title.i)))
      }
    }else{
      title <- tbl_titles
    }


    # To avoid "no visible binding for global variable" warning for non-standard evaluation
    y <- NULL
    value <- NULL

    table <- ggplot(lab, aes(x = x, y = y, label = value)) +
      geom_text(
        size = text_size * 0.8,
        hjust = 0,
        vjust = 0
      ) +
      # geom_text(
      #   data = lab_title,
      #   aes(x = x, y = y, label = value),
      #   size = text_size,
      #   hjust = 0,
      #   vjust = 0
      # ) +
      coord_cartesian(xlim = x_limit,
                      ylim = y_limit,
                      expand = F) +
      geom_hline(yintercept = y_lines - 0.3) + #max(plotdata$ID) + 2.1) +
      scale_y_continuous(breaks = y_breaks) +
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
      labs(x = "", y = "") +
      ggtitle(title)

    return(table)
  }

