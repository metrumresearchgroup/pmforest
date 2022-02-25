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
           y_breaks
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
    #area_per_column <- cumsum(c(1, apply(rbind(tbl_titles, tbl), 2, function(x) max(round(max(nchar(x, keepNA = FALSE))/100, 2),  0.03))))

    x_values <- area_per_column[1:ncol(tbl)]
    x_limit <- range(area_per_column)

    lab <- data.frame(
      y = rep(ID, ncol(tbl)),
      x = rep(x_values,
              each = length(ID)),
      value = v,
      stringsAsFactors = FALSE
    )

    # Table title
    lab_title <-
      data.frame(
        y = rep(max(plotdata$ID) + text_size - 1, times = length(tbl_titles)),
        x = x_values,
        value = tbl_titles
      )

    # Add extra space to y_limit depending on number of lines in table title
    num_lines <- stringr::str_count(lab_title$value, "\n") + 1
    if(num_lines>=4){stop("`CI_label` must be less than 4 lines")}
    title_fmt_val <- dplyr::case_when(
      num_lines <= 1 ~ 0,
      num_lines == 2 ~ 0.5,
      num_lines > 2 ~ num_lines - 1.5)
    y_limit[2] <- y_limit[2] + title_fmt_val

    # Fix alignment based on other args
    lab <- format_table_spacing(lab, text_size, title_fmt_val)

    # To avoid "no visible binding for global variable" warning for non-standard evaluation
    y <- NULL
    value <- NULL

    spacer_val <- dplyr::case_when(
      num_lines <= 2 ~ title_fmt_val,
      num_lines > 2 ~ (title_fmt_val - 0.2))

    table <- ggplot(lab, aes(x = x, y = y, label = value)) +
      geom_text(
        size = text_size * 0.8,
        hjust = 0,
        vjust = 0,
        # lineheight = .25,
        nudge_y = -1.5 + (text_size - 3.5) + spacer_val
      ) +
      geom_text(
        data = lab_title,
        aes(x = x, y = y, label = value),
        size = text_size,
        hjust = 0,
        vjust = 0
      ) +
      coord_cartesian(xlim = x_limit,
                      ylim = y_limit,
                      expand = F) +
      geom_hline(yintercept = lab_title$y - 0.3) + #max(plotdata$ID) + 2.1) +
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
      labs(x = "", y = "")

    return(table)
  }

