#' Function to create forest plots
#'
#' Creates a forest plot with or without confidence intervals
#' Expects..
#' See params below..
#' @param data a dataframe or tibble that contains the mean values and the lows & highs of the confidence interval
#' @param stat is the column to perform calculations on (i.e. median/mean, lower, and upper CI)
#' @param statistic is the actual statistic to output (i.e. median/mean)
#' @param CI is the confidence interval to plot
#' @param group (optional) column that defines subgroups within the data- all subgroups will be shown on the same plot but grouped together.
#' @param study_labels (optional) column name that corresponds to y-axis tick labels. If not specified, y-axis will be numbered.
#' @param metagroup (optional) column name that corresponds to metagroups. Similar to facet wrap. Will produce independent plots per metagroup.
#' @param nsim (optional) column name that corresponds to simulation or bootstrap column. If specified, additional CI's of the individual statistics will be drawn.
#' If `nsim` is specified and no caption is set, a default caption will be set. Set `caption` to "" to override this functionality.
#' @param summary_labels (optional) labeler function created using `ggplot2::as_labeller`. Labels for group and metagroup.
#' @param vline_intercept (optional) numeric. Default 0.
#' @param annotate_CI logical. Default `TRUE`. Show a table next to the graph with the numeric values for the confidence interval.
#' @param x_lab string. x-axis label.
#' @param y_lab string. Default is not to label y axis.
#' @param CI_label string. Ignored if `annotate_CI` is `FALSE`.
#' @param text_size numeric. Text size for labels. Must be at least 3.5
#' @param plot_width numeric. Value between 1 and 12 to denote the ratio of plot : CI table
#' @param caption string. A patchwork styled caption for the overall plot.
#' @param shaded_interval numeric vector. Specified as c(lo, hi) for the interval you want to shade over. Default is NULL.
#' @param x_breaks numeric vector of breaks to be used for the x-axis. See scale_x_continuous() for details.
#' @param x_limit numeric vector (c(lo, hi)) specifying the minimum and maximum values to show on the x-axis. See coord_cartesian() for details
#' @param ... additional args passed to `patchwork::wrap_plots()` for metagrouped plots.
#' @param jitter_nsim logical. Whether or not to vertically "jitter" the additional confidence intervals when using the `nsim` argument
#' @export
plot_forest <- function(data,
                        stat = NULL,
                        statistic = "median",
                        CI=0.95,
                        group = NULL,
                        study_labels = NULL,
                        metagroup = NULL,
                        nsim = NULL,
                        summary_label = NULL,
                        vline_intercept = 0,
                        annotate_CI = TRUE,
                        shaded_interval = NULL,
                        x_lab = "Effect",
                        y_lab = NULL,
                        CI_label = NULL,
                        text_size = 3,
                        plot_width = 8,
                        caption = NULL,
                        x_breaks = NULL,
                        x_limit = NULL,
                        jitter_nsim = FALSE,
                        ...){

  assert_that(text_size >= 3.5, msg = "'text_size' must be at least 3.5")
  assert_that(is_logical(annotate_CI), msg = "'annotate_CI' must be a logical value (T/F)")
  assert_that(is_logical(jitter_nsim), msg = "'jitter_nsim' must be a logical value (T/F)")

  lst <- summarize_data(data,
                        stat = {{stat}},
                        group = {{group}},
                        study_labels = {{study_labels}},
                        metagroup = {{metagroup}},
                        nsim = {{nsim}},
                        statistic={{statistic}},
                        CI=CI)
  data <- lst[[1]]
  args <- lst[[2]]

  metagroups <- data %>% dplyr::select({{metagroup}})
  if(ncol(data)>=9) nsim <- "nsim" # should find a better way to do this

  if(ncol(metagroups) == 0){

    plt <- forest_constructor(
      data = data,
      args = {{ args }},
      group = {{ group }},
      study_labels = {{ study_labels }},
      nsim = {{ nsim }},
      summary_label = summary_label,
      vline_intercept = vline_intercept,
      annotate_CI = annotate_CI,
      confidence_level = CI,
      shaded_interval = shaded_interval,
      statistic = {{ statistic }},
      x_lab = x_lab,
      y_lab = y_lab,
      plot_width = plot_width,
      CI_label = CI_label,
      text_size = text_size,
      x_limit = x_limit,
      x_breaks = x_breaks,
      jitter_nsim = jitter_nsim
    )

  } else {

    metagroups <- metagroups %>%
      unlist(use.names = FALSE) %>%
      unique()

    # if(!is.null(y_lab)){
    #   message("y_lab is ignored when using metagroup argument. specify y axis labels by including them in summary labels with metagroup = label")
    # }

    if(!is.null(summary_label)){
      metagroup_labels <- purrr::flatten(purrr::map(metagroups, summary_label))
      na_labels <- which(is.na(metagroup_labels))
      metagroup_labels[na_labels] <- metagroups[na_labels]
    } else {
      metagroup_labels <- metagroups
    }

    plt <-
      purrr::map2(metagroups, metagroup_labels, function(.x, .y) {

        data <- data %>%
          dplyr::filter({{ metagroup }} == .x)

        forest_constructor(
          data = data,
          args = {{ args }},
          group = {{ group }},
          study_labels = {{ study_labels }},
          nsim = {{ nsim }},
          summary_label = summary_label,
          vline_intercept = vline_intercept,
          annotate_CI = annotate_CI,
          confidence_level = CI,
          shaded_interval = shaded_interval,
          statistic = {{ statistic }},
          x_lab = x_lab,
          y_lab = y_lab,
          plot_width = plot_width,
          facet_titles = .y,
          CI_label = CI_label,
          text_size = text_size,
          x_limit = x_limit,
          x_breaks = x_breaks,
          jitter_nsim = jitter_nsim
        )

      })

    # plt <- patchwork::wrap_plots(plt, ...) +
    #   plot_annotation(caption = caption)

  }

  plt <- patchwork::wrap_plots(plt, ...)

  if(!is.null(nsim) & is.null(caption)){
    plt <- plt + patchwork::plot_annotation(caption = glue::glue('Lower line represents the median of the summary statistics ({stringr::str_to_title(statistic)}, {(100-CI*100)/2}th and {CI*100 + (100-CI*100)/2}th quantiles).
       Upper lines represent the {CI*100}% CI of the individual statistics.'))
  }else{
    plt <- plt + plot_annotation(caption = caption)
  }
  plt

}
