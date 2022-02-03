#' Function to create forest plots
#'
#' Creates a forest plot with or without confidence intervals
#' Expects..
#' @param data a dataframe or tibble that contains the summarized data you want
#'   to plot. This must be in the same format as the tibble that is output by
#'   [summarize_data()]. See "Output Data" in Details section of [summarize_data()] documentation.
#' @inheritParams summarize_data
#' @param summary_labels (optional) labeler function created using `ggplot2::as_labeller`. Labels for group and metagroup.
#' @param vline_intercept (optional) numeric. Default 0.
#' @param annotate_CI logical. Default `TRUE`. Show a table next to the graph with the numeric values for the confidence interval.
#' @param sigfig numeric. Number of significant digits to round the table values to.
#' @param x_lab string. x-axis label.
#' @param y_lab string. Default is not to label y axis.
#' @param CI_label string. Ignored if `annotate_CI` is `FALSE`.
#' @param caption string. A patchwork styled caption for the overall plot.
#' @param text_size numeric. Text size for labels. Must be at least 3.5
#' @param plot_width numeric. Value between 1 and 12 to denote the ratio of plot : CI table
#' @param shaded_interval numeric vector. Specified as c(lo, hi) for the interval you want to shade over. Default is NULL.
#' @param x_breaks numeric vector of breaks to be used for the x-axis. See scale_x_continuous() for details.
#' @param x_limit numeric vector (c(lo, hi)) specifying the minimum and maximum values to show on the x-axis. See coord_cartesian() for details
#' @param ... additional args passed to `patchwork::wrap_plots()` for metagrouped plots.
#' @param jitter_nsim logical. Whether or not to vertically "jitter" the additional confidence intervals when using the `nsim` argument
#' @export
plot_forest <- function(data,
                        summary_label = NULL,
                        vline_intercept = 0,
                        annotate_CI = TRUE,
                        shaded_interval = NULL,
                        sigfig = 2,
                        x_lab = "Effect",
                        y_lab = NULL,
                        CI_label = NULL,
                        caption = NULL,
                        text_size = 3.5,
                        plot_width = 8,
                        x_breaks = NULL,
                        x_limit = NULL,
                        jitter_nsim = FALSE,
                        ...){

  assert_that(is.numeric(sigfig) & sigfig > 1, msg = "`sigfig` must be a numeric value greater than 1")
  assert_that(text_size >= 3.5, msg = "`text_size` must be at least 3.5")
  assert_that(is_logical(annotate_CI), msg = "`annotate_CI` must be a logical value (T/F)")
  assert_that(is_logical(jitter_nsim), msg = "`jitter_nsim` must be a logical value (T/F)")

  # TODO: this will be refactored once we refactor some of the downstream code
  if (all(VALUE_COLS %in% names(data))) {
    args <- VALUE_COLS
    nsim <- NULL
  } else if (all(VALUE_COLS_NSIM %in% names(data))) {
    args <- VALUE_COLS_NSIM
    nsim <- "nsim" # should find a better way to do this
  } else {
    # TODO: add test for this case
    stop(paste(
      "`data` does not have required columns. Must have grouping columns and either:",
      paste(VALUE_COLS, collapse = ", "),
      "  OR ",
      paste(VALUE_COLS_NSIM, collapse = ", "),
      glue("`data` has columns: {paste(names(data), collapse = ', ')}"),
      sep = "\n"
    ))
  }

  if(!("metagroup" %in% names(data))){
    # no metagroups
    plt <- forest_constructor(
      data = data,
      args = args,
      nsim = nsim,
      summary_label = summary_label,
      vline_intercept = vline_intercept,
      annotate_CI = annotate_CI,
      sigfig = sigfig,
      shaded_interval = shaded_interval,
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
    # with metagroups
    metagroups <- data %>%
      dplyr::select(metagroup) %>%
      unlist(use.names = FALSE) %>%
      unique()

    # TODO: is this still true or should we remove this code?
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
          dplyr::filter(.data$metagroup == .x)

        forest_constructor(
          data = data,
          args = args,
          nsim = nsim,
          summary_label = summary_label,
          vline_intercept = vline_intercept,
          annotate_CI = annotate_CI,
          sigfig = sigfig,
          shaded_interval = shaded_interval,
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
  }

  plt <- patchwork::wrap_plots(plt, ...)

  if (!is.null(caption)) {
    plt <- plt + plot_annotation(caption = caption)
  }

  return(plt)
}
