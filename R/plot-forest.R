#' Create forest plots
#'
#' Creates a forest plot with or without confidence intervals.
#' See [the "Getting Started" vignette](https://metrumresearchgroup.github.io/pmforest/articles/getting-started.html#summarize-data)
#' for usage and examples.
#' @param data a dataframe or tibble that contains the summarized data you want
#'   to plot. This must be in the same format as the tibble that is output by
#'   [summarize_data()]. See "Output Data" in Details section of [summarize_data()] documentation.
#' @param summary_label (optional) labeler function created using `ggplot2::as_labeller`. Labels for group and metagroup.
#' @param vline_intercept (optional) numeric. Default 0.
#' @param annotate_CI logical. Default `TRUE`. Show a table next to the graph with the numeric values for the confidence interval.
#' @param digits numeric. Number of significant digits to round the table values to. Passed through to [pmtables::sig()]
#' @param maxex numeric. Maximum number of significant digits before moving to scientific notation. Passed through to [pmtables::sig()].
#' @param x_lab string. x-axis label.
#' @param y_lab string. Default is not to label y axis.
#' @param CI_label string. Label above the CI table. Ignored if `annotate_CI` is `FALSE`.
#' @param CI_bracket_open string. Denotes whether to use brackets (`[`) or parentheses (`(`) for the **opening** interval. Ignored if `annotate_CI` is `FALSE`.
#' @param CI_bracket_close string. Denotes whether to use brackets (`]`) or parentheses (`)`) for the **closing** interval. Ignored if `annotate_CI` is `FALSE`.
#' @param caption string. A patchwork styled caption for the overall plot.
#' @param text_size numeric. Text size for labels. Must be at least 3.5
#' @param plot_width numeric. Value between 1 and 12 to denote the ratio of plot : CI table
#' @param shaded_interval numeric vector. Specified as c(lo, hi) for the interval you want to shade over. Default is NULL.
#' @param x_breaks numeric vector of breaks to be used for the x-axis. See scale_x_continuous() for details.
#' @param x_limit numeric vector (c(lo, hi)) specifying the minimum and maximum values to show on the x-axis. See coord_cartesian() for details
#' @param ... additional args passed to `patchwork::wrap_plots()` for metagrouped plots.
#' @param jitter_reps logical. Whether or not to vertically "jitter" the
#'   additional confidence intervals when using multiple replicates (i.e. when
#'   input data has 9 numeric columns instead of 3).
#' @param shapes string. Denotes the shape of the mean/median value, passed to [ggplot2::geom_point()].
#'   Can be one of the following: "square", "diamond", "circle", or "triangle".
#' @param shape_size numeric. Value between 1 and 4 that denotes the relative size of the `shape` argument. Defaults to 3.5.
#' @param ggplot_theme a ggplot theme. Note that some legend options, text size, and grid elements may be overwritten.
#'   See [ggplot2::theme()] for details.
#' @export
plot_forest <- function(data,
                        summary_label = NULL,
                        vline_intercept = 0,
                        annotate_CI = TRUE,
                        shaded_interval = NULL,
                        digits = 3,
                        maxex = NULL,
                        x_lab = "Effect",
                        y_lab = NULL,
                        CI_label = NULL,
                        CI_bracket_open = c("[", "("),
                        CI_bracket_close = c("]", ")"),
                        caption = NULL,
                        text_size = 3.5,
                        plot_width = 8,
                        x_breaks = NULL,
                        x_limit = NULL,
                        jitter_reps = FALSE,
                        shapes = c("diamond", "square", "circle", "triangle"),
                        shape_size = 3.5,
                        ggplot_theme = ggplot2::theme_bw(),
                        ...){

  assert_that(is.numeric(digits) & digits > 1, msg = "`digits` must be a numeric value greater than 1")
  if (!is.null(maxex)) assert_that(is.numeric(maxex) & maxex > 1, msg = "`maxex` must be a numeric value greater than 1")
  assert_that(text_size >= 3.5, msg = "`text_size` must be at least 3.5")
  assert_that((is.character(caption) | is.null(caption)), msg = "`caption` must be a character scalar.")
  assert_that(is_logical(annotate_CI), msg = "`annotate_CI` must be a logical value (T/F)")
  assert_that(is_logical(jitter_reps), msg = "`jitter_reps` must be a logical value (T/F)")
  assert_that(all(class(ggplot_theme) == c("theme", "gg")), msg = "`ggplot_theme` must be a ggplot theme. See `?ggplot2::theme` for details.")
  shapes <- match.arg(shapes)
  assert_that(is.numeric(shape_size) & shape_size >= 1 & shape_size <= 4, msg = "`shape_size` must be a numeric value between 1 and 4")
  CI_bracket_open <- match.arg(CI_bracket_open)
  CI_bracket_close <- match.arg(CI_bracket_close)

  # TODO: this will be refactored once we refactor some of the downstream code
  if (all(VALUE_COLS %in% names(data))) {
    args <- VALUE_COLS
    nsim <- NULL
  } else if (all(VALUE_COLS_NSIM %in% names(data))) {
    args <- VALUE_COLS_NSIM
    nsim <- "nsim" # should find a better way to do this
  } else {
    stop(paste(
      "`data` does not have required columns. Must have grouping columns and either:",
      paste(VALUE_COLS, collapse = ", "),
      "  OR ",
      paste(VALUE_COLS_NSIM, collapse = ", "),
      glue("`data` has columns: {paste(names(data), collapse = ', ')}"),
      sep = "\n"
    ))
  }

  # check grouping columns
  if(!("group" %in% names(data))){
    stop(paste(
      "`data` does not have required columns. Must have column labeled `group`.",
      glue("`data` has columns: {paste(names(data), collapse = ', ')}"),
      sep = "\n"
    ))
  }

  ignored_cols <- names(data)[!(names(data) %in% c("group", "group_level", "metagroup", args))]
  if(length(ignored_cols) > 0){
    message(paste(
      glue("`data` has extra columns that will be ignored:  {paste(ignored_cols, collapse = ', ')}"),
      "If these were intended to be grouping columns, they must be named either `group`, `group_level`, or `metagroup`.",
      "See `?summarize_data` for more detail.",
      sep = "\n"
    ))
  }

  # create the plot
  if(!("metagroup" %in% names(data))){
    # no metagroups
    plt <- forest_constructor(
      data = data,
      args = args,
      nsim = nsim,
      summary_label = summary_label,
      vline_intercept = vline_intercept,
      annotate_CI = annotate_CI,
      digits = digits,
      maxex = maxex,
      shaded_interval = shaded_interval,
      x_lab = x_lab,
      y_lab = y_lab,
      plot_width = plot_width,
      facet_titles = "",
      CI_label = CI_label,
      CI_bracket_open = CI_bracket_open,
      CI_bracket_close = CI_bracket_close,
      text_size = text_size,
      x_limit = x_limit,
      x_breaks = x_breaks,
      jitter_reps = jitter_reps,
      shapes = shapes,
      shape_size = shape_size,
      ggplot_theme = ggplot_theme
    )

  } else {
    # with metagroups
    metagroups <- data %>%
      dplyr::select(metagroup) %>%
      unlist(use.names = FALSE) %>%
      unique()

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
          digits = digits,
          maxex = maxex,
          shaded_interval = shaded_interval,
          x_lab = x_lab,
          y_lab = y_lab,
          plot_width = plot_width,
          facet_titles = .y,
          CI_label = CI_label,
          CI_bracket_open = CI_bracket_open,
          CI_bracket_close = CI_bracket_close,
          text_size = text_size,
          x_limit = x_limit,
          x_breaks = x_breaks,
          jitter_reps = jitter_reps,
          shapes = shapes,
          shape_size = shape_size,
          ggplot_theme = ggplot_theme
        )

      })
  }

  plt <- patchwork::wrap_plots(plt, ...)

  if (!is.null(caption)) {
    plt <- plt + plot_annotation(caption = caption)
  }

  return(plt)
}
