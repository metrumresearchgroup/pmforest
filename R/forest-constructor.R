#' Forest Constructor
#'
#' Private helper to manipulate data and organize the structure of the forest plot and CI table
#' @inheritParams plot_forest
#' @param args mean/median, low CI, upper CI relating to the selected reference column
#' @keywords internal
forest_constructor <- function(data,
                               args,
                               covariate = NULL,
                               cov_level = NULL,
                               nsim = NULL,
                               summary_label = NULL,
                               vline_intercept = 0,
                               annotate_CI = TRUE,
                               confidence_level,
                               shaded_interval = NULL,
                               statistic = NULL,
                               x_lab = "Effect",
                               y_lab = NULL,
                               plot_width,
                               facet_titles = NULL,
                               CI_label = NULL,
                               table_layout = NULL,
                               text_size = 3,
                               x_limit = NULL,
                               x_breaks = NULL,
                               jitter_nsim) {
  # Handle input object
  if (missing(data)) {
    stop("argument data is missing, with no default.")
  }

  forest_data <- data %>% dplyr::select({
    {
      args
    }
  })

  covariate <- data %>% dplyr::select({{ covariate }})
  if(ncol(covariate) == 0){
    covariate <- NULL
  } else {
    covariate <- covariate %>%
      unlist(use.names = FALSE)
  }

  cov_level <- data %>% dplyr::select({{ cov_level }})
  if(ncol(cov_level) == 0){
    cov_level <- NULL
  } else {
    cov_level <- cov_level %>%
      unlist(use.names = FALSE)
  }

  if(!is.null(summary_label)){
    summary_label <- summary_label(levels(factor(covariate))) %>%
      unlist(use.names = FALSE)
  }

  # if (!ncol(forest_data) %in% c(2, 3) & is.null(nsim)) {
  #   stop('number of data args must be equal to 2 or 3, see help')
  # }
  # if (!ncol(forest_data) %in% c(9) & !is.null(nsim)) {
  #   stop('number of data args must be equal to 9 if nsim column is specified, see help')
  # }

  # check if a data.frame or matrix with at least two columns is supplied
  if ((is.data.frame(forest_data) || is.matrix(forest_data))) {
    # check if there are missing values
    if (sum(is.na(forest_data)) != 0) {
      warning(
        "The effect sizes or standard errors contain missing values, only complete cases are used."
      )
      cov_level <-
        cov_level[stats::complete.cases(forest_data)]

      forest_data <-
        forest_data[stats::complete.cases(forest_data),]
    }


    # check if input is numeric
    if (sum(apply(forest_data, 2, is.numeric)) != ncol(forest_data)) {
      stop("All input arguments have to be numeric columns")
    }

  } else {
    stop("Unknown input argument")
  }


  n <- nrow(forest_data)

  if (!is.null(covariate) && !is.factor(covariate)) {
    covariate <- as.factor(covariate)
  }
  # check if covariate vector has the right length
  if (!is.null(covariate) && (length(covariate) != n)) {
    warning(
      "length of supplied covariate vector does not correspond to the number of studies; covariate argument is ignored"
    )
    covariate <- NULL
  }

  # if no covariate argument is supplied, use all cases
  if (is.null(covariate)) {
    covariate <- factor(rep(1, times = n))
  }

  # drop unused levels of covariate factor
  covariate <- droplevels(covariate)
  k <- length(levels(covariate))

  forest_data$covariate <- covariate

  if (is.null(cov_level) || length(cov_level) != n) {
    if (!is.null(cov_level) && length(cov_level) != n) {
      warning("Argument cov_level has wrong length and is ignored.")
    }
    cov_level <- 1:n
  }

  # if not exactly one name for every subgroup is supplied the default is used
  if (is.null(summary_label) || length(summary_label) != k) {
    if (!is.null(summary_label) && length(summary_label) != k) {
      warning("Argument summary_label has wrong length and is ignored.")
    }
    if (k != 1) {
      summary_label <- levels(covariate)
    } else {
      summary_label <- "Summary"
    }
  }

  ids <- function(covariate, n) {
    k <- length(levels(covariate))
    ki_start <- cumsum(c(3, as.numeric(table(covariate))[-k] + 2))
    ki_end <- ki_start + as.numeric(table(covariate)) - .5
    study_IDs <- numeric(n)
    for (i in 1:k) {
      study_IDs[covariate == levels(covariate)[i]] <- ki_start[i]:ki_end[i]
    }
    summary_IDs <- ki_end + 1 #.5
    data.frame("ID" = -((n + 3 * k + 2) - c(study_IDs, summary_IDs)),
               "type" = factor(c(
                 rep("study", times = length(study_IDs)),
                 rep ("summary", times = length(summary_IDs))
               )))
  }

  ID <- ids(covariate, n = n)

  madata <- data.frame(
    "summary_es" = rep(0, k),
    "summary_se" = rep(0, k),
    "summary_tau2" = rep(0, k),
    "ID" = ID$ID[ID$type == "summary"]
  )

  if(is.null(nsim)){
    es <- forest_data[, 1]
    lo <- forest_data[, 2]
    hi <- forest_data[, 3]

    plotdata <- data.frame(
      "x" = unlist(es, use.names = FALSE),
      "x_min" = unlist(lo, use.names = FALSE),
      "x_max" = unlist(hi, use.names = FALSE),
      "se" = rep(1, nrow(forest_data)),
      "ID" = ID$ID[ID$type == "study"],
      "labels" = cov_level,
      "covariate" = covariate
    )
  }else{
    med_es <- forest_data[, 1]
    lo_es <- forest_data[, 2]
    hi_es <- forest_data[, 3]
    med_lo <- forest_data[, 4]
    lo_lo <- forest_data[, 5]
    hi_lo <- forest_data[, 6]
    med_hi <- forest_data[, 7]
    lo_hi <- forest_data[, 8]
    hi_hi <- forest_data[, 9]

    plotdata <- data.frame(
      "med_dot" = unlist(med_es, use.names = FALSE),
      "lo_dot" = unlist(lo_es, use.names = FALSE),
      "hi_dot" = unlist(hi_es, use.names = FALSE),
      "med_lo" = unlist(med_lo, use.names = FALSE),
      "lo_lo" = unlist(lo_lo, use.names = FALSE),
      "hi_lo" = unlist(hi_lo, use.names = FALSE),
      "med_hi" = unlist(med_hi, use.names = FALSE),
      "lo_hi" = unlist(lo_hi, use.names = FALSE),
      "hi_hi" = unlist(hi_hi, use.names = FALSE),
      "se" = rep(1, nrow(forest_data)),
      "ID" = ID$ID[ID$type == "study"],
      "labels" = cov_level,
      "covariate" = covariate
    )
  }


  args <- c(
    list(
      plotdata = plotdata,
      madata = madata,
      nsim = nsim,
      cov_level = cov_level,
      summary_label = summary_label,
      annotate_CI = annotate_CI,
      shaded_interval = shaded_interval,
      statistic = statistic,
      vline_intercept = vline_intercept,
      confidence_level = confidence_level,
      facet_titles = facet_titles,
      col = "Blues",
      summary_col = "Blues",
      text_size = text_size,
      x_lab = x_lab,
      y_lab = y_lab,
      x_limit = x_limit,
      x_breaks = x_breaks,
      jitter_nsim = jitter_nsim
    )
  )

  p <- do.call(classicforest, args)


  if (annotate_CI == TRUE) {
    # set limits for the y axis of the table plots

    y_limit <- c(min(plotdata$ID) - 3, max(plotdata$ID) + text_size)

    # set table headers

    if (is.null(CI_label)) {
      table_headers_right <- x_lab
    } else{
      table_headers_right <- CI_label
    }

    if(is.null(nsim)){
      x_hat <- plotdata$x
      lb <- plotdata$x_min
      ub <- plotdata$x_max
    }else{
      x_hat <- plotdata$med_dot
      lb <- plotdata$med_lo
      ub <- plotdata$med_hi
    }
    lb <- format(round(lb, 2), nsmall = 2)
    ub <- format(round(ub, 2), nsmall = 2)
    x_hat <- format(round(x_hat, 2), nsmall = 2)

    CI <-
      c(paste(x_hat, " [", lb, ", ", ub, "]", sep = ""), summary_label)
    CI_label <- data.frame(CI = CI, stringsAsFactors = FALSE)

    # Ensure alignment of CI table with forest plot tick marks
    y_breaks <- ggplot_build(p)$layout$panel_params[[1]]$y$breaks

    spacer <- ifelse(is.null(p$labels$title),TRUE,FALSE)

    table_CI <-
      table_plot(
        tbl = CI_label,
        summary_label = summary_label,
        ID = c(plotdata$ID, madata$ID),
        l = 0,
        r = 11,
        tbl_titles = table_headers_right,
        plotdata=plotdata,
        text_size=text_size,
        y_limit=y_limit,
        y_breaks=y_breaks,
        spacer=spacer
      )

    layout_matrix <- matrix(c(rep(1,plot_width), rep(2,(12-plot_width))), nrow = 1)
    p2 <- gridExtra::arrangeGrob(p, table_CI, layout_matrix = layout_matrix)
  } else {
    table_CI <- NULL
    p2 <- gridExtra::arrangeGrob(p)
  }

  ggpubr::as_ggplot(p2)

}

