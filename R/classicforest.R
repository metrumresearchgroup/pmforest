#' classic forest
#'
#' Private helper to generate the forest plot. Called internally by forest_constructor
#' @inheritParams plot_forest
#' @param plotdata dataframe constructed by forest_constructor
#' @param madata dataframe constructed by forest_constructor
#' @keywords internal
classicforest <- function(plotdata,
                          madata,
                          nsim = NULL,
                          group_level = NULL,
                          summary_label = NULL,
                          annotate_CI = FALSE,
                          shaded_interval = NULL,
                          facet_titles = NULL,
                          vline_intercept = 0,
                          col = "Blues",
                          summary_col = "Blues",
                          tick_col = "firebrick",
                          text_size = 3,
                          x_lab = "Effect",
                          y_lab = NULL,
                          x_limit = NULL,
                          x_breaks = NULL,
                          jitter_reps,
                          shapes,
                          shape_size,
                          ggplot_theme) {

  n <- nrow(plotdata)
  k <- length(levels(plotdata$group))

  # weight of each study used to scale the height of each raindrop

  weight <-
    1 / (plotdata$se ^ 2 + madata$summary_tau2[as.numeric(plotdata$group)])
  plotdata$rel_weight <- weight / sum(weight)


  y_limit <- c(min(plotdata$ID) - 3, max(plotdata$ID) + 4)
  y_tick_names <-
    c(as.vector(group_level), as.vector(summary_label))[order(c(plotdata$ID, madata$ID), decreasing = T)]
  suppressWarnings({
    if(any(is.na(as.numeric(y_tick_names)))){
      y_tick_names[!is.na(as.numeric(y_tick_names))] <- ""
    }
  })
  y_breaks <- sort(c(plotdata$ID, madata$ID), decreasing = T)
  y_lines <- sort(madata$ID, decreasing = T)

  # set limits for the x axis if none are supplied
  if (is.null(x_limit)) {
    x_limit <- if(is.null(nsim)){
      c(range(c(plotdata$x_min, plotdata$x_max))[1] - diff(range(c(
        plotdata$x_min, plotdata$x_max
      ))) * 0.05,
      range(c(plotdata$x_min, plotdata$x_max))[2] + diff(range(c(
        plotdata$x_min, plotdata$x_max
      ))) * 0.05)
    }else{
      c(range(c(plotdata$lo_lo, plotdata$hi_hi))[1] - diff(range(c(
        plotdata$lo_lo, plotdata$hi_hi
      ))) * 0.05,
      range(c(plotdata$lo_lo, plotdata$hi_hi))[2] + diff(range(c(
        plotdata$lo_lo, plotdata$hi_hi
      ))) * 0.05)
    }
  }


  # Set Color palette for shading
  if (all(col %in% c("Blues", "Greys", "Oranges", "Greens", "Reds", "Purples"))) {
    col <-
      unlist(lapply(col, function(x)
        RColorBrewer::brewer.pal(n = 9, name = x)))
  }

  if (all(summary_col %in% c("Blues", "Greys", "Oranges", "Greens", "Reds", "Purples"))) {
    summary_col <-
      unlist(lapply(summary_col, function(x)
        RColorBrewer::brewer.pal(n = 9, name = x)[9]))
  }

  if (length(summary_col) > 1) {
    summary_col <- rep(summary_col, times = 4)
  }


  # Set plot margins. If table is aligned on the left, no y axis breaks and ticks are plotted
  l <- 5.5
  r <- 11
  if (annotate_CI == TRUE) {
    r <- 1
  }

  # workaround for "Undefined global functions or variables" Note in R CMD check while using ggplot2.
  ID <- NULL
  x <- NULL
  y <- NULL
  x_min <- NULL
  x_max <- NULL

  shape_value <- switch(shapes,
                   "square" = 22,
                   "diamond" = 23,
                   "circle" = 21,
                   "triangle" = 24
  )

  # create classic forest plot
  if(is.null(nsim)){
    p <-
      ggplot(data = plotdata, aes(y = ID, x = x, group=factor(group))) +
      geom_vline(xintercept = vline_intercept, linetype = 4) +
      geom_errorbarh(data = plotdata,
                     # col = "black",
                     aes(
                       col = factor(group),
                       xmin = x_min,
                       xmax = x_max,
                       y = ID,
                       height = 0
                     ))
  }else{
    p <-
      ggplot(data = plotdata, aes(y = ID, x = mid_mid, group=factor(group))) +
      geom_vline(xintercept = vline_intercept, linetype = 4) +
      geom_errorbarh(data = plotdata,
                     # col = "black",
                     aes(
                       col = factor(group),
                       xmin = lo_mid,
                       xmax = hi_mid,
                       y = ID,
                       height = 0
                     ))
  }
  if(is.null(y_lab)){
    y_lab <- ""
  }

  p1 <-
    p +
    geom_point(data=plotdata,aes(size = weight, color=factor(group), fill=factor(group)),
               shape = shape_value, size = shape_size, col = "black") +
    geom_hline(yintercept = y_lines) +
    scale_y_continuous(name = y_lab,
                       breaks = y_breaks,
                       labels = y_tick_names) +
    coord_cartesian(xlim = x_limit,
                    ylim = y_limit,
                    expand = F)
  if(!is.null(nsim)){
    # Add geom_point for when the lo's and hi's are the same value
    if(jitter_reps){
      p1 <- p1 +
        geom_point(aes(x=lo_mid, color=factor(group)), position = position_nudge(y = 0.3), size=0.1) +
        geom_linerange(data=plotdata, aes(xmin = lo_lo, xmax = lo_hi, color=factor(group)),
                       position = position_nudge(y = 0.3), size=0.7) +
        geom_point(aes(x=hi_mid, color=factor(group)), position = position_nudge(y = 0.55), size=0.1) +
        geom_linerange(data=plotdata, aes(xmin = hi_lo, xmax = hi_hi, color=factor(group)),
                       position = position_nudge(y = 0.55), size=0.7) +
        geom_point(aes(x=mid_mid, color=factor(group)), position = position_nudge(y = 0.425), size=0.1) +
        geom_linerange(data=plotdata, aes(xmin = mid_lo, xmax = mid_hi, color=factor(group)),
                       position = position_nudge(y = 0.425), size=0.7)
    }else{
      p1 <- p1 +
        geom_point(aes(x=lo_mid, color=factor(group)), position = position_nudge(y = 0.5), size=0.1) +
        geom_linerange(data=plotdata, aes(xmin = lo_lo, xmax = lo_mid, color=factor(group)),
                       position = position_nudge(y = 0.5), size=0.7) +
        geom_point(aes(x=hi_mid, color=factor(group)), position = position_nudge(y = 0.5), size=0.1) +
        geom_linerange(data=plotdata, aes(xmin = hi_lo, xmax = hi_hi, color=factor(group)),
                       position = position_nudge(y = 0.5), size=0.7) +
        geom_point(aes(x=mid_mid, color=factor(group)), position = position_nudge(y = 0.5), size=0.1) +
        geom_linerange(data=plotdata, aes(xmin = mid_lo, xmax = mid_hi, color=factor(group)),
                       position = position_nudge(y = 0.5), size=0.7)
    }

  }

  if (!is.null(shaded_interval)) {
    ymin_box <- min(y_breaks) - abs(min(y_breaks))
    p1 <- p1 + annotate("rect",xmin = min(shaded_interval),
                        xmax = max(shaded_interval),
                        ymin=ymin_box, ymax=Inf, alpha=0.2)
  }

  if (is.null(x_breaks)) {
    p1 <- p1 +
      scale_x_continuous(name = x_lab)
  } else {
    p1 <- p1 +
      scale_x_continuous(breaks = x_breaks,
                         name = x_lab)
  }

  p2 <- p1 +
    ggtitle(facet_titles) +
    scale_size_area(max_size = 3) +
    ggplot_theme +
    theme(
      text = element_text(size = 1 / 0.352777778  * text_size),
      legend.position = "none",
      panel.grid.major.y = element_blank(),
      panel.grid.minor.y = element_blank(),
      panel.grid.major.x = element_line("grey"),
      panel.grid.minor.x = element_line("grey"),
      plot.margin = margin(
        t = 5.5,
        r = r,
        b = 5.5,
        l = l,
        unit = "pt"
      )
    ) #+ scale_fill_manual(values = col)

  p2
}
