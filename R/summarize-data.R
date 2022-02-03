#' Summarize Data
#'
#' Summarize input data to prepare for forest plot. See Details.
#'
#' @details
#' **Input Data**
#'
#' Should have 2-5 columns: value, group, and optionally any of
#'   group_level, metagroup, or replicate.
#'
#' **Output Data**
#' The tibble output from this function will have one of two formats, depending on whether
#' `replicate` was passed. To be continued...
#'
#' @param data A dataframe or tibble to summarize. See Details section for required format.
#' @param value is the column to perform calculations on (i.e. median/mean, lower, and upper CI)
#' @param group column that defines subgroups within the data- all subgroups will be shown on the same plot but grouped together.
#' @param group_level (optional) column name that corresponds to y-axis tick labels. If not specified, y-axis will be numbered.
#' @param metagroup (optional) column name that corresponds to metagroups. Similar to facet wrap. Will produce independent plots per metagroup.
#' @param replicate (optional) column name that corresponds to simulation or bootstrap column. If specified, additional CI's of the individual statistics will be drawn.
#' If `replicate` is specified and no caption is set, a default caption will be set. Set `caption` to "" to override this functionality.
#' @param CI is the confidence interval to plot, defaults to `0.95`.
#' @param statistic is the actual statistic to output (i.e. median/mean)
#' @export
summarize_data <- function(
  data,
  value,
  group,
  group_level = NULL,
  metagroup = NULL,
  replicate = NULL,
  CI=0.95,
  statistic = c("median", "mean")
){

  statistic <- match.arg(statistic)
  assert_that(CI > 0 & CI < 1, msg = "`CI` must be between 0 and 1")

  stat_func <- function(x,statistic){
    switch(statistic,
           mean=mean(x, na.rm=TRUE),
           median=median(x, na.rm=TRUE))
  }

  value <- data %>% dplyr::select({{ value }}) %>% names()
  group <- data %>% dplyr::select({{ group }}) %>% names()
  # TODO: some error checking here to make sure something sensible was passed?

  group_level <- data %>% dplyr::select({{ group_level }})
  if(ncol(group_level) == 0){
    group_level <- NULL
  } else {
    group_level <- names(group_level)
  }
  metagroup <- data %>% dplyr::select({{ metagroup }})
  if(ncol(metagroup) == 0){
    metagroup <- NULL
  } else {
    metagroup <- names(metagroup)
  }
  replicate <- data %>% dplyr::select({{ replicate }})
  if(ncol(replicate) == 0){
    replicate <- NULL
  } else {
    replicate <- names(replicate)
  }

  groups <- c(group, group_level, metagroup)
  lci <- (1-CI)/2

  if(is.null(replicate)){
    suppressMessages({
      sum <-
        data %>%
        group_by(across(all_of(groups))) %>%
        summarise(
          mid = stat_func(!!sym(value),statistic),
          lo  = quantile(!!sym(value), lci),
          hi  = quantile(!!sym(value), 1-lci)
        ) %>% ungroup
    })
  }else{
    suppressMessages({
      sum <-
        data %>%
        group_by(across(c(replicate, all_of(groups)))) %>%
        summarise(
          mid = stat_func(!!sym(value),statistic),
          lo  = quantile(!!sym(value), lci),
          hi  = quantile(!!sym(value), 1-lci)
        ) %>% ungroup
      sum <- sum %>%
        group_by(across(all_of(groups))) %>%
        summarise(
          mid_mid = median(mid),
          mid_lo  = quantile(mid, lci),
          mid_hi  = quantile(mid, 1-lci),
          lo_mid  = median(lo),
          lo_lo  = quantile(lo, lci),
          lo_hi  = quantile(lo, 1-lci),
          hi_mid  = median(hi),
          hi_lo  = quantile(hi, lci),
          hi_hi  = quantile(hi, 1-lci)
        ) %>% ungroup
    })
  }

  # rename output grouping columns
  if (!is.null(group)) {
    sum <- rename(sum, group = {{ group }})
  }
  if (!is.null(group_level)) {
    sum <- rename(sum, group_level = {{ group_level }})
  }
  if (!is.null(metagroup)) {
    sum <- rename(sum, metagroup = {{ metagroup }})
  }

  return(sum)
}
