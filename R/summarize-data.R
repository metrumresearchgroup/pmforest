#' Summarize Data
#'
#' Summarize input data to prepare for passing to [plot_forest()]. Takes a
#' data.frame or tibble, calculates the relevant confidence intervals, and
#' returns a tibble that can be passed directly to [plot_forest()]. See Details
#' section for data specification and format.
#'
#' @details
#' **Input Data**
#'
#' The tibble passed to `data` must be in a "long" format and has 2-5
#' columns: `value`, `group`, and optionally any of `group_level`, `metagroup`,
#' and/or `replicate`. These are each described in detail in the input arguments
#' section.
#'
#' **Output Data**
#' The tibble output from this function has one of two formats, depending on whether
#' `replicate` was passed (details below).
#'
#' Either way, the output tibble has a column named `group`, containing the
#' values in the column you passed to the `group` argument, and optionally analogous columns
#' for `group_level` and `metagroup` if those were passed.
#'
#' * **Without `replicate`** If `replicate` is _not_ passed, the output data has three
#' additional columns `mid`, `lo`, and `hi`, containing the summarized values corresponding
#' to what was passed to `statistic` (`mid`) and `probs` (`lo`/`hi`).
#'
#' * **With `replicate`** If `replicate` _is_ passed, the output data has
#' nine additional columns `mid_mid`, `mid_lo`, `mid_hi`, plus three more each
#' for `lo_*` and `hi_*`, containing the summarized values. In this case, the
#' `mid_mid`, `lo_mid`, and `hi_mid` correspond to the values of the major
#' interval (i.e. the big lines and data point) and the `*_mid`, `*_lo`, and
#' `*_hi` correspond to the values for each minor interval (i.e. the small
#' lines).
#'
#' @param data A dataframe or tibble to summarize. See Details section for required format.
#' @param value name of the column in `data` to perform calculations on (i.e.
#'   median/mean, lower, and upper CI)
#' @param group name of the column in `data` that defines groups within the
#'   data. Often, this will contain the names of the covariates you are grouping
#'   by.
#' @param group_level (optional) name of the column in `data` that contains
#'   subgroups to group by. For example, if your `group` column contains
#'   covariates like `WEIGHT` and `AGE`, this column could contain categories
#'   like `underweight`, `average`, `overweight`, `young`, `mid`, `elderly`, etc.
#' @param metagroup (optional) name of the column in `data` that contains
#'   `metagroups`. Similar to facet wrap, if passed, this will cause
#'   [plot_forest()] to produce independent plots per metagroup.
#' @param replicate (optional) name of the column in `data` that contains to an
#'   index of replicates, for example with multiple simulations or
#'   bootstrapping. If specified, [plot_forest()] will draw additional CI's of
#'   the individual statistics, as small lines above each primary line.
#' @param probs numeric vector of length two, both between 0 and 1, corresponding to
#'   your lower and upper tail probabilities. Defaults to `c(0.05, 0.95)`
#' @param statistic is the actual statistic to output (i.e. median/mean)
#' @param rep_probs same as `probs` but used **only when `replicate` is passed**
#'   for the minor intervals (i.e. the small lines) above the major interval (i.e. the big lines).
#' @param rep_statistic same as `statistic` but used **only when `replicate` is
#'   passed** for the minor intervals (i.e. the small lines) above the major interval (i.e. the big lines).
#' @export
summarize_data <- function(
  data,
  value,
  group,
  group_level = NULL,
  metagroup = NULL,
  replicate = NULL,
  probs = c(0.05, 0.95),
  statistic = c("median", "mean", "geo_mean"),
  rep_probs = c(0.025, 0.975),
  rep_statistic = c("median", "mean", "geo_mean")
){

  statistic <- match.arg(statistic)
  rep_statistic <- match.arg(rep_statistic)
  assert_that(all(probs > 0 & probs < 1), msg = "`probs` must be between 0 and 1")
  assert_that(probs[1] < probs[2], msg = "`probs[1]` must be lower than `probs[2]`")
  assert_that(all(rep_probs > 0 & rep_probs < 1), msg = "`rep_probs` must be between 0 and 1")
  assert_that(rep_probs[1] < rep_probs[2], msg = "`rep_probs[1]` must be lower than `rep_probs[2]`")
  val_col <- data %>% pull({{ value }})
  assert_that(is.numeric(val_col), msg = "`value` must be a numeric column")
  assert_that(all(!is.na(val_col)), msg = "`value` column cannot contain NA's")

  stat_func <- function(x,statistic){
    switch(statistic,
           mean=mean(x),
           median=median(x),
           geo_mean=exp(mean(log(x)))
    )
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
  lci <- probs[1]
  uci <- probs[2]

  if(is.null(replicate)){
    suppressMessages({
      sum <-
        data %>%
        group_by(across(all_of(groups))) %>%
        summarise(
          mid = stat_func(!!sym(value),statistic),
          lo  = quantile(!!sym(value), lci),
          hi  = quantile(!!sym(value), uci)
        ) %>% ungroup
    })
  }else{
    rep_lci <- rep_probs[1]
    rep_uci <- rep_probs[2]
    suppressMessages({
      sum <-
        data %>%
        group_by(across(c(replicate, all_of(groups)))) %>%
        summarise(
          mid = stat_func(!!sym(value),statistic),
          lo  = quantile(!!sym(value), lci),
          hi  = quantile(!!sym(value), uci)
        ) %>% ungroup
      sum <- sum %>%
        group_by(across(all_of(groups))) %>%
        summarise(
          mid_mid = stat_func(mid, rep_statistic),
          mid_lo  = quantile(mid, rep_lci),
          mid_hi  = quantile(mid, rep_uci),
          lo_mid  = stat_func(lo, rep_statistic),
          lo_lo  = quantile(lo, rep_lci),
          lo_hi  = quantile(lo, rep_uci),
          hi_mid  = stat_func(hi, rep_statistic),
          hi_lo  = quantile(hi, rep_lci),
          hi_hi  = quantile(hi, rep_uci)
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
