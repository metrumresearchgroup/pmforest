#' Summarize Data
#'
#' Summarize input data to prepare for forest plot. See Details.
#'
#' @details
#' **Input Data**
#'
#' Should have 3-5 columns: stat, covariate,
#'   cov_level, optionally metagroup, optionally nsim
#'
#' **Output Data**
#' The tibble output from this function will have one of two formats, depending on whether
#' `nsim` was passed. To be continued...
#'
#' @inheritParams plot_forest
#' @param data A dataframe or tibble to summarize. See Details section for required format.
#' @param r,l parameters corresponding to the plot margin for the confidence intervals
#' @export
summarize_data <- function(
  data,
  stat,
  covariate,
  cov_level,
  metagroup,
  nsim,
  CI=CI,
  statistic = c("median", "mean")
){

  statistic <- match.arg(statistic)
  assert_that(CI > 0 & CI < 1, msg = "`CI` must be between 0 and 1")

  stat_func <- function(x,statistic){
    switch(statistic,
           mean=mean(x, na.rm=TRUE),
           median=median(x, na.rm=TRUE))
  }

  stat <- data %>% dplyr::select({{ stat }}) %>% names()
  covariate <- data %>% dplyr::select({{ covariate }})
  if(ncol(covariate) == 0){
    covariate <- NULL
  } else {
    covariate <- names(covariate)
  }
  cov_level <- data %>% dplyr::select({{ cov_level }})
  if(ncol(cov_level) == 0){
    cov_level <- NULL
  } else {
    cov_level <- names(cov_level)
  }
  metagroup <- data %>% dplyr::select({{ metagroup }})
  if(ncol(metagroup) == 0){
    metagroup <- NULL
  } else {
    metagroup <- names(metagroup)
  }
  nsim <- data %>% dplyr::select({{ nsim }})
  if(ncol(nsim) == 0){
    nsim <- NULL
  } else {
    nsim <- names(nsim)
  }

  groups <- c(covariate, cov_level, metagroup)
  lci <- (1-CI)/2

  if(is.null(nsim)){
    suppressMessages({
      sum <-
        data %>%
        group_by(across(all_of(groups))) %>%
        summarise(
          mid = stat_func(!!sym(stat),statistic),
          lo  = quantile(!!sym(stat), lci),
          hi  = quantile(!!sym(stat), 1-lci)
        ) %>% ungroup
    })
  }else{
    suppressMessages({
      sum <-
        data %>%
        group_by(across(c(nsim,all_of(groups)))) %>%
        summarise(
          mid = stat_func(!!sym(stat),statistic),
          lo  = quantile(!!sym(stat), lci),
          hi  = quantile(!!sym(stat), 1-lci)
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

  args <- names(sum)
  args <- args[!grepl(paste0(groups, collapse = "|"), args)]
  return(list(sum,args))
}
