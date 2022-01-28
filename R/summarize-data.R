#' Summarize Data
#'
#' Private helper to summarize the referenced column. Called internally by plot_forest
#' @inheritParams plot_forest
#' @param r,l parameters corresponding to the plot margin for the confidence intervals
#' @keywords internal
summarize_data <- function(data, stat, covariate, cov_level, metagroup, nsim, CI=CI, statistic){

  stat_func <- function(x,statistic){
    switch(statistic,
           mean=mean(x),
           median=median(x))
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
          dot = as.numeric(sig(stat_func(!!sym(stat),statistic))),
          lo  = as.numeric(sig(quantile(!!sym(stat), lci))),
          hi  = as.numeric(sig(quantile(!!sym(stat), 1-lci)))
        ) %>% ungroup
    })
  }else{
    suppressMessages({
      sum <-
        data %>%
        group_by(across(c(nsim,all_of(groups)))) %>%
        summarise(
          dot = as.numeric(sig(stat_func(!!sym(stat),statistic))),
          lo  = as.numeric(sig(quantile(!!sym(stat), lci))),
          hi  = as.numeric(sig(quantile(!!sym(stat), 1-lci)))
        ) %>% ungroup
      sum <- sum %>%
        group_by(across(all_of(groups))) %>%
        summarise(
          med_dot = as.numeric(sig(median(dot))),
          lo_dot  = as.numeric(sig(quantile(dot, lci))),
          hi_dot  = as.numeric(sig(quantile(dot, 1-lci))),
          med_lo  = as.numeric(sig(median(lo))),
          lo_lo  = as.numeric(sig(quantile(lo, lci))),
          hi_lo  = as.numeric(sig(quantile(lo, 1-lci))),
          med_hi  = as.numeric(sig(median(hi))),
          lo_hi  = as.numeric(sig(quantile(hi, lci))),
          hi_hi  = as.numeric(sig(quantile(hi, 1-lci)))
        ) %>% ungroup
    })
  }

  args <- names(sum)
  args <- args[!grepl(paste0(groups, collapse = "|"), args)]
  return(list(sum,args))
}
