#' Summarize Data
#'
#' Private helper to summarize the referenced column. Called internally by plot_forest
#' @inheritParams plot_forest
#' @param r,l parameters corresponding to the plot margin for the confidence intervals
#' @keywords internal
summarize_data <- function(data, stat, group, study_labels, metagroup, nsim, CI=CI, statistic){

  stat_func <- function(x,statistic){
    switch(statistic,
           mean=mean(x),
           median=median(x))
  }

  stat <- data %>% dplyr::select({{ stat }}) %>% names()
  group <- data %>% dplyr::select({{ group }})
  if(ncol(group) == 0){
    group <- NULL
  } else {
    group <- names(group)
  }
  study_labels <- data %>% dplyr::select({{ study_labels }})
  if(ncol(study_labels) == 0){
    study_labels <- NULL
  } else {
    study_labels <- names(study_labels)
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

  groups <- c(group, study_labels, metagroup)
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
