#' format table spacing
#'
#' Private helper to format vertical alignment of table. Called internally by table_plot
#' @inheritParams table_plot
#' @param lab dataframe constructed by table_plot
#' @param title_fmt_val additional space based on number of rows of CI_label
#' @keywords internal

format_table_spacing <- function(lab, text_size, title_fmt_val){

  lab <- lab %>% arrange(desc(y))
  lab <- lab %>% mutate(
    diff = c(NA, apply(lab[-c(2:3)] , 2 , diff ))
  )

  CI_label_adj <- dplyr::case_when(
    title_fmt_val <= 0.5 ~ title_fmt_val/7,
    title_fmt_val > 0.5 ~ title_fmt_val/9.5)


  lab_format <- lab
  for(i in 1:nrow(lab_format)){
    if(i!=1 & lab_format$diff[i]==min(lab_format$diff, na.rm = TRUE)){
      lab_format$y[i:nrow(lab_format)] <- lab_format$y[i:nrow(lab_format)] + 0.25 - (text_size - 3.5)*0.1 - CI_label_adj
    }else{
      lab_format$y[i:nrow(lab_format)] <- lab_format$y[i:nrow(lab_format)] - 0.01 - (text_size - 3.5)*0.05
    }
  }

  return(lab_format)
}
