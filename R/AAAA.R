#' @importFrom glue glue
#' @importFrom here here
#' @importFrom readr read_table read_csv
#' @importFrom tibble as_tibble tibble
#' @import rlang
#' @import dplyr
#' @import ggplot2
#' @importFrom gridExtra arrangeGrob
#' @importFrom ggpubr as_ggplot
#' @import patchwork
#' @importFrom stringr str_detect
#' @importFrom utils write.csv
#' @importFrom assertthat assert_that
#'
#' @include forest-constructor.R
#' @include table-plot.R
#' @include classicforest.R
#'
NULL


VALUE_COLS <- c(
  "mid",
  "lo",
  "hi"
)

VALUE_COLS_NSIM <- c(
  "mid_mid",
  "mid_lo",
  "mid_hi",
  "lo_mid",
  "lo_lo",
  "lo_hi",
  "hi_mid",
  "hi_lo",
  "hi_hi"
)



