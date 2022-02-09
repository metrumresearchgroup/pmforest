#' @importFrom glue glue
#' @importFrom here here
#' @importFrom readr read_table read_csv
#' @importFrom tibble as_tibble tibble
#' @import rlang
#' @import dplyr
#' @importFrom purrr map flatten
#' @import ggplot2
#' @importFrom gridExtra arrangeGrob
#' @importFrom ggpubr as_ggplot
#' @import patchwork
#' @importFrom stringr str_detect
#' @importFrom utils write.csv
#' @importFrom assertthat assert_that
#' @importFrom pmtables sig
#' @importFrom utils globalVariables
#' @importFrom stats median quantile
#' @importFrom RColorBrewer brewer.pal
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

# column names used with tidyselect syntax throughout
# declaring here to appease R CMD CHECK
utils::globalVariables(c(
  VALUE_COLS,
  VALUE_COLS_NSIM,
  "group",
  "metagroup",
  "x", "y"
))
