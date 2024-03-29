#' @importFrom glue glue
#' @importFrom here here
#' @importFrom readr read_table read_csv
#' @importFrom tibble as_tibble tibble
#' @importFrom rlang is_logical
#' @importFrom dplyr select mutate filter all_of rename pull across arrange
#'   desc group_by summarise ungroup
#' @importFrom purrr map flatten
#' @import ggplot2
#' @importFrom gridExtra arrangeGrob
#' @importFrom grid grobTree
#' @importFrom patchwork wrap_plots plot_annotation plot_layout
#' @importFrom stringr str_detect
#' @importFrom utils write.csv
#' @importFrom assertthat assert_that
#' @importFrom pmtables sig
#' @importFrom utils globalVariables
#' @importFrom stats median quantile
#' @importFrom RColorBrewer brewer.pal
#'
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
