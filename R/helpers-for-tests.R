#' Skip test if not on Metworx or Drone
#'
#' Checks for custom `PMF_SKIP_VDIFFR` environment variable and skips any tests
#' that use `vdiffr` if set to `"true"`. This is useful because
#' `vdiffr::expect_doppleganger()` is very fragile and tends to fail with false
#' positives in other environments (like CI and `R CMD CHECK`).
#'
#' @details
#' Uses `isTRUE(as.logical(Sys.getenv("PMF_SKIP_VDIFFR")))` to check. This resolves to `TRUE`
#' if `PMF_SKIP_VDIFFR` is set to any of the following:
#'
#' * `"true"`
#' * `"TRUE"`
#' * `"T"`
#' * `"True"`
#'
#' @keywords internal
skip_vdiffr <- function() {
  vdiffr_env_var <- Sys.getenv("PMF_SKIP_VDIFFR")
  if (isTRUE(as.logical(vdiffr_env_var))) {
    testthat::skip(paste0("skipped because PMF_SKIP_VDIFFR=", vdiffr_env_var))
  }
}
