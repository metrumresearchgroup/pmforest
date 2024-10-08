% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/helpers-for-tests.R
\name{skip_vdiffr}
\alias{skip_vdiffr}
\title{Skip test if \code{PMF_SKIP_VDIFFR} environment variable set to \code{TRUE}}
\usage{
skip_vdiffr()
}
\description{
Checks for custom \code{PMF_SKIP_VDIFFR} environment variable and skips any tests
that use \code{vdiffr} if set to \code{"true"}. This is useful because
\code{vdiffr::expect_doppleganger()} is very fragile and tends to fail with false
positives in other environments (like CI and \verb{R CMD CHECK}).
}
\details{
Uses \code{isTRUE(as.logical(Sys.getenv("PMF_SKIP_VDIFFR")))} to check. This resolves to \code{TRUE}
if \code{PMF_SKIP_VDIFFR} is set to any of the following:
\itemize{
\item \code{"true"}
\item \code{"TRUE"}
\item \code{"T"}
\item \code{"True"}
}
}
\keyword{internal}
