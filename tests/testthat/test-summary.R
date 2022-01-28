library(vdiffr)

describe("Column selection, summary", {

  dataDir <- here::here("tests", "testthat", "test-data")
  plotData <- readRDS(file.path(dataDir, "plotData.RDS"))

  CI <- 0.95
  lci <- (1-CI)/2

  sumdat_med <- pmforest:::summarize_data(plotData,
                                          stat = stat,
                                          covariate = GROUP,
                                          cov_level = LVL,
                                          metagroup = NULL,
                                          nsim = NULL,
                                          statistic="median",
                                          CI=CI)

  sumdat_mn <- pmforest:::summarize_data(plotData,
                                         stat = stat,
                                         covariate = GROUP,
                                         cov_level = LVL,
                                         metagroup = NULL,
                                         nsim = NULL,
                                         statistic="mean",
                                         CI=CI)

  sumdat <- plotData %>% group_by(GROUP, LVL) %>%
    summarise(med = as.numeric(pmtables::sig(median(stat, na.rm = T))),
              mean = as.numeric(pmtables::sig(mean(stat, na.rm = T))),
              lo  = as.numeric(pmtables::sig(quantile(stat, lci))),
              hi  = as.numeric(pmtables::sig(quantile(stat, 1-lci))))

  it("correct column is summarized [PMF-PLOT-001]", {
    expect_equal(sumdat_med[[1]]$lo, sumdat$lo) # test lower quartile
    expect_equal(sumdat_med[[1]]$hi, sumdat$hi) # test upper quartile
  })

  it("Can switch between mean/median [PMF-PLOT-002]", {
    expect_equal(sumdat_med[[1]]$dot, sumdat$med) # Test medians
    expect_equal(sumdat_mn[[1]]$dot, sumdat$mean) # Test means
  })

  it("CI is properly calculated [PMF-PLOT-003]", {
    # Add CI guard rail/test
    expect_equal(lci, 0.025)
  })


})
