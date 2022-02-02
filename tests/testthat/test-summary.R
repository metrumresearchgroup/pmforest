library(vdiffr)

describe("Column selection, summary", {

  dataDir <- system.file("test-data", package = "pmforest")
  plotData <- readRDS(file.path(dataDir, "plotData.RDS"))
  plotData2 <- readRDS(file.path(dataDir, "plotDataEXP.RDS"))

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
    summarise(med = median(stat, na.rm = T),
              mean = mean(stat, na.rm = T),
              lo  = quantile(stat, lci),
              hi  = quantile(stat, 1-lci))

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


  it("Summary for multiple CI's [PMF-PLOT-018]", {

    sumdat_med <- pmforest:::summarize_data(plotData2,
                                            stat = stat,
                                            covariate = GROUP,
                                            cov_level = LVL,
                                            metagroup = NULL,
                                            nsim = nsim,
                                            statistic="median",
                                            CI=CI)

    sumdat <- plotData2 %>% group_by(nsim, GROUP, LVL) %>%
      summarise(med = median(stat, na.rm = T),
                lo  = quantile(stat, lci),
                hi  = quantile(stat, 1-lci))
    sumDat2 <- sumdat %>%
      group_by(GROUP, LVL) %>%
      summarise(
        med_dot = median(med),
        lo_dot  = quantile(med, lci),
        hi_dot  = quantile(med, 1-lci),
        med_lo  = median(lo),
        lo_lo  = quantile(lo, lci),
        hi_lo  = quantile(lo, 1-lci),
        med_hi  = median(hi),
        lo_hi  = quantile(hi, lci),
        hi_hi  = quantile(hi, 1-lci)
      )

    expect_equal(sumdat_med[[1]]$med_dot, sumDat2$med_dot) # Test medians
  })
})
