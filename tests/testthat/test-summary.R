library(vdiffr)

describe("Column selection, summary", {

  dataDir <- system.file("test-data", package = "pmforest")
  plotData <- readRDS(file.path(dataDir, "plotData.RDS"))
  plotData2 <- readRDS(file.path(dataDir, "plotDataEXP.RDS"))

  CI <- 0.95
  lci <- (1-CI)/2

  sumdat_med <- summarize_data(
    plotData,
    value = stat,
    group = GROUP,
    group_level = LVL,
    statistic = "median",
    CI = CI
  )

  sumdat_mn <- summarize_data(
    plotData,
    value = stat,
    group = GROUP,
    group_level = LVL,
    statistic = "mean",
    CI = CI
  )

  sumdat <- plotData %>% group_by(GROUP, LVL) %>%
    summarise(med = median(stat, na.rm = T),
              mean = mean(stat, na.rm = T),
              lo  = quantile(stat, lci),
              hi  = quantile(stat, 1-lci))

  it("correct column is summarized [PMF-PLOT-001]", {
    expect_equal(sumdat_med$lo, sumdat$lo) # test lower quartile
    expect_equal(sumdat_med$hi, sumdat$hi) # test upper quartile
  })

  it("Can switch between mean/median [PMF-PLOT-002]", {
    expect_equal(sumdat_med$mid, sumdat$med) # Test medians
    expect_equal(sumdat_mn$mid, sumdat$mean) # Test means
  })

  it("CI is properly calculated [PMF-PLOT-003]", {
    # Add CI guard rail/test
    expect_equal(lci, 0.025)
  })


  it("Summary for multiple CI's [PMF-PLOT-018]", {

    sumdat_med <- summarize_data(
      plotData2,
      value = stat,
      group = GROUP,
      group_level = LVL,
      replicate = nsim,
      statistic = "median",
      CI = CI
    )

    sumdat <- plotData2 %>% group_by(nsim, GROUP, LVL) %>%
      summarise(mid = median(stat, na.rm = T),
                lo  = quantile(stat, lci),
                hi  = quantile(stat, 1-lci))
    sumDat2 <- sumdat %>%
      group_by(GROUP, LVL) %>%
      summarise(
        mid_mid = median(mid),
        mid_lo  = quantile(mid, lci),
        mid_hi  = quantile(mid, 1-lci),
        lo_mid  = median(lo),
        lo_lo  = quantile(lo, lci),
        lo_hi  = quantile(lo, 1-lci),
        hi_mid  = median(hi),
        hi_lo  = quantile(hi, lci),
        hi_hi  = quantile(hi, 1-lci)
      )

    expect_equal(sumdat_med$mid_mid, sumDat2$mid_mid) # Test medians
  })
})
