library(vdiffr)

describe("Column selection, summary", {

  dataDir <- system.file("test-data", package = "pmforest")
  plotData <- readRDS(file.path(dataDir, "plotData.RDS"))
  plotData2 <- readRDS(file.path(dataDir, "plotDataEXP.RDS"))

  lci <- 0.025
  uci <- 0.975

  sumdat_med <- summarize_data(
    plotData,
    value = stat,
    group = GROUP,
    group_level = LVL,
    statistic = "median",
    probs = c(lci, uci)
  )

  sumdat_mn <- summarize_data(
    plotData,
    value = stat,
    group = GROUP,
    group_level = LVL,
    statistic = "mean",
    probs = c(lci, uci)
  )

  sumdat <- plotData %>% group_by(GROUP, LVL) %>%
    summarise(med = median(stat, na.rm = T),
              mean = mean(stat, na.rm = T),
              lo  = quantile(stat, lci),
              hi  = quantile(stat, uci))

  it("correct column is summarized [PMF-PLOT-001]", {
    expect_equal(sumdat_med$lo, sumdat$lo) # test lower quartile
    expect_equal(sumdat_med$hi, sumdat$hi) # test upper quartile
  })

  it("Can switch between mean/median [PMF-PLOT-002]", {
    expect_equal(sumdat_med$mid, sumdat$med) # Test medians
    expect_equal(sumdat_mn$mid, sumdat$mean) # Test means
  })


  it("Summary for multiple CI's [PMF-PLOT-019]", {

    sumdat_med <- summarize_data(
      plotData2,
      value = stat,
      group = GROUP,
      group_level = LVL,
      replicate = nsim,
      statistic = "median",
      probs = c(lci, uci)
    )

    sumdat <- plotData2 %>% group_by(nsim, GROUP, LVL) %>%
      summarise(mid = median(stat, na.rm = T),
                lo  = quantile(stat, lci),
                hi  = quantile(stat, uci))
    sumDat2 <- sumdat %>%
      group_by(GROUP, LVL) %>%
      summarise(
        mid_mid = median(mid),
        mid_lo  = quantile(mid, lci),
        mid_hi  = quantile(mid, uci),
        lo_mid  = median(lo),
        lo_lo  = quantile(lo, lci),
        lo_hi  = quantile(lo, uci),
        hi_mid  = median(hi),
        hi_lo  = quantile(hi, lci),
        hi_hi  = quantile(hi, uci)
      )

    expect_equal(sumdat_med$mid_mid, sumDat2$mid_mid) # Test medians
  })
})
