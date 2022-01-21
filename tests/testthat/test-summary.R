library(vdiffr)

dataDir <- here::here("tests",  "test-data")
plotData <- readRDS(file.path(dataDir, "plotData.RDS"))

describe("Column selection, summary", {

  CI <- 0.95

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

  it("correct column is summarized [PMF-PLOT-001]", {

    lci <- (1-CI)/2

    sumdat <- plotData %>% group_by(GROUP, LVL) %>%
      summarise(med = as.numeric(sig(median(stat, na.rm = T))),
                mean = as.numeric(sig(mean(stat, na.rm = T))),
                lo  = as.numeric(sig(quantile(stat, lci))),
                hi  = as.numeric(sig(quantile(stat, 1-lci))))

    expect_equal(sumdat_med[[1]]$lo, sumdat$lo) # test lower quartile
    expect_equal(sumdat_med[[1]]$hi, sumdat$hi) # test upper quartile

  })

  it("Can switch between mean/median [PMF-PLOT-002]", {

    expect_equal(sumdat_med[[1]]$dot, sumdat$med) # Test medians
    expect_equal(sumdat_mn[[1]]$dot, sumdat$mean) # Test means
  })

  it("CI is properly calculated [PMF-PLOT-003]", {

    expect_equal(lci, 0.025)

  })


})
