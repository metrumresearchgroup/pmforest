library(vdiffr)

specDir <- here::here("tests",  "test-data", "spec")
dataDir <- here::here("tests",  "test-data")

specFile <- yspec::ys_load(file.path(specDir, 'analysis3.yml'))

all_labels <- yspec::ys_get_short(specFile, title_case = TRUE)
all_labels$EGFR <- 'Renal Function'
all_labels$CL <- 'CL (L/hr)'
all_labels$V2 <- 'V (L)'

plot_labels <- as_labeller(unlist(all_labels))

describe("Base plots", {

  plotData <- readRDS(file.path(dataDir, "plotData.RDS"))

  it("Test metagroup (Cl and V2) [PMF-PLOT-004]", {

    plotData2 <- readRDS(file.path(dataDir, "plotData2.RDS"))

    plt <- plot_forest(data = plotData2 ,
                       statistic = "median",
                       CI=0.95,
                       stat = stat,
                       covariate = GROUP,
                       metagroup = param,
                       cov_level = LVL,
                       annotate_CI = F
    )
    plt
    expect_doppelganger("Test metagroup (Cl and V2)", plt)

    plt <- plot_forest(data = plotData2 ,
                       statistic = "median",
                       CI=0.95,
                       stat = stat,
                       covariate = GROUP,
                       metagroup = param,
                       cov_level = LVL,
                       annotate_CI = F,
                       nrow = 2
    )

    expect_doppelganger("Test metagroup with nrow (Cl and V2)", plt)

  })

  it("Test metagroup (Cl and V2) with labels [PMF-PLOT-005]", {

    plotData2 <- readRDS(file.path(dataDir, "plotData2.RDS"))

    plt <- plot_forest(data = plotData2 ,
                       statistic = "median",
                       CI=0.95,
                       stat = stat,
                       covariate = GROUP,
                       metagroup = param,
                       cov_level = LVL,
                       annotate_CI = F,
                       nrow = 2,
                       summary_label = plot_labels
    )

    expect_doppelganger("Test metagroup (Cl and V2)", plt)
  })

  it("CI Table - mean [PMF-PLOT-006]", {

    plt <- plot_forest(data = plotData ,
                       statistic = "mean",
                       CI=0.99,
                       stat = stat,
                       covariate = GROUP,
                       metagroup = param,
                       cov_level = LVL,
                       annotate_CI = TRUE
    )

    expect_doppelganger("CI Table", plt)
  })

  it("CI Table - median [PMF-PLOT-007]", {

    plt <- plot_forest(data = plotData ,
                       statistic = "median",
                       CI=0.99,
                       stat = stat,
                       covariate = GROUP,
                       metagroup = param,
                       cov_level = LVL,
                       annotate_CI = TRUE
    )

    expect_doppelganger("CI Table", plt)
  })

  it("Plot/Table width [PMF-PLOT-008]", {

    plt <- plot_forest(data = plotData ,
                       statistic = "median",
                       CI=0.95,
                       stat = stat,
                       covariate = GROUP,
                       metagroup = param,
                       cov_level = LVL,
                       plot_width = 6 # not the default value
    )

    expect_doppelganger("Plot/Table width", plt)
  })

  it("Vertical Intercept [PMF-PLOT-009]", {

    plt <- plot_forest(data = plotData ,
                       statistic = "median",
                       CI=0.95,
                       stat = stat,
                       covariate = GROUP,
                       metagroup = param,
                       cov_level = LVL,
                       vline_intercept = 1
    )

    expect_doppelganger("Vertical Intercept", plt)
  })

  it("shaded interval displays over correct range [PMF-PLOT-010]", {
    plt <- plot_forest(data = plotData ,
                       statistic = "median",
                       CI=0.95,
                       stat = stat,
                       covariate = GROUP,
                       metagroup = param,
                       cov_level = LVL,
                       shaded_interval = c(0.8,1.25),
    )

    expect_doppelganger("shaded interval", plt)
  })

  it("update labels via yaml file [PMF-PLOT-011]", {
    plt <- plot_forest(data = plotData ,
                       statistic = "median",
                       CI=0.95,
                       stat = stat,
                       covariate = GROUP,
                       cov_level = LVL,
                       summary_label = plot_labels
    )

    expect_doppelganger("update labels via yaml file", plt)
  })

  it("Axis labels and captions [PMF-PLOT-014]", {
    plt <- plot_forest(data = plotData ,
                       statistic = "median",
                       CI=0.95,
                       stat = stat,
                       covariate = GROUP,
                       cov_level = LVL,
                       x_lab = "Fraction and 95% CI \nRelative to Reference",
                       CI_label = "Median [95% CI]",
                       caption = "The shaded area corresponds
                                  to the interval (0.8, 1.25)"
    )

    expect_doppelganger("Axis labels and captions", plt)
  })

  it("Test breaks and limits of x-axis [PMF-PLOT-015]", {
    plt <- plot_forest(data = plotData ,
                       statistic = "median",
                       CI=0.95,
                       stat = stat,
                       covariate = GROUP,
                       metagroup = param,
                       cov_level = LVL,
                       x_breaks = c(0.4,0.6, 0.8, 1, 1.2, 1.4,1.6),
                       x_limit = c(0.4,1.45),
    )

    expect_doppelganger("Test breaks and limits of x-axis", plt)
  })


  it("Modify text size [PMF-PLOT-016]", {
    plt <- plot_forest(data = plotData ,
                       statistic = "median",
                       CI=0.95,
                       stat = stat,
                       covariate = GROUP,
                       metagroup = param,
                       cov_level = LVL,
                       text_size = 4
    )

    expect_doppelganger("Modify text size", plt)
  })

  it("Base plot [PMF-PLOT-0017]", {
    plt <- plot_forest(data = plotData ,
                       statistic = "median",
                       CI=0.95,
                       stat = stat,
                       covariate = GROUP,
                       cov_level = LVL
    )

    expect_doppelganger("Base plot", plt)
  })

  it("Full Test [PMF-PLOT-0018]", {
    plt <- plot_forest(data = plotData ,
                       statistic = "median",
                       CI=0.95,
                       stat = stat,
                       covariate = GROUP,
                       metagroup = param,
                       cov_level = LVL,
                       shaded_interval = c(0.8,1.25),
                       summary_label = plot_labels,
                       text_size = 3.5,
                       vline_intercept = 1,
                       x_lab = "Fraction and 95% CI \nRelative to Reference",
                       CI_label = "Median [95% CI]",
                       caption = "The shaded area corresponds
                   to the interval (0.8, 1.25)",
                   plot_width = 8, # out of 12
                   x_breaks = c(0.4,0.6, 0.8, 1, 1.2, 1.4,1.6),
                   x_limit = c(0.4,1.45),
                   annotate_CI=T
    )

    expect_doppelganger("Full Test", plt)
  })
})
