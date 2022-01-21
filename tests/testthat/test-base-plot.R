specDir <- here::here("tests",  "test-data", "spec")
dataDir <- here::here("tests",  "test-data")

specFile <- yspec::ys_load(file.path(specDir, 'analysis3.yml'))

all_labels <- yspec::ys_get_short(specFile, title_case = TRUE)
all_labels$EGFR <- 'Renal Function'
all_labels$CL <- 'CL (L/hr)'
all_labels$V2 <- 'V (L)'

plot_labels <- as_labeller(unlist(all_labels))

describe("Base plot - one metagroup", {

  plotData <- readRDS(file.path(dataDir, "plotData.RDS"))

  it("Test metagroup (Cl and V2) [PMF-PLOT-005]", {

    plotData2 <- readRDS(file.path(dataDir, "plotData2.RDS"))

    plt <- plot_forest(data = plotData2 ,
                       statistic = "median",
                       CI=0.95,
                       stat = stat,
                       covariate = GROUP,
                       metagroup = param,
                       cov_level = LVL,
                       nrow = 2
    )
    plt
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
    plt
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
    plt
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
    plt

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
    plt

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
    plt

  })

  it("Test breaks and limits of x-axis [PMF-PLOT-015]", {
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
                   annotate_CI=T,
                   nrow = 1) # change nrow to 2 if displaying two parameters
    plt

  })


  it("Modify text size [PMF-PLOT-016]", {
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
                   annotate_CI=T,
                   nrow = 1) # change nrow to 2 if displaying two parameters
    plt

  })

  it("Base plot [PMF-PLOT-0017]", {
    plt <- plot_forest(data = plotData ,
                       statistic = "median",
                       CI=CI,
                       stat = stat,
                       covariate = GROUP,
                       cov_level = LVL,
                       annotate_CI=TRUE)
    plt
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
                   annotate_CI=T,
                   nrow = 1) # change nrow to 2 if displaying two parameters
    plt

  })
})
