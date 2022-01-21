specDir <- here::here("tests",  "test-data", "spec")
dataDir <- here::here("tests",  "test-data")

specFile <- yspec::ys_load(file.path(specDir, 'analysis3.yml'))

all_labels <- yspec::ys_get_short(specFile, title_case = TRUE)
all_labels$EGFR <- 'Renal Function'
all_labels$CL <- 'CL (L/hr)'
all_labels$V2 <- 'V (L)' # uncomment this line if using V2

plot_labels <- as_labeller(unlist(all_labels))

describe("Multiple simulations [PMF-PLOT-012]", {

  plotData <- readRDS(file.path(dataDir, "plotDataEXP.RDS"))

  plt <- plot_forest(data = plotData,
                     statistic = "mean",
                     CI=0.95,
                     stat = stat,
                     covariate = GROUP,
                     nsim = nsim,
                     cov_level = LVL,
                     shaded_interval = c(0.8,1.25),
                     summary_label = plot_labels,
                     text_size = 3.5,
                     vline_intercept = 1,
                     x_lab = "Fraction and 95% CI \nRelative to Reference",
                     CI_label = "Median [95% CI]"
  )

  plt


})




describe("Multiple CI's with jitter [PMF-PLOT-013]", {

  plotData <- readRDS(file.path(dataDir, "plotDataEXP.RDS")) %>% mutate(stat = stat + (nsim/300))

  plt <- plot_forest(data = plotData,
                     statistic = "mean",
                     CI=0.95,
                     stat = stat,
                     covariate = GROUP,
                     metagroup = param,
                     nsim = nsim,
                     cov_level = LVL,
                     shaded_interval = c(0.8,1.25),
                     summary_label = plot_labels,
                     text_size = 4,
                     vline_intercept = 1,
                     x_lab = "Fraction and 95% CI \nRelative to Reference",
                     CI_label = "Median [95% CI]",
                     jitter_nsim = TRUE)

  plt


})



