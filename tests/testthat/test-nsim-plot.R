specDir <- here::here("tests",  "test-data", "spec")
dataDir <- here::here("tests",  "test-data")

specFile <- yspec::ys_load(file.path(specDir, 'analysis3.yml'))

all_labels <- yspec::ys_get_short(specFile, title_case = TRUE)
all_labels$EGFR <- 'Renal Function'
all_labels$CL <- 'CL (L/hr)'
all_labels$V2 <- 'V (L)' # uncomment this line if using V2

plot_labels <- as_labeller(unlist(all_labels))

describe("Multiple CI's", {

  plotData <- readRDS(file.path(dataDir, "plotDataEXP.RDS"))

  plt <- plot_forest(data = plotData,
                    statistic = "mean",
                    CI=0.95,
                    stat = stat,
                    group = GROUP,
                    # metagroup = param,
                    nsim = nsim,
                    study_labels = LVL,
                    shaded_interval = c(0.8,1.25),
                    summary_label = plot_labels,
                    text_size = 3.5,
                    # vline_intercept = 1,
                    x_lab = "Fraction and 95% CI \nRelative to Reference",
                    CI_label = "Median [95% CI]",
                    plot_width = 8, # out of 12
                    # caption = "",
                    x_breaks = c(0.4,0.6, 0.8, 1, 1.2, 1.4,1.6),
                    x_limit = c(0.4,1.45),
                    annotate_CI=T,
                    nrow = 1)

  plt


})




describe("Multiple CI's with jitter", {
  
  plotData <- readRDS(file.path(dataDir, "plotDataEXP.RDS")) %>% mutate(stat = stat + (nsim/300))
  
  plt <- plot_forest(data = plotData,
                     statistic = "mean",
                     CI=0.95,
                     stat = stat,
                     group = GROUP,
                     # metagroup = param,
                     nsim = nsim,
                     study_labels = LVL,
                     shaded_interval = c(0.8,1.25),
                     summary_label = plot_labels,
                     text_size = 3.5,
                     # vline_intercept = 1,
                     x_lab = "Fraction and 95% CI \nRelative to Reference",
                     CI_label = "Median [95% CI]",
                     plot_width = 8, # out of 12
                     # caption = "",
                     x_breaks = c(0.4,0.6, 0.8, 1, 1.2, 1.4,1.6),
                     x_limit = c(0.4,1.45),
                     annotate_CI=T,
                     nrow = 1,
                     jitter_nsim = TRUE)
  
  plt
  
  
})



