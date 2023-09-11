

dataDir <- system.file("test-data", package = "pmforest")
specDir <- file.path(dataDir, "spec")

describe("Multiple simulations", {

  specFile <- yspec::ys_load(file.path(specDir, 'analysis3.yml'))

  all_labels <- yspec::ys_get_short(specFile, title_case = TRUE)
  all_labels$EGFR <- 'Renal Function'
  all_labels$CL <- 'CL (L/hr)'
  all_labels$V2 <- 'V (L)' # uncomment this line if using V2
  plot_labels <- ggplot2::as_labeller(unlist(all_labels))

  plotData <- readRDS(file.path(dataDir, "plotDataEXP.RDS"))
  plotData2 <- readRDS(file.path(dataDir, "plotDataEXP.RDS")) %>%
    mutate(stat = stat + (nsim/300)) # make larger to test jitter

  it("Multiple simulations base test", {
    skip_on_cran()
    skip_vdiffr()
    plt <- plotData %>%
      summarize_data(
        value = stat,
        group = GROUP,
        replicate = nsim,
        group_level = LVL,
        statistic = "mean"
      ) %>%
      plot_forest(
         shaded_interval = c(0.8,1.25),
         summary_label = plot_labels,
         text_size = 3.5,
         vline_intercept = 1,
         x_lab = "Fraction and 95% CI \nRelative to Reference",
         CI_label = "Mean [95% CI]"
    )
    vdiffr::expect_doppelganger("Multiple simulations", plt)
  })

  it("Multiple CI's with jitter", {
    skip_on_cran()
    skip_vdiffr()
    plt <- plotData2 %>%
      summarize_data(
        value = stat,
        group = GROUP,
        replicate = nsim,
        group_level = LVL,
      ) %>%
      plot_forest(
        shaded_interval = c(0.8, 1.25),
        summary_label = plot_labels,
        text_size = 4,
        vline_intercept = 1,
        x_lab = "Fraction and 95% CI \nRelative to Reference",
        CI_label = "Median [95% CI]",
        jitter_reps = TRUE
      )
    vdiffr::expect_doppelganger("Multiple CI's with jitter", plt)
  })

})

