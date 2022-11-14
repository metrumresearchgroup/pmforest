

dataDir <- system.file("test-data", package = "pmforest")
specDir <- file.path(dataDir, "spec")

describe("Base plots", {

  specFile <- yspec::ys_load(file.path(specDir, 'analysis3.yml'))

  all_labels <- yspec::ys_get_short(specFile, title_case = TRUE)
  all_labels$EGFR <- 'Renal Function'
  all_labels$CL <- 'CL (L/hr)'
  all_labels$V2 <- 'V (L)'
  plot_labels <- ggplot2::as_labeller(unlist(all_labels))

  plotData <- readRDS(file.path(dataDir, "plotData.RDS"))
  sumData <- plotData %>%
    summarize_data(
      value = stat,
      group = GROUP,
      metagroup = param,
      group_level = LVL
    )

  plotData2 <- readRDS(file.path(dataDir, "plotData2.RDS"))
  sumData2 <- plotData2 %>%
    summarize_data(
      value = stat,
      group = GROUP,
      metagroup = param,
      group_level = LVL
    )

  it("Test metagroup (Cl and V2) [PMF-PLOT-004]", {
    skip_on_cran()
    skip_vdiffr()
    plt1 <- sumData2 %>%
      plot_forest(annotate_CI = F)
    vdiffr::expect_doppelganger("Test metagroup", plt1)

    plt2 <- sumData2 %>%
      plot_forest(annotate_CI = F,
                  nrow = 2)
    vdiffr::expect_doppelganger("Test metagroup with nrow", plt2)

  })

  it("Test metagroup (Cl and V2) with labels [PMF-PLOT-005]", {
    skip_on_cran()
    skip_vdiffr()
    plt <- plot_forest(data = sumData2,
                       annotate_CI = F,
                       nrow = 2,
                       summary_label = plot_labels
    )
    vdiffr::expect_doppelganger("Test metagroup with labels", plt)
  })

  it("CI Table - mean [PMF-PLOT-006]", {
    skip_on_cran()
    skip_vdiffr()
    plt <- plotData %>%
      summarize_data(
        value = stat,
        group = GROUP,
        metagroup = param,
        group_level = LVL,
        statistic = "mean",
        probs = c(0.005, 0.995)
      ) %>%
      plot_forest(annotate_CI = TRUE)
    vdiffr::expect_doppelganger("CI Table - mean", plt)
  })

  it("CI Table - median [PMF-PLOT-007]", {
    skip_on_cran()
    skip_vdiffr()
    plt <- plotData %>%
      summarize_data(
        value = stat,
        group = GROUP,
        metagroup = param,
        group_level = LVL,
        statistic = "median",
        probs = c(0.005, 0.995)
      ) %>%
      plot_forest(annotate_CI = TRUE)
    vdiffr::expect_doppelganger("CI Table - median", plt)
  })

  it("Plot/Table width [PMF-PLOT-008]", {
    skip_on_cran()
    skip_vdiffr()
    plt <- plot_forest(data = sumData,
                       plot_width = 6 # not the default value
    )
    vdiffr::expect_doppelganger("Plot/Table width", plt)
  })

  it("Vertical Intercept [PMF-PLOT-009]", {
    skip_on_cran()
    skip_vdiffr()
    plt <- plot_forest(data = sumData,
                       vline_intercept = 1
    )
    vdiffr::expect_doppelganger("Vertical Intercept", plt)
  })

  it("shaded interval displays over correct range [PMF-PLOT-010]", {
    skip_on_cran()
    skip_vdiffr()
    plt <- plot_forest(data = sumData,
                       shaded_interval = c(0.8,1.25),
    )
    vdiffr::expect_doppelganger("shaded interval", plt)
  })

  it("update labels via yaml file [PMF-PLOT-011]", {
    skip_on_cran()
    skip_vdiffr()
    plt <- plot_forest(data = sumData,
                       summary_label = plot_labels
    )
    vdiffr::expect_doppelganger("update labels via yaml file", plt)
  })

  it("Axis labels and captions [PMF-PLOT-014]", {
    skip_on_cran()
    skip_vdiffr()
    plt <- plotData %>%
      summarize_data(
        value = stat,
        group = GROUP,
        group_level = LVL
      ) %>%
      plot_forest(
        x_lab = "Fraction and 95% CI \nRelative to Reference",
        CI_label = "Median [95% CI]",
        caption = "The shaded area corresponds
                                  to the interval (0.8, 1.25)"
      )
    vdiffr::expect_doppelganger("Axis labels and captions", plt)
  })

  it("Test breaks and limits of x-axis [PMF-PLOT-015]", {
    skip_on_cran()
    skip_vdiffr()
    plt <- plot_forest(data = sumData,
                       x_breaks = c(0.4,0.6, 0.8, 1, 1.2, 1.4,1.6),
                       x_limit = c(0.4,1.45)
    )
    vdiffr::expect_doppelganger("Test breaks and limits of x-axis", plt)
  })


  it("Modify text size [PMF-PLOT-016]", {
    skip_on_cran()
    skip_vdiffr()
    plt <- plot_forest(data = sumData,
                       text_size = 4
    )
    vdiffr::expect_doppelganger("Modify text size", plt)
  })

  it("Base plot [PMF-PLOT-017]", {
    skip_on_cran()
    skip_vdiffr()
    plt <- plotData %>%
      summarize_data(
        value = stat,
        group = GROUP,
        group_level = LVL
      ) %>%
      plot_forest()
    vdiffr::expect_doppelganger("Base plot", plt)
  })

  it("Full Test [PMF-PLOT-018]", {
    skip_on_cran()
    skip_vdiffr()
    plt <- plot_forest(data = sumData,
                       shaded_interval = c(0.8,1.25),
                       summary_label = plot_labels,
                       text_size = 3.5,
                       digits = 2,
                       vline_intercept = 1,
                       x_lab = "Fraction and 95% CI \nRelative to Reference",
                       CI_label = "Median [95% CI]",
                       caption = "The shaded area corresponds
                   to the interval (0.8, 1.25)",
                   plot_width = 8,
                   x_breaks = c(0.4,0.6, 0.8, 1, 1.2, 1.4,1.6),
                   x_limit = c(0.4,1.45),
                   ggplot_theme = theme_classic(),
                   shape_size = 2,
                   annotate_CI=T
    )
    vdiffr::expect_doppelganger("Full Test", plt)
  })

  it("plots without group_level [PMF-PLOT-019]", {
    skip_on_cran()
    skip_vdiffr()
    df <-
      data.frame(
        group = c("a", "b", "c"),
        lo = c(1, 2, 3),
        mid = c(3, 4, 5),
        hi = c(5, 6, 7)
      )
    plt <- plot_forest(df)
    vdiffr::expect_doppelganger("No group_level", plt)
  })

  it("modify shape of median value [PMF-PLOT-020]", {
    skip_on_cran()
    skip_vdiffr()
    plt <- plot_forest(data = sumData,
                       shapes = "square"
    )
    vdiffr::expect_doppelganger("Modify shape", plt)
  })

  it("modify shape size of median value [PMF-PLOT-021]", {
    skip_on_cran()
    skip_vdiffr()
    plt <- plot_forest(data = sumData,
                       shape_size = 2
    )
    vdiffr::expect_doppelganger("Modify shape size", plt)
  })

  it("modify ggplot theme [PMF-PLOT-022]", {
    skip_on_cran()
    skip_vdiffr()
    plt <- plot_forest(data = sumData,
                       ggplot_theme = theme_classic()
    )
    vdiffr::expect_doppelganger("Modify ggplot theme", plt)
  })

  it("Multiple lines for CI_label [PMF-PLOT-023]", {
    skip_on_cran()
    skip_vdiffr()
    expect_error(
      plot_forest(data = sumData, CI_label = "Fraction and 95% CI \nRelative to Reference\n test \n test"),
      "must be less than 4 lines"
    )

    plt <- plot_forest(data = sumData,
                       CI_label = "Line1\nLine2\nLine3"
    )
    vdiffr::expect_doppelganger("Multiple lines for CI_label", plt)

  })

  it("Plot alignment with different saving to different sizes [PMF-PLOT-024]", {
    skip_on_cran()
    skip_vdiffr()
    plt <- plot_forest(data = sumData,
                       shaded_interval = c(0.8,1.25),
                       summary_label = plot_labels,
                       text_size = 3.5,
                       digits = 2,
                       vline_intercept = 1,
                       x_lab = "Fraction and 95% CI \nRelative to Reference",
                       CI_label = "Median [95% CI]",
                       caption = "The shaded area corresponds
                   to the interval (0.8, 1.25)",
                   plot_width = 8,
                   x_breaks = c(0.4,0.6, 0.8, 1, 1.2, 1.4,1.6),
                   x_limit = c(0.4,1.45),
                   ggplot_theme = theme_classic(),
                   shape_size = 2,
                   annotate_CI=T
    )

    save_small <- function(plot, file, title = "") {
      ggplot2::ggsave(file, plot, device = "svg", height = 3, width = 3)
    }
    save_big <- function(plot, file, title = "") {
      ggplot2::ggsave(file, plot, device = "svg", height = 8, width = 10)
    }
    vdiffr::expect_doppelganger("Full Test - small", plt, writer = save_small)
    vdiffr::expect_doppelganger("Full Test - big", plt, writer = save_big)
  })


  it("group_level works with numeric data [PMF-PLOT-024]", {
    skip_on_cran()
    skip_vdiffr()
    df <- plotData %>%
      summarize_data(
        value = stat,
        group = GROUP,
        group_level = LVL
      )
    df <- df %>% mutate(group_level = 1:dplyr::n())

    plt1 <- plot_forest(df)
    vdiffr::expect_doppelganger("Numeric group_level", plt1)

    # Character representations of numeric data (e.g. '10.0' also used to not work)
    # Test for this as well
    df <- df %>% mutate(group_level = as.character(group_level))

    plt2 <- plot_forest(df)
    vdiffr::expect_doppelganger("Character interpretation of numeric group_level", plt2)

  })
})


