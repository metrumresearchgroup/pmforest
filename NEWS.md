# pmforest 0.2.0

## New Features:

* Adds two new arguments, `CI_bracket_open`, and `CI_bracket_close` for denoting the opening and closing intervals respectively (#32)

## Bug Fixes:

* Fixes to DESCRIPTION file (no need to collate files anymore) (#33)
* Validation cleanup: fixes and cleanup associated with migrating away from validation via `mrgvalidate` and `mrgvalprep` (#33)

# pmforest 0.1.1

## Bug Fixes:

* Fixed tests relating to plot labels that began failing with `yspec 0.5.2` (#17)
* Updated test references when using a custom writer function (#18)
* Numeric `group_levels` column can now be used properly. They were previously treated as missing (#25)
* Dealt with deprecation warnings from `tidyselect` functions (#27)
* Added support for `ggplot 3.4.0`, which changed the `size` attribute to `linewidth` in the case of `geom_line()` and similar functions (#28)


# pmforest 0.1.0

## New Features:

* `shapes` argument. Will denote the shape of the mean/median value. Users can specify one of the following: "square", "diamond", "circle", or "triangle". (#10)
* `shape_size` argument. Numeric value between 1 and 4 that denotes the relative size of the `shape` argument. (#10)
* `ggplot_theme` argument. Users can now pass a ggplot theme object, such as `theme_bw()`, `theme_classic()`, or a theme you create on your own. Some settings may be overwritten to ensure table/plot alignment. (#10)
* `pmforest 0.1.0` is now on MPN (March 2022 snapshot and later).

## Bug Fixes:

* Previously the lower minor interval was incorrect (for use with replicates). This has been addressed. (#13)
* The plot and table are now perfectly aligned in all cases. (#14)
* Multiple lines can now be entered for `CI_label` and `x_lab`. (#14)
* Some formatting adjustments; plots look nicer. (#14)


# pmforest 0.0.1

Initial release of pmforest package. See vignettes below for usage:


* [Getting Started with bbr](https://metrumresearchgroup.github.io/pmforest/articles/getting-started.html) -- Data specifications, summarization, and basic plotting methods.
* [Multiple Simulations](https://metrumresearchgroup.github.io/pmforest/articles/multiple-simulations.html) -- Plotting additional confidence intervals over the 'replicate' or simulation run.


Expect more minor releases coming soon. Please file any issues or feature requests [here](https://github.com/metrumresearchgroup/pmforest/issues). 
