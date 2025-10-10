pmf_expect_plot <- function(test_str, fig, writer = vdiffr::write_svg){
  ggver <- utils::packageVersion("ggplot2")
  # Distinguish major version families (3.x vs 4.x)
  variant <- if(ggver < "4.0.0") "ggplot2-pre-4" else "ggplot2-4"

  vdiffr::expect_doppelganger(test_str, fig, writer = writer, variant = variant)
}
