test_that("plot_forest() input data check errors with missing value column", {
  df <-
    data.frame(
      group = c("a", "b", "c"),
      lo = c(1, 2, 3),
      mid = c(3, 4, 5)
    )
  expect_error(
    plot_forest(df),
    "does not have required columns"
  )
})

test_that("plot_forest() input data check errors with missing group column", {
  df <-
    data.frame(
      lo = c(1, 2, 3),
      mid = c(3, 4, 5),
      hi = c(5, 6, 7)
    )
  expect_error(
    plot_forest(df),
    "does not have required columns"
  )
})

test_that("plot_forest() input data check warns with extra columns", {
  df <-
    data.frame(
      group = c("a", "b", "c"),
      lo = c(1, 2, 3),
      mid = c(3, 4, 5),
      hi = c(5, 6, 7),
      naw = c(7, 8, 9)
    )
  expect_message(
    plt <- plot_forest(df),
    "has extra columns"
  )
})

