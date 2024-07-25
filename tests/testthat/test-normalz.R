testdata <- df_normalz()
data <- testdata$data
base <- testdata$base

test_that("test data", {
  expect_type(testdata, "list")
  expect_length(testdata, 3)
  expect_s3_class(data, "data.frame")
  expect_identical(dim(data), as.integer(c(testdata$n, 4)))
  expect_identical(dim(base), c(9L, 3L))
})


test_that("normalz", {
  out <- data |>
    normaliz(
      cols = c("amt1", "amt2"), base = base,
      id_cols = c("group", "year"), scale = 100,
      inverse = FALSE, keep = FALSE
    )
  expect_identical(sum(is.na(out$amt1)), 3L)
  expect_identical(round(sum(out$amt1, na.rm = TRUE), 0), 5622)
  expect_identical(round(sum(out$amt2, na.rm = TRUE), 0), 13205)
})


test_that("normalz: base_amt column", {
  data <- testdata$data
  base <- testdata$base

  # using a user-provided name
  amt_col <- "val_amt"

  names(base)[names(base) == "base_amt"] <- amt_col

  out <- data |>
    normaliz(
      cols = c("amt1", "amt2"), base = base,
      id_cols = c("group", "year"), scale = 100,
      inverse = FALSE, keep = FALSE,
      base_amt = amt_col
    )
  expect_identical(dim(data), as.integer(c(testdata$n, 4)))

  # should return an error when base_amt is invalid
  expect_error(
    normaliz(
      data,
      cols = c("amt1", "amt2"), base = base,
      id_cols = c("group", "year"), scale = 100,
      inverse = FALSE, keep = FALSE
    ),
    regexp = "Assertion on 'base_amt' failed"
  )

  # should return an error when base_amt is the name of a column of data
  data$base_amt <- 0L
  expect_error(
    normaliz(
      data,
      cols = c("amt1", "amt2"), base = base,
      id_cols = c("group", "year"), scale = 100,
      inverse = FALSE, keep = FALSE
    ),
    regexp = "Assertion on 'base_amt' failed"
  )
})


test_that("normalz inverse", {
  out <- data |>
    normaliz(
      cols = c("amt1", "amt2"), base = base,
      id_cols = c("group", "year"), scale = 100,
      inverse = FALSE, keep = FALSE
    )

  inv <- out |>
    normaliz(
      cols = c("amt1", "amt2"), base = base,
      id_cols = c("group", "year"), scale = 100,
      inverse = TRUE, keep = FALSE
    )
  expect_identical(sum(is.na(inv$amt1)), 3L)
  expect_identical(sum(is.na(inv$amt2)), 3L)
  expect_true(all(data$amt1 == inv$amt1, na.rm = TRUE))
  expect_true(all(data$amt2 == inv$amt2, na.rm = TRUE))
})
