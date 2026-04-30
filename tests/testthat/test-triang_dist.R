test_that("dtriang returns correct density values", {
  expect_equal(dtriang(0, 0, 1, 0.5), 0)
  expect_equal(dtriang(0.5, 0, 1, 0.5), 2)
  expect_equal(dtriang(1, 0, 1, 0.5), 0)
  expect_equal(dtriang(-1, 0, 1, 0.5), 0)
  expect_equal(dtriang(2, 0, 1, 0.5), 0)
})

test_that("ptriang returns correct probabilities", {
  expect_equal(ptriang(0, 0, 1, 0.5), 0)
  expect_equal(ptriang(0.5, 0, 1, 0.5), 0.5)
  expect_equal(ptriang(1, 0, 1, 0.5), 1)
  expect_equal(ptriang(-1, 0, 1, 0.5), 0)
  expect_equal(ptriang(2, 0, 1, 0.5), 1)
})

test_that("qtriang returns correct quantiles", {
  expect_equal(qtriang(0, 0, 1, 0.5), 0)
  expect_equal(qtriang(0.5, 0, 1, 0.5), 0.5)
  expect_equal(qtriang(1, 0, 1, 0.5), 1)
})

test_that("rtriang returns random values inside range", {
  set.seed(123)
  values <- rtriang(100, 0, 1, 0.5)

  expect_length(values, 100)
  expect_true(all(values >= 0))
  expect_true(all(values <= 1))
})

test_that("functions validate parameters", {
  expect_error(dtriang(0.5, min = 1, max = 0, mode = 0.5))
  expect_error(ptriang(0.5, min = 1, max = 0, mode = 0.5))
  expect_error(qtriang(0.5, min = 1, max = 0, mode = 0.5))

  expect_error(dtriang(0.5, min = 0, max = 1, mode = 2))
  expect_error(ptriang(0.5, min = 0, max = 1, mode = -1))
  expect_error(qtriang(0.5, min = 0, max = 1, mode = 2))

  expect_error(qtriang(-0.1, min = 0, max = 1, mode = 0.5))
  expect_error(qtriang(1.1, min = 0, max = 1, mode = 0.5))
})

test_that("functions are vectorized", {
  density <- dtriang(
    x = c(0.25, 0.5, 0.75),
    min = 0,
    max = 1,
    mode = 0.5
  )

  probability <- ptriang(
    q = c(0.25, 0.5, 0.75),
    min = 0,
    max = 1,
    mode = 0.5
  )

  quantile <- qtriang(
    p = c(0.25, 0.5, 0.75),
    min = 0,
    max = 1,
    mode = 0.5
  )

  expect_length(density, 3)
  expect_length(probability, 3)
  expect_length(quantile, 3)
})

test_that("special cases with mode equal to min or max work", {
  expect_equal(dtriang(0, min = 0, max = 1, mode = 0), 2)
  expect_equal(dtriang(1, min = 0, max = 1, mode = 1), 2)

  expect_equal(ptriang(0.5, min = 0, max = 1, mode = 0), 0.75)
  expect_equal(ptriang(0.5, min = 0, max = 1, mode = 1), 0.25)

  expect_equal(qtriang(0, min = 0, max = 1, mode = 0), 0)
  expect_equal(qtriang(1, min = 0, max = 1, mode = 1), 1)
})
