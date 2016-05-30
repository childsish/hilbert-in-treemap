library(HilbertInTreemap)
context('Squarify')

test_that('basic squarify gives right results', {
  rows = squarify(c(6, 6, 4, 3, 2, 2, 1))

  expect_that(rows[[1]], equals(c(6, 6)))
  expect_that(rows[[2]], equals(c(4, 3)))
  expect_that(rows[[3]], equals(c(2)))
  expect_that(rows[[4]], equals(c(2)))
  expect_that(rows[[5]], equals(c(1)))
})
