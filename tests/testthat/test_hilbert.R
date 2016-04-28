library(HilbertInTreemap)
context('Hilbert curve')

test_that('default hilbert produces correct output', {
  curve = hilbert(4)

  expect_that(curve$x, equals(c(0, 1, 1, 0, 0, 0, 1, 1, 2, 2, 3, 3, 3, 2, 2, 3)))
  expect_that(curve$y, equals(c(0, 0, 1, 1, 2, 3, 3, 2, 2, 3, 3, 2, 1, 1, 0, 0)))
})

test_that('hilbert with positions produces correct output', {
  curve = hilbert(4, 5:12)

  expect_that(curve$x, equals(c(0, 0, 1, 1, 2, 2, 3, 3)))
  expect_that(curve$y, equals(c(2, 3, 3, 2, 2, 3, 3, 2)))
})

test_that('hilbert stops on wrong side length', {
  expect_that(hilbert(6), throws_error())
})

test_that('flip and rotate works', {
  curve = data.frame(x = c(0, 1, 0, 1, 0, 1, 0, 1),
                     y = c(2, 3, 2, 3, 2, 3, 2, 3),
                     rx = c(0, 0, 0, 0, 1, 1, 1, 1),
                     ry = c(0, 0, 1, 1, 0, 0, 1, 1),
                     position = 0)
  curve = flip.and.rotate(curve, 4)

  expect_that(curve$x, equals(c(2, 3, 0, 1, 1, 0, 0, 1)))
  expect_that(curve$y, equals(c(0, 1, 2, 3, 3, 2, 2, 3)))
})
