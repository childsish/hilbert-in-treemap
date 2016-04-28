library(HilbertInTreemap)
context('Pseudo-Hilbert curve')

test_that('splitting edges in two returns correct results', {
  edges = split.edge(4:10, 2)

  expect_that(edges, equals(matrix(c(2, 2, 2, 3, 2, 4, 3, 4, 4, 4, 4, 5, 4, 6), ncol=2, byrow=T)))
})

test_that('splitting edges in three returns correct results', {
  edges = split.edge(6:12, 3)

  expect_that(edges, equals(matrix(c(2, 2, 2, 2, 2, 3, 2, 2, 4, 2, 3, 4, 2, 4, 4, 3, 4, 4, 4, 4, 4), ncol=3, byrow=T)))
})

test_that('get.segments with long width returns correct results', {
  segments = get.segments(174, 30)

  expect_that(segments, equals(data.frame(width = c(28, 28, 28, 30, 30, 30),
                                          height = c(30, 30, 30, 30, 30, 30),
                                          x = c(0, 28, 56, 84, 114, 144),
                                          y = c(0, 0, 0, 0, 0, 0))))
})

test_that('get.segments with long height returns correct results', {
  segments = get.segments(30, 174)

  expect_that(segments, equals(data.frame(width = c(30, 30, 30, 30, 30, 30),
                                          height = c(28, 28, 28, 30, 30, 30),
                                          x = c(0, 0, 0, 0, 0, 0),
                                          y = c(0, 28, 56, 84, 114, 144))))
})
