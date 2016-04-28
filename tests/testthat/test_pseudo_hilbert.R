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
