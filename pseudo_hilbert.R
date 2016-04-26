library(dplyr)

induction.matrix = matrix(c(
  2, 1, 1, 4,
  1, 2, 2, 3,
  4, 3, 3, 2,
  3, 4, 4, 1), nrow = 4, byrow = TRUE)

terminal.x = array(c(
  #2x2, p=1
  0, 0, 1, 1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
  0, 1, 1, 0, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
  1, 1, 0, 0, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
  1, 0, 0, 1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,

  #2x3, p=2
  0, 0, 0, 1, 1, 1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
  -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
  1, 1, 1, 0, 0, 0, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
  -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,

  #2x4, p=3
  0, 0, 0, 0, 1, 1, 1, 1, -1, -1, -1, -1, -1, -1, -1, -1,
  0, 1, 1, 0, 0, 1, 1, 0, -1, -1, -1, -1, -1, -1, -1, -1,
  1, 1, 1, 1, 0, 0, 0, 0, -1, -1, -1, -1, -1, -1, -1, -1,
  1, 0, 0, 1, 1, 0, 0, 1, -1, -1, -1, -1, -1, -1, -1, -1,

  #3x2, p=4
  -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
  0, 1, 2, 2, 1, 0, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
  -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
  2, 1, 0, 0, 1, 2, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,

  #3x3, p=5
  0, 0, 0, 1, 2, 2, 1, 1, 2, -1, -1, -1, -1, -1, -1, -1,
  0, 1, 2, 2, 2, 1, 1, 0, 0, -1, -1, -1, -1, -1, -1, -1,
  2, 2, 2, 1, 0, 0, 1, 1, 0, -1, -1, -1, -1, -1, -1, -1,
  2, 1, 0, 0, 0, 1, 1, 2, 2, -1, -1, -1, -1, -1, -1, -1,

  #3x4, p=6
  -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
  0, 1, 2, 2, 2, 2, 1, 1, 1, 0, 0, 0, -1, -1, -1, -1,
  -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
  2, 1, 0, 0, 0, 0, 1, 1, 1, 2, 2, 2, -1, -1, -1, -1,

  #4x2, p=7
  0, 0, 1, 1, 2, 2, 3, 3, -1, -1, -1, -1, -1, -1, -1, -1,
  0, 1, 2, 3, 3, 2, 1, 0, -1, -1, -1, -1, -1, -1, -1, -1,
  3, 3, 2, 2, 1, 1, 0, 0, -1, -1, -1, -1, -1, -1, -1, -1,
  3, 2, 1, 0, 0, 1, 2, 3, -1, -1, -1, -1, -1, -1, -1, -1,

  #4x3, p=8
  0, 0, 0, 1, 2, 3, 3, 2, 1, 1, 2, 3, -1, -1, -1, -1,
  -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
  3, 3, 3, 2, 1, 0, 0, 1, 2, 2, 1, 0, -1, -1, -1, -1,
  -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,

  #4x4, p=9
  0, 1, 1, 0, 0, 0, 1, 1, 2, 2, 3, 3, 3, 2, 2, 3,
  0, 0, 1, 1, 2, 3, 3, 2, 2, 3, 3, 2, 1, 1, 0, 0,
  3, 2, 2, 3, 3, 3, 2, 2, 1, 1, 0, 0, 0, 1, 1, 0,
  3, 3, 2, 2, 1, 0, 0, 1, 1, 0, 0, 1, 2, 2, 3, 3
), c(16, 4, 9))  # i, g, p

terminal.y = array(c(
  #2x2, p=1
  0, 1, 1, 0, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
  0, 0, 1, 1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
  1, 0, 0, 1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
  1, 1, 0, 0, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,

  #2x3, p=2
  0, 1, 2, 2, 1, 0, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
  -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
  2, 1, 0, 0, 1, 2, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
  -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,

  #2x4, p=3
  0, 1, 2, 3, 3, 2, 1, 0, -1, -1, -1, -1, -1, -1, -1, -1,
  0, 0, 1, 1, 2, 2, 3, 3, -1, -1, -1, -1, -1, -1, -1, -1,
  3, 2, 1, 0, 0, 1, 2, 3, -1, -1, -1, -1, -1, -1, -1, -1,
  3, 3, 2, 2, 1, 1, 0, 0, -1, -1, -1, -1, -1, -1, -1, -1,

  #3x2, p=4
  -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
  0, 0, 0, 1, 1, 1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
  -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
  1, 1, 1, 0, 0, 0, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,

  #3x3, p=5
  0, 1, 2, 2, 2, 1, 1, 0, 0, -1, -1, -1, -1, -1, -1, -1,
  0, 0, 0, 1, 2, 2, 1, 1, 2, -1, -1, -1, -1, -1, -1, -1,
  2, 1, 0, 0, 0, 1, 1, 2, 2, -1, -1, -1, -1, -1, -1, -1,
  2, 2, 2, 1, 0, 0, 1, 1, 0, -1, -1, -1, -1, -1, -1, -1,

  #3x4, p=6
  -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
  0, 0, 0, 1, 2, 3, 3, 2, 1, 1, 2, 3, -1, -1, -1, -1,
  -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
  3, 3, 3, 2, 1, 0, 0, 1, 2, 2, 1, 0, -1, -1, -1, -1,

  #4x2, p=7
  0, 1, 1, 0, 0, 1, 1, 0, -1, -1, -1, -1, -1, -1, -1, -1,
  0, 0, 0, 0, 1, 1, 1, 1, -1, -1, -1, -1, -1, -1, -1, -1,
  1, 0, 0, 1, 1, 0, 0, 1, -1, -1, -1, -1, -1, -1, -1, -1,
  1, 1, 1, 1, 0, 0, 0, 0, -1, -1, -1, -1, -1, -1, -1, -1,

  #4x3, p=6
  0, 1, 2, 2, 2, 2, 1, 1, 1, 0, 0, 0, -1, -1, -1, -1,
  -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
  2, 1, 0, 0, 0, 0, 1, 1, 1, 2, 2, 2, -1, -1, -1, -1,
  -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,

  #4x4, p=9
  0, 0, 1, 1, 2, 3, 3, 2, 2, 3, 3, 2, 1, 1, 0, 0,
  0, 1, 1, 0, 0, 0, 1, 1, 2, 2, 3, 3, 3, 2, 2, 3,
  3, 3, 2, 2, 1, 0, 0, 1, 1, 0, 0, 1, 2, 2, 3, 3,
  3, 2, 2, 3, 3, 3, 2, 2, 1, 1, 0, 0, 0, 1, 1, 0

), c(16, 4, 9))  # i, g, p

pseudo.hilbert = function(l, width, height, rotation=1) {
  M = get.M(width, height)
  position = as.vector(l)
  rotation = array((rotation - 1) %% 4 + 1, length(l))
  width = array(width, length(l))
  height = array(height, length(l))
  x = array(0, length(l))
  y = array(0, length(l))
  k = M
  while (k > 1) {
    quadrant.data = data.frame(position = position,
                               width = width,
                               height = height,
                               rotation = rotation,
                               x = x,
                               y = y)
    quadrants = split(quadrant.data)
    x = quadrants$x
    y = quadrants$y
    position = quadrants$position
    width = quadrants$width
    height = quadrants$height

    rotation = induction.matrix[cbind(rotation, quadrants$quadrant)]
    k = k - 1
  }

  end.point = get.end.point(width, height)
  x = terminal.x[cbind(position, rotation, end.point)] + x
  y = terminal.y[cbind(position, rotation, end.point)] + y
  return(data.frame(x=x, y=y))
}

get.quadrant.sizes = function(quadrant.data) {
  x = t(terminal.x[1:4,quadrant.data$rotation,1])
  y = t(terminal.y[1:4,quadrant.data$rotation,1])

  width = apply(x, 2, function(column) split.edge(quadrant.data$width, column != x[,1]))
  height = apply(y, 2, function(column) split.edge(quadrant.data$height, column != y[,1]))

  return(matrix(width * height, ncol=4))
}

get.quadrant = function(quadrant.data) {
  sizes = get.quadrant.sizes(quadrant.data) %>%
    apply(1, cumsum) %>%
    t()
  quadrant = apply(quadrant.data$position <= sizes, 1, function(row) which(row)[1])
  position = quadrant.data$position - cbind(0, sizes)[cbind(1:length(quadrant), quadrant)]
  return(data.frame(position, quadrant))
}

split = function(quadrant.data) {
  next.quadrant = get.quadrant(quadrant.data)
  next.split = quadrant.data %>%
    mutate(position = next.quadrant[,1],
           quadrant = next.quadrant[,2],
           entry.x = terminal.x[cbind(1, rotation, 1)] == terminal.x[cbind(quadrant, rotation, 1)],
           entry.y = terminal.y[cbind(1, rotation, 1)] == terminal.y[cbind(quadrant, rotation, 1)],
           x = x + terminal.x[cbind(quadrant, rotation, 1)] * split.edge(width, entry.x),
           y = y + terminal.y[cbind(quadrant, rotation, 1)] * split.edge(height, entry.y),
           width = split.edge(width, !entry.x),
           height = split.edge(height, !entry.y))
  return(next.split)
}

split.edge = function(edge, gets.remainder, n=2) {
  half = edge %/% 2
  edge.is.odd = edge %% 2
  half.is.odd = half %% 2
  half[edge.is.odd & gets.remainder] = half[edge.is.odd & gets.remainder] + 1
  half[half.is.odd & !edge.is.odd] = half[half.is.odd & !edge.is.odd] + 2 * gets.remainder[half.is.odd & !edge.is.odd] - 1
  return(half)
}

get.end.point = function(width, height) {
  res = array(0, length(width))
  res[width == 2 & height == 2] = 1
  res[width == 2 & height == 3] = 2
  res[width == 2 & height == 4] = 3
  res[width == 3 & height == 2] = 4
  res[width == 3 & height == 3] = 5
  res[width == 3 & height == 4] = 6
  res[width == 4 & height == 2] = 7
  res[width == 4 & height == 3] = 8
  res[width == 4 & height == 4] = 9
  if (0 %in% res) {
    stop('invalid height and width reached')
  }
  return(res)
}

get.M = function(width, height) {
  lower.bound = 2 * 2 ** (0:11)
  upper.bound = 4 * 2 ** (0:11)
  Ms = which(lower.bound <= height & height <= upper.bound &
             lower.bound <= width & width <= upper.bound)
  if (length(Ms) == 0 || length(Ms) > 2) {
    stop('invalid height and width')
  }
  return(Ms[1])
}
