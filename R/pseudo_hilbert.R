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

  #2x4, p=2
  0, 0, 0, 0, 1, 1, 1, 1, -1, -1, -1, -1, -1, -1, -1, -1,
  0, 1, 1, 0, 0, 1, 1, 0, -1, -1, -1, -1, -1, -1, -1, -1,
  1, 1, 1, 1, 0, 0, 0, 0, -1, -1, -1, -1, -1, -1, -1, -1,
  1, 0, 0, 1, 1, 0, 0, 1, -1, -1, -1, -1, -1, -1, -1, -1,

  #4x2, p=3
  0, 0, 1, 1, 2, 2, 3, 3, -1, -1, -1, -1, -1, -1, -1, -1,
  0, 1, 2, 3, 3, 2, 1, 0, -1, -1, -1, -1, -1, -1, -1, -1,
  3, 3, 2, 2, 1, 1, 0, 0, -1, -1, -1, -1, -1, -1, -1, -1,
  3, 2, 1, 0, 0, 1, 2, 3, -1, -1, -1, -1, -1, -1, -1, -1,

  #4x4, p=4
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

  #2x4, p=2
  0, 1, 2, 3, 3, 2, 1, 0, -1, -1, -1, -1, -1, -1, -1, -1,
  0, 0, 1, 1, 2, 2, 3, 3, -1, -1, -1, -1, -1, -1, -1, -1,
  3, 2, 1, 0, 0, 1, 2, 3, -1, -1, -1, -1, -1, -1, -1, -1,
  3, 3, 2, 2, 1, 1, 0, 0, -1, -1, -1, -1, -1, -1, -1, -1,

  #4x2, p=3
  0, 1, 1, 0, 0, 1, 1, 0, -1, -1, -1, -1, -1, -1, -1, -1,
  0, 0, 0, 0, 1, 1, 1, 1, -1, -1, -1, -1, -1, -1, -1, -1,
  1, 0, 0, 1, 1, 0, 0, 1, -1, -1, -1, -1, -1, -1, -1, -1,
  1, 1, 1, 1, 0, 0, 0, 0, -1, -1, -1, -1, -1, -1, -1, -1,

  #4x4, p=4
  0, 0, 1, 1, 2, 3, 3, 2, 2, 3, 3, 2, 1, 1, 0, 0,
  0, 1, 1, 0, 0, 0, 1, 1, 2, 2, 3, 3, 3, 2, 2, 3,
  3, 3, 2, 2, 1, 0, 0, 1, 1, 0, 0, 1, 2, 2, 3, 3,
  3, 2, 2, 3, 3, 3, 2, 2, 1, 1, 0, 0, 0, 1, 1, 0

), c(16, 4, 9))  # i, g, p

pseudo.hilbert = function(l, width, height, rotation=1) {
  M = get.M(width, height)
  quadrant.data = data.frame(position = as.vector(l),
                             rotation = array((rotation - 1) %% 4 + 1, length(l)),
                             width = array(width, length(l)),
                             height = array(height, length(l)),
                             x = array(0, length(l)),
                             y = array(0, length(l)))

  for (i in 2:M) {
    quadrant.data = split.quadrant(quadrant.data)
  }

  end.point = get.end.point(quadrant.data$width, quadrant.data$height)
  x = terminal.x[cbind(quadrant.data$position, quadrant.data$rotation, end.point)] + quadrant.data$x
  y = terminal.y[cbind(quadrant.data$position, quadrant.data$rotation, end.point)] + quadrant.data$y
  return(data.frame(x=x, y=y))
}

split.quadrant = function(quadrant.data) {
  widths = split.edge(quadrant.data$width)
  heights = split.edge(quadrant.data$height)
  width.matrix = t(sapply(1:nrow(quadrant.data), function(i) {
    widths[i,][terminal.x[1:4,quadrant.data$rotation[i],1] + 1]
  }))
  height.matrix = t(sapply(1:nrow(quadrant.data), function(i) {
    heights[i,][terminal.y[1:4,quadrant.data$rotation[i],1] + 1]
  }))
  sizes = width.matrix * height.matrix
  cumulative.sizes = t(apply(sizes, 1, cumsum))
  quadrant = apply(quadrant.data$position <= cumulative.sizes, 1, function(row) which(row)[1])

  position = quadrant.data$position - cbind(0, cumulative.sizes)[cbind(1:nrow(quadrant.data), quadrant)]
  rotation = induction.matrix[cbind(quadrant.data$rotation, quadrant)]
  width = width.matrix[cbind(1:nrow(quadrant.data), quadrant)]
  height = height.matrix[cbind(1:nrow(quadrant.data), quadrant)]
  x = terminal.x[cbind(quadrant, quadrant.data$rotation, 1)] * apply(widths, 1, min)
  y = terminal.y[cbind(quadrant, quadrant.data$rotation, 1)] * apply(heights, 1, min)

  return(data.frame(position, rotation, width, height,
    x = quadrant.data$x + x,
    y = quadrant.data$y + y
  ))
}

split.edge = function(edge, n=2) {
  if (min(edge) < n) {
    stop('splitting edge will give edges of length 0')
  }

  res = matrix(0, nrow = length(edge), ncol=n)
  divisor = 2 * n
  for (i in 1:n) {
    res[,i] = (edge + 2 * i - 2) %/% divisor * 2 + (edge + 2 * i - 1) %/% divisor - (edge + 2 * i - 2) %/% divisor
  }
  return(res)
}

get.end.point = function(width, height) {
  res = array(0, length(width))
  res[width == 2 & height == 2] = 1
  res[width == 2 & height == 4] = 2
  res[width == 4 & height == 2] = 3
  res[width == 4 & height == 4] = 4
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
    stop(paste('invalid width and height:', width, height))
  }
  return(Ms[1])
}
