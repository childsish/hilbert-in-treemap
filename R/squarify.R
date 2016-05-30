#' @export
adjust.till.fit = function(children, ratios=NULL, iterations=100) {
  original = children
  for (i in 1:iterations) {
    ratio = get.best.ratio(children, ratios)
    height = sqrt(sum(children) / ratio)
    width = height * ratio
    rows = squarify(children, width, height)
    rectangles = squarify.coordinates(rows, width, height)
    rectangles = adjust.coordinates(rectangles)

    adj = (rectangles$width * rectangles$height - original)
    if (all(adj > 0)) {
      break
    }
    children = children - ifelse(adj < 0, adj, 0)
  }
  return(rectangles)
}

#' @export
get.best.ratio = function(children, ratios=NULL) {
  if (is.null(ratios)) {
    ratios = 0:20 / 20 + 1
  }
  worst.ratios = c()
  for (ratio in ratios) {
    height = sqrt(sum(children) / ratio)
    width = height * ratio

    rows = squarify(children, width, height)

    dimensions = c(width, height)
    current = which(dimensions == min(dimensions))[1]

    worst.ratio = 1
    for (row in rows) {
      t = sum(row) / prod(dimensions) * dimensions[-current]
      worst.ratio = max(max(row / t, t) / min(row / t, t), worst.ratio)
      dimensions[-current] = dimensions[-current] - t
      current = which(dimensions == min(dimensions))
    }
    worst.ratios[length(worst.ratios) + 1] = worst.ratio
  }
  return(ratios[worst.ratios == min(worst.ratios)][1])
}

#' @export
squarify = function(children, width=NULL, height=NULL) {
  if (is.null(width) || is.null(height)) {
    height = sqrt(sum(children) / 1.618)
    width = height * 1.618
  }

  rows = list()
  row = children[1]
  dimensions = c(width, height)
  current = which(dimensions == min(dimensions))
  for (child in children[-1]) {
    if (worst(c(row, child), dimensions[current]) <= worst(row, dimensions[current])) {
      row = c(row, child)
    } else {
      rows[[length(rows) + 1]] = row
      dimensions[-current] = dimensions[-current] - sum(row) / prod(dimensions) * dimensions[-current]
      current = which(dimensions == min(dimensions))
      row = child
    }
  }
  rows[[length(rows) + 1]] = row
  return(rows)
}

worst = function(R, w) {
  if (length(R) == 0) {
    return(w)
  }
  s = sum(R)
  res = max(w ** 2 * max(R) / s ** 2,
            s ** 2 / (w ** 2 * min(R)))
  return(res)
}

#' @export
squarify.coordinates = function(rows, width, height) {
  origin = c(0, 0)
  dimensions = c(width, height)
  current = which(dimensions == min(dimensions))

  os = list()
  ds = list()
  for (row in rows) {
    t = sum(row) / prod(dimensions) * dimensions[-current]
    t.ds = matrix(0, nrow = length(row), ncol = 2)
    t.ds[,current] = row / t
    t.ds[,-current] = t
    t.os = matrix(0, nrow = length(row), ncol = 2)
    t.os[,current] = cumsum(c(origin[current], t.ds[,current]))[-(length(row) + 1)]
    t.os[,-current] = origin[-current]

    os[[length(os) + 1]] = t.os
    ds[[length(ds) + 1]] = t.ds

    origin[-current] = origin[-current] + t
    dimensions[-current] = dimensions[-current] - t
    current = which(dimensions == min(dimensions))
  }
  os = do.call('rbind', os)
  ds = do.call('rbind', ds)
  res = data.frame(os, ds)
  colnames(res) = c('x', 'y', 'width', 'height')
  return(res)
}

#' @export
adjust.coordinates = function(coordinates) {
  x1 = coordinates$x
  y1 = coordinates$y
  x2 = coordinates$x + coordinates$width
  y2 = coordinates$y + coordinates$height
  x1 = x1 %/% 2 * 2
  y1 = y1 %/% 2 * 2
  x2 = x2 %/% 2 * 2
  y2 = y2 %/% 2 * 2

  return(data.frame(
    x = x1,
    y = y1,
    width = x2 - x1,
    height = y2 - y1
  ))
}
