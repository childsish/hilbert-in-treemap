squarify = function(children, width, height) {
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
