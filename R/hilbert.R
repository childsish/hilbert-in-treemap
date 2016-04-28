library(dplyr)

hilbert = function(d, n) {
  res = data.frame(x=0, y=0, rx=FALSE, ry=FALSE, t=d-1)
  s = 1
  while (s < n) {
    res = mutate(res,
                 rx = bitwAnd(1, (t %/% 2)),
                 ry = bitwAnd(1, bitwXor(t, rx)))
    res = rot(res, s)
    res = mutate(res,
                 x = x + s * rx,
                 y = y + s * ry,
                 t = t / 4)
    s = s * 2
  }
  return(select(res, x, y))
}

rot = function(res, n) {
  res = mutate(res,
               x = ifelse(ry == 0 & rx == 1, n - 1 - x, x),
               y = ifelse(ry == 0 & rx == 1, n - 1 - y, y),
               tmp = ifelse(ry == 0, x, NA),
               x = ifelse(ry == 0, y, x),
               y = ifelse(ry == 0, tmp, y))
  return(res)
}

n = 256
points = hilbert(1:(n * n), n)
plot(points, type='l')
