#' @export
hilbert = function(side.length, position = NULL) {
  if (log2(side.length) %% 1 != 0) {
    stop('side length must be a power of two')
  }
  else if (is.null(position)) {
    position = 1:(side.length * side.length)
  }

  curve = data.frame(x = 0, y = 0, rx = 0, ry = 0, t = position - 1)
  s = 1
  while (s < side.length) {
    curve$rx = bitwAnd(1, curve$t %/% 2)
    curve$ry = bitwAnd(1, bitwXor(curve$t, curve$rx))
    curve = flip.and.rotate(curve, s)
    curve$x = curve$x + s * curve$rx
    curve$y = curve$y + s * curve$ry
    curve$t = curve$t / 4
    s = s * 2
  }
  return(curve[,c('x', 'y')])
}

flip.and.rotate = function(curve, side.length) {
  flip = !curve$ry & curve$rx
  curve$x[flip] = side.length - 1 - curve$x[flip]
  curve$y[flip] = side.length - 1 - curve$y[flip]

  rotate = !curve$ry
  tmp = curve$x[rotate]
  curve$x[rotate] = curve$y[rotate]
  curve$y[rotate] = tmp
  return(curve)
}
