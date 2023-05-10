# adapted from https://rpubs.com/ruben_jerome/861505

empty_canvas <- function(xlim, ylim, background_color = "gray20") {
  par(mar = rep(1, 4), bg = background_color)
  plot(1,
       type = "n",
       bty  = "n",
       xlab = "", xaxt = "n",
       ylab = "", yaxt = "n",
       xlim = xlim, ylim = ylim
  )
}

draw_line <- function(line, col = "white", lwd = 1) {
  segments(
    x0 = line[1],
    y0 = line[2],
    x1 = line[3],
    y1 = line[4],
    col = col,
    lwd = lwd
  )
}

draw_object <- function(object, col = "white", lwd = 1) {
  invisible(apply(object, 1, draw_line, col = col, lwd = lwd))
}

new_line <- function(line, angle, reduce = 1) {
  x0 <- line[1]
  y0 <- line[2]
  x1 <- line[3]
  y1 <- line[4]

  dx <- unname(x1 - x0) # change in x direction
  dy <- unname(y1 - y0) # change in y direction
  l <- sqrt(dx^2 + dy^2) # length of the line

  theta <- atan(dy / dx) * 180 / pi # angle between line and origin
  rad <- (angle + theta) * pi / 180 # (theta + new angle) in radians

  coeff <- sign(theta) * sign(dy) # coefficient of direction
  if (coeff == 0) coeff <- -1

  x2 <- x0 + coeff * l * cos(rad) * reduce + dx # new x location
  y2 <- y0 + coeff * l * sin(rad) * reduce + dy # new y location
  return(c(x1, y1, x2, y2))
}

# function to run next iteration based on "ifun()"
iterate <- function(object, ifun, ...) {
  lines_list <- vector("list", 0)
  for (i in 1:nrow(object)) {
    old_line <- matrix(object[i, ], nrow = 1)
    new_line <- ifun(old_line, ...)
    lines_list[[length(lines_list) + 1]] <- new_line
  }
  new_object <- do.call(rbind, lines_list)
  return(new_object)
}

# iterator function: recursive tree
tree <- function(line0, angle = 30, reduce = .7, randomness = 0) {
  # angles and randomness
  angle1 <-  angle + rnorm(1, 0, randomness) # left branch
  angle2 <- -angle + rnorm(1, 0, randomness) # right branch

  # new branches
  line1 <- new_line(line0, angle = angle1, reduce = reduce)
  line2 <- new_line(line0, angle = angle2, reduce = reduce)

  # store in matrix and return
  mat <- matrix(c(line1, line2), byrow = T, ncol = 4)
  return(mat)
}

#' @export
draw_fractal <- function(
    maxiter = 10,
    lwd_start = 7,
    lwd_adjust = 1,
    angle = 23,
    cols_start = "white",
    cols = 0:maxiter,
    randomness = 0,
    background_color = "grey20") {

  fractal <- matrix(c(0, 0, 0, 10), nrow = 1)
  empty_canvas(xlim = c(-30, 30), ylim = c(0, 35), background_color = background_color)
  lwd <- lwd_start
  draw_object(fractal, lwd = lwd, col = cols_start)

  cols <- colorspace::terrain_hcl(maxiter)
  for (i in 1:maxiter) {
    lwd <- lwd * lwd_adjust
    col <- cols[i + 1]
    fractal <- iterate(fractal,
                       ifun = tree,
                       angle = angle, randomness = randomness
    )

    draw_object(fractal, col = col, lwd = lwd)

  }
}
