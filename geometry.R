## ---- geometry-load ----

library(tikzDevice)
tikzLatexPackages_opt <- getOption("tikzLatexPackages")
tikzLatexPackages_opt <- tikzLatexPackages_opt[
  !tikzLatexPackages_opt == "\\usepackage{amsfonts}"
]
tikzLatexPackages_opt <- tikzLatexPackages_opt[
  !tikzLatexPackages_opt == "\\usepackage{bm}"
]
options(
  #   tikzFooter = "\\caption{a caption}",
  tikzLatexPackages = c(
    tikzLatexPackages_opt,
    "\\usepackage{amsfonts}",
    "\\usepackage{bm}"
  )
)


# functions
point <- function(p, pch = 20, ...) {
  if (!is.matrix(p) || ncol(p) == 1) {
    p <- t(p)
  }
  points(p[, 1], p[, 2], pch = pch, ...)
}
line <- function(start, end, ...) {
  if (!is.matrix(start) || ncol(start) == 1) {
    start <- t(start)
  }
  if (!is.matrix(end) || ncol(end) == 1) {
    end <- t(end)
  }
  for (i in seq_len(nrow(start))) {
    lines(c(start[i, 1], end[i, 1]), c(start[i, 2], end[i, 2]), ...)
  }
}
arrow <- function(start, end, length = 0.05, ...) {
  arrows(start[[1]], start[[2]], end[[1]], end[[2]], length = length, ...)
}
tex <- function(p, ...) {
  text(p[[1]], p[[2]], ...)
}
#' @param p point
#' @param l line
project <- function(p, l, W = diag(2)) {
  if (!is.matrix(p) || ncol(p) == 1) {
    p <- t(p)
  }
  W_inv <- solve(W)
  SP <- l %*% solve(t(l) %*% W_inv %*% l) %*% t(l) %*% W_inv
  t(SP %*% t(p))
}

# colours
col_hat <- "#0063a7"
col_til <- "#cc5900"
col_proj <- "#0063a7"


insert_figure <- function(file) {
  content <- readLines(file)
  on.exit(unlink(file))
  output <- c(
    r'(\begin{figure})',
    r'(\centering)',
    r'(\resizebox{\textwidth}{!}{%)',
    content,
    r'(})',
    # r'(\caption{})',
    r'(\end{figure})'
  )
  cat(output, sep = "\n")
}


height <- 2.5
width <- 4
window_ylim_max <- 4.5
window_xlim_max <- 5.5
pos_1_offset <- 0.8
asp <- diff(par("usr")[2:1]) / diff(par("usr")[4:3])
## ---- geom-out ----
# run when called
insert_figure(file_geom)

## ---- ortho-single ----

file_geom <- tempfile(fileext = ".tex")
tikz(file_geom, height = height, width = width)
par(mar = rep(0, 4))

plot.new()
# if (FALSE) {
plot.window(
  xlim = c(0, window_xlim_max) - 0.5,
  ylim = c(0, window_ylim_max) - 1,
  asp = asp
)
# x-axis
line(c(-0.5, 0), c(window_xlim_max - 0.5, 0))
# y-axis
ylim <- window_ylim_max - 1
line(c(0, -1), c(0, ylim))
# coherent space
s_space <- c(window_xlim_max - 1, window_ylim_max - 1.5)
arrow(c(0, 0), s_space, lwd = 2)

tex(s_space, r'($\mathfrak{s}$)', pos = 3)

y <- s_space * 0.2
yhat <- y + c(0.9, 2)
point(y)
point(yhat, col = col_hat)
ytil <- project(yhat, s_space)
point(ytil, col = col_til)

line(y, yhat, lty = "dashed")
arrow(yhat, ytil, col = col_proj)

tex(y, r'($\bm{y}$)', pos = 1, offset = pos_1_offset)
tex(yhat, r'($\hat{\bm{y}}$)', pos = 4)
tex(ytil, r'($\bm{\tilde{y}}$)', pos = 1, offset = pos_1_offset, col = col_til)
# }
dev.off()


## ---- points ----
window_xlim_max <- 9
ylim_movedown <- 2

file_geom <- tempfile(fileext = ".tex")
tikz(file_geom, height = height, width = width)
par(mar = rep(0, 4))

plot.new()
plot.window(
  xlim = c(0, window_xlim_max) - 0.5,
  ylim = c(0, window_ylim_max) - ylim_movedown,
  asp = asp
)

# x-axis
line(c(-0.5, 0), c(window_xlim_max - 0.5, 0))
# y-axis
ylim <- window_ylim_max - 1
line(c(0, -ylim_movedown), c(0, ylim))
# coherent space
s_space <- c(window_xlim_max * 0.8, window_ylim_max * 0.3)
arrow(c(0, 0), s_space, lwd = 2)
# direction of the residuals
r_space <- c(window_xlim_max * 0.3, window_ylim_max * 0.6)
arrow(c(0, 0), r_space)

y <- c(window_xlim_max * 0.4, 0)
y[[2]] <- y[[1]] * s_space[[2]] / s_space[[1]]

set.seed(2223)
n <- 30
rmat <- cbind(r_space, c(-1, 1))
e <- t(rmat %*% rbind(rnorm(n, 0, 0.3), rnorm(n, 0, 0.1)))
yhat <- t(t(e) + y)


# points
point(yhat, col = col_hat)
point(y)

tex(s_space, r'($\mathfrak{s}$)', pos = 3)
tex(r_space, r'($\mathfrak{R}$)', pos = 3)


tex(y, r'($\bm{y}$)', pos = 1, offset = pos_1_offset)
tex(
  yhat[which.max(yhat[, 1]), ],
  r'($\bm{\hat{y}}$)',
  pos = 1,
  offset = pos_1_offset,
  col = col_hat
)


dev.off()

## ---- points-ortho ----

window_xlim_max <- 9
ylim_movedown <- 2

file_geom <- tempfile(fileext = ".tex")
tikz(file_geom, height = height, width = width)
par(mar = rep(0, 4))

plot.new()
plot.window(
  xlim = c(0, window_xlim_max) - 0.5,
  ylim = c(0, window_ylim_max) - ylim_movedown,
  asp = asp
)

# x-axis
line(c(-0.5, 0), c(window_xlim_max - 0.5, 0))
# y-axis
ylim <- window_ylim_max - 1
line(c(0, -ylim_movedown), c(0, ylim))
# coherent space
s_space <- c(window_xlim_max * 0.8, window_ylim_max * 0.3)
arrow(c(0, 0), s_space, lwd = 2)
# direction of the residuals
r_space <- c(window_xlim_max * 0.3, window_ylim_max * 0.6)
arrow(c(0, 0), r_space)

y <- c(window_xlim_max * 0.4, 0)
y[[2]] <- y[[1]] * s_space[[2]] / s_space[[1]]

set.seed(2223)
n <- 30
rmat <- cbind(r_space, c(-1, 1))
e <- t(rmat %*% rbind(rnorm(n, 0, 0.3), rnorm(n, 0, 0.1)))
yhat <- t(t(e) + y)
ytil <- project(yhat, s_space)


# points
line(ytil, yhat, lty = "dashed")
point(yhat, col = col_hat)

point(ytil, col = col_til)

point(y)

tex(s_space, r'($\mathfrak{s}$)', pos = 3)
tex(r_space, r'($\mathfrak{R}$)', pos = 3)


tex(y, r'($\bm{y}$)', pos = 1, offset = pos_1_offset)


dev.off()

## ---- points-obliq ----

window_xlim_max <- 9
ylim_movedown <- 2

file_geom <- tempfile(fileext = ".tex")
tikz(file_geom, height = height, width = width)
par(mar = rep(0, 4))

plot.new()
plot.window(
  xlim = c(0, window_xlim_max) - 0.5,
  ylim = c(0, window_ylim_max) - ylim_movedown,
  asp = asp
)

# x-axis
line(c(-0.5, 0), c(window_xlim_max - 0.5, 0))
# y-axis
ylim <- window_ylim_max - 1
line(c(0, -ylim_movedown), c(0, ylim))
# coherent space
s_space <- c(window_xlim_max * 0.8, window_ylim_max * 0.3)
arrow(c(0, 0), s_space, lwd = 2)
# direction of the residuals
r_space <- c(window_xlim_max * 0.3, window_ylim_max * 0.6)
arrow(c(0, 0), r_space)

y <- c(window_xlim_max * 0.4, 0)
y[[2]] <- y[[1]] * s_space[[2]] / s_space[[1]]

set.seed(2223)
n <- 30
rmat <- cbind(r_space, c(-1, 1))
e <- t(rmat %*% rbind(rnorm(n, 0, 0.3), rnorm(n, 0, 0.1)))
yhat <- t(t(e) + y)
ytil <- project(yhat, s_space, W = cov(yhat))


# points
line(ytil, yhat, lty = "dashed")
point(yhat, col = col_hat)

point(ytil, col = col_til)

point(y)

tex(s_space, r'($\mathfrak{s}$)', pos = 3)
tex(r_space, r'($\mathfrak{R}$)', pos = 3)


tex(y, r'($\bm{y}$)', pos = 1, offset = pos_1_offset)


dev.off()
