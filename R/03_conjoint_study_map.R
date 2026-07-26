#' Get scale bar coordinates for a lon-lat map
#'
#' Creates a simple scale bar in map coordinates for use with ggplot.
#'
#' @param x.limits Numeric vector of length 2 giving x-axis limits.
#' @param y.limits Numeric vector of length 2 giving y-axis limits.
#' @param bar.length.km Numeric. Desired scale-bar length in km.
#' @param x.offset.prop Numeric. Horizontal offset from left as proportion
#'   of map width.
#' @param y.offset.prop Numeric. Vertical offset from bottom as proportion
#'   of map height.
#'
#' @return A list with line and label coordinates.
getScaleBarData <- function(
  x.limits,
  y.limits,
  bar.length.km = 200,
  x.offset.prop = 0.05,
  y.offset.prop = 0.06
) {
  x.range <- diff(x.limits)
  y.range <- diff(y.limits)
  
  x.start <- x.limits[1] + x.offset.prop * x.range
  y.start <- y.limits[1] + y.offset.prop * y.range
  
  mean.lat <- mean(y.limits)
  
  km.per.degree.lon <- 111.32 * cos(mean.lat * pi / 180)
  bar.length.deg <- bar.length.km / km.per.degree.lon
  
  x.end <- x.start + bar.length.deg
  
  tick.height <- 0.012 * y.range
  label.y <- y.start + 0.018 * y.range
  
  line.df <- data.frame(
    x = c(x.start, x.start, x.end),
    y = c(y.start, y.start - tick.height, y.start - tick.height),
    xend = c(x.end, x.start, x.end),
    yend = c(y.start, y.start + tick.height, y.start + tick.height)
  )
  
  label.df <- data.frame(
    x = c(x.start, x.end),
    y = c(label.y, label.y),
    label = c("0", paste0(bar.length.km, " km"))
  )
  
  list(
    line.df = line.df,
    label.df = label.df
  )
}

