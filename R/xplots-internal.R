#' Internal xplots functions
#'
#' Internal xplots functions
#' @details
#' These functions are not intended to be called by the user.
#' @name xplots-internal
#' @keywords internal
NULL

#' @keywords internal
#' @rdname xplots-internal
plotUG <- function(marks, plotTitle, zeros, altAssRows, alt){

  # If zeros = FALSE then replace values of 0 with R's missing value code NA
  if (!zeros) {
    marks[marks == 0] <- NA
    gap <- 0
  } else {
    gap <- 0.5
  }

  # Produces scatter plots of columns of component marks in `marks'.
  UGpanel <- function(x, y, lty = 3, col){
    graphics::abline(0, 1, lwd = 0.5)
    graphics::points(x[-c(1, 2)], y[-c(1, 2)], pch = 20, xpd = TRUE)
    graphics::axis(side = 1, at = c(0, 40, 70, 100), labels = FALSE,
                   tcl = -0.2)
    graphics::axis(side = 2, at = c(0, 40, 70, 100), labels = FALSE,
                   tcl = -0.2)
    graphics::segments(0, 40, 40, 40, lty = lty)
    graphics::segments(40, 0, 40, 40, lty = lty)
    graphics::segments(70, 70, 70, 100, lty = lty)
    graphics::segments(70, 70, 100, 70, lty = lty)
    graphics::points(x[-c(1, 2)], y[-c(1, 2)], pch = 20, col = col)
  }

  # Produce the plot
  marks1 <- rbind(0, 100, marks)
  col <- rep("black", nrow(marks))
  if (alt == "red") {
    col[altAssRows] <- "red"
  } else if (alt == "exclude") {
    col[altAssRows] <- "white"
  }
  graphics::par(las = 1, xaxs = "i", yaxs = "i", lab = c(4, 4, 7))
  graphics::pairs(marks1, oma = c(14, 3, 14, 3), panel = UGpanel, gap = gap,
                  col = col)
  graphics::title(plotTitle)

  return(invisible())
}

#' @keywords internal
#' @rdname xplots-internal
plotPG <- function(marks, plotTitle, zeros, altAssRows, alt){

  # If zeros = FALSE then replace values of 0 with R's missing value code NA
  if (!zeros) {
    marks[marks == 0] <- NA
    gap <- 0
  } else {
    gap <- 0.5
  }

  # Produces scatter plots of columns of component marks in `marks'.
  PGpanel <- function(x, y, lty = 3, col){
    graphics::rect(0, 0, 40, 100, border = NA, col = grDevices::grey(0.8))
    graphics::rect(0, 0, 100, 40, border = NA, col = grDevices::grey(0.8))
    graphics::abline(0, 1, lwd = 0.5)
    plotchars <- rep(20, length(x))
    plotchars[x < 40 | y < 40] <- 13
    graphics::points(x[-c(1, 2)], y[-c(1, 2)], pch = plotchars[-c(1, 2)],
                     col = col)
    graphics::axis(side = 1, at = c(0, 40, 70, 100), tcl = -0.2,
                   labels = FALSE)
    graphics::axis(side = 2, at = c(0, 40, 70, 100), tcl = -0.2,
                   labels = FALSE)
    graphics::axis(3, labels = FALSE)
    graphics::axis(4, labels = FALSE)
    graphics::segments(0, 50, 50, 50, lty = lty)
    graphics::segments(50, 0, 50, 50, lty = lty)
    graphics::segments(70, 70, 70, 100, lty = lty)
    graphics::segments(70, 70, 100, 70, lty = lty)
  }

  # Produce the plot
  marks1 <- rbind(0, 100, marks)
  col <- rep("black", nrow(marks))
  if (alt == "red") {
    col[altAssRows] <- "red"
  } else if (alt == "exclude") {
    col[altAssRows] <- "white"
  }
  graphics::par(las = 1, xaxs = "i", yaxs = "i", lab = c(4, 4, 7))
  graphics::pairs(marks1, oma = c(14, 3, 14, 3), panel = PGpanel, gap = gap,
                  col = col)
  graphics::title(plotTitle)

  return(invisible())
}
