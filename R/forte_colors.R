#' FoRTE color palette

#' The FoRTE color palettte
#'
#' These are a handful of color palettes pulled from photographs of US National Parks.
#'
#'
#' @return A vector of colours.
#' @export
#' @keywords colors
#' @examples
#'
#' pal <- forte_colors()
forte_colors <- function() {
  pal <- c("#000000", "#009E73", "#0072B2", "#D55E00")
  #structure(pal, class = "palette", name = forte)
}

#' @export
#' @importFrom graphics rect par image text
#' @importFrom grDevices rgb
print.palette <- function(x, ...) {
  n <- length(x)
  old <- par(mar = c(0.5, 0.5, 0.5, 0.5))
  on.exit(par(old))

  image(1:n, 1, as.matrix(1:n), col = x,
        ylab = "", xaxt = "n", yaxt = "n", bty = "n")

  rect(0, 0.9, n + 1, 1.1, col = rgb(1, 1, 1, 0.8), border = NA)
  text((n + 1) / 2, 1, labels = attr(x, "name"), cex = 1, family = "serif")
}

