#' Colour scale constructor for Jecs colours
#'
#' Colour scale constructor for Jecs colours
#'
#' @import ggplot2
#'
#' @param palette Character name of palette in jecs_palettes
#' @param discrete Boolean indicating whether colour aesthetic is discrete or not
#' @param reverse Boolean indicating whether the palette should be reversed
#' @param ... Additional arguments passed to discrete_scale() or
#'            scale_color_gradientn(), used respectively when discrete is TRUE or FALSE
#'
# library(ggplot2)
# # Colour by discrete variable using default palette
# ggplot(iris, aes(Sepal.Width, Sepal.Length, colour = Species)) +
#   geom_point(size = 4) +
#   scale_colour_jecs()
#
# # Colour by numeric variable with cool palette
# ggplot(iris, aes(Sepal.Width, Sepal.Length, colour = Sepal.Length)) +
#   geom_point(size = 4, alpha = .6) +
#   scale_colour_jecs(discrete = FALSE, palette = 'mj')

# Colour scale constructor for Arashi colours
scale_colour_jecs <- function(palette = 'jecs', discrete = TRUE, reverse = FALSE, ...) {
  pal <- jecs_pal(palette = palette, reverse = reverse)

  if (discrete) {
    discrete_scale('colour', paste0('jecs_', palette), palette = pal, ...)
  } else {
    scale_color_gradientn(colours = pal(256), ...)
  }
}


