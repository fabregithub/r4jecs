#' Fill scale constructor for Arashi colours
#'
#' Fill scale constructor for Arashi colours
#'
#' @import ggplot2
#'
#' @param palette Character name of palette in arashi_palettes
#' @param discrete Boolean indicating whether colour aesthetic is discrete or not
#' @param reverse Boolean indicating whether the palette should be reversed
#' @param ... Additional arguments passed to discrete_scale() or
#'            scale_fill_gradientn(), used respectively when discrete is TRUE or FALSE
#'
# library(ggplot2)
# # Fill by discrete variable with different palette + remove legend (guide)
# ggplot(mpg, aes(manufacturer, fill = manufacturer)) +
#   geom_bar() +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
#   scale_fill_arashi(palette = 'arashi', guide = 'none')
#


scale_fill_arashi <- function(palette = 'arashi', discrete = TRUE, reverse = FALSE, ...) {
  pal <- arashi_pal(palette = palette, reverse = reverse)

  if (discrete) {
    discrete_scale('fill', paste0('arashi_', palette), palette = pal, ...)
  } else {
    scale_fill_gradientn(colours = pal(256), ...)
  }
}

