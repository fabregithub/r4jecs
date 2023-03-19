#' Fill scale constructor for Jecs colours
#'
#' Fill scale constructor for Jecs colours
#'
#' @import ggplot2
#'
#' @param palette Character name of palette in jecs_palettes
#' @param discrete Boolean indicating whether colour aesthetic is discrete or not
#' @param reverse Boolean indicating whether the palette should be reversed
#' @param ... Additional arguments passed to discrete_scale() or
#'            scale_fill_gradientn(), used respectively when discrete is TRUE or FALSE
#'
#' @export
#'
#' @examples
#' \dontrun{
#' library(ggplot2)
#' # Fill by discrete variable with different palette + remove legend (guide)
#' ggplot(mpg, aes(manufacturer, fill = manufacturer)) +
#' geom_bar() +
#'   theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
#'   scale_fill_jecs(palette = 'jecs', guide = 'none')
#' }
#'


scale_fill_jecs <- function(palette = 'jecs', discrete = TRUE, reverse = FALSE, ...) {
  pal <- jecs_pal(palette = palette, reverse = reverse)

  if (discrete) {
    discrete_scale('fill', paste0('jecs_', palette), palette = pal, ...)
  } else {
    scale_fill_gradientn(colours = pal(256), ...)
  }
}

