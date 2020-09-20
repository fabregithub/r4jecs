#' Return function to interpolate an Arashi colour palette
#'
#' Return function to interpolate an Arashi colour palette
#'
#' @param palette Character name of palette in arashi_palettes
#' @param reverse Boolean indicating whether the palette should be reversed
#' @param ... Additional arguments to pass to colorRampPalette()
#'


# Return function to interpolate a Arashi colour palette
arashi_pal <- function(palette = 'arashi', reverse = FALSE, ...) {
  pal <- arashi_palettes[[palette]]

  if (reverse) pal <- rev(pal)

  colorRampPalette(pal, ...)
}

