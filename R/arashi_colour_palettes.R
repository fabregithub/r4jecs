#' Return function to interpolate an Jecs colour palette
#'
#' Return function to interpolate an Jecs colour palette
#'
#' @param palette Character name of palette in jecs_palettes
#' @param reverse Boolean indicating whether the palette should be reversed
#' @param ... Additional arguments to pass to colorRampPalette()
#'


# Return function to interpolate a Jecs colour palette
jecs_pal <- function(palette = 'jecs', reverse = FALSE, ...) {
  pal <- jecs_palettes[[palette]]

  if (reverse) pal <- rev(pal)

  colorRampPalette(pal, ...)
}

