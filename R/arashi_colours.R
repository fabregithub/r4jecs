#' Arashi colour palette
#'
#' This is to create Arashi colour palette for ggplot2
#'
#' @importFrom ggplot2
#'
#' @examples
#' # Colour by discrete variable using default palette
#' ggplot(iris, aes(Sepal.Width, Sepal.Length, colour = Species)) +
#'   geom_point(size = 4) +
#'   scale_colour_arashi()
#'
#' # Colour by numeric variable with cool palette
#' ggplot(iris, aes(Sepal.Width, Sepal.Length, colour = Sepal.Length)) +
#'   geom_point(size = 4, alpha = .6) +
#'   scale_colour_arashi(discrete = FALSE, palette = 'mj')
#'
#' # Fill by discrete variable with different palette + remove legend (guide)
#' ggplot(mpg, aes(manufacturer, fill = manufacturer)) +
#'   geom_bar() +
#'   theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
#'   scale_fill_arashi(palette = 'arashi', guide = 'none')

theme_set(theme_minimal())

# Arashi colours
# https://www.colordic.org/w
# https://www.colordic.org/colorscheme/2391
mj_colours <- c(
  `mj0` = '#b44c97',
  `mj1` = '#b50082',
  `mj2` = '#b5248c',
  `mj3` = '#b54896',
  `mj4` = '#b56da1',
  `mj5` = '#b591ab'
)

# https://www.colordic.org/colorscheme/2077
os_colours <- c(
  `os0` = '#007bbb',
  `os1` = '#007bbb',
  `os2` = '#2586ba',
  `os3` = '#4a93ba',
  `os4` = '#70a0ba',
  `os5` = '#95adba'
)

# https://www.colordic.org/colorscheme/2165
nk_colours <- c(
  `nk0` = '#ffd900',
  `nk1` = '#ffd900',
  `nk2` = '#ffe033',
  `nk3` = '#ffe866',
  `nk4` = '#fff099',
  `nk5` = '#fff7cc'
)

# https://www.colordic.org/colorscheme/2410
am_colours <- c(
  `am0` = '#c7dc68',
  `am1` = '#b3db00',
  `am2` = '#bbdb2c',
  `am3` = '#c3db58',
  `am4` = '#cbdb84',
  `am5` = '#d3dbaf'
)

# https://www.colordic.org/colorscheme/2014
ss_colours <- c(
  `ss0` = '#d7003a',
  `ss1` = '#d7003a',
  `ss2` = '#d62b59',
  `ss3` = '#d65678',
  `ss4` = '#d68197',
  `ss5` = '#d6abb7'
)

arashi_colours <- c(
  mj_colours,
  os_colours,
  nk_colours,
  am_colours,
  ss_colours
)

#' Function to extract Arashi colours as hex codes
#'
#' @param ... Character names of arashi_colours
#'
arashi_cols <- function(...) {
  cols <- c(...)

  if (is.null(cols))
    return (arashi_colours)

  arashi_colours[cols]
}
# arashi_cols()

# Arashi colour palettes
arashi_palettes <- list(
  `mj` = arashi_cols('mj0', 'mj1', 'mj2', 'mj3', 'mj4', 'mj5'),
  `os` = arashi_cols('os0', 'os1', 'os2', 'os3', 'os4', 'os5'),
  `am` = arashi_cols('am0', 'am1', 'am2', 'am3', 'am4', 'am5'),
  `nk` = arashi_cols('nk0', 'nk1', 'nk2', 'nk3', 'nk4', 'nk5'),
  `ss` = arashi_cols('ss0', 'ss1', 'ss2', 'ss3', 'ss4', 'ss5'),
  `arashi` = arashi_cols('mj0', 'os0', 'am0', 'nk0', 'ss0')
)

#' Return function to interpolate a Arashi colour palette
#'
#' @param palette Character name of palette in arashi_palettes
#' @param reverse Boolean indicating whether the palette should be reversed
#' @param ... Additional arguments to pass to colorRampPalette()
#'
arashi_pal <- function(palette = 'arashi', reverse = FALSE, ...) {
  pal <- arashi_palettes[[palette]]

  if (reverse) pal <- rev(pal)

  colorRampPalette(pal, ...)
}
arashi_pal('mj')(6)

#' Colour scale constructor for Arashi colours
#'
#' @param palette Character name of palette in arashi_palettes
#' @param discrete Boolean indicating whether colour aesthetic is discrete or not
#' @param reverse Boolean indicating whether the palette should be reversed
#' @param ... Additional arguments passed to discrete_scale() or
#'            scale_color_gradientn(), used respectively when discrete is TRUE or FALSE
#'
scale_colour_arashi <- function(palette = 'arashi', discrete = TRUE, reverse = FALSE, ...) {
  pal <- arashi_pal(palette = palette, reverse = reverse)

  if (discrete) {
    discrete_scale('colour', paste0('arashi_', palette), palette = pal, ...)
  } else {
    scale_color_gradientn(colours = pal(256), ...)
  }
}

#' Fill scale constructor for Arashi colours
#'
#' @param palette Character name of palette in arashi_palettes
#' @param discrete Boolean indicating whether colour aesthetic is discrete or not
#' @param reverse Boolean indicating whether the palette should be reversed
#' @param ... Additional arguments passed to discrete_scale() or
#'            scale_fill_gradientn(), used respectively when discrete is TRUE or FALSE
#'
scale_fill_arashi <- function(palette = 'arashi', discrete = TRUE, reverse = FALSE, ...) {
  pal <- arashi_pal(palette = palette, reverse = reverse)

  if (discrete) {
    discrete_scale('fill', paste0('arashi_', palette), palette = pal, ...)
  } else {
    scale_fill_gradientn(colours = pal(256), ...)
  }
}




