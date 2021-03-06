#' Function to extract Jecs colours as hex codes
#'
#' This is to create Jecs colour palette for ggplot2. First a function to
#' extract Jecs colours as hex codes
#'
#' @param ... Character names of jecs_colours
#'
#' @export
#'
#'
#'

jecs_cols <- function(...) {
  cols <- c(...)

  if (is.null(cols))
    return (jecs_colours)

  jecs_colours[cols]
}

# Jecs colours
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

jecs_colours <- c(
  mj_colours,
  os_colours,
  nk_colours,
  am_colours,
  ss_colours
)

# Jecs colour palettes
jecs_palettes <- list(
  `am` = jecs_cols('am0', 'am1', 'am2', 'am3', 'am4', 'am5'),
  `mj` = jecs_cols('mj0', 'mj1', 'mj2', 'mj3', 'mj4', 'mj5'),
  `nk` = jecs_cols('nk0', 'nk1', 'nk2', 'nk3', 'nk4', 'nk5'),
  `os` = jecs_cols('os0', 'os1', 'os2', 'os3', 'os4', 'os5'),
  `ss` = jecs_cols('ss0', 'ss1', 'ss2', 'ss3', 'ss4', 'ss5'),
  `jecs` = jecs_cols('am0', 'mj0', 'nk0', 'os0', 'ss0')
)
