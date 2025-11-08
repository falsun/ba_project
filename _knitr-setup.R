
library(chunkhooks)

chunkhooks::hook_figure_unit('cm')

knitr::opts_chunk$set(
  dev = "svglite",
  fig.width = 30,
  fig.asp = 0.75,
  echo = FALSE,
  warning = FALSE,
  message = FALSE,
  include = TRUE
)
