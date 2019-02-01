## ----setup, echo=FALSE, results='hide', warning=FALSE, error=FALSE, message=FALSE, cache=FALSE----
library(knitr)
opts_chunk$set(
  cache       = TRUE,
  autodep     = TRUE,
  echo        = FALSE,
  warning     = FALSE,
  error       = FALSE,
  message     = FALSE,
  out.width   = 700,
  fig.width   = 12,
  fig.height  = 8,
  dpi         = 300,
  cache.path  = "cache/ggrepel/",
  fig.path    = "figures/ggrepel/",
  pngquant    = "--speed=1 --quality=0-10",
  concordance = TRUE
)
knit_hooks$set(
  pngquant = hook_pngquant
)
library(gridExtra)
library(ggplot2)
theme_set(theme_classic(base_size = 18) %+replace% theme(
  axis.line.y = element_line(colour = "black", size = 0.2),
  axis.line.x = element_line(colour = "black", size = 0.2),
  axis.ticks = element_line(colour = "black", size = 0.2)
))

