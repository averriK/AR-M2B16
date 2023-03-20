library(data.table)
library(stringr)
library(stats)
library(flextable)
library(highcharter)
library(hrbrthemes)
library(readxl)
# flextable ----
knitr::opts_chunk$set("ft.shadow" = FALSE)
flextable::use_df_printer()
flextable::set_flextable_defaults( font.family="Helvetica",border.color = "gray")



resave <- function(..., list = character(), file) {
  previous  <- load(file)
  var.names <- c(list, as.character(substitute(list(...)))[-1L])
  for (var in var.names) assign(var, get(var, envir = parent.frame()))
  save(list = unique(c(previous, var.names)), file = file)
}
