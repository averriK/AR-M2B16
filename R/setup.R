library(data.table)
library(stringr)
library(stats)
library(flextable)
library(highcharter)
library(hrbrthemes,warn.conflicts = F, quietly = T)
library(readxl)
# flextable ----
knitr::opts_chunk$set("ft.shadow" = FALSE)
flextable::use_df_printer()
flextable::set_flextable_defaults( font.family="Helvetica",border.color = "gray")



