null <- NULL
save(null,file="data/tables.RData")

# ***************** siteR: TBL_NEHRP ----

SID_SET <- c("BC","C","CD","D","DE")
DT <- data.table(
  SC=c("A","B","BC","C","CD","D","DE","E","F"),
  Description=c(
    "Hard rock",
    "Medium hard rock",
    "Soft rock",
    "Very dense soil or hard clay",
    "Dense sand of very stiff clay", 
    "Medium dense sand or stiff clay",
    "Loose sand or medium stiff clay",
    "Very loose sand or soft clay",
    "Soils requiring site response analysis (ASCE 7-22 21.1)"),
  
  "Vs30 [m/s]"=c(">1500",">910-1500",">640-910",">440-640",">300-440",">210-300",">150-210",">=150","*See section 20.2.1"))

TBL_NEHRP <- DT[order(SC)] |> flextable() |> 
  theme_vanilla() |> 
  align(align="center",part="all") |>
  align(align="left",j=2) |>
  set_table_properties(layout = "autofit") |> 
  autofit(part="all") |> 
  bold(part = "header")

resave(TBL_NEHRP,file="data/tables.RData")


# ***************** hazard: TBL_AEP_GISTM TBL_AEP_CDA TBL_AEP_ANCOLD----
Category <- c("Low","Significant","High","Very High","Extreme")
OBE <- c("1:200","1:1,000","1:2,500","1:5,000","1:10,000")
MDE <- c("1:10,000","1:10,000","1:10,000","1:10,000","1:10,000")

DT <- data.table(
  "Category"=Category,
  "Active care (OBE)"=OBE,
  "Passive care (MDE)"=MDE)
CAPTION <- "Annual Exceedance Probability (AEP) and Risk Category "
TBL_AEP_GISTM <- DT |> flextable() |> 
  theme_vanilla() |> 
  align(align="center",part="all") |>
  add_header_row(values = c(" ","AEP [1:yr]"), colwidths = c(1,ncol(DT)-1))  |> 
  
  set_table_properties(layout = "autofit") |> 
  autofit(part="all") |> 
  bold(part = "header")
  
resave(TBL_AEP_GISTM,file="data/tables.RData")

Category <- c("Low","Significant","High","Very High","Extreme")
OBE <- c("1:100","1:100 - 1:1,1000","1:2,475","1:2,475 - 1:10,000","1:10,000")
MDE <- c("1:1,000","1:2,475","1:2,475 - 1:10,000","1:10,000","1:10,000")

DT <- data.table(
  "Category"=Category,
  "Active care (OBE)"=OBE,
  "Passive care (MDE)"=MDE)
CAPTION <- "Annual Exceedance Probability (AEP) and Risk Category "
TBL_AEP_CDA<- DT |> flextable() |> 
  theme_vanilla() |> 
  align(align="center",part="all") |>
  add_header_row(values = c(" ","AEP [1:yr]"), colwidths = c(1,ncol(DT)-1))  |> 
  
  set_table_properties(layout = "autofit") |> 
  autofit(part="all") |> 
  bold(part = "header")

resave(TBL_AEP_CDA,file="data/tables.RData")



Category <- c("Low","Significant","High A","High B","High C","Extreme")
OBE <- c("1:475","1:475","1:475 - 1:1,000","1:475 - 1:1,000","1:475 - 1:1,000","1:475 - 1:1,000")
MDE <- c("1:1,000","1:1,1000","1:2,000","1:5,000","1:10,000","1:10,000")

DT <- data.table(
  "Category"=Category,
  "Active care (OBE)"=OBE,
  "Passive care (SEE)"=MDE)
CAPTION <- "Annual Exceedance Probability (AEP) and Risk Category "
TBL_AEP_ANCOLD<- DT |> flextable() |> 
  theme_vanilla() |> 
  align(align="center",part="all") |>
  add_header_row(values = c(" ","AEP [1:yr]"), colwidths = c(1,ncol(DT)-1))  |> 
  
  set_table_properties(layout = "autofit") |> 
  autofit(part="all") |> 
  bold(part = "header")

resave(TBL_AEP_ANCOLD,file="data/tables.RData")
