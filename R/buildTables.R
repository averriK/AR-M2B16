

# ***************** hazard: TBL_Mm,TBL_Rm----
DT1 <- buildTable_MmRm(DisaggTable=UserData$DisaggTable,Tn=0,SN=SN_TARGET)

# 
DT2 <- buildTable_MmRm(DisaggTable=UserData$DisaggTable,Tn=0.2,SN=SN_TARGET) 

# 
DT3 <- buildTable_MmRm(DisaggTable=UserData$DisaggTable,Tn=0.5,SN=SN_TARGET)

DT4 <- buildTable_MmRm(DisaggTable=UserData$DisaggTable,Tn=0.7,SN=SN_TARGET)

DT5 <- buildTable_MmRm(DisaggTable=UserData$DisaggTable,Tn=1.0,SN=SN_TARGET)

DT6 <- buildTable_MmRm(DisaggTable=UserData$DisaggTable,Tn=1.5,SN=SN_TARGET)

                       
DT <- rbindlist(list(DT1,DT2,DT3,DT4,DT5,DT6))[,-c("NEHRP","Rm")]
DT <- reshape(DT,idvar =c("TR"), timevar = c("Tn"), direction = "wide")
COL <- colnames(DT)
NCOL <- str_replace(COL,pattern = "Mm[[.]]","Tn=")
setnames(DT,old=COL,new=NCOL)
TBL_Mm <- DT[order(TR)] |> flextable() |>
  theme_vanilla() |>
  align(align="center",part="all") |>
  set_table_properties(layout = "autofit") |>
  autofit(part="all") |>
  add_header_row(values = c(" ",paste0("Mm")), colwidths = c(1,ncol(DT)-1))  |>
  bold(part = "header")



DT <- rbindlist(list(DT1,DT2,DT3,DT4,DT5,DT6))[,-c("NEHRP","Mm")]
DT <- reshape(DT,idvar =c("TR"), timevar = c("Tn"), direction = "wide")
COL <- colnames(DT)
NCOL <- str_replace(COL,pattern = "Rm[[.]]","Tn=")
setnames(DT,old=COL,new=NCOL)
TBL_Rm <- DT[order(TR)] |> flextable() |>
  theme_vanilla() |>
  align(align="center",part="all") |>
  set_table_properties(layout = "autofit") |>
  autofit(part="all") |>
  add_header_row(values = c(" ",paste0("Rm [km]")), colwidths = c(1,ncol(DT)-1))  |>
  bold(part = "header")





# ***************** hazard: TBL_AEP_GISTM TBL_AEP_CDA TBL_AEP_ANCOLD----
Category <- c("Low","Significant","High","Very High","Extreme")
OBE <- c("1:200","1:1,000","1:2,500","1:5,000","1:10,000")
MDE <- c("1:10,000","1:10,000","1:10,000","1:10,000","1:10,000")

DT <- data.table(
  "Category"=Category,
  "Active Care [1/yr]"=OBE,
  "Passive Care [1/yr]"=MDE)
CAPTION <- "Annual Exceedance Probability (AEP) and Risk Category "
TBL_AEP_GISTM <- DT |> flextable() |> 
  theme_vanilla() |> 
  align(align="center",part="all") |>
  add_header_row(values = c(" ","AEP [1:yr]"), colwidths = c(1,ncol(DT)-1))  |> 
  
  set_table_properties(layout = "autofit") |> 
  autofit(part="all") |> 
  bold(part = "header")
  
# ***************** hazard: TBL_Sa_84, TBL_Sa_mean-------
TR_TARGET <- 10000
Tn_SET <- c(0.2,0.5,1.0)
TR_SET <- c(500,1000,2500,5000,10000)

pID_TARGET <- "mean"
Tn_TARGET <- 0
DT <- buildTable_Sa(Tn=Tn_TARGET ,SaTR=UserData$SaTR,SN=SN_TARGET,pID=pID_TARGET,SID_SET="BC")

for(tn in Tn_SET){
  DT <- rbindlist(list(DT,buildTable_Sa(Tn=tn ,SaTR=UserData$SaTR,SN=SN_TARGET,pID=pID_TARGET,SID_SET="BC")))
}
setnames(DT,old="NEHRP",new="SC")

CAPTION <- "Design ground-motions in [g] - (+84% fractiles)"
TBL_Sa_mean <- DT[order(SC)] |> flextable() |>
  theme_vanilla() |>
  align(align="center",part="all") |>
  set_table_properties(layout = "autofit") |>
  autofit(part="all") |>
  add_header_row(values = c(" ",paste0("Sa(Tn) [g] - p=",pID_TARGET)), colwidths = c(1,ncol(DT)-1))  |>
  bold(part = "header")


pID_TARGET <- "+84%"
Tn_TARGET <- 0
DT <- buildTable_Sa(Tn=Tn_TARGET ,SaTR=UserData$SaTR,SN=SN_TARGET,pID=pID_TARGET,SID_SET="BC")

for(tn in Tn_SET){
  DT <- rbindlist(list(DT,buildTable_Sa(Tn=tn ,SaTR=UserData$SaTR,SN=SN_TARGET,pID=pID_TARGET,SID_SET="BC")))
}
setnames(DT,old="NEHRP",new="SC")

CAPTION <- "Design ground-motions in [g] - (+84% fractiles)"
TBL_Sa_84 <- DT[order(SC)] |> flextable() |>
  theme_vanilla() |>
  align(align="center",part="all") |>
  set_table_properties(layout = "autofit") |>
  autofit(part="all") |>
  add_header_row(values = c(" ",paste0("Sa(Tn) [g] - p=",pID_TARGET)), colwidths = c(1,ncol(DT)-1))  |>
  bold(part = "header")


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



# ***************** siteR: TBL_AF_84, TBL_AF_mean -------

SID_SET <- c("BC","C","CD","D","DE")
pID_TARGET <- "+84%"
DT <- buildTable_AFPGA(SaTR=UserData$SaTR,pID=pID_TARGET,SID_SET=SID_SET,SN=SN_TARGET) #|> setnames(old=c("NEHRP"),new=c("SC"))

TBL_AF_84 <- DT[order(SC)] |> flextable() |>
  theme_vanilla() |>
  align(align="center",part="all") |>
  set_table_properties(layout = "autofit") |>
  autofit(part="all") |>
  add_header_row(values = c(" ","F(Tn=0)"), colwidths = c(1,ncol(DT)-1))  |>
  bold(part = "header")

pID_TARGET <- "mean"
DT <- buildTable_AFPGA(SaTR=UserData$SaTR,pID=pID_TARGET,SID_SET=SID_SET,SN=SN_TARGET) #|> setnames(old=c("NEHRP"),new=c("SC"))

TBL_AF_mean <- DT[order(SC)] |> flextable() |>
  theme_vanilla() |>
  align(align="center",part="all") |>
  set_table_properties(layout = "autofit") |>
  autofit(part="all") |>
  add_header_row(values = c(" ","F(Tn=0)"), colwidths = c(1,ncol(DT)-1))  |>
  bold(part = "header")

# ***************** siteR: TBL_AFPGA_84, TBL_AFPGA_mean -------
pID_TARGET <- "+84%"
SID_SET <- c("BC","C","CD","D","DE")

DT <- buildTable_PGA(SaTR=UserData$SaTR,SN=SN_TARGET,pID=pID_TARGET,SID_SET=SID_SET) #|> setnames(old=c("NEHRP"),new=c("SC"))
TBL_AFPGA_84 <- DT[order(SC)] |> flextable() |>
  theme_vanilla() |> 
  align(align="center",part="all") |>
  set_table_properties(layout = "autofit") |> 
  autofit(part="all") |> 
  add_header_row(values = c(" ","PGA [g]"), colwidths = c(1,ncol(DT)-1))  |> 
  bold(part = "header")


pID_TARGET <- "mean"
DT <- buildTable_PGA(SaTR=UserData$SaTR,SN=SN_TARGET,pID=pID_TARGET,SID_SET=SID_SET) #|> setnames(old=c("NEHRP"),new=c("SC"))
TBL_AFPGA_mean <- DT[order(SC)] |> flextable() |>
  theme_vanilla() |> 
  align(align="center",part="all") |>
  set_table_properties(layout = "autofit") |> 
  autofit(part="all") |> 
  add_header_row(values = c(" ","PGA [g]"), colwidths = c(1,ncol(DT)-1))  |> 
  bold(part = "header")



# ***************** siteR: TBL_AFPGV_84,TBL_AFPGV_mean -------
pID_TARGET <- "+84%"
SID_SET <- c("BC","C","CD","D","DE")
DT <- buildTable_PGV(SaTR=UserData$SaTR,SN=SN_TARGET,pID=pID_TARGET,SID_SET=SID_SET) #|> setnames(old=c("NEHRP"),new=c("SC"))
TBL_AFPGV_84 <- DT[order(SC)] |> flextable() |>
  theme_vanilla() |> 
  align(align="center",part="all") |>
  set_table_properties(layout = "autofit") |> 
  autofit(part="all") |> 
  add_header_row(values = c(" ","PGV [m/s]"), colwidths = c(1,ncol(DT)-1))  |> 
  bold(part = "header")

pID_TARGET <- "mean"
DT <- buildTable_PGV(SaTR=UserData$SaTR,SN=SN_TARGET,pID=pID_TARGET,SID_SET=SID_SET) #|> setnames(old=c("NEHRP"),new=c("SC"))
TBL_AFPGV_mean <- DT[order(SC)] |> flextable() |>
  theme_vanilla() |> 
  align(align="center",part="all") |>
  set_table_properties(layout = "autofit") |> 
  autofit(part="all") |> 
  add_header_row(values = c(" ","PGV [m/s]"), colwidths = c(1,ncol(DT)-1))  |> 
  bold(part = "header")


# ***************** siteR: TBL_AFAI_mean, TBL_AFAI_84 -------
pID_TARGET <- "+84%"
SID_SET <- c("BC","C","CD","D","DE")
DT <- buildTable_AI(SaTR=UserData$SaTR,SN=SN_TARGET,pID=pID_TARGET,SID_SET=SID_SET) #|> setnames(old=c("NEHRP"),new=c("SC"))
TBL_AFAI_84 <- DT[order(SC)] |> flextable() |>
  theme_vanilla() |> 
  align(align="center",part="all") |>
  set_table_properties(layout = "autofit") |> 
  autofit(part="all") |> 
  add_header_row(values = c(" ","AI [m/s]"), colwidths = c(1,ncol(DT)-1))  |> 
  bold(part = "header")

pID_TARGET <- "mean"
SID_SET <- c("BC","C","CD","D","DE")
DT <- buildTable_AI(SaTR=UserData$SaTR,SN=SN_TARGET,pID=pID_TARGET,SID_SET=SID_SET) #|> setnames(old=c("NEHRP"),new=c("SC"))
TBL_AFAI_mean <- DT[order(SC)] |> flextable() |>
  theme_vanilla() |> 
  align(align="center",part="all") |>
  set_table_properties(layout = "autofit") |> 
  autofit(part="all") |> 
  add_header_row(values = c(" ","AI [m/s]"), colwidths = c(1,ncol(DT)-1))  |>  
  bold(part = "header")



# ***************** siteR: tr=10000 TBL_AFSa_TR10000_84, TBL_AFSa_TR10000_mean-------
SID_SET <- c("BC","C","CD","D","DE")
TR_TARGET <- 10000
Tn_SET <- c(0.2,0.3,0.5,0.75,1.0,1.5,2.5)
TR_SET <- c(500,1000,2500,5000,10000)
pID_TARGET <- "+84%"
Tn_TARGET <- 0
DT <- buildTable_Sa(Tn=Tn_TARGET ,SaTR=UserData$SaTR,SN=SN_TARGET,pID=pID_TARGET,SID_SET=SID_SET,TR_SET=TR_TARGET)[,-c("Tn")]|> setnames(old=c("NEHRP",paste0("TR=",TR_TARGET)),new=c("SC","PGA"))
for(tn in Tn_SET){
  DT <- DT[(buildTable_Sa(Tn=tn ,SaTR=UserData$SaTR,SN=SN_TARGET,pID=pID_TARGET,SID_SET=SID_SET,TR_SET=TR_TARGET)[,-c("Tn")]|> setnames(old=c("NEHRP",paste0("TR=",TR_TARGET)),new=c("SC",paste0("Sa(",tn,")")))),on=.(SC)]
}
CAPTION <- "Design ground-motions in [g] - (+84% fractiles)"
TBL_AFSa_TR10000_84 <- DT[order(SC)] |> flextable() |>
  theme_vanilla() |>
  align(align="center",part="all") |>
  set_table_properties(layout = "autofit") |>
  autofit(part="all") |>
  add_header_row(values = c(" ",paste0("Sa(Tn) [g] - p=",pID_TARGET)), colwidths = c(1,ncol(DT)-1))  |>
  bold(part = "header")


pID_TARGET <- "mean"
Tn_TARGET <- 0
DT <- buildTable_Sa(Tn=Tn_TARGET ,SaTR=UserData$SaTR,SN=SN_TARGET,pID=pID_TARGET,SID_SET=SID_SET,TR_SET=TR_TARGET)[,-c("Tn")]|> setnames(old=c("NEHRP",paste0("TR=",TR_TARGET)),new=c("SC","PGA"))
for(tn in Tn_SET){
  DT <- DT[(buildTable_Sa(Tn=tn ,SaTR=UserData$SaTR,SN=SN_TARGET,pID=pID_TARGET,SID_SET=SID_SET,TR_SET=TR_TARGET)[,-c("Tn")]|> setnames(old=c("NEHRP",paste0("TR=",TR_TARGET)),new=c("SC",paste0("Sa(",tn,")")))),on=.(SC)]
}
CAPTION <- "Design ground-motions in [g] - (+84% fractiles)"
TBL_AFSa_TR0000_mean <- DT[order(SC)] |> flextable() |>
  theme_vanilla() |>
  align(align="center",part="all") |>
  set_table_properties(layout = "autofit") |>
  autofit(part="all") |>
  add_header_row(values = c(" ",paste0("Sa(Tn) [g] - p=",pID_TARGET)), colwidths = c(1,ncol(DT)-1))  |>
  bold(part = "header")


# ***************** siteR: TBL_AFSa_Tn1_84 TBL_AFSa_Tn1_mean -------

SID_SET <- c("BC","C","CD","D","DE")

pID_TARGET <- "+84%"
Tn_TARGET <- 1.0
DT <- buildTable_AFSa(SaTR=UserData$SaTR,Tn=Tn_TARGET,pID=pID_TARGET,SID_SET=SID_SET,SN=SN_TARGET) # |> setnames(old=c("NEHRP"),new=c("SC"))
TBL_AFSa_Tn1_84 <- DT[order(SC)] |> flextable() |>
  theme_vanilla() |>
  align(align="center",part="all") |>
  set_table_properties(layout = "autofit") |>
  autofit(part="all") |>
  add_header_row(values = c(" ",paste0("AF(Tn=",Tn_TARGET,") - p=",pID_TARGET)), colwidths = c(1,ncol(DT)-1))  |>
  bold(part = "header")

pID_TARGET <- "mean"
Tn_TARGET <- 1.0
DT <- buildTable_AFSa(SaTR=UserData$SaTR,Tn=Tn_TARGET,pID=pID_TARGET,SID_SET=SID_SET,SN=SN_TARGET) # |> setnames(old=c("NEHRP"),new=c("SC"))
TBL_AFSa_Tn1_mean <- DT[order(SC)] |> flextable() |>
  theme_vanilla() |>
  align(align="center",part="all") |>
  set_table_properties(layout = "autofit") |>
  autofit(part="all") |>
  add_header_row(values = c(" ",paste0("AF(Tn=",Tn_TARGET,") - p=",pID_TARGET)), colwidths = c(1,ncol(DT)-1))  |>
  bold(part = "header")



# ***************** slopeR: TBL_kh_Ts ----
Ts_TARGET<- 0.65
TR_TARGET <- 10000
pID_TARGET <- "+84%"
Da_TARGET <- Da0
DT <- buildTable_Kh(SN=SN_TARGET,SaTR = UserData$SaTR,BM19TR = UserData$BM19TR,Da=Da_TARGET,Ts=Ts_TARGET,SID_SET=SID_SET,TR_SET = TR_TARGET,pID=pID_TARGET)[,-c("Da","Ts")] |> setnames(old=c("NEHRP",paste0("TR=",TR_TARGET)),new=c("SC",paste0("Da=",Da_TARGET,"cm")))

#*****
Da_TARGET <- 2*Da0
DT <- DT[(buildTable_Kh(SN=SN_TARGET,SaTR = UserData$SaTR,BM19TR = UserData$BM19TR,Da=Da_TARGET,Ts=Ts_TARGET,SID_SET=SID_SET,TR_SET = TR_TARGET,pID=pID_TARGET)[,-c("Da","Ts")] |> setnames(old=c("NEHRP",paste0("TR=",TR_TARGET)),new=c("SC",paste0("Da=",Da_TARGET,"cm")))),on=.(SC)]
Da_TARGET <- 3*Da0
DT <- DT[(buildTable_Kh(SN=SN_TARGET,SaTR = UserData$SaTR,BM19TR = UserData$BM19TR,Da=Da_TARGET,Ts=Ts_TARGET,SID_SET=SID_SET,TR_SET = TR_TARGET,pID=pID_TARGET)[,-c("Da","Ts")] |> setnames(old=c("NEHRP",paste0("TR=",TR_TARGET)),new=c("SC",paste0("Da=",Da_TARGET,"cm")))),on=.(SC)]
Da_TARGET <- 6*Da0
DT <- DT[(buildTable_Kh(SN=SN_TARGET,SaTR = UserData$SaTR,BM19TR = UserData$BM19TR,Da=Da_TARGET,Ts=Ts_TARGET,SID_SET=SID_SET,TR_SET = TR_TARGET,pID=pID_TARGET)[,-c("Da","Ts")] |> setnames(old=c("NEHRP",paste0("TR=",TR_TARGET)),new=c("SC",paste0("Da=",Da_TARGET,"cm")))),on=.(SC)]
Da_TARGET <- 10*Da0
DT <- DT[(buildTable_Kh(SN=SN_TARGET,SaTR = UserData$SaTR,BM19TR = UserData$BM19TR,Da=Da_TARGET,Ts=Ts_TARGET,SID_SET=SID_SET,TR_SET = TR_TARGET,pID=pID_TARGET)[,-c("Da","Ts")] |> setnames(old=c("NEHRP",paste0("TR=",TR_TARGET)),new=c("SC",paste0("Da=",Da_TARGET,"cm")))),on=.(SC)]

TBL_kh_Ts<- DT[order(SC)] |> flextable() |>
  theme_vanilla() |> 
  align(align="center",part="all") |>
  set_table_properties(layout = "autofit") |> 
  autofit(part="all") |> 
  add_header_row(values = c(" ",paste0("kh [% PGA] - Ts=",Ts_TARGET," s. - TR=",TR_TARGET)), colwidths = c(1,ncol(DT)-1))  |> 
  bold(part = "header")




# ***************** slopeR: TBL_kmax_Ts ----
Ts_TARGET<- 0.65
TR_TARGET <- 10000
Da_TARGET <- Da0
DT <- buildTable_Kmax(SN=SN_TARGET,BM19TR = UserData$BM19TR,Da=Da_TARGET,Ts=Ts_TARGET,SID_SET=SID_SET,TR_SET = TR_TARGET)[,-c("Da","Ts")] |> setnames(old=c("NEHRP",paste0("TR=",TR_TARGET)),new=c("SC",paste0("Da=",Da_TARGET,"cm")))

#*****
Da_TARGET <- 2*Da0
DT <- DT[(buildTable_Kmax(SN=SN_TARGET,BM19TR = UserData$BM19TR,Da=Da_TARGET,Ts=Ts_TARGET,SID_SET=SID_SET,TR_SET = TR_TARGET)[,-c("Da","Ts")] |> setnames(old=c("NEHRP",paste0("TR=",TR_TARGET)),new=c("SC",paste0("Da=",Da_TARGET,"cm")))),on=.(SC)]
Da_TARGET <- 3*Da0
DT <- DT[(buildTable_Kmax(SN=SN_TARGET,BM19TR = UserData$BM19TR,Da=Da_TARGET,Ts=Ts_TARGET,SID_SET=SID_SET,TR_SET = TR_TARGET)[,-c("Da","Ts")] |> setnames(old=c("NEHRP",paste0("TR=",TR_TARGET)),new=c("SC",paste0("Da=",Da_TARGET,"cm")))),on=.(SC)]
Da_TARGET <- 6*Da0
DT <- DT[(buildTable_Kmax(SN=SN_TARGET,BM19TR = UserData$BM19TR,Da=Da_TARGET,Ts=Ts_TARGET,SID_SET=SID_SET,TR_SET = TR_TARGET)[,-c("Da","Ts")] |> setnames(old=c("NEHRP",paste0("TR=",TR_TARGET)),new=c("SC",paste0("Da=",Da_TARGET,"cm")))),on=.(SC)]
Da_TARGET <- 10*Da0
DT <- DT[(buildTable_Kmax(SN=SN_TARGET,BM19TR = UserData$BM19TR,Da=Da_TARGET,Ts=Ts_TARGET,SID_SET=SID_SET,TR_SET = TR_TARGET)[,-c("Da","Ts")] |> setnames(old=c("NEHRP",paste0("TR=",TR_TARGET)),new=c("SC",paste0("Da=",Da_TARGET,"cm")))),on=.(SC)]

TBL_kmax_Ts <- DT[order(SC)] |> flextable() |>
  theme_vanilla() |> 
  align(align="center",part="all") |>
  set_table_properties(layout = "autofit") |> 
  autofit(part="all") |> 
  add_header_row(values = c(" ",paste0("kmax [g] - Ts=",Ts_TARGET," s. - TR=",TR_TARGET)), colwidths = c(1,ncol(DT)-1))  |> 
  bold(part = "header")




# ***************** slopeR: TBL_SC_ANCOLD_OBE TBL_SC_ANCOLD_MDE ----
Ts_TARGET<- 0.65
Da_TARGET <- Da0
SID_TARGET <- "BC"

DT1 <- buildTable_Kh_ANCOLD_OBE(SN=SN_TARGET,BM19TR = UserData$BM19TR,SaTR = UserData$SaTR,Da=Da_TARGET,Ts=Ts_TARGET,SID=SID_TARGET)
DT2 <- buildTable_Kmax_ANCOLD_OBE(SN=SN_TARGET,BM19TR = UserData$BM19TR,Da=Da_TARGET,Ts=Ts_TARGET,SID=SID_TARGET)
DT <- DT1[DT2,on="Category"][,`:=`("Da [cm]"=Da_TARGET,"Ts [s]"=Ts_TARGET,"SC"=SID_TARGET)]
TBL_SC_ANCOLD_OBE <- DT[order(SC)] |> flextable() |>
  theme_vanilla() |> 
  align(align="center",part="all") |>
  set_table_properties(layout = "autofit") |> 
  autofit(part="all") |> 
  bold(part = "header")


DT1 <- buildTable_Kh_ANCOLD_MDE(SN=SN_TARGET,BM19TR = UserData$BM19TR,SaTR = UserData$SaTR,Da=Da_TARGET,Ts=Ts_TARGET,SID=SID_TARGET)
DT2 <- buildTable_Kmax_ANCOLD_MDE(SN=SN_TARGET,BM19TR = UserData$BM19TR,Da=Da_TARGET,Ts=Ts_TARGET,SID=SID_TARGET)
DT <- DT1[DT2,on="Category"][,`:=`("Da [cm]"=Da_TARGET,"Ts [s]"=Ts_TARGET,"SC"=SID_TARGET)]
TBL_SC_ANCOLD_MDE <- DT[order(SC)] |> flextable() |>
  theme_vanilla() |> 
  align(align="center",part="all") |>
  set_table_properties(layout = "autofit") |> 
  autofit(part="all") |> 
  bold(part = "header")

# ***************** slopeR: TBL_SC_CDA_OBE TBL_SC_CDA_MDE ----
DT1 <- buildTable_Kh_CDA_OBE(SN=SN_TARGET,BM19TR = UserData$BM19TR,SaTR = UserData$SaTR,Da=Da_TARGET,Ts=Ts_TARGET,SID=SID_TARGET)
DT2 <- buildTable_Kmax_CDA_OBE(SN=SN_TARGET,BM19TR = UserData$BM19TR,Da=Da_TARGET,Ts=Ts_TARGET,SID=SID_TARGET)
DT <- DT1[DT2,on="Category"][,`:=`("Da [cm]"=Da_TARGET,"Ts [s]"=Ts_TARGET,"SC"=SID_TARGET)]
TBL_SC_CDA_OBE <- DT[order(SC)] |> flextable() |>
  theme_vanilla() |> 
  align(align="center",part="all") |>
  set_table_properties(layout = "autofit") |> 
  autofit(part="all") |> 
  bold(part = "header")


DT1 <- buildTable_Kh_CDA_MDE(SN=SN_TARGET,BM19TR = UserData$BM19TR,SaTR = UserData$SaTR,Da=Da_TARGET,Ts=Ts_TARGET,SID=SID_TARGET)
DT2 <- buildTable_Kmax_CDA_MDE(SN=SN_TARGET,BM19TR = UserData$BM19TR,Da=Da_TARGET,Ts=Ts_TARGET,SID=SID_TARGET)
DT <- DT1[DT2,on="Category"][,`:=`("Da [cm]"=Da_TARGET,"Ts [s]"=Ts_TARGET,"SC"=SID_TARGET)]
TBL_SC_CDA_MDE <- DT[order(SC)] |> flextable() |>
  theme_vanilla() |> 
  align(align="center",part="all") |>
  set_table_properties(layout = "autofit") |> 
  autofit(part="all") |> 
  bold(part = "header")

# ***************** slopeR: TBL_SC_GISTM_OBE TBL_SC_CDA_MDE ----
DT1 <- buildTable_Kh_GISTM_OBE(SN=SN_TARGET,BM19TR = UserData$BM19TR,SaTR = UserData$SaTR,Da=Da_TARGET,Ts=Ts_TARGET,SID=SID_TARGET)
DT2 <- buildTable_Kmax_GISTM_OBE(SN=SN_TARGET,BM19TR = UserData$BM19TR,Da=Da_TARGET,Ts=Ts_TARGET,SID=SID_TARGET)
DT <- DT1[DT2,on="Category"][,`:=`("Da [cm]"=Da_TARGET,"Ts [s]"=Ts_TARGET,"SC"=SID_TARGET)]
TBL_SC_GISTM_OBE <- DT[order(SC)] |> flextable() |>
  theme_vanilla() |> 
  align(align="center",part="all") |>
  set_table_properties(layout = "autofit") |> 
  autofit(part="all") |> 
  bold(part = "header")


DT1 <- buildTable_Kh_GISTM_MDE(SN=SN_TARGET,BM19TR = UserData$BM19TR,SaTR = UserData$SaTR,Da=Da_TARGET,Ts=Ts_TARGET,SID=SID_TARGET)
DT2 <- buildTable_Kmax_GISTM_MDE(SN=SN_TARGET,BM19TR = UserData$BM19TR,Da=Da_TARGET,Ts=Ts_TARGET,SID=SID_TARGET)
DT <- DT1[DT2,on="Category"][,`:=`("Da [cm]"=Da_TARGET,"Ts [s]"=Ts_TARGET,"SC"=SID_TARGET)]
TBL_SC_GISTM_MDE <- DT[order(SC)] |> flextable() |>
  theme_vanilla() |> 
  align(align="center",part="all") |>
  set_table_properties(layout = "autofit") |> 
  autofit(part="all") |> 
  bold(part = "header")





# ***************** slopeR: TBL_kmax_HVAR_84 -----

Bo <- 15 # [m]
Bmax <- 477 # [m]
L <- round(Bo/Bmax,digits=2) # lambda: Factor de forma

Mset <- seq(0.4,0.8,by=0.025)
Hset <- seq(5,35,by=5)
Vset <- seq(300,450,by=50)

TR_TARGET <- 10000
SID_TARGET <- "C"
DATA <- NULL


Da0 <- 25
AUX <- DG85[l ==L & m %in% Mset & n==1][,-c("l")]
for(Hmax in Hset){
  VSmax <- 250
  DT <- AUX[,.(SN=SN_TARGET,SID=SID_TARGET,TR=TR_TARGET,Td=(quantile(4*pi*Hmax/(an*(2 - m)*VSmax),probs=0.84)) )][,.(Hmax,kmax=getKmaxbyTR(BM19TR=UserData$BM19TR,Ts_TARGET=Td,Da_TARGET=Da0,SN_TARGET=SN,TR_TARGET=TR,SID_TARGET=SID))] |> setnames(old="kmax",new=paste0("VSo=",VSmax))
  for(VSmax in Vset){
    DT <- DT[AUX[,.(SN=SN_TARGET,SID=SID_TARGET,TR=TR_TARGET,Td=(quantile(4*pi*Hmax/(an*(2 - m)*VSmax),probs=0.84)))][,.(Hmax,kmax=getKmaxbyTR(BM19TR=UserData$BM19TR,Ts_TARGET=Td,Da_TARGET=Da0,SN_TARGET=SN,TR_TARGET=TR,SID_TARGET=SID))] |> setnames(old="kmax",new=paste0("VSo=",VSmax)),on=c("Hmax")]
  }
  DATA <- rbindlist(list(DATA,DT),use.names = TRUE) 
}

TBL_kmax_HVAR_84_Da25 <- DATA |> flextable() |> 
  theme_vanilla() |> 
  align(align="center",part="all") |>
  set_table_properties(layout = "autofit") |> 
  add_header_row(values = c(" ","kmax [g]"), colwidths = c(1,ncol(DATA)-1))  |>
  autofit(part="all") |> 
  bold(part = "header")

Da0 <- 5
DATA <- NULL
for(Hmax in Hset){
  VSmax <- 250
  DT <- AUX[,.(SN=SN_TARGET,SID=SID_TARGET,TR=TR_TARGET,Td=(quantile(4*pi*Hmax/(an*(2 - m)*VSmax),probs=0.84)) )][,.(Hmax,kmax=getKmaxbyTR(BM19TR=UserData$BM19TR,Ts_TARGET=Td,Da_TARGET=Da0,SN_TARGET=SN,TR_TARGET=TR,SID_TARGET=SID))] |> setnames(old="kmax",new=paste0("VSo=",VSmax))
  for(VSmax in Vset){
    DT <- DT[AUX[,.(SN=SN_TARGET,SID=SID_TARGET,TR=TR_TARGET,Td=(quantile(4*pi*Hmax/(an*(2 - m)*VSmax),probs=0.84)))][,.(Hmax,kmax=getKmaxbyTR(BM19TR=UserData$BM19TR,Ts_TARGET=Td,Da_TARGET=Da0,SN_TARGET=SN,TR_TARGET=TR,SID_TARGET=SID))] |> setnames(old="kmax",new=paste0("VSo=",VSmax)),on=c("Hmax")]
  }
  DATA <- rbindlist(list(DATA,DT),use.names = TRUE) 
}

TBL_kmax_HVAR_84_Da5 <- DATA |> flextable() |> 
  theme_vanilla() |> 
  align(align="center",part="all") |>
  set_table_properties(layout = "autofit") |> 
  add_header_row(values = c(" ","kmax [g]"), colwidths = c(1,ncol(DATA)-1))  |>
  autofit(part="all") |> 
  bold(part = "header")


Da0 <- 50
DATA <- NULL
for(Hmax in Hset){
  VSmax <- 250
  DT <- AUX[,.(SN=SN_TARGET,SID=SID_TARGET,TR=TR_TARGET,Td=(quantile(4*pi*Hmax/(an*(2 - m)*VSmax),probs=0.84)) )][,.(Hmax,kmax=getKmaxbyTR(BM19TR=UserData$BM19TR,Ts_TARGET=Td,Da_TARGET=Da0,SN_TARGET=SN,TR_TARGET=TR,SID_TARGET=SID))] |> setnames(old="kmax",new=paste0("VSo=",VSmax))
  for(VSmax in Vset){
    DT <- DT[AUX[,.(SN=SN_TARGET,SID=SID_TARGET,TR=TR_TARGET,Td=(quantile(4*pi*Hmax/(an*(2 - m)*VSmax),probs=0.84)))][,.(Hmax,kmax=getKmaxbyTR(BM19TR=UserData$BM19TR,Ts_TARGET=Td,Da_TARGET=Da0,SN_TARGET=SN,TR_TARGET=TR,SID_TARGET=SID))] |> setnames(old="kmax",new=paste0("VSo=",VSmax)),on=c("Hmax")]
  }
  DATA <- rbindlist(list(DATA,DT),use.names = TRUE) 
}

TBL_kmax_HVAR_84_Da50 <- DATA |> flextable() |> 
  theme_vanilla() |> 
  align(align="center",part="all") |>
  set_table_properties(layout = "autofit") |> 
  add_header_row(values = c(" ","kmax [g]"), colwidths = c(1,ncol(DATA)-1))  |>
  autofit(part="all") |> 
  bold(part = "header")


Da0 <- 10
DATA <- NULL
for(Hmax in Hset){
  VSmax <- 250
  DT <- AUX[,.(SN=SN_TARGET,SID=SID_TARGET,TR=TR_TARGET,Td=(quantile(4*pi*Hmax/(an*(2 - m)*VSmax),probs=0.84)) )][,.(Hmax,kmax=getKmaxbyTR(BM19TR=UserData$BM19TR,Ts_TARGET=Td,Da_TARGET=Da0,SN_TARGET=SN,TR_TARGET=TR,SID_TARGET=SID))] |> setnames(old="kmax",new=paste0("VSo=",VSmax))
  for(VSmax in Vset){
    DT <- DT[AUX[,.(SN=SN_TARGET,SID=SID_TARGET,TR=TR_TARGET,Td=(quantile(4*pi*Hmax/(an*(2 - m)*VSmax),probs=0.84)))][,.(Hmax,kmax=getKmaxbyTR(BM19TR=UserData$BM19TR,Ts_TARGET=Td,Da_TARGET=Da0,SN_TARGET=SN,TR_TARGET=TR,SID_TARGET=SID))] |> setnames(old="kmax",new=paste0("VSo=",VSmax)),on=c("Hmax")]
  }
  DATA <- rbindlist(list(DATA,DT),use.names = TRUE) 
}

TBL_kmax_HVAR_84_Da10 <- DATA |> flextable() |> 
  theme_vanilla() |> 
  align(align="center",part="all") |>
  set_table_properties(layout = "autofit") |> 
  add_header_row(values = c(" ","kmax [g]"), colwidths = c(1,ncol(DATA)-1))  |>
  autofit(part="all") |> 
  bold(part = "header")



# ***************** slopeR: TBL_kmax_HFIX_84 -----

Bo <- 15 # [m]
Bmax <- 477 # [m]
L <- round(Bo/Bmax,digits=2) # lambda: Factor de forma

SID_SET <- c("BC","C","CD","D","DE")
Mset <- seq(0.4,0.8,by=0.025)
Vset <- seq(300,450,by=50)
TR_TARGET <- 10000
DATA <- NULL
Hmax <- 30
Da0 <- 25
AUX <- DG85[l ==L & m %in% Mset & n==1][,-c("l")]|> unique()
for(sid in SID_SET){
  VSmax <- 250
  DT <- AUX[,.(TR=TR_TARGET,SN=SN_TARGET,Td=(mean(4*pi*Hmax/(an*(2 - m)*VSmax))) )][,.(SID=sid,kmax=getKmaxbyTR(BM19TR=UserData$BM19TR,Ts_TARGET=Td,Da_TARGET=Da0,SN_TARGET=SN,TR_TARGET=TR,SID_TARGET=sid))] |> setnames(old="kmax",new=paste0("VSo=",VSmax))
  for(VSmax in Vset){
    DT <- DT[AUX[,.(TR=TR_TARGET,SN=SN_TARGET,Td=(mean(4*pi*Hmax/(an*(2 - m)*VSmax))))][,.(SID=sid,kmax=getKmaxbyTR(BM19TR=UserData$BM19TR,Ts_TARGET=Td,Da_TARGET=Da0,SN_TARGET=SN,TR_TARGET=TR,SID_TARGET=sid))] |> setnames(old="kmax",new=paste0("VSo=",VSmax)),on=c("SID")]
  }
  DATA <- rbindlist(list(DATA,DT),use.names = TRUE) 
}

TBL_kmax_HFIX_84_Da25 <- DATA |> flextable() |> 
  theme_vanilla() |> 
  align(align="center",part="all") |>
  set_table_properties(layout = "autofit") |> 
  add_header_row(values = c(" ",paste0("kmax [g] - da=",Da_TARGET," cm")), colwidths = c(1,ncol(DATA)-1))  |> 
  autofit(part="all") |> 
  bold(part = "header")


DATA <- NULL
Da0 <- 5
for(sid in SID_SET){
  VSmax <- 250
  DT <- AUX[,.(TR=TR_TARGET,SN=SN_TARGET,Td=(mean(4*pi*Hmax/(an*(2 - m)*VSmax))) )][,.(SID=sid,kmax=getKmaxbyTR(BM19TR=UserData$BM19TR,Ts_TARGET=Td,Da_TARGET=Da0,SN_TARGET=SN,TR_TARGET=TR,SID_TARGET=sid))] |> setnames(old="kmax",new=paste0("VSo=",VSmax))
  for(VSmax in Vset){
    DT <- DT[AUX[,.(TR=TR_TARGET,SN=SN_TARGET,Td=(mean(4*pi*Hmax/(an*(2 - m)*VSmax))))][,.(SID=sid,kmax=getKmaxbyTR(BM19TR=UserData$BM19TR,Ts_TARGET=Td,Da_TARGET=Da0,SN_TARGET=SN,TR_TARGET=TR,SID_TARGET=sid))] |> setnames(old="kmax",new=paste0("VSo=",VSmax)),on=c("SID")]
  }
  DATA <- rbindlist(list(DATA,DT),use.names = TRUE) 
}



TBL_kmax_HFIX_84_Da5 <- DATA |> flextable() |> 
  theme_vanilla() |> 
  align(align="center",part="all") |>
  set_table_properties(layout = "autofit") |> 
  add_header_row(values = c(" ","kmax [g]"), colwidths = c(1,ncol(DATA)-1))  |> 
  autofit(part="all") |> 
  bold(part = "header")


Da0 <- 50
DATA <- NULL
for(sid in SID_SET){
  VSmax <- 250
  DT <- AUX[,.(TR=TR_TARGET,SN=SN_TARGET,Td=(mean(4*pi*Hmax/(an*(2 - m)*VSmax))) )][,.(SID=sid,kmax=getKmaxbyTR(BM19TR=UserData$BM19TR,Ts_TARGET=Td,Da_TARGET=Da0,SN_TARGET=SN,TR_TARGET=TR,SID_TARGET=sid))] |> setnames(old="kmax",new=paste0("VSo=",VSmax))
  for(VSmax in Vset){
    DT <- DT[AUX[,.(TR=TR_TARGET,SN=SN_TARGET,Td=(mean(4*pi*Hmax/(an*(2 - m)*VSmax))))][,.(SID=sid,kmax=getKmaxbyTR(BM19TR=UserData$BM19TR,Ts_TARGET=Td,Da_TARGET=Da0,SN_TARGET=SN,TR_TARGET=TR,SID_TARGET=sid))] |> setnames(old="kmax",new=paste0("VSo=",VSmax)),on=c("SID")]
  }
  DATA <- rbindlist(list(DATA,DT),use.names = TRUE) 
}

TBL_kmax_HFIX_84_Da50 <- DATA |> flextable() |> 
  theme_vanilla() |> 
  align(align="center",part="all") |>
  set_table_properties(layout = "autofit") |> 
  add_header_row(values = c(" ","kmax [g]"), colwidths = c(1,ncol(DATA)-1))  |> 
  autofit(part="all") |> 
  bold(part = "header")

Da0 <- 10
DATA <- NULL
for(sid in SID_SET){
  VSmax <- 250
  DT <- AUX[,.(TR=TR_TARGET,SN=SN_TARGET,Td=(mean(4*pi*Hmax/(an*(2 - m)*VSmax))) )][,.(SID=sid,kmax=getKmaxbyTR(BM19TR=UserData$BM19TR,Ts_TARGET=Td,Da_TARGET=Da0,SN_TARGET=SN,TR_TARGET=TR,SID_TARGET=sid))] |> setnames(old="kmax",new=paste0("VSo=",VSmax))
  for(VSmax in Vset){
    DT <- DT[AUX[,.(TR=TR_TARGET,SN=SN_TARGET,Td=(mean(4*pi*Hmax/(an*(2 - m)*VSmax))))][,.(SID=sid,kmax=getKmaxbyTR(BM19TR=UserData$BM19TR,Ts_TARGET=Td,Da_TARGET=Da0,SN_TARGET=SN,TR_TARGET=TR,SID_TARGET=sid))] |> setnames(old="kmax",new=paste0("VSo=",VSmax)),on=c("SID")]
  }
  DATA <- rbindlist(list(DATA,DT),use.names = TRUE) 
}

TBL_kmax_HFIX_84_Da10 <- DATA |> flextable() |> 
  theme_vanilla() |> 
  align(align="center",part="all") |>
  set_table_properties(layout = "autofit") |> 
  add_header_row(values = c(" ","kmax [g]"), colwidths = c(1,ncol(DATA)-1))  |> 
  autofit(part="all") |> 
  bold(part = "header")




# ***************** slopeR: TBL_Td_mean -----
# Hmax <- 66# [m]
Bo <- 15 # [m]
Bmax <- 477 # [m]
L <- round(Bo/Bmax,digits=2) # lambda: Factor de forma

Mset <- seq(0.4,0.8,by=0.025)
Hset <- seq(5,35,by=5)
Vset <- seq(300,450,by=50)
DATA <- NULL
AUX <- DG85[l ==L & m %in% Mset & n==1]
for(Hmax in Hset){
  
  VSmax <- 250
  DT <- AUX[,.(Hmax,l=L,Td=round(mean(4*pi*Hmax/(an*(2 - m)*VSmax)),digits=2) )] |> setnames(old="Td",new=paste0("VSo=",VSmax))
  for(VSmax in Vset){
    DT <- DT[AUX[,.(Hmax,l=L,Td=round(mean(4*pi*Hmax/(an*(2 - m)*VSmax)),digits=2))] |> setnames(old="Td",new=paste0("VSo=",VSmax)),on=c("Hmax","l")]
  }
  DATA <- rbindlist(list(DATA,DT),use.names = TRUE) 
}

TBL_Td_mean <- DATA |> flextable() |> 
  theme_vanilla() |> 
  align(align="center",part="all") |>
  set_table_properties(layout = "autofit") |> 
  autofit(part="all") |> 
  bold(part = "header")



# ***************** slopeR: TBL_Td_84 -----

#Hmax <- 66# [m]
Bo <- 15 # [m]
Bmax <- 477 # [m]
L <- round(Bo/Bmax,digits=2) # lambda: Factor de forma

SID_SET <- c("BC","C","CD","D","DE")
Mset <- seq(0.4,0.8,by=0.025)
Hset <- seq(5,35,by=5)
Vset <- seq(300,450,by=50)
DATA <- NULL
AUX <- DG85[l ==L & m %in% Mset & n==1]
for(Hmax in Hset){
  
  VSmax <- 250
  DT <- AUX[,.(Hmax,l=L,Td=round(quantile(4*pi*Hmax/(an*(2 - m)*VSmax),probs=0.84),digits=2) )] |> setnames(old="Td",new=paste0("VSo=",VSmax))
  for(VSmax in Vset){
    DT <- DT[AUX[,.(Hmax,l=L,Td=round(quantile(4*pi*Hmax/(an*(2 - m)*VSmax),probs=0.84),digits=2))] |> setnames(old="Td",new=paste0("VSo=",VSmax)),on=c("Hmax","l")]
  }
  DATA <- rbindlist(list(DATA,DT),use.names = TRUE) 
}

TBL_Td_84 <- DATA |> flextable() |> 
  theme_vanilla() |> 
  align(align="center",part="all") |>
  set_table_properties(layout = "autofit") |> 
  add_header_row(values = c(" ","Ts [s]"), colwidths = c(1,ncol(DATA)-1))  |> 
  autofit(part="all") |> 
  bold(part = "header")



# ***************** slopeR: TBL_Td_HFIX-----
Lset <- c(0.40)
Mset <- seq(0.4,0.8,by=0.025)
Hset <- 30
Vset <- seq(300,450,by=50)
DATA <- NULL
for(Hmax in Hset){
  for(L in Lset){
    AUX <- DG85[l ==L & m %in% Mset & n==1]
    VSmax <- 250
    DT <- AUX[,.(m,l=L,Td=round((4*pi*Hmax/(an*(2 - m)*VSmax)),digits=2) )] |> setnames(old="Td",new=paste0("VSo=",VSmax))
    for(VSmax in Vset){
      DT <- DT[AUX[,.(m,l=L,Td=round((4*pi*Hmax/(an*(2 - m)*VSmax)),digits=2))] |> setnames(old="Td",new=paste0("VSo=",VSmax)),on=c("m","l")]
    }
    DATA <- rbindlist(list(DATA,DT),use.names = TRUE) 
  }
  
}

TBL_Td_HFIX <- DT |> flextable() |> 
  theme_vanilla() |> 
  align(align="center",part="all") |>
  set_table_properties(layout = "autofit") |> 
  autofit(part="all") |> 
  add_header_row(values = c(" ","Ts [s]"), colwidths = c(1,ncol(DT)-1))  |> 
  bold(part = "header")


# ***************** slopeR: TBL_roots_a1,TBL_roots_a2 ----
Lset <- seq(0.02,0.45,by=0.02)
Mset <- seq(0.4,0.8,by=0.05)
DATA <- DG85[l %in% Lset &m %in% Mset & n==1][,an:=round(an,digits = 3)]
DATA <- reshape(DATA[,-c("n")],idvar=c("l"),timevar = "m",direction = "wide")
OCOL <- grep(colnames(DATA),pattern = "an.",value = TRUE)
NCOL <- paste0("m=",str_remove(OCOL,pattern = "an."))
setnames(DATA,old=OCOL,new=NCOL)

TBL_roots_a1 <- DATA |> flextable() |> 
  theme_vanilla() |> 
  align(align="center",part="all") |>
  set_table_properties(layout = "autofit") |> 
  autofit(part="all") |> 
  bold(part = "header")

DATA <- DG85[l %in% Lset &m %in% Mset & n==2][,an:=round(an,digits = 3)]
DATA <- reshape(DATA[,-c("n")],idvar=c("l"),timevar = "m",direction = "wide")
OCOL <- grep(colnames(DATA),pattern = "an.",value = TRUE)
NCOL <- paste0("m=",str_remove(OCOL,pattern = "an."))
setnames(DATA,old=OCOL,new=NCOL)

TBL_roots_a2 <- DATA |> flextable() |> 
  theme_vanilla() |> 
  align(align="center",part="all") |>
  set_table_properties(layout = "autofit") |> 
  autofit(part="all") |> 
  bold(part = "header")


# ***  SAVE DATA    *** ----
save(TBL_Mm,TBL_Rm,TBL_AEP_GISTM,TBL_Sa_mean,TBL_Sa_84,TBL_NEHRP,TBL_AF_84,TBL_AF_mean,
     TBL_AFPGA_84,TBL_AFPGA_mean,
     TBL_AFPGV_84,TBL_AFPGV_mean,
     TBL_AFAI_mean,TBL_AFAI_84,
     TBL_AFSa_TR10000_84,TBL_AFSa_TR0000_mean,
     TBL_AFSa_Tn1_84,TBL_AFSa_Tn1_mean,TBL_kh_Ts,
     TBL_kmax_Ts,
     TBL_kmax_HVAR_84_Da25,TBL_kmax_HVAR_84_Da5,TBL_kmax_HVAR_84_Da50,TBL_kmax_HVAR_84_Da10,
     TBL_kmax_HFIX_84_Da25,TBL_kmax_HFIX_84_Da5,TBL_kmax_HFIX_84_Da50,TBL_kmax_HFIX_84_Da10,
     TBL_Td_mean,TBL_Td_84,TBL_Td_HFIX,
     file="data/tables.RData")

