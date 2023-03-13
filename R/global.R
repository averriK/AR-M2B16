SN_TARGET <- (yaml::yaml.load_file("_variables.yml"))$SN_TARGET #"CEAGEMAK2"
CREDITS <- (yaml::yaml.load_file("_variables.yml"))$CREDITS #"SRK Consulting (Kazakhstan)"

PATH <-  "D:/Public/Database/gmdp/index"
FILE <- file.path(PATH,"PSHA.Rds")
# 
# PATH <- file.path("D:","Public","Database","gmdp","hazard")
# REGION <- (yaml::yaml.load_file("_variables.yml"))$REGION #"SSA"
# MODEL <- (yaml::yaml.load_file("_variables.yml"))$MODEL #"gem"
# FILENAME <- paste0("PSHA.",REGION,MODEL,".Rds")
# FILE <- file.path(PATH,FILENAME)


stopifnot(file.exists(FILE))
UserData <- readRDS(file = FILE) |> na.omit()
# fitPGV <- fread(file="data/PGVmodel.csv",yaml=TRUE)
# fitPGA <- fread(file="data/PGAmodel.csv",yaml = TRUE)
# PGA <- fread(file=file.path("data/PGA.csv"),yaml = TRUE)
# PGV <- fread(file=file.path("data/PGV.csv"),yaml = TRUE)

# xxxx ----
PATH <- file.path("D:","Public","Database","gmdp","index")
FILE <- file.path(PATH,"SiteMaster.xlsx")
stopifnot(file.exists(FILE))
SiteMaster <- readxl::read_xlsx(FILE,sheet="SiteMaster",progress=FALSE) |> as.data.table()
SiteMaster <- SiteMaster[,.(ID,RID,MID,PID,LID,Project,Location,LON,LAT)]
# UserData <- readRDS(file = file.path(PATH,"PSHA.Rds"))
QoTable <- .buildQoTable() #.buildQoTable(INI$qo)

  
# load PGV model  ----
PATH <- "D:/Public/Database/gmdp/model"
PGV_model <-readRDS(file=file.path(PATH,"pgv.Rds"))
AI_model <-readRDS(file=file.path(PATH,"ia.Rds"))
DG85 <- readRDS(file=file.path(PATH,"dg85.Rds")) #.buildDG85()


# 
input <- list(logScale=c("Tn"),plotAlign="left",plotLayout=NULL,plotType="spline",palette="Hawaii",HCT= "hc_theme_smpl",layout="horizontal",bins=40,theta=135,phi=20)

#

