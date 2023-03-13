# GMDP ----
# binFolder <- file.path("D:","Public","Database","gmdp","byteC")
# compiler::loadcmp(file.path(binFolder,"setup.Rc"))

source("D:/git/GMDPV2/setup.R")

HC_THEMES <- list(
  hc_theme_smpl(),
  hc_theme_db() ,
  hc_theme_538(),
  hc_theme_alone(),
  hc_theme_bloom(),
  hc_theme_chalk(),
  hc_theme_darkunica(),
  hc_theme_economist(),
  hc_theme_elementary(),
  hc_theme_ffx(),
  hc_theme_flat(),
  hc_theme_flatdark(),
  hc_theme_ft(),
  hc_theme_ggplot2(),
  hc_theme_google(),
  hc_theme_gridlight(),
  hc_theme_handdrawn(),
  hc_theme_hcrt(),
  hc_theme_monokai(),
  hc_theme_null(),
  hc_theme_sandsignika(),
  hc_theme_superheroes(),
  hc_theme_tufte()
)

# .buildPlot ----
buildHCPlot <- function(
    HEIGHT=500,
    LEGEND=TRUE,
    TIP = "ID:{point.series.name}<br> d={point.y} mm",
    CURVE=NULL,  BAR=NULL,  POINT=NULL,
    XT="t [days]",YT="Y [mm]",
    PALETTE="Blue-Red",
    XLOG=TRUE,YLOG=FALSE,
    XREV=FALSE,YREV=FALSE,
    LAYOUT="horizontal",ALIGN="right",VALIGN="top",
    THEME=hc_theme_hcrt()){
  on.exit(expr={rm(list = ls())}, add = TRUE)
  stopifnot(!is.null(POINT)|!is.null(CURVE)|!is.null(BAR))
  
  HC <- highcharter::highchart() 
  
  if(!is.null(CURVE)){
    HC <- HC |> hc_add_series(
      CURVE[,.(ID,X,Y)],# main curve
      type="spline",
      dashStyle = "Solid",
      hcaes(x=X,y=Y, group=ID))
  }
  if(!is.null(BAR)){
    HC <- HC |> hc_add_series(
      BAR[,.(ID,X,Y)],# main curve
      type="column",
      hcaes(x=X,y=Y, group=ID))
  }
  if(!is.null(POINT)){
    HC <- HC |>  hc_add_series(
      POINT[,.(ID,X,Y)],# main curve
      type="scatter",
      hcaes(x=X,y=Y, group=ID))
  }
  NC <- max(10,length(unique(CURVE$ID)),length(unique(POINT$ID)))
  HC <- HC |> 
    hc_yAxis(
      title= list(text=YT),
      minorTickInterval = "auto",
      minorGridLineDashStyle = "Dot",
      showFirstLabel = FALSE,
      showLastLabel = TRUE) |>
    
    hc_xAxis(
      title= list(text=XT),
      minorTickInterval = "auto",
      minorGridLineDashStyle = "Dot",
      showFirstLabel = TRUE,
      showLastLabel = TRUE) |>
    
    hc_add_theme(hc_thm = THEME) |>
    
    hc_colors(
      # colors <- c("blue","brown","red")
      # colors = hcl.colors(12,palette = PALETTE)
      colors = hcl.colors(NC,palette = PALETTE)
    ) |> 
    
    hc_tooltip(
      sort = FALSE, 
      split=FALSE, 
      crosshairs = TRUE, 
      pointFormat = TIP) |> 
    
    hc_size( height = HEIGHT)
  
  if(LEGEND==TRUE){
    HC <- HC |> 
      
      hc_legend(
        align = "left",
        verticalAlign = "top",
        layout = LAYOUT,
        x = 100,
        y = 100
      ) |> 
      
      hc_chart(
        style=list(fontFamily = "Helvetica")) 
  }
  
  
  if(XLOG==TRUE) {
    HC <- HC |> hc_xAxis(reversed=XREV,type = "logarithmic")
  } else {
    HC <- HC |> hc_xAxis(reversed=XREV)
  }
  
  if(YLOG==TRUE) {
    HC <- HC |> hc_yAxis(reversed=YREV,type = "logarithmic")
  } else {
    HC <- HC |> hc_yAxis(reversed=YREV)
  }
  return(HC)
}


.getIMmodel<- function(.SD=NULL){
  # el dataframe debe tener LnI y LnT definidos
  MDL <- lm( LnI~LnT, data = .SD,weights = WGT)
  SMDL <- summary(MDL)
  a_MDL <- unname(MDL$coefficients)[1] 
  b_MDL <- unname(MDL$coefficients)[2] 
  sd_MDL <- SMDL$sigma
  DT <-data.table(a=a_MDL,b=b_MDL,sdLnA=sd_MDL)
  return(DT)
}
