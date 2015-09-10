#################### DSSAT RUN #####################################

########### Load functions necessary
# path_functions <- "/home/jeisonmesa/Proyectos/BID/DSSAT-R/"
# path_project <- "/home/jeisonmesa/Proyectos/BID/bid-cc-agricultural-sector/"
# 
# # Cargar data frame entradas para DSSAT
# 
# load(paste0(path_project, "/08-Cells_toRun/matrices_cultivo/Rice_riego.Rdat"))
# load(paste0(path_project, "/08-Cells_toRun/matrices_cultivo/Rice_secano.Rdat"))
# load(paste0(path_project, "14-ObjectsR/Soil.RData"))
# 
# source(paste0(path_functions, "main_functions.R"))     ## Cargar funciones principales
# source(paste0(path_functions, "make_xfile.R"))         ## Cargar funcion para escribir Xfile DSSAT
# source(paste0(path_functions, "make_wth.R")) 
# source(paste0(path_functions, "dssat_batch.R"))



# Separacion Aplicacion de Nitrogeno
# Cambiar crop_riego o crop_secano (para cada cultivo cambia la aplicacion de  nitrogeno tanto la cantidad como el dia de la aplicacion)

# day0 <- crop_riego$N.app.0d
# day_aplication0 <- rep(0, length(day0))
# 
# day30 <- crop_riego$N.app.30d
# day_aplication30 <- rep(30, length(day30))
# 
# amount <- data.frame(day0, day30)
# day_app <- data.frame(day_aplication0, day_aplication30)


# # Configurando el experimento para WFD (datos Historicos 1971-1999)
# 
# years <- 71:99
# data_xfile <- list()
# data_xfile$crop <- "RICE" 
# data_xfile$exp_details <- "*EXP.DETAILS: BID17101RZ RICE LAC"
# data_xfile$name <- "./JBID.RIX" 
# data_xfile$CR <- "RI"
# data_xfile$INGENO <- "IB0118"
# data_xfile$CNAME <- "IRNA"
# data_xfile$initation <- crop_riego$mirca.start
# data_xfile$final <- crop_riego$mirca.end
# data_xfile$system <- "irrigation"  ## Irrigation or rainfed, if is irrigation then automatic irrigation
# data_xfile$year <- years[1]
# data_xfile$nitrogen_aplication <- list(amount = amount, day_app = day_app)
# data_xfile$smodel <- "RIXCER"     ##  Fin Model
# data_xfile$bname <- "DSSBatch.v45"

## to test
## Xfile(data_xfile, 1) 


## Cargar datos climaticos WFD

# load(paste0(path_project, "14-ObjectsR/wfd/", "ValorWFD.RDat"))

# Climate Data Set
# climate_data <- list()
# climate_data$year <- 71:99       ## Years where they will simulate yields
# climate_data$Srad <- Srad        ## [[year]][pixel, ]   
# climate_data$Tmax <- TempMax     ## [[year]][pixel, ]
# climate_data$Tmin <- TempMin     ## [[year]][pixel, ]
# climate_data$Prec <- Prec        ## [[year]][pixel, ]
# climate_data$lat <- crop_riego[,"y"]        ## You can include a vector of latitude
# climate_data$long <- crop_riego[, "x"]         ## You can include a vector of longitude
# climate_data$wfd <- "wfd"       ## Switch between "wfd" and "model"
# climate_data$id <- crop_riego[, "Coincidencias"]

## Entradas para las corridas de DSSAT 

# input_data<- list()
# input_data$xfile <- data_xfile
# Xfile(input_data$xfile, 158)
# input_data$climate <- climate_data

## to test
# pixel <- 7
# i <- 1

# sapply(1:length(input_data$climate$year), function(i) {
# WriteWTH(input_data$climate$year[i], input_data$climate$Srad[[i]][pixel, ], input_data$climate$Tmax[[i]][pixel, ], input_data$climate$Tmin[[i]][pixel, ], 
#          input_data$climate$Prec[[i]][pixel, ], input_data$climate$lat[pixel], input_data$climate$long[pixel], input_data$climate$wfd)
# 
# })

# dir_dssat <- "/home/jeisonmesa/Proyectos/BID/DSSAT/bin/csm45_1_23_bin_ifort/"
# dir_base <- "/home/jeisonmesa/Proyectos/BID/bid-cc-agricultural-sector/Scratch"

## to test
# input <- input_data
# run_dssat(input_data, 50, dir_dssat, dir_base)

run_dssat <- function(input, pixel, dir_dssat, dir_base) {
  #sfCat(pixel, "/n")
  print(pixel)
  run_id <- round(runif(1,10,2000000))
  
  dir_run <- paste0(dir_base, "/", run_id)
  
  
  #gc()
  
  if(!file.exists(dir_run)) { 
    
    dir.create(dir_run, showWarnings = TRUE, recursive = TRUE, mode = "7777")
    system(paste("cp", paste0(dir_dssat, "/*.*"), dir_run))
    
    setwd(paste(dir_run))
  }
  
  setwd(paste(dir_run))
    
  ## Listar las salidas de DSSAT .out .soil and .wth
  ## para ser eliminados antes de la corrida a fin de poder escribir los nuevos archivos
  outList <- list.files(pattern = ".OUT")
  wthList <- list.files(pattern = ".WTH")
  soilList <- list.files(pattern = ".SOL")
  file.remove(outList)
  file.remove(wthList)
  file.remove(soilList)
  
  ## Make DSSatbatch
  
  CSMbatch(input$xfile$crop, input$xfile$name, input$xfile$bname) 
  
  ## Make Soil
  Extraer.SoilDSSAT(values[input$climate$id[pixel]],getwd())
  
  
  
  ## Make Xfile 
  if(input$xfile$system == "rainfed") {
    in_conditions <- initial_conditions("SOIL.SOL", input$xfile$system)
    Xfile(input$xfile, pixel, in_conditions, initial = T)
    
  } else {
    in_conditions <- initial_conditions("SOIL.SOL", input$xfile$system)
    Xfile(input$xfile, pixel, in_conditions, initial = T)
  }
  
  
#   if(input$xfile$system == "irrigated") {
#     Xfile(input$xfile, pixel, in_conditions = F)
#     
#   }
  

  ## Make WTH
  
  if(input$climate$wfd == "wfd"){
    
    sapply(1:length(input_data$climate$year), function(i) {
    WriteWTH(input_data$climate$year[i], input_data$climate$Srad[[i]][input$climate$id[pixel], ], input_data$climate$Tmax[[i]][input$climate$id[pixel], ], input_data$climate$Tmin[[i]][input$climate$id[pixel], ], 
             input_data$climate$Prec[[i]][input$climate$id[pixel], ], input_data$climate$lat[pixel], input_data$climate$long[pixel], input_data$climate$wfd)
    
    })
    
    gc()
  }
  
  
  if(input$climate$wfd  == "model"){
    
    sapply(1:length(input_data$climate$year), function(i) {
      WriteWTH(input_data$climate$year[i], input_data$climate$Srad[[input$climate$id[pixel]]][[i]], input_data$climate$Tmax[[input$climate$id[pixel]]][[i]], input_data$climate$Tmin[[input$climate$id[pixel]]][[i]], 
               input_data$climate$Prec[[input$climate$id[pixel]]][[i]], input_data$climate$lat[pixel], input_data$climate$long[pixel], input_data$climate$wfd)
      
    })
    
    
  }
  
  
  system(paste0("./DSCSM045.EXE " , input$xfile$smodel," B DSSBatch.v45"), ignore.stdout = T)
  
pat <- "SDAT|PDAT|ADAT|MDAT|IRCM|HWAH|HIAM|EPCM|NICM|NDCH|PRCP|ETCP|CWAM|YPTM|YPEM|YPNAM|YPNUM"
imp.head <- scan("Summary.OUT", what = "character", skip = 3, nlines = 1, quiet = T)
headers <- imp.head[grep(pat, imp.head, perl = TRUE)]

seps <- c(-92, 8, 8, -8, 8, 8, -14, 8, -8, 8, -36, 6, -12, 6, -12, 6, -30, 6, -179, 9, 9, -27, 9, 9, 6, -31, 7, 7)
text_summary <- readLines('Summary.OUT', skipNul = T)
imp.dat <- read.fwf(textConnection(text_summary), width = seps, skip = 4)
colnames(imp.dat) <- headers

  return(imp.dat) 
  ## gc() Tener cuidado de quitar
  
#sfCat("Change Dir")
  setwd(paste(dir_base))

  if(getwd() == dir_base){
    
    setwd(paste(dir_base))
    
  }
  
  
}
  
  
  
