# Climate Change Vulnerability In The Agricultural Sector
# WFD to WTH files based in Jeison's code
# H. Achicanoy
# CIAT, 2015

# Processing information from WFD to WTH files

# Load directories
wfd_dir <- "/mnt/data_cluster_4/observed/gridded_products/wfd/nc-files/wfd_0_5_deg_lat"

# Load packages
library(ncdf)
library(raster)

## Years to extract (Change if is Baseline or Future)
años <- 1971:2000

#------------------------------------------------------------------------------------------------------------#
# 1. Function to produce dates sequence
#------------------------------------------------------------------------------------------------------------#

añosToRun <- function(inicial,final){
  y<-seq(as.Date(paste(inicial,"-01-01",sep="")),as.Date(paste(final,"-12-31",sep="")),by=1)
  return(y)
}

años_mod <- lapply(1:length(años),function(i) añosToRun(años[i],años[i]))

#------------------------------------------------------------------------------------------------------------#
# 2. Read files in .nc format
#------------------------------------------------------------------------------------------------------------#

Meses=c(paste("0",sep="",1:9,".nc"),paste(sep="",10:12,".nc"))
Años_Prec <- c(paste0("lat_Rainf_daily_WFD_GPCC_19",71:99),paste0("lat_Rainf_daily_WFD_GPCC_2000"))
Años_Radt <- c(paste0("lat_SWdown_daily_WFD_19",71:99),paste0("lat_SWdown_daily_WFD_2000"))
Años_TemMax <- c(paste0("lat_Tmax_daily_WFD_19",71:99),paste0("lat_Tmax_daily_WFD_2000"))
Años_TemMin <- c(paste0("lat_Tmin_daily_WFD_19",71:99),paste0("lat_Tmin_daily_WFD_2000"))

SerieAnual_Prec <- lapply(1:30,function(i) paste(Años_Prec[i],Meses,sep=""))
SerieAnual_Radt <- lapply(1:30,function(i) paste(Años_Radt[i],Meses,sep=""))
SerieAnual_Tmax <- lapply(1:30,function(i) paste(Años_TemMax[i],Meses,sep=""))
SerieAnual_Tmin <- lapply(1:30,function(i) paste(Años_TemMin[i],Meses,sep=""))

#------------------------------------------------------------------------------------------------------------#
# 3. Function to extract bands
#------------------------------------------------------------------------------------------------------------#

lecturabandas=function(data){
  data=paste(data)
  lectura=raster(paste(data),band=T)
  dias=sapply(1:lectura@ file@nbands, function(i) raster(paste(data),band=i) )
  return(stack(dias))
}

#------------------------------------------------------------------------------------------------------------#
# 4. Parallelize raster reading
#------------------------------------------------------------------------------------------------------------#

#-----------------------------------------------------#
# Precipitación
#-----------------------------------------------------#
setwd(paste0(wfd_dir,"/Rainf_daily_WFD_GPCC"))

library(snowfall)
sfInit( parallel=TRUE, cpus=20)
sfLibrary(snowfall)
sfLibrary(raster)
sfLibrary (sp)
sfLibrary (rgdal)
sfLibrary (maps)
sfLibrary (mapproj)
sfLibrary(stringr)    ## libreria necesaria para las funciones de tipo caracter
sfLibrary(date)       ## configuracion de fecha tipo dia juliano
sfLibrary(ncdf)
sfExportAll()

Raster_Prec <- sfLapply(1:30,function(j) sfLapply(1:12,function(i) lecturabandas(SerieAnual_Prec[[j]][i])))
sfStop()

#-----------------------------------------------------#
# Radiación solar
#-----------------------------------------------------#
setwd(paste0(wfd_dir,"/SWdown_daily_WFD"))

library(snowfall)
sfInit( parallel=TRUE, cpus=7)
sfLibrary(snowfall)
sfLibrary(raster)
sfLibrary (sp)
sfLibrary (rgdal)
sfLibrary (maps)
sfLibrary (mapproj)
sfLibrary(stringr)    ## libreria necesaria para las funciones de tipo caracter
sfLibrary(date)       ## configuracion de fecha tipo dia juliano
sfLibrary(ncdf)
sfExportAll()

Raster_Radt <- sfLapply(1:30,function(j) sfLapply(1:12,function(i) lecturabandas(SerieAnual_Radt[[j]][i])))
sfStop()

#-----------------------------------------------------#
# Temperatura máxima
#-----------------------------------------------------#
setwd(paste0(wfd_dir,"/Tmax_daily_WFD"))

library(snowfall)
sfInit( parallel=TRUE, cpus=7)
sfLibrary(snowfall)
sfLibrary(raster)
sfLibrary (sp)
sfLibrary (rgdal)
sfLibrary (maps)
sfLibrary (mapproj)
sfLibrary(stringr)    ## libreria necesaria para las funciones de tipo caracter
sfLibrary(date)       ## configuracion de fecha tipo dia juliano
sfLibrary(ncdf)
sfExportAll()

Raster_TempMax <- sfLapply(1:30,function(j) sfLapply(1:12,function(i) lecturabandas(SerieAnual_Tmax[[j]][i])))
sfStop()

#-----------------------------------------------------#
# Temperatura mínima
#-----------------------------------------------------#
setwd(paste0(wfd_dir,"/Tmin_daily_WFD"))

library(snowfall)
sfInit( parallel=TRUE, cpus=7)
sfLibrary(snowfall)
sfLibrary(raster)
sfLibrary (sp)
sfLibrary (rgdal)
sfLibrary (maps)
sfLibrary (mapproj)
sfLibrary(stringr)    ## libreria necesaria para las funciones de tipo caracter
sfLibrary(date)       ## configuracion de fecha tipo dia juliano
sfLibrary(ncdf)
sfExportAll()

Raster_TempMin <- sfLapply(1:30,function(j) sfLapply(1:12,function(i) lecturabandas(SerieAnual_Tmin[[j]][i])))
sfStop()

# Save important files
setwd("/mnt/workspace_cluster_3/bid-cc-agricultural-sector/14-ObjectsR/wfd")
save(Raster_Prec,file="Prec.Rdat")
save(Raster_Radt,file="Radt.Rdat")
save(Raster_TempMax,Raster_TempMin,file="Temp.Rdat")

#------------------------------------------------------------------------------------------------------------#
# 5. Extract values for each pixel by year
#------------------------------------------------------------------------------------------------------------#

Extraervalores_grilla=function(data,año,grilla){
  tamdias=sfSapply(1:12,function(i) dim(data[[año]][[i]])[3])
  value<-sfSapply(1:length(tamdias),function(j) sfSapply(1:tamdias[j],function(i) as.vector(extract(data[[año]][[j]][[i]],grilla) )))
  return(unlist(value))
}

#------------------------------------------------------------------------------------------------------------#
# 6. Parallelize reading for rasters of climate serie
#------------------------------------------------------------------------------------------------------------#

# Read coordinates
load("/mnt/workspace_cluster_3/bid-cc-agricultural-sector/14-ObjectsR/coordenadas.RDat")

library(snowfall)
sfInit( parallel=TRUE, cpus=7)
sfLibrary(snowfall)
sfLibrary(raster)
sfExportAll()

setwd("/mnt/workspace_cluster_3/bid-cc-agricultural-sector/14-ObjectsR/wfd")
load("Prec.Rdat")
load("Radt.Rdat")
load("Temp.Rdat")

Raster_Prec <- lapply(Raster_Prec,FUN=stack)
Raster_Radt <- lapply(Raster_Radt,FUN=stack)
Raster_TempMax <- lapply(Raster_TempMax,FUN=stack)
Raster_TempMin <- lapply(Raster_TempMin,FUN=stack)

Raster_Prec <- lapply(Raster_Prec, function(x){r <- x; proj4string(r) <- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"); r <- rotate(r); return(r)})
Raster_Radt <- lapply(Raster_Radt, function(x){r <- x; proj4string(r) <- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"); r <- rotate(r); return(r)})
Raster_TempMax <- lapply(Raster_TempMax, function(x){r <- x; proj4string(r) <- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"); r <- rotate(r); return(r)})
Raster_TempMin <- lapply(Raster_TempMin, function(x){r <- x; proj4string(r) <- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"); r <- rotate(r); return(r)})

Prec <- sfLapply(Raster_Prec, FUN=extract,Coordenadas)
Radt <- sfLapply(Raster_Radt, FUN=extract,Coordenadas)
TempMax <- sfLapply(Raster_TempMax, FUN=extract,Coordenadas)
TempMin <- sfLapply(Raster_TempMin, FUN=extract,Coordenadas)

save(Prec,Radt,TempMax,TempMin,file="wfd_data.RDat") # Save data.frames with information for each day and pixel

sfStop()

# Create a data frame saving longitude, latitude and ID for each pixel
id_pixel <- as.data.frame(Coordenadas)
id_pixel$ID <- c(paste0("000",1:9),paste0("00",10:99),paste0("0",100:999),1000:8199)
id_pixel <- id_pixel[,c("ID","x","y")]
save(id_pixel, file="/mnt/workspace_cluster_3/bid-cc-agricultural-sector/14-ObjectsR/id_coordinates.Rdat")

#------------------------------------------------------------------------------------------------------------#
# 7. Prepare data to run function
#------------------------------------------------------------------------------------------------------------#

# Define years to run
años <- c(71:99,20)

# Load climate information
setwd("/mnt/workspace_cluster_3/bid-cc-agricultural-sector/14-ObjectsR/wfd")
load("wfd_data.RDat")
clima <- list()
clima$Radiacion <- Radt
clima$Tmax <- TempMax
clima$Tmin <- TempMin
clima$Prec <- Prec

# Function to generate WTH files
WriteWTH <- function(años,clima){
  
  if(20 %in% años){
    x <- as.data.frame(expand.grid(años*1000,1:365))
    x$ID <- x[,1] + x[,2]; yrs <- x$ID; yrs <- sort(yrs); rm(x)
    yrs <- c(yrs[-c(1:365)],yrs[1:365])
  } else{
    x <- as.data.frame(expand.grid(años*1000,1:365))
    x$ID <- x[,1] + x[,2]; yrs <- x$ID; yrs <- sort(yrs); rm(x)
  }
  
  load("/mnt/workspace_cluster_3/bid-cc-agricultural-sector/14-ObjectsR/wfd/coordinates.Rdat")
  
  library(parallel)
  
  pixelProcess <- function(i)
  {
    pixel <- i
    
    Prec <- lapply(1:length(clima$Prec), function(i){z <- na.omit(as.vector(clima$Prec[[i]][pixel,])); z <- z[1:365]*86400; z <- round(z,1); return(z)})
    Prec <- Reduce(function(...) c(..., recursive=FALSE), Prec)
    Srad <- lapply(1:length(clima$Radiacion), function(i){z <- na.omit(as.vector(clima$Radiacion[[i]][pixel,])); z <- z/11.5740741; z <- z[1:365]; z <- round(z,1); return(z)})
    Srad <- Reduce(function(...) c(..., recursive=FALSE), Srad)
    Tmax <- lapply(1:length(clima$Tmax), function(i){z <- na.omit(as.vector(clima$Tmax[[i]][pixel,])); z <- z-273.15; z <- z[1:365]; z <- round(z,1); return(z)})
    Tmax <- Reduce(function(...) c(..., recursive=FALSE), Tmax)
    Tmin <- lapply(1:length(clima$Tmin), function(i){z <- na.omit(as.vector(clima$Tmin[[i]][pixel,])); z <- z-273.15; z <- z[1:365]; z <- round(z,1); return(z)})
    Tmin <- Reduce(function(...) c(..., recursive=FALSE), Tmin)
    
    Tav <- mean(c(Tmin,Tmax),na.rm=TRUE)
    Amp <- mean(Tmax-Tmin,na.rm=TRUE)
    
    sink(file=paste0("/mnt/workspace_cluster_3/bid-cc-agricultural-sector/01-climate-data/wth_by_pixel_processed/WFD/",id_pixel$ID[pixel],"7101.WTH"),append=T,type="output")
    cat(paste("*WEATHER DATA :"),paste("BID"))
    cat("\n")
    cat("\n")
    cat(c("@ INSI      LAT     LONG  ELEV   TAV   AMP REFHT WNDHT"))
    cat("\n")
    cat(sprintf("%6s %8.3f %8.3f %5.0f %5.1f %5.2f %5.2f %5.2f",id_pixel$ID[pixel], id_pixel$y[pixel], id_pixel$x[pixel], -99,Tav, Amp, 0, 0))
    cat("\n")
    cat(c('@DATE  SRAD  TMAX  TMIN  RAIN'))
    cat("\n")
    cat(cbind(sprintf("%5s %5.1f %5.1f %5.1f %5.1f",yrs,Srad,Tmax,Tmin,Prec)),sep="\n")
    sink()
    
    return(cat("Done.\n"))
  }
  
  mclapply(1:8199,pixelProcess,mc.cores=15)
  
}

WriteWTH(años=años, clima=clima)
