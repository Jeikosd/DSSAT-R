################################################# WFD  en CIAT #########################################################
library(snowfall)
sfInit(parallel=TRUE, cpus=6)
sfLibrary(snowfall)
sfLibrary(raster)
sfLibrary(ncdf)


setwd("/mnt/workspace_cluster_3/bid-cc-agricultural-sector/14-ObjectsR/wfd")
load("Prec.Rdat")
load("Radt.Rdat")
load("Temp.Rdat")
load("/mnt/workspace_cluster_3/bid-cc-agricultural-sector/14-ObjectsR/coordenadas.RDat")

Coordenadas[,1]<-Coordenadas[,1]+360

sfExportAll()
Raster_Prec <- lapply(Raster_Prec,FUN=stack)
Raster_Radt <- lapply(Raster_Radt,FUN=stack)
Raster_TempMax <- lapply(Raster_TempMax,FUN=stack)
Raster_TempMin <- lapply(Raster_TempMin,FUN=stack)


Prec <- sfLapply(Raster_Prec, FUN=extract,Coordenadas)
Srad <- sfLapply(Raster_Radt, FUN=extract,Coordenadas) 
TempMax <- sfLapply(Raster_TempMax, FUN=extract,Coordenadas)
TempMin <- sfLapply(Raster_TempMin, FUN=extract,Coordenadas)


save(TempMax,TempMin,Prec,Srad,file="ValorWFD.RDat")


########################### Extraer Precipitacion y Radiacion #########################################

library(snowfall)
sfInit(parallel=TRUE, cpus=6)
sfLibrary(snowfall)
sfLibrary(raster)
sfLibrary (sp)
sfLibrary(ncdf)


modelos <- c("bcc_csm1_1","csiro_mk3_6_0","inm_cm4","miroc_esm_chem",
             "mpi_esm_mr","bnu_esm","ipsl_cm5a_lr","miroc_miroc5",
             "mri_cgcm3","cccma_canesm2","gfld_esm2g","ipsl_cm5a_mr",
             "mohc_hadgem2_cc","ncc_noresm1_m","gfld_esm2g",
             "ipsl_cm5b_lr","mohc_hadgem2_es","cnrm_cm5","miroc_esm",
             "mpi_esm_lr")

sfExport("modelos")
# sfSource("/home/jeisonmesa/Proyectos/BID/bid-cc-agricultural-sector/_scripts/ExtraerClima.R",encoding="latin1")
sfSource("/mnt/workspace_cluster_3/bid-cc-agricultural-sector/_scripts/ExtraerClima-CIAT.R",encoding="latin1")
sapply(1:length(modelos), function(i) Extraer_Srad_or_Prec("Presente",modelos[i],"Precipitacion",8000))
sapply(1:length(modelos), function(i) Extraer_Srad_or_Prec("Futuro",modelos[i],"Precipitacion",8000))







