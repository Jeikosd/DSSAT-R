##############################################################################
########################## Condiciones iniciales #############################
##############################################################################

initial_conditions <- function(data, system) {
  
  if(system == "rainfed"){
    
    ## Extraer Condiciones iniciales de los archivos de suelos
    SOIL <- readLines(paste(data))  ## Archivo de Suelo
    profiles <- grep("SLB", SOIL)  ## Posicion en la que coincide con las variables a extraer
    imp.head <- scan(paste(data), what = "character", skip = profiles[1] - 1, nlines = 1, quiet = T)  # Encabezado (titulo para las columnas)
    seps <- c(6, -6, 6)    ## Separadores para las variables
    Initial_conditions <- read.fwf("SOIL.SOL", width = seps, header = F, skip = profiles[1], n = profiles[2] - profiles[1] - 1)
    
    pat <- "SLB|SDUL"
    headers <- imp.head[grep(pat, imp.head, perl = TRUE)]
    
    colnames(Initial_conditions) <- headers
    
    return(Initial_conditions)
    
  }
  
  if(system == "irrigation"){
    
    ## Extraer Condiciones iniciales de los archivos de suelos
    SOIL <- readLines(paste(data))  ## Archivo de Suelo
    profiles <- grep("SLB", SOIL)  ## Posicion en la que coincide con las variables a extraer
    imp.head <- scan(paste(data), what = "character", skip = profiles[1] - 1, nlines = 1, quiet = T)  # Encabezado (titulo para las columnas)
    seps <- c(6, -6, 6)    ## Separadores para las variables
    Initial_conditions <- read.fwf("SOIL.SOL", width = seps, header = F, skip = profiles[1], n = profiles[2] - profiles[1] - 1)
    
    pat <- "SLB|SDUL"
    headers <- imp.head[grep(pat, imp.head, perl = TRUE)]
    
    Initial_conditions <- data.frame(Initial_conditions[, 1], rep(-99, dim(Initial_conditions)[1]))
    
    colnames(Initial_conditions) <- headers
    
    return(Initial_conditions)
  }

  
}





##############################################################################
########################## Convert day for DSSAT #############################
##############################################################################

convert_date <- function(date, year) {
  ## esta es la funcion que se agrega pero sino sirve 60 dias antes dejar como estaba antes
  year_simul <- year
  year <- year + 1
  if(date <= 0){
    date1 <- 365 + date
    date1 <- paste0(year_simul, date1)
  }
  ############################################# lo que funcionaba
  if(date < 10 & date > 0) {
    date1 <- paste0(year, "00", date)
  }
  if(date < 100 & date >= 10 & date > 0) {
    date1 <- paste0(year, "0", date)
  }
  if(date >= 100 & date > 0) {
    date1 <- paste0(year, date)
  }
  return(date1)
}



##############################################################################
########################## Settings leap year ################################
##############################################################################

leap_year <- function(year) {
  ## Settings leap year
  ## yrs to create the label .WTH
  ## yrs2 Year Julian days
  if((year %% 4) == 0) {
    if((year %% 100) == 0) {
      if((year %% 400) == 0) {
        # print(paste(year,"is a leap year"))
        yrs <- year
        yrs2 <- (yrs * 100):((yrs * (100)) + 366)
        yrs <- (yrs * 1000):((yrs * (1000)) + 366)
        
        yrs <- yrs[-1]
        yrs2 <- yrs2[-1]
        
      } else {
        # print(paste(year,"is not a leap year"))
        yrs <- year
        yrs2 <- (yrs * 100):((yrs * (100)) + 365)
        yrs <- (yrs * 1000):((yrs * (1000)) + 365)
        
        yrs <- yrs[-1]               ## day 00 is not possible
        yrs2 <- yrs2[-1]             ## day 00 is not possible              
        
      }
    } else {
      # print(paste(year,"is a leap year"))
      yrs <- year
      yrs2 <- (yrs * 100):((yrs * (100)) + 366)
      yrs <- (yrs * 1000):((yrs * (1000)) + 366)
      
      yrs <- yrs[-1]                ## day 00 is not possible
      yrs2 <- yrs2[-1]              ## day 00 is not possible 
      
    }
  } else {
    # print(paste(year,"is not a leap year"))
    yrs <- year
    yrs2 <- (yrs * 100):((yrs * (100)) + 365)
    yrs <- (yrs * 1000):((yrs * (1000)) + 365)
    
    yrs <- yrs[-1]                  ## day 00 is not possible
    yrs2 <- yrs2[-1]                ## day 00 is not possible  
  }
  
  years <- list(yrs, yrs2)
  names(years) <- c("yrs", "yrs2")
  return(years)
  
}


##############################################################################
########################## Leer Summary.OUT DSSAT ############################
##############################################################################

# path <- "/home/jeisonmesa/Proyectos/BID/DSSAT/bin/csm45_1_23_bin_ifort"
# setwd(path)
# 
# pat <- "SDAT|PDAT|ADAT|MDAT|IRCM|HWAH|HIAM|EPCM|NICM|NDCH|PRCP|ETCP|CWAM"
# imp.head <- scan("Summary.OUT", what = "character", skip = 3, nlines = 1, quiet = T)
# headers <- imp.head[grep(pat, imp.head, perl = TRUE)]
# 
# # Read in main table in fixed width format
# #seps <- c(-92, 8, 8, -8, 8, 8, -30, 8, -36, 6, -30, 6, -315, 7, 7)
# seps <- c(-92, 8, 8, -8, 8, 8, -14, 8, -8, 8, -36, 6, -12, 6, -12, 6, -30, 6, -242, 6, -31, 7, 7)
# 
# imp.dat <- read.fwf("Summary.OUT", width = seps, skip = 4, header = F, sep = "")
# colnames(imp.dat) <- headers

################## Leer Overview.out #######################################


read.overview <- function(...){
  
  overview <- readLines("OVERVIEW.OUT")
  stress <- grep("CROP GROWTH", overview)
  imp.head <- scan("OVERVIEW.OUT", what = "character", skip = stress[1], nlines = 1, quiet = T) 
  path <- getwd()
  seps <- c(-1, 6, 5, 11, 8, 7, 6, 5, 5, 6, 6, 6, 6, 6)
  
  length.stress <- function(datos, path){
    
    k<-0
    test <- try(strsplit(overview[datos + 3],split = path),silent = TRUE)
    while(!is.na(length(test)==1 && test[[1]]!="")){
      test <- try(strsplit(overview[datos+k],split = path),silent = TRUE)
      k<- k+1
      
    }  
    
    return(k-2)
    
  }
  
  year.overview <- lapply(1:length(stress), function(i) read.fwf("OVERVIEW.OUT", width = seps, skip = stress[i] + 2, header = F, n = length.stress(stress[i], getwd()) - 2))
  
  headers <- function(datos){
    
    colnames(datos) <- imp.head
    return(datos)
    
  }
  
  year.overview <- lapply(year.overview, headers)
  
  return(year.overview)
  
}


