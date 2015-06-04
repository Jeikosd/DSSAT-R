##############################################################################
############################### Make Xfile ###################################
##############################################################################

# source("/home/jeisonmesa/Proyectos/BID/DSSAT-R/main_functions.R")
# 
# # to test
# # carpeta donde se encuentra la informacion necesaria para correr
# path <- "/home/jeisonmesa/Proyectos/BID/bid-cc-agricultural-sector/08-Cells_toRun/matrices_cultivo/"
# 
# # Cargar data frame entradas para DSSAT
# 
# load(paste0(path, "Rice_riego.Rdat"))
# load(paste0(path, "Rice_secano.Rdat"))
# 
# # Separacion Aplicacion de Nitrogeno
# # Cambiar crop_riego o crop_secano
# 
# day0 <- crop_riego$N.app.0d
# day_aplication0 <- rep(0, length(day0))
# 
# day30 <- crop_riego$N.app.30d
# day_aplication30 <- rep(30, length(day30))
# 
# amount <- data.frame(day0, day30)
# day_app <- data.frame(day_aplication0, day_aplication30)
# 
#   
# # Multiple Experiments
# years <- 68:94 ## Coincide con los años bisiestos a futuro
# ## año <- c(68:94)
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

# 

## Separacion Aplicacion de Nitrogeno
## Cambiar crop_riego o crop_secano

# day0 <- crop_secano$N.app.0d
# day_aplication0 <- rep(0, length(day0))
# 
# day30 <- crop_secano$N.app.30d
# day_aplication30 <- rep(30, length(day30))
# 
# mount <- data.frame(day0, day30)
# day_app <- data.frame(day_aplication0, day_aplication30)



## Multiple Experiments
# years <- 71:99
# pixel <- 1
# data_xfile <- list()
# data_xfile$exp_details <- "*EXP.DETAILS: BID17101RZ RICE LAC"
# data_xfile$name <- "./JBID_dryland.RIX" 
# data_xfile$CR <- "RI"
# data_xfile$INGENO <- "IB0118"
# data_xfile$CNAME <- "IRNA"
# data_xfile$initation <- crop_secano$mirca.start[pixel]
# data_xfile$final <- crop_secano$mirca.end[pixel]
# data_xfile$system <- "rainfed"  ## Irrigation or rainfed, if is irrigation then automatic irrigation
# data_xfile$year <- years[1]
# data_xfile$nitrogen_aplication <- list(mount = mount[pixel, ], day_app = day_app[pixel, ])
# data_xfile$smodel <- "RIXCER"     ##  Fin Model
# Xfile(data_xfile, 1) 


# information <- data_xfile  ## Only used for testing


Xfile <- function(information, pixel) {
  
  
  information$initation <- information$initation[pixel]
  information$final <- crop_riego$mirca.end[pixel]
  information$nitrogen_aplication <- list(amount = amount[pixel, ], day_app = day_app[pixel, ])

  
  
  
  midpoint_window <- round(mean(c(information$initation, information$final))) ## Midpoint planting window
  
  if( information$system == "irrigation" ){
    
    pdate <- convert_date(pmax(midpoint_window, 0), information$year)        ## Planting date
    sdate <- convert_date(pmax(midpoint_window - 15, 0), information$year)   ## Simulation Start
    A <- "PL"      ## label name
    first <- -99  ## Start planting window
    last <- -99   ## End planting window
    IRR <- "A"    ## Automatic irrigation
    plant <- "R"    ## On reported date
    
  }
  
  
  if( information$system == "rainfed" ) {
    
    
    first <- pmax(midpoint_window - 20, 0)  ## First Window
    last <- pmax(midpoint_window + 20, 0)   ## Last Window
    sdate <- convert_date(pmax(first -15, 0), information$year)    ## Simulation Start
    pdate <- -99      ## Planting date (you will take the planting window data mirca 2000)
    A <- "PL"         ## Label Name
    IRR <- "N"        ## No irrigation (rainfed)
    plant<-"A"        ## Automatic planting seed given window
    
    
    
  }
  
  
  ## Defining the Experiment
  in_data <- list()
  
  
  ## General data of the Experiment
  
  in_data$general <- list(PEOPLE = "Diego Obando Jeison Mesa", ADDRESS = "CIAT", SITE = "CALI")
  
  
  ## Definition simulate treatment
  in_data$treatments <- data.frame(N = 1, R = 1, O = 1, C = 0, TNAME = "BID001", CU = 1, FL = 1, SA = 0, IC = 0, MP = 1,
                                   MI = 0, MF = 1, MR = 0, MC = 0, MT = 0, ME = 0, MH = 0, SM = 1)
  
  ## Definition simulate cultivar
  
  in_data$cultivars <- data.frame(C = 1, CR = information$CR, INGENO = information$INGENO, CNAME = information$CNAME)
  
  ## Field
  
  in_data$fields <- data.frame(L = 1, ID_FIELD = "BID1", WSTA = "JBID", FLSA=-99, FLOB = -99, FLDT = "DR000",
                               FLDD = -99, FLDS = -99, FLST = -99, SLTX = -99, SLDP = -99, ID_SOIL="BID0000001",
                               FLNAME = "FIELD01", XCRD = -99, YCRD = -99, ELEV = -99, AREA = -99, SLEN=-99,
                               FLWR = -99, SLAS = -99, FLHST = -99, FHDUR=-99)
  
  ## initial conditions of the experiment
  ## Aqui investigar acerca de ICDAT
  ## Segun el manual Initial Conditions Measurement date, year + days
  in_data$ini_cond_properties <- data.frame(C=1,PCR="RI",ICDAT="50001",ICRT=-99,ICND=-99,ICRN=-99,ICRE=-99,
                                            ICWD=-99,ICRES=-99,ICREN=-99,ICREP=-99,ICRIP=-99,ICRID=-99,
                                            ICNAME="inicond1")
  
  #in_data$ini_cond_profile <- data.frame(C=rep(1,5),ICBL=rep(-99,5),SH2O=rep(-99,5),SNH4=rep(-99,5),
  #                                       SNO3=rep(-99,5))
  
  
  ## Planting Details
  in_data$planting <- data.frame( P = 1, PDATE = pdate, EDATE = -99, PPOP = 200, PPOE = 175, PLME = "S", 
                                  PLDS = "B", PLRS = -99, PLRD = 0, PLDP = 2.5,
                                  PLWT = -99, PAGE = -99, PENV = -99, PLPH = -99, SPRL = -99)
  
  ## Simulation Control 
  in_data$sim_ctrl <- data.frame(N = 1, GENERAL = "GE", NYERS = 28, NREPS = 1, START = "S", SDATE = sdate, 
                                 RSEED=2150, SNAME = "simctr1", SMODEL = paste(information$smodel), 
                                 OPTIONS = "OP", WATER = "Y", NITRO = "Y", SYMBI = "N",
                                 PHOSP = "N", POTAS = "N", DISES = "N", CHEM = "N", TILL = "N", 
                                 CO2 = "M", METHODS = "ME", WTHER = "M", INCON = "M", LIGHT = "E", 
                                 EVAPO = "R", INFIL = "S", PHOTO = "C", HYDRO = "R",
                                 NSWIT = 1, MESOM = "G", MESEV = "S", MESOL =2, MANAGEMENT = "MA", 
                                 PLANT = plant, IRRIG = IRR,
                                 FERTI = "D", RESID = "R", HARVS = "M", OUTPUTS = "OU", FNAME = "N", 
                                 OVVEW = "Y", SUMRY = "Y", FROPT = 1, GROUT = "Y", CAOUT = "Y", 
                                 WAOUT = "Y", NIOUT = "N", MIOUT = "N",
                                 DIOUT = "N", VBOSE = "N", CHOUT = "N", OPOUT = "N")
  
  ## AUTOMATIC MANAGEMENT 
  
  in_data$auto_mgmt <- data.frame(N = 1, PLANTING = A, PFRST = first, PLAST = last, PH2OL = 50, PH2OU= 100,
                                  PH2OD = 30, PSTMX = 40, PSTMN = 10, IRRIGATION = "IR", IMDEP=30, ITHRL = 50, 
                                  ITHRU =100, IROFF = "GS000", IMETH = "IR001", IRAMT = 10, IREFF = 1,
                                  NITROGEN = "NI", NMDEP = 30, NMTHR = 50, NAMNT = 25, NCODE = "FE001",
                                  NAOFF = "GS000", RESIDUES = "RE", RIPCN = 100, RTIME = 1, RIDEP = 20, 
                                  HARVEST = "HA", HFRST = 0, HLAST = 00001, HPCNP = 100, HPCNR = 0)
  
  
  
  
  # Make Xfile
  
  ## test
  ## out_file <- "./JBID.RIX"
  # overwrite <- F
  
  make_xfile <- function(in_data, out_file, overwrite = F) {
    #open file in write mode
    if (file.exists(out_file)) {
      if (overwrite) {
        pf <- file(out_file, open = "w")
      } else {
        rnum <- round(runif(1, 10000, 20000), 0)
        tmpvar <- unlist(strsplit(out_file, "/", fixed = T))
        pth_ref <- paste(tmpvar[1:(length(tmpvar) - 1)], collapse = "/")
        out_file <- paste(pth_ref, "/copy-", rnum, "_", tmpvar[length(tmpvar)], sep = "")
        pf <- file(out_file, open = "w")
      }
    } else {
      pf <- file(out_file,open="w")
    }
    
    #write header and stuff
    #pf <- file(out_file,open="w")
    cat(paste0(information$exp_details, "\n"), file = pf)
    cat("\n",file = pf)
    
    #general stuff
    cat("*GENERAL\n@PEOPLE\n", file = pf)
    cat(paste(sprintf("%-12s", as.character(in_data$general$PEOPLE)), "\n", sep = ""), file = pf)
    cat("@ADDRESS\n", file = pf)
    cat(paste(sprintf("%-12s", as.character(in_data$general$ADDRESS)), "\n", sep = ""), file = pf)
    cat("@SITE\n", file = pf)
    cat(paste(sprintf("%-12s", as.character(in_data$general$SITE)), "\n", sep = ""), file = pf)
    
    #treatments
    cat("*TREATMENTS                        -------------FACTOR LEVELS------------\n", file = pf)
    cat("@N R O C TNAME.................... CU FL SA IC MP MI MF MR MC MT ME MH SM\n", file = pf)
    for (i in 1:nrow(in_data$treatments)) {
      cat(paste(sprintf("%1$2d%2$2d%3$2d%4$2d",as.integer(in_data$treatments$N[i]),as.integer(in_data$treatments$R[i]),
                        as.integer(in_data$treatments$O[i]),as.integer(in_data$treatments$C[i])),
                " ",sprintf("%1$-25s%2$3d%3$3d%4$3d%5$3d%6$3d%7$3d%8$3d%9$3d%10$3d%11$3d%12$3d%13$3d%14$3d",in_data$treatments$TNAME[i],
                            as.integer(in_data$treatments$CU[i]),as.integer(in_data$treatments$FL[i]),as.integer(in_data$treatments$SA[i]),
                            as.integer(in_data$treatments$IC[i]),as.integer(in_data$treatments$MP[i]),as.integer(in_data$treatments$MI[i]),
                            as.integer(in_data$treatments$MF[i]),as.integer(in_data$treatments$MR[i]),as.integer(in_data$treatments$MC[i]),
                            as.integer(in_data$treatments$MT[i]),as.integer(in_data$treatments$ME[i]),as.integer(in_data$treatments$MH[i]),
                            as.integer(in_data$treatments$SM[i])),
                "\n", sep = ""), file = pf)
    }
    cat("\n", file = pf)
    
    #cultivars
    cat("*CULTIVARS\n", file = pf)
    cat("@C CR INGENO CNAME\n", file = pf)
    for (i in 1:nrow(in_data$cultivars)) {
      cat(paste(sprintf("%2d",as.integer(in_data$cultivars$C[i]))," ",sprintf("%2s", in_data$cultivars$CR[i]),
                " ", sprintf("%6s",in_data$cultivars$INGENO[i])," ",sprintf("%-12s",in_data$cultivars$CNAME[i]),
                "\n", sep = ""), file = pf)
    }
    cat("\n", file = pf)
    
    #fields
    cat("*FIELDS\n", file = pf)
    cat("@L ID_FIELD WSTA....  FLSA  FLOB  FLDT  FLDD  FLDS  FLST SLTX  SLDP  ID_SOIL    FLNAME\n", file = pf)
    cat(paste(sprintf("%2d",as.integer(in_data$fields$L))," ",sprintf("%-8s",in_data$fields$ID_FIELD),
              " ",sprintf("%-8s",in_data$fields$WSTA),sprintf("%6d",as.integer(in_data$fields$FLSA)),
              sprintf("%6d",as.integer(in_data$fields$FLOB)),sprintf("%6s",in_data$fields$FLDT),
              sprintf("%6d",as.integer(in_data$fields$FLDD)),sprintf("%6s",as.integer(in_data$fields$FLDS)),
              sprintf("%6d",as.integer(in_data$fields$FLST))," ",sprintf("%-4d",as.integer(in_data$fields$SLTX)),
              sprintf("%6d",as.integer(in_data$fields$SLDP)),"  ",sprintf("%-10s",in_data$fields$ID_SOIL)," ",
              sprintf("%-12s",in_data$fields$FLNAME),"\n",sep=""),file=pf)
    cat("@L ...........XCRD ...........YCRD .....ELEV .............AREA .SLEN .FLWR .SLAS FLHST FHDUR\n",file=pf)
    cat(paste(sprintf("%2d",as.integer(in_data$fields$L))," ",sprintf("%15.3f",in_data$fields$XCRD)," ",
              sprintf("%15.3f",in_data$fields$YCRD)," ",sprintf("%9d",as.integer(in_data$fields$ELEV))," ",
              sprintf("%17d",as.integer(in_data$fields$AREA))," ",sprintf("%5d",as.integer(in_data$fields$SLEN))," ",
              sprintf("%5d",as.integer(in_data$fields$FLWR))," ",sprintf("%5d",as.integer(in_data$fields$SLAS))," ",
              sprintf("%5d",as.integer(in_data$fields$FLHST))," ",sprintf("%5d",as.integer(in_data$fields$FHDUR)),
              "\n",sep=""),file=pf)
    cat("\n",file=pf)
    
    #initial conditions
    #cat("*INITIAL CONDITIONS\n",file=pf)
    #cat("@C   PCR ICDAT  ICRT  ICND  ICRN  ICRE  ICWD ICRES ICREN ICREP ICRIP ICRID ICNAME\n",file=pf)
    #cat(paste(sprintf("%2d",as.integer(in_data$ini_cond_properties$C))," ",sprintf("%5s",in_data$ini_cond_properties$PCR),
    #          " ",sprintf("%5s",in_data$ini_cond_properties$ICDAT)," ",sprintf("%5d",as.integer(in_data$ini_cond_properties$ICRT)),
    #          " ",sprintf("%5d",as.integer(in_data$ini_cond_properties$ICND))," ",sprintf("%5d",as.integer(in_data$ini_cond_properties$ICRN)),
    #          " ",sprintf("%5d",as.integer(in_data$ini_cond_properties$ICRE))," ",sprintf("%5d",as.integer(in_data$ini_cond_properties$ICWD)),
    #          " ",sprintf("%5d",as.integer(in_data$ini_cond_properties$ICRES))," ",sprintf("%5d",as.integer(in_data$ini_cond_properties$ICREN)),
    #          " ",sprintf("%5d",as.integer(in_data$ini_cond_properties$ICREP))," ",sprintf("%5d",as.integer(in_data$ini_cond_properties$ICRIP)),
    #          " ",sprintf("%5d",as.integer(in_data$ini_cond_properties$ICRID))," ",sprintf("%-12s",in_data$ini_cond_properties$ICNAME),
    #          "\n",sep=""),file=pf)
    #cat("@C  ICBL  SH2O  SNH4  SNO3\n",file=pf)
    #for (i in 1:nrow(in_data$ini_cond_profile)) {
    #  cat(paste(sprintf("%2d",as.integer(in_data$ini_cond_properties$C))," ",sprintf("%5.0f",as.integer(in_data$ini_cond_profile$ICBL[i])),
    #            " ",sprintf("%5.0f",as.integer(in_data$ini_cond_profile$SH2O[i]))," ",sprintf("%5.0f",as.integer(in_data$ini_cond_profile$SNH4[i])),
    #            " ",sprintf("%5.0f",as.integer(in_data$ini_cond_profile$SNO3[i])),"\n",sep=""),file=pf)
    #}
    #cat("\n",file=pf)
    
    #planting details
    cat("*PLANTING DETAILS\n",file = pf)
    cat("@P PDATE EDATE  PPOP  PPOE  PLME  PLDS  PLRS  PLRD  PLDP  PLWT  PAGE  PENV  PLPH  SPRL                        PLNAME\n",file=pf)
    cat(paste(sprintf("%2d",as.integer(in_data$planting$P))," ",sprintf("%5s",in_data$planting$PDATE),
              " ",sprintf("%5s",in_data$planting$EDATE)," ",sprintf("%5d",as.integer(in_data$planting$PPOP)),
              " ",sprintf("%5d",as.integer(in_data$planting$PPOE))," ",sprintf("%5s",in_data$planting$PLME),
              " ",sprintf("%5s",in_data$planting$PLDS)," ",sprintf("%5d",as.integer(in_data$planting$PLRS)),
              " ",sprintf("%5d",as.integer(in_data$planting$PLRD))," ",sprintf("%5d",as.integer(in_data$planting$PLDP)),
              " ",sprintf("%5d",as.integer(in_data$planting$PLWT))," ",sprintf("%5d",as.integer(in_data$planting$PAGE)),
              " ",sprintf("%5d",as.integer(in_data$planting$PENV))," ",sprintf("%5d",as.integer(in_data$planting$PLPH)),
              " ",sprintf("%5d",as.integer(in_data$planting$SPRL))," ",sprintf("%29s",in_data$planting$PLNAME),
              "\n", sep = ""), file = pf)
    cat("\n", file = pf)
    
    ## Details Fertilization
    cat("*FERTILIZERS (INORGANIC)\n", file = pf)
    cat("@F FDATE  FMCD  FACD  FDEP  FAMN  FAMP  FAMK  FAMC  FAMO  FOCD FERNAME                       \n", file = pf)
    for(i in 1:dim(information$nitrogen_aplication$amount)[2]){
      if(!is.na(information$nitrogen_aplication$amount[, i])) {
        cat(sprintf("%2s %5s %4s %5s %5.2f %5i %5i %5i %5i %5i %5i %1i",1,1,"FE005","AP002", information$nitrogen_aplication$amount[, i], 
                    information$nitrogen_aplication$day_app[, i], 0, -99, -99, -99, -99, -99), "\n", file = pf)
      }
    }
    cat("\n", file = pf)
    
    #simulation controls
    cat("*SIMULATION CONTROLS\n", file = pf)
    cat("@N GENERAL     NYERS NREPS START SDATE RSEED SNAME.................... SMODEL\n", file = pf)
    cat(paste(sprintf("%2d",as.integer(in_data$sim_ctrl$N))," ",sprintf("%-11s",in_data$sim_ctrl$GENERAL),
              " ",sprintf("%5d",as.integer(in_data$sim_ctrl$NYERS))," ",sprintf("%5d",as.integer(in_data$sim_ctrl$NREPS)),
              " ",sprintf("%5s",in_data$sim_ctrl$START)," ",sprintf("%5s",in_data$sim_ctrl$SDATE),
              " ",sprintf("%5d",as.integer(in_data$sim_ctrl$RSEED))," ",sprintf("%-25s",in_data$sim_ctrl$SNAME),
              " ",sprintf("%-6s",in_data$sim_ctrl$SMODEL),"\n",sep=""),file=pf)
    cat("@N OPTIONS     WATER NITRO SYMBI PHOSP POTAS DISES  CHEM  TILL   CO2\n",file=pf)
    cat(paste(sprintf("%2d",as.integer(in_data$sim_ctrl$N))," ",sprintf("%-11s",in_data$sim_ctrl$OPTIONS),
              " ",sprintf("%5s",in_data$sim_ctrl$WATER)," ",sprintf("%5s",in_data$sim_ctrl$NITRO),
              " ",sprintf("%5s",in_data$sim_ctrl$SYMBI)," ",sprintf("%5s",in_data$sim_ctrl$PHOSP),
              " ",sprintf("%5s",in_data$sim_ctrl$POTAS)," ",sprintf("%5s",in_data$sim_ctrl$DISES),
              " ",sprintf("%5s",in_data$sim_ctrl$CHEM)," ",sprintf("%5s",in_data$sim_ctrl$TILL),
              " ",sprintf("%5s",in_data$sim_ctrl$CO2),"\n",sep=""),file=pf)
    cat("@N METHODS     WTHER INCON LIGHT EVAPO INFIL PHOTO HYDRO NSWIT MESOM MESEV MESOL\n",file=pf)
    cat(paste(sprintf("%2d",as.integer(in_data$sim_ctrl$N))," ",sprintf("%-11s",in_data$sim_ctrl$METHODS),
              " ",sprintf("%5s",in_data$sim_ctrl$WTHER)," ",sprintf("%5s",in_data$sim_ctrl$INCON),
              " ",sprintf("%5s",in_data$sim_ctrl$LIGHT)," ",sprintf("%5s",in_data$sim_ctrl$EVAPO),
              " ",sprintf("%5s",in_data$sim_ctrl$INFIL)," ",sprintf("%5s",in_data$sim_ctrl$PHOTO),
              " ",sprintf("%5s",in_data$sim_ctrl$HYDRO)," ",sprintf("%5d",as.integer(in_data$sim_ctrl$NSWIT)),
              " ",sprintf("%5s",in_data$sim_ctrl$MESOM)," ",sprintf("%5s",in_data$sim_ctrl$MESEV),
              " ",sprintf("%5d",as.integer(in_data$sim_ctrl$MESOL)),"\n",sep=""),file=pf)
    cat("@N MANAGEMENT  PLANT IRRIG FERTI RESID HARVS\n",file=pf)
    cat(paste(sprintf("%2d",as.integer(in_data$sim_ctrl$N))," ",sprintf("%-11s",in_data$sim_ctrl$MANAGEMENT),
              " ",sprintf("%5s",in_data$sim_ctrl$PLANT)," ",sprintf("%5s",in_data$sim_ctrl$IRRIG),
              " ",sprintf("%5s",in_data$sim_ctrl$FERTI)," ",sprintf("%5s",in_data$sim_ctrl$RESID),
              " ",sprintf("%5s",in_data$sim_ctrl$HARVS),"\n",sep=""),file=pf)
    cat("@N OUTPUTS     FNAME OVVEW SUMRY FROPT GROUT CAOUT WAOUT NIOUT MIOUT DIOUT VBOSE CHOUT OPOUT\n",file=pf)
    cat(paste(sprintf("%2d",as.integer(in_data$sim_ctrl$N))," ",sprintf("%-11s",in_data$sim_ctrl$OUTPUTS),
              " ",sprintf("%5s",in_data$sim_ctrl$FNAME)," ",sprintf("%5s",in_data$sim_ctrl$OVVEW),
              " ",sprintf("%5s",in_data$sim_ctrl$SUMRY)," ",sprintf("%5s",in_data$sim_ctrl$FROPT),
              " ",sprintf("%5s",in_data$sim_ctrl$GROUT)," ",sprintf("%5s",in_data$sim_ctrl$CAOUT),
              " ",sprintf("%5s",in_data$sim_ctrl$WAOUT)," ",sprintf("%5s",in_data$sim_ctrl$NIOUT),
              " ",sprintf("%5s",in_data$sim_ctrl$MIOUT)," ",sprintf("%5s",in_data$sim_ctrl$DIOUT),
              " ",sprintf("%5s",in_data$sim_ctrl$VBOSE)," ",sprintf("%5s",in_data$sim_ctrl$CHOUT),
              " ",sprintf("%5s",in_data$sim_ctrl$OPOUT),"\n",sep=""),file=pf)
    cat("\n", file = pf)
    
    #automatic management
    cat("@  AUTOMATIC MANAGEMENT\n", file = pf)
    cat("@N PLANTING    PFRST PLAST PH2OL PH2OU PH2OD PSTMX PSTMN\n", file = pf)
    cat(paste(sprintf("%2d",as.integer(in_data$auto_mgmt$N))," ",sprintf("%-11s",in_data$auto_mgmt$PLANTING),
              " ",sprintf("%5s",in_data$auto_mgmt$PFRST)," ",sprintf("%5s",in_data$auto_mgmt$PLAST),
              " ",sprintf("%5d",as.integer(in_data$auto_mgmt$PH2OL))," ",sprintf("%5d",as.integer(in_data$auto_mgmt$PH2OU)),
              " ",sprintf("%5d",as.integer(in_data$auto_mgmt$PH2OD))," ",sprintf("%5d",as.integer(in_data$auto_mgmt$PSTMX)),
              " ",sprintf("%5d",as.integer(in_data$auto_mgmt$PSTMN)),"\n",sep=""),file=pf)
    cat("@N IRRIGATION  IMDEP ITHRL ITHRU IROFF IMETH IRAMT IREFF\n",file=pf)
    cat(paste(sprintf("%2d",as.integer(in_data$auto_mgmt$N))," ",sprintf("%-11s",in_data$auto_mgmt$IRRIGATION),
              " ",sprintf("%5d",as.integer(in_data$auto_mgmt$IMDEP))," ",sprintf("%5d",as.integer(in_data$auto_mgmt$ITHRL)),
              " ",sprintf("%5d",as.integer(in_data$auto_mgmt$ITHRU))," ",sprintf("%5s",in_data$auto_mgmt$IROFF),
              " ",sprintf("%5s",in_data$auto_mgmt$IMETH)," ",sprintf("%5d",as.integer(in_data$auto_mgmt$IRAMT)),
              " ",sprintf("%5d",as.integer(in_data$auto_mgmt$IREFF)),"\n",sep=""),file=pf)
    cat("@N NITROGEN    NMDEP NMTHR NAMNT NCODE NAOFF\n",file=pf)
    cat(paste(sprintf("%2d",as.integer(in_data$auto_mgmt$N))," ",sprintf("%-11s",in_data$auto_mgmt$NITROGEN),
              " ",sprintf("%5d",as.integer(in_data$auto_mgmt$NMDEP))," ",sprintf("%5d",as.integer(in_data$auto_mgmt$NMTHR)),
              " ",sprintf("%5d",as.integer(in_data$auto_mgmt$NAMNT))," ",sprintf("%5s",in_data$auto_mgmt$NCODE),
              " ",sprintf("%5s",in_data$auto_mgmt$NAOFF),"\n",sep=""),file=pf)
    cat("@N RESIDUES    RIPCN RTIME RIDEP\n",file=pf)
    cat(paste(sprintf("%2d",as.integer(in_data$auto_mgmt$N))," ",sprintf("%-11s",in_data$auto_mgmt$RESIDUES),
              " ",sprintf("%5d",as.integer(in_data$auto_mgmt$RIPCN))," ",sprintf("%5d",as.integer(in_data$auto_mgmt$RTIME)),
              " ",sprintf("%5d",as.integer(in_data$auto_mgmt$RIDEP)),"\n",sep=""),file=pf)
    cat("@N HARVEST     HFRST HLAST HPCNP HPCNR\n",file=pf)
    cat(paste(sprintf("%2d",as.integer(in_data$auto_mgmt$N))," ",sprintf("%-11s",in_data$auto_mgmt$HARVEST),
              " ",sprintf("%5d",as.integer(in_data$auto_mgmt$HFRST))," ",sprintf("%5d",as.integer(in_data$auto_mgmt$HLAST)),
              " ",sprintf("%5d",as.integer(in_data$auto_mgmt$HPCNP))," ",sprintf("%5d",as.integer(in_data$auto_mgmt$HPCNR)),
              "\n",sep=""),file=pf)
    
    #close file
    close(pf)
    
    #output
    return(out_file)
  }
  make_xfile(in_data, out_file = information$name, overwrite = T)
}
  ## test
  ## make_xfile(in_data, out_file= "./JBID.RIX", overwrite=T) Example Xfile Rice
  