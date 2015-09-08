Xfile <- function(information, pixel, in_conditions, initial) {
  
  cultivars <- information$INGENO
  PPOP <- information$PPOP
  PPOE <- information$PPOE
  PLME <- paste(information$PLME)
  PLDS <- paste(information$PLDS)
  PLRD <- information$PLRD  
  PLDP <- information$PLDP 
  PLWT <- information$PLWT
  SPRL <- information$SPRL
  
  information$initation <- information$initation[pixel]
  #information$final <- crop_riego$mirca.end[pixel]
  information$nitrogen_aplication <- list(amount = amount[pixel, ], day_app = day_app[pixel, ])
  
  #   if(information$initation > information$final){
  #     information$final <- 365
  #   }
  
  
  #midpoint_window <- round(mean(c(information$initation, information$final))) ## Midpoint planting window
  
  if( information$system == "irrigation" ){
    
    
    
    
    # Initial Conditions
    if(information$crop == "RICE"){
      SNH4 <- c(4, 4, 2, 1, 0.1, 0.1, 0.1, 0.1, 0.1)
      SNO3 <- c(0.7, 0.7, 0.5, 0.3, 0.1, 0.1, 0.1, 0.1, 0.1)
      
    } else{
      SNH4 <- c(3, 3, 2, 1, 0.1, 0.1, 0.1, 0.1, 0.1)
      SNO3 <- c(3, 3, 2, 1, 0.1, 0.1, 0.1, 0.1, 0.1)
      
    }
    
    
    #     pdate <- convert_date(pmax(midpoint_window, 0), information$year)        ## Planting date
    IC <- 1
    pdate <- convert_date(information$initation, information$year)        ## Planting date
    #     sdate <- convert_date(pmax(information$initation - 60, 0), information$year)   ## Simulation Start
    #  sdate <- convert_date(information$initation - 30, information$year)   ## Simulation Start
    sdate <- pdate
    A <- "PL"      ## label name
    first <- -99  ## Start planting window
    last <- -99   ## End planting window
    IRR <- "A"    ## Automatic irrigation
    plant <- "R"    ## On reported date
    
  }
  
  #SNH4 <- c(10, 10, 0, 0, 0, 0, 0, 0, 0)
  #SNO3 <- c(5, 2, 0, 0, 0, 0, 0, 0, 0)
  
  if( information$system == "rainfed" ) {
    
    
    #      SNH4 <- c(1.1, 1.1, 0.7, 0.7, 0.5, 0.3, 0.1, 0.1, 0.1)
    #      SNO3 <- c(0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1)
    
    #     SNH4 <- c(10, 10, 0, 0, 0, 0, 0, 0, 0)
    #     SNO3 <- c(5, 2, 0, 0, 0, 0, 0, 0, 0)
    
    
    # Initial Conditions 13 July in the morning (2015)
    
    #     SNH4 <- c(4, 4, 2, 1, 0.1, 0.1, 0.1, 0.1, 0.1)
    #     SNO3 <- c(0.7, 0.7, 0.5, 0.3, 0.1, 0.1, 0.1, 0.1, 0.1)
    
    # Initial Conditions 13 July in the afternoun (2015)
    
    SNH4 <- c(3, 3, 2, 1, 0.1, 0.1, 0.1, 0.1, 0.1)
    SNO3 <- c(3, 3, 2, 1, 0.1, 0.1, 0.1, 0.1, 0.1)
    
    IC <- 1
    pdate <- convert_date(information$initation, information$year)        ## Planting date
    sdate <- convert_date(information$initation, information$year)    ## Simulation Start
    A <- "PL"         ## Label Name
    IRR <- "N"        ## No irrigation (rainfed)
    plant <-"R"        ## Automatic planting seed given window
    ### Reconvertir first para que corra con dia de simulacion un año antes
    first <- -99  ## Start planting window
    last <- -99
    
  }
  
  
  ## Defining the Experiment
  in_data <- list()
  
  
  ## General data of the Experiment
  
  in_data$general <- list(PEOPLE = "Diego Obando Jeison Mesa", ADDRESS = "CIAT", SITE = "CALI")
  
  
  ## Definition simulate treatment

#   in_data$treatments <- data.frame(N = 1:dim(cultivars)[1], R = rep(1, dim(cultivars)[1]), O = rep(1, dim(cultivars)[1]), C = rep(0, dim(cultivars)[1]),
#                                   TNAME = cultivars[, "Genotype"], CU = rep(1 ,dim(cultivars)[1]), FL = rep(1, dim(cultivars)[1]), SA = rep(0, dim(cultivars)[1]),
#                                   IC = rep(IC, dim(cultivars)[1]), MP = rep(1, dim(cultivars)[1]),
#                                   MI = rep(0, dim(cultivars)[1]), MF = rep(1, dim(cultivars)[1]), MR = rep(0, dim(cultivars)[1]), MC = rep(0, dim(cultivars)[1]),
#                                   MT = rep(0, dim(cultivars)[1]), ME = rep(0, dim(cultivars)[1]), MH = rep(0, dim(cultivars)[1]), SM = rep(0, dim(cultivars)[1]))
#     
  
  in_data$treatments <- data.frame(N = 1, R = 1, O = 1, C = 0, TNAME = "BID001", CU = 1, FL = 1, SA = 0, IC = IC, MP = 1,
                                 MI = 0, MF = 1, MR = 0, MC = 0, MT = 0, ME = 0, MH = 1, SM = 1)
  
  ## Definition simulate cultivar
  
#   in_data$cultivars <- data.frame(C = 1:dim(cultivars)[1], CR = rep(information$CR, dim(cultivars)[1]),
#                                   INGENO = cultivars[, "Code"], CNAME = rep(information$CNAME, dim(cultivars)[1]))
  
  in_data$cultivars <- data.frame(C = 1, CR = information$CR, INGENO = information$INGENO[pixel], CNAME = information$CNAME)

  ## Field
  
  in_data$fields <- data.frame(L = 1, ID_FIELD = "BID1", WSTA = "JBID", FLSA=-99, FLOB = -99, FLDT = "DR000",
                               FLDD = -99, FLDS = -99, FLST = -99, SLTX = -99, SLDP = -99, ID_SOIL="BID0000001",
                               FLNAME = "FIELD01", XCRD = -99, YCRD = -99, ELEV = -99, AREA = -99, SLEN=-99,
                               FLWR = -99, SLAS = -99, FLHST = -99, FHDUR=-99)
  
  ## initial conditions of the experiment
  ## Aqui investigar acerca de ICDAT
  ## Segun el manual Initial Conditions Measurement date, year + days
  in_data$ini_cond_properties <- data.frame(C = 1, PCR = information$CR, ICDAT = "50001", ICRT = -99, ICND = -99, ICRN = -99, ICRE = -99,
                                            ICWD = -99, ICRES = -99, ICREN = -99, ICREP = -99, ICRIP = -99, ICRID = -99,
                                            ICNAME = "inicond1")
  
  #in_data$ini_cond_profile <- data.frame(C=rep(1,5),ICBL=rep(-99,5),SH2O=rep(-99,5),SNH4=rep(-99,5),
  #                                       SNO3=rep(-99,5))
  
  
  ## Planting Details
  in_data$planting <- data.frame( P = 1, PDATE = pdate, EDATE = -99, PPOP, PPOE, PLME, 
                                  PLDS, PLRS = -99, PLRD, PLDP,
                                  PLWT, PAGE = -99, PENV = -99, PLPH = -99, SPRL)

  ## Harvest Details

  in_data$harvest <- data.frame(H = 1, HADTE = 1, HSTG = "GS000", HCOM = -99, HSIZE = -99, HPC = -99, HBPC = -99)
  
  ## Simulation Control 
  in_data$sim_ctrl <- data.frame(N = 1, GENERAL = "GE", NYERS = 25, NREPS = 1, START = "S", SDATE = sdate, 
                                 RSEED=2150, SNAME = "simctr1", SMODEL = paste(information$smodel), 
                                 OPTIONS = "OP", WATER = "Y", NITRO = "Y", SYMBI = "N",
                                 PHOSP = "N", POTAS = "N", DISES = "N", CHEM = "N", TILL = "N", 
                                 CO2 = "D", METHODS = "ME", WTHER = "M", INCON = "M", LIGHT = "E", 
                                 EVAPO = "R", INFIL = "S", PHOTO = "C", HYDRO = "R",
                                 NSWIT = 1, MESOM = "G", MESEV = "S", MESOL =2, MANAGEMENT = "MA", 
                                 PLANT = plant, IRRIG = IRR,
                                 FERTI = "D", RESID = "R", HARVS = "D", OUTPUTS = "OU", FNAME = "N", 
                                 OVVEW = "Y", SUMRY = "Y", FROPT = 1, GROUT = "Y", CAOUT = "Y", 
                                 WAOUT = "Y", NIOUT = "Y", MIOUT = "Y",
                                 DIOUT = "Y", VBOSE = "Y", CHOUT = "Y", OPOUT = "Y")
  
  ## AUTOMATIC MANAGEMENT 
  
  in_data$auto_mgmt <- data.frame(N = 1, PLANTING = A, PFRST = first, PLAST = last, PH2OL = 50, PH2OU = 100,
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
    cat("@L ..........XCRD ...........YCRD .....ELEV .............AREA .SLEN .FLWR .SLAS FLHST FHDUR\n",file=pf)
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
    
    # Initial Conditions
    if(exists("in_conditions") & initial == T) {
      
      cat("*INITIAL CONDITIONS\n", file = pf)
      cat("@C   PCR ICDAT  ICRT  ICND  ICRN  ICRE  ICWD ICRES ICREN ICREP ICRIP ICRID ICNAME\n", file = pf)
      cat(sprintf("%2d %5s %5s %5s %5s %5s %5s %5s %5s %5s %5s %5s %5s %2s", 1, "MZ", sdate, 1, -99, 1, 1, -99,  -99, -99, -99, -99, -99, -99), "\n", file = pf)
      
      cat("@C  ICBL  SH2O  SNH4  SNO3\n", file = pf)
      for(i in 1:dim(in_conditions)[1]){
        
        if(information$system  == "rainfed"){
          cat(paste(sprintf("%2d %5d %5.2f %5.2f %5.2f", 1, in_conditions[i, 1], in_conditions[i, 2], SNH4[i], SNO3[i])), "\n", file = pf)
        }
        
        if(information$system  == "irrigation"){
          cat(paste(sprintf("%2d %5d %5.0f %5.2f %5.2f", 1, in_conditions[i, 1], in_conditions[i, 2], SNH4[i], SNO3[i])), "\n", file = pf)
        }
        
        
      }      
      
      
    }
    
    cat("\n", file = pf)
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
        
        
        if(i == 1){
          
          cat(sprintf("%2s %5s %4s %5s %5i %5.1f %5i %5i %5i %5i %5i %1i", 1, information$nitrogen_aplication$day_app[, i], "FE005", "AP002", 
                      4, information$nitrogen_aplication$amount[, i], 0, -99, -99, -99, -99, -99), "\n", file = pf)
          
        } else{
          cat(sprintf("%2s %5s %4s %5s %5i %5.1f %5i %5i %5i %5i %5i %1i", 1, information$nitrogen_aplication$day_app[, i], "FE005", "AP002", 
                      0, information$nitrogen_aplication$amount[, i], 0, -99, -99, -99, -99, -99), "\n", file = pf)
        }
        
      }
      
    }
    cat("\n", file = pf)
    
    ## Detalle de Cosecha
    
    cat("*HARVEST DETAILS\n", file = pf)
    cat("@H HDATE HSTG  HCOM HZISE   HPC  HBPC HNAME \n", file = pf)
    cat(" 1   180 GS000  -99   -99   -99   -99 \n", file = pf)
    
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
