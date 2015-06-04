##############################################################################################################
#  Create batch file for CSM (.v45 file) 
##############################################################################################################

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


# to test
# CSMbatch(data_xfile$crop, data_xfile$name, data_xfile$bname) 
CSMbatch <- function(crop, name, bname) {

  outbatch <- rbind(
    rbind(
      # Batchfile headers            
      paste0("$BATCH(", crop, ")"),            
      "!",            
      cbind(sprintf("%6s %92s %6s %6s %6s %6s", "@FILEX", "TRTNO", "RP", "SQ", "OP", 
                    "CO"))),            
    cbind(sprintf("%6s %88s %6i %6i %6i %6i",            
                  paste0(name),
                  1,  # Variable for treatment number            
                  1,  # Default value for RP element            
                  0,  # Default value for SQ element            
                  1,  # Default value for OP element            
                  0)))  # Default value for CO element 
  
  # Write the batch file to the selected folder  
  write(outbatch, bname, append = F)

}

