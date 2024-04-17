read_mean_outputvalue <- function(sim_dir, years,
                                  model = "PHENOFIT", output_var = "Fitness",
                                  correct_date = FALSE){
  
  if(model == "PHENOFIT" & output_var != "EcodormancyCustom" & output_var != "SenescenceCustom" & output_var != "MaturationCustom"){
    output <- fread(paste0(sim_dir,"/", output_var, ".txt"), header=T, sep="\t", fill=T)
    output <- data.table::transpose(output) # transform to a more-friendly format
    colnames(output) <- as.character(output[1,])
    output <- output[-1,] # get rid of first line
    output <- output[, lapply(.SD, as.numeric)] # transform to numeric
    
    if(correct_date){
      output <- output[, replace(.SD, .SD >= 365 | .SD <= -999 | is.na(.SD), 366)] # correct dates if necessary
    }
    
    output[, mean := rowMeans(.SD), .SDcols = as.character(years)] # average over years
    output <- data.frame(output[,c("Location latitude", "Location longitude", "mean")]) # people prefer data.frame!
  }else if(model == "PHENOFIT" & output_var == "SenescenceCustom"){
    
    output <- fread(paste0(sim_dir,"/", "LeafSenescenceDate", ".txt"), header=T, sep="\t", fill=T)
    output <- transpose(output) # transform to a more-friendly format
    colnames(output) <- as.character(output[1,])
    output <- output[-1,] # get rid of first line
    output <- output[, lapply(.SD, as.numeric)] # transform to numeric
    
    if(correct_date){
      output <- output[, replace(.SD, .SD >= 365 | .SD <= -999 | is.na(.SD), NA)] # correct dates if necessary
    }
    
    output[, mean := rowMeans(.SD, na.rm = TRUE), .SDcols = as.character(years)] # average over years
    output <- data.frame(output[,c("Location latitude", "Location longitude", "mean")]) # people prefer data.frame!
  }else if(model == "PHENOFIT" & output_var == "MaturationCustom"){
    
    output <- fread(paste0(sim_dir,"/", "FruitMaturationDate", ".txt"), header=T, sep="\t", fill=T)
    output <- transpose(output) # transform to a more-friendly format
    colnames(output) <- as.character(output[1,])
    output <- output[-1,] # get rid of first line
    output <- output[, lapply(.SD, as.numeric)] # transform to numeric
    
    if(correct_date){
      output <- output[, replace(.SD, .SD >= 365 | .SD <= -999 | is.na(.SD), NA)] # correct dates if necessary
    }
    
    output[, mean := rowMeans(.SD, na.rm = TRUE), .SDcols = as.character(years)] # average over years
    output <- data.frame(output[,c("Location latitude", "Location longitude", "mean")]) # people prefer data.frame!
  }else if(model == "PHENOFIT" & output_var == "EcodormancyCustom"){
    
    output <- fread(paste0(sim_dir,"/", "LeafDormancyBreakDate", ".txt"), header=T, sep="\t", fill=T)
    output <- transpose(output) # transform to a more-friendly format
    colnames(output) <- as.character(output[1,])
    output <- output[-1,] # get rid of first line
    output <- output[, lapply(.SD, as.numeric)] # transform to numeric
    endodormancy <- output
    
    output <- fread(paste0(sim_dir,"/", "LeafUnfoldingDate", ".txt"), header=T, sep="\t", fill=T)
    output <- transpose(output) # transform to a more-friendly format
    colnames(output) <- as.character(output[1,])
    output <- output[-1,] # get rid of first line
    output <- output[, lapply(.SD, as.numeric)] # transform to numeric
    leafout <- output
    
    if(correct_date){
      endodormancy <- endodormancy[, replace(.SD, .SD >= 365 | .SD <= -999 | is.na(.SD), NA)] # correct dates if necessary
      leafout <- leafout[, replace(.SD, .SD >= 365 | .SD <= -999 | is.na(.SD), NA)]
    }
    
    substract <- data.table()
    cols <- as.character(years)
    ecodormancy <- as.data.table(as.matrix(leafout[,..cols])-as.matrix(endodormancy[,..cols]))
    
    ecodormancy$`Location latitude` <- leafout$`Location latitude`
    ecodormancy$`Location longitude` <- leafout$`Location longitude`
    
    ecodormancy[, mean := rowMeans(.SD, na.rm = TRUE), .SDcols = as.character(years)] # average over years
    output <- data.frame(ecodormancy[,c("Location latitude", "Location longitude", "mean")]) # people prefer data.frame!
  }
  
  return(output)
  
}
