read_mean_outputvalue <- function(sim_dir, years,
                                  model = "PHENOFIT", output_var = "Fitness",
                                  correct_date = FALSE){
  
  if(model == "PHENOFIT"){
    output <- fread(paste0(sim_dir,"/", output_var, ".txt"), header=T, sep="\t", fill=T)
    if(correct_date){
      output[output >= 365] <- 366
      output[output <= -999] <- 366
      output[is.na(output)] <- 366
    }
    output_mean <- apply(output[output$`Location id` %in% as.character(years),-1], 2, mean)
    output <- data.frame(lat = as.numeric(output[1,-1]), lon = as.numeric(output[2,-1]), value = output_mean)
  }
  
  return(output)
  
}
