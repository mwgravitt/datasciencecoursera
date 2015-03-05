corr <- function(directory, threshold = 0) {
  
  corr_values <- c()
  
  monitors <- data.frame(id=numeric(),
                         nobs=numeric(),
                         stringsAsFactors=FALSE) 
  
  ## call 'complete' function 
  monitors <- complete(directory, 1:332)
  
  ## subset result from complete function by threshold criteria
  monitors <- subset(monitors, nobs >= threshold)
  
  ## run and return correlation
  monitors_above_threshold <- monitors$id
  
  for (i in monitors_above_threshold) {
    
    filename <- i
    
    if (filename < 10) filename <- paste("00", filename, ".csv", sep="")
    else if (filename < 100) filename <- paste("0", filename, ".csv", sep="")
    else filename <- paste(filename, ".csv", sep="")
    
    full_filename <- paste (directory, "/", filename, sep="")
    
    temp_frame <- read.csv (file=full_filename,head=TRUE,sep=",")
    
    comp <- temp_frame[complete.cases(temp_frame$nitrate, temp_frame$sulfate),]
    
    corr_values <- c(corr_values, cor(comp$nitrate, comp$sulfate))
  }
  
  return (corr_values)
}