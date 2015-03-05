
complete <- function(directory, id = 1:332) {
  
  my_result <- data.frame(id=numeric(),
                          nobs=numeric(),
                          stringsAsFactors=FALSE) 
  
  for (i in id){
    filename <- i
    
    if (filename < 10) filename <- paste("00", filename, ".csv", sep="")
    else if (filename < 100) filename <- paste("0", filename, ".csv", sep="")
    else filename <- paste(filename, ".csv", sep="")
    
    full_filename <- paste (directory, "/", filename, sep="")
    
    temp_frame <- read.csv (file=full_filename,head=TRUE,sep=",")
    
    comp <- complete.cases(temp_frame$nitrate, temp_frame$sulfate)
    
    my_result <- rbind (my_result, c(i, sum(comp)))   
  }
  
  names(my_result)[1]<-paste("id")
  names(my_result)[2]<-paste("nobs")
  
  return (my_result)
}