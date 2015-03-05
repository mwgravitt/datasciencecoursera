
pollutantmean <- function(directory, pollutant, id = 1:332) {
  
  #  print ( paste("directory: ", directory))
  #  print ( paste("pollutant: ", pollutant))
  #  print ( paste("id: ", id))
  
  my_monitor <- data.frame(Date=as.Date(character()),
                           sulfate=numeric(), 
                           nitrate=numeric(), 
                           ID=character(),
                           stringsAsFactors=FALSE) 
  
  for (filename in id){
    if (filename < 10) filename <- paste("00", filename, ".csv", sep="")
    else if (filename < 100) filename <- paste("0", filename, ".csv", sep="")
    else filename <- paste(filename, ".csv", sep="")
    
    full_filename <- paste (directory, "/", filename, sep="")
    #    print (full_filename)
    
    temp_frame <- read.csv (file=full_filename,head=TRUE,sep=",")
    
    my_monitor <- rbind (my_monitor, temp_frame)
  }
  
  if (pollutant == "nitrate") {
    #    print ("nitrate")
    round(mean(my_monitor$nitrate, na.rm=TRUE), digits=3)
  }
  else if (pollutant == "sulfate") {
    #    print ("sulfate")
    round(mean(my_monitor$sulfate, na.rm=TRUE), digits=3)
  }
  else {
    print (paste ("pollutant of ", pollutant, " no supported", sep=""))
  }
}

