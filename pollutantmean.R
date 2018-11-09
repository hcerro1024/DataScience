pollutantmean <- function(directory = " ", pollutant = " ", id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the .csv files
  ##----------------------------------------------------------------------------
  ## 'pollutant' is a character vector of length 1 indicating
  ## the name of the pollutant for which we will calculate the
  ## mean; either "sulfate" or "nitrate".
  ##----------------------------------------------------------------------------  
  ## Return the mean of the pollutant across all monitors list
  ## in the 'id' vector (ignoring NA values)
  ## NOTE: Do not round the result!
  ## setwd("~/Varen/Training/coursera/R Programming for Data Science")
  ##----------------------------------------------------------------------------
  ## Error Handling
  ##----------------------------------------------------------------------------  
  dir_const = "Directory Input Error"
  pollute_const = "Pollutant Input Error"
  id_const = "ID Input Error"
  ##----------------------------------------------------------------------------  
  if (directory == "specdata") dir_const = "specdata" else {
    print(paste("Directory error!", directory, "is not a valid entry."), sep = "")
    stop()
  }
  
  if(length(id) == 1) {
    if(id > 332) {
    print("ID Input Error. Value must be less than 333")
    stop()
    }
  }
  
  if (pollutant == "sulfate") pollute_const = "sulfate"
  else if(pollutant == "nitrate") pollute_const = "nitrate" else {
    print(paste("Pollutant error!", pollutant, "is not a valid entry."), sep = "")
    stop()
  }
  ##----------------------------------------------------------------------------  
  ## Retrieve the data and calculate the mean (ignoring "NA" values)
  ##----------------------------------------------------------------------------    
  if(length(id) == 1) {
    if(id < 333) {
      id_const = formatC(id, width = 3, format = "d", flag = "0")
      pfile <- paste("./", dir_const, "/", id_const, ".csv", sep = "")
      fdate <- read.csv(file = pfile, header = TRUE, sep = ",", skipNul = FALSE, allowEscapes = TRUE, fileEncoding = "UTF-8")
      if(pollute_const == "nitrate") value_to_return <- fdate$nitrate
      else value_to_return <- fdate$sulfate
      mean(value_to_return, na.rm = TRUE)
    }
  }
  else print("Too many ID vectors")
}
