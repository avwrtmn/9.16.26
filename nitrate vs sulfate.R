## creating pollutant mean

pollutantmean <- function(directory, pollutant, id = 1:332) {
  
  ## empty data frame to collect all data
  all_data <- data.frame()
  
  ## loop through each ID number provided in "id" from data set
  for (i in id) {
    
    ## build file name using the id, going up 3 decimal points for
    ## each csv file in the data folder
    file_name <- sprintf("%03d.csv", i)
    
    ## combine initial "directory" argument with the file name
    ## to access the path and data
    file.path <- file.path(directory, file_name)
    
    ## read that specific CSV file into a temporary data frame
    monitor_data <- read.csv(file.path)
    
    ## Add this data onto the collection for function execution
    ## put rows of temporary data frame <monitor_data> into empty data frame
    all_data <- rbind(all_data, monitor_data)
  }
  
  ## Extract the column matching the initial 'pollutant' argument 
  ## <sulfate or nitrate>, and compute the mean ignoring NA values with function
  mean(all_data[[pollutant]], na.rm = TRUE)
}

## function should report values for the following code

pollutantmean("data", "sulfate", 1:10)
pollutantmean("data", "nitrate", 70:72)
pollutantmean("data", "sulfate", 34)
pollutantmean("data", "nitrate")


## part 2
## function showing complete cases observed

complete <- function(directory, id = 1:332) {
  
  ## empty data frame to collect data
  results <- data.frame(
    id = integer(),
    nobs = integer()
  )
  
  ## id is integer vector indicating monitor ID numbers 
  ## to be used from the file
  ## loop through each id vector in data set
  
  for (i in id) {
    file_name <- sprintf("%03d.csv", i)
    
    ## directory indicates location of CSV files relative
    ## to id number
    file_path <- file.path(directory, file_name)
    
    ## read csv file from previous function
    monitor_data <- read.csv(file_path)
    
    ## count complete cases using data from csv files
    complete_cases <- sum(complete.cases(monitor_data))
    
    
    ## Add this data onto the collection for function execution
    ## update data frame <results> with data
    results <- rbind(
      results, 
      data.frame(id = i, nobs = complete_cases))
  }
  return(results)
}
  
  ## run code and report values that execute from function
  
  cc <- complete("data", c(6, 10, 20, 30, 34,
                           100, 200, 310))
  print(cc$nobs)
 
  cc<- complete("data", 54)
  print(cc$nobs)

  RNGversion("3.5.1")
  set.seed(42)
  cc<- complete("data", 332:1)
  use<- sample(332, 10)
  print(cc[use, "nobs"])

  ## Part 3
  ## function that calculates the correlation between sulfate
  ## and nitrate for monitor locations where number of 
  ## observed cases is above a certain threshhold
  
  corr <- function(directory, threshold = 10) {
    
    ## empty vector to store correlations
    correlations <- numeric()
    
    ## loop through all monitor IDs
    for (i in 1:332) {
      file_name <- sprintf("%03d.csv", i)
      file_path <- file.path(directory, file_name)
      monitor_data <- read.csv(file_path)
      
      ## count completely observed cases from function 2
      complete_cases <- sum(complete.cases(monitor_data))
      
      ## only calculate correlation if complete cases > threshold
      ## logical statements
      if (complete_cases > threshold) {
        ## select rows with complete data
        complete_data <- monitor_data[complete.cases(monitor_data), ]
        
        ## calculate correlation between sulfate and nitrate
        correlation <-cor(
          complete_data$sulfate,
          complete_data$nitrate
        )
        
        ## add correlation to vector
        correlations <- c(correlations, correlation)
      }
  }

    correlations
  }
## run code with function and report values
  cr <- corr("data")
  cr <- sort(cr)
  RNGversion("3.5.1")
  set.seed(868)
  out <- round(cr[sample(length(cr), 5)], 4)
  print(out)

  cr <- corr("data", 129)
  cr <- sort(cr)
  n <- length(cr)
  RNGversion("3.5.1")
  
  set.seed(197)
  out <- c(n, round(cr[sample(n,5)],4))
  print(out)

  cr<-corr("data", 2000)
  n <- length(cr)
  cr <- corr("data", 1000)
  cr <- sort(cr)
  print(c(n,round(cr, 4)))







