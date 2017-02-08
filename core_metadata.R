#------------------------------------
# Script Information
#------------------------------------
# Purpose: Extra metadata from raw tree core measurement files 
# Creator: Drew Duckett, 08 February 2017
# Contact: duckettdj@g.cofc.edu
#------------------------------------

#------------------------------------
# Description
#------------------------------------
# Defines a function (core_metadata) to extract metadata (saple names, species, start and end years)
#   from decadal tree core measurement files. Then runn the function with the appropriate user input and
#   write data to a csv file

# core_meatdata input:
# 1. Project - the name of the project (string)
# 2. Location - where the cores were taken (string)
# 3. PI - who took the cores (string)
# 4. A name to store the dataframe as

# core_metadata output:
# dataframe with one row for each sample

# additional script input:
# 1. A file name for the csv

# additional script output:
# A csv file in the current directory containing all the meatadata for that directory

# Note: check the dataframe before writing to a csv. Different data files from different PI's may use different 
#   naming conventions, which may require some formatting

#------------------------------------
# Workflow Overview
#------------------------------------
# core_meatadata:
# 1) Get names for all filenames in directory that are not excel spreadsheets
# 2) Import data using dplR package to convert from decadal format to dataframe
# 3) Isolate metadata and write to dataframe

#------------------------------------



setwd() #set wd to location of files

core_metadata <- function(project, location, PI){
  
  #Load required libraries
  library(dplR)
  library(utils)
  
  #get all raw file names to run
  dat_files <- Sys.glob("*") #get all file names in directory
  rem_files <- dat_files[grep("*.xlsx$", dat_files)] #get file names to remove
  dat_files <- dat_files[!dat_files %in% rem_files] #remove those files with .xlsx
  
  #create df for all data
  df_final <- data.frame(stringsAsFactors = FALSE)
  #assign(label, df_name)
  
  f = 1
  for (y in 1:length(dat_files)){
    
    rwl_df <- read.rwl(dat_files[f]) #read in data
    
    i = 1
    for (z in 1:ncol(rwl_df)){ #for each sample
      
      #get sample name
      samp_name <- names(rwl_df[i])
      
      #get species name
      species_name <- gsub("\\d", "", samp_name) #replace digits
      
      #get start and end years
      no_na <- which(!is.na(rwl_df[,i]), arr.ind = TRUE) #get indices of rows without NA
      begin <- row.names(rwl_df)[no_na[1]] #get year of first datapoint
      end <- row.names(rwl_df)[no_na[length(no_na)]] #get year of last datapoint
      
      #combine data in single df
      data_list <- list(project, location, PI, samp_name, species_name, begin, end, dat_files[f]) #combine data in list
      df_final <- rbind(df_final, data_list, stringsAsFactors = FALSE) #add list as row
      
      i = i + 1
    }
    
    f = f + 1
  }
  
  colnames(df_final) <- c("Project", "Location", "PI", "Sample", "Species", "YR_Start", "YR_End", "File_name") #rename columns
  return(df_final)
}


# Run as: input_name <- core_metadata("Project_Name", "Location_Name", "PI_Name")
#         write.csv(input_name, file = "input.csv")

Ryerson_Flatwoods <- core_metadata("INAI", "Ryerson", "Bob Fahey") #run function

# Check dataframe

write.csv(Ryerson_Flatwoods, file = "Ryerson_Flatwoods.csv") #write df to csv

