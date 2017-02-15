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
# Defines two functions (core_metadata and species_correction) to extract metadata (saple names, species, start and end years)
#   from decadal tree core measurement files and convert species IDs to genus and species names. Then run the function with the appropriate user input and
#   write data to a csv file

# core_meatdata input:
# 1. Project - the name of the project (string)
# 2. Location - where the cores were taken (string)
# 3. PI - who took the cores (string)
# 4. A name to store the dataframe as

# core_metadata output:
# dataframe with one row for each sample

# species_correction input:
# 1. data - the name of the dataframe that was created with core_metadata
# 2. LookupTable - the imported lookup table

# species_correction output:
# 1. the same dataframe with corrected genus and species data

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

# species_correction
# 1) Match species IDs in lookup table to those in dataframe
# 2) Change genus and species information to matching values in lookup table

#------------------------------------



setwd() #set wd to location of files

core_metadata <- function(project, location, Lat, Lon, PI){
  
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
      
      genus <- c("") #make empty string for genus
      
      #combine data in single df
      data_list <- list(project, location, Lat, Lon, PI, samp_name, genus, species_name, begin, end, dat_files[f]) #combine data in list
      df_final <- rbind(df_final, data_list, stringsAsFactors = FALSE) #add list as row
      
      i = i + 1
    }
    
    f = f + 1
  }
  
  colnames(df_final) <- c("Project", "Location", "Lat", "Lon", "PI", "Sample", "Genus", "Species", "YR_Start", "YR_End", "File_name") #rename columns
  return(df_final)
}


# Run as: input_name <- core_metadata("Project_Name", "Location_Name", "Lat", "Lon" "PI_Name")
Ryerson_Flatwoods <- core_metadata("INAI", "Ryerson", "42.181355", "-87.914795", "Bob Fahey") #run function


#import lookup table
library(readr)
LookupTable <- read.csv("core_metadata_lookup.csv")


# Correct species based on lookup table

species_correction <- function(data, LookupTable){ #data = dataframe that was just created
  
  for (z in 1:nrow(LookupTable)){ # for each species value in the lookup table
    if (LookupTable$lookupValue[z] %in% data$Species == TRUE){ # if the species value is found in the dataframe
      dex <- matches(LookupTable$lookupValue[z], Ryerson_Flatwoods$Species, list = TRUE) # find all matches of that species value
      dex <- unlist(dex)

      for (d in 1:length(dex)){ # for each match
        data$Genus[dex][d] <- LookupTable$genus_Value[z] # apply genus name to dataframe
        data$Species[dex][d] <- LookupTable$species_Value[z] # apply species name to dataframe
      }
    }
  }
  return(data)
}


# Run as: input_name <- species_correction(input_name, LookupTable)
Ryerson_Flatwoods <- species_correction(Ryerson_Flatwoods, LookupTable)



# Check dataframe and manually fix any errors



# Run as: write.csv(input_name, file = "input.csv")
write.csv(Ryerson_Flatwoods, file = "Ryerson_Flatwoods.csv") #write df to csv

