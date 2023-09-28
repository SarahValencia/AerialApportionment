#' @title GetRSEIData
#' 
#' @description Function to download and save publicly available data from the EPA's Risk-Screening Environmental Indicators (RSEI) Model.
#' 
#' @details Five RSEI tables, including the Facility, Chemicals, Submissions, Releases, and Elements are downloaded. See the RSEI Data Dictionary for more information on what is contained in each table.
#' 
#' @param rseiversion Users can currently specify "v2311" (the most recent version of RSEI data) or "v2310" (the last version). 
#' 
#' @param savetodisk Logical. If TRUE, the dataframes are saved as csv files in a folder in the directory provided. 
#' 
#' @param directory An optional path specifying where the data should be saved.
#' 
#' @param seconds The number of seconds before the download times out. Defaults to 500 but can be extended if necessary.

#' @returns A list containing the five RSEI tables. These tables are also assigned to the global environment for use with other functions in this package, and saved as csv files if savetodisk is TRUE.
#' 
#' @examples
#' \dontrun{RSEIList <- GetRSEIData(rseiversion = "v2311", savetodisk = FALSE)}
#' 
#' @export
GetRSEIData <- function(rseiversion = "v2311", savetodisk = FALSE, directory = NULL, seconds=500){
  
  if(rseiversion == "v2311"){
    
    # Extends download time to 500 seconds to prevent timing out
    options(timeout = seconds)
    
    # Opens a temporary file to store downloaded zip file in
    temp <- tempfile()
    
    #link to download zip file:
    download.file("https://gaftp.epa.gov/rsei/Current_Version/V2311_RY2021/Public_Release_Data/RSEIv2311_Public_Release_Data.zip",temp, quiet=FALSE)
    
    print_color("Download complete. Unzipping Files.\n", "green")
    
    # unzips and extracts each csv file necessary for RSEI data pull function
    Chems <- read.csv(unz(description=temp, filename = "chemical_data_rsei_v2311.csv"))
    Facility <- read.csv(unz(description=temp, filename = "facility_data_rsei_v2311.csv")) # has all Facility info
    Submission <- read.csv(unz(description=temp, filename = "submissions_data_rsei_v2311.csv"))# - TRI data by year for facility. CAS links to chem table.  note that there is no s on submission for 23.10 data
    Elements <- read.csv(unz(description=temp, filename = "elements_data_rsei_v2311.csv"))  #RSEI scores, links to release table
    Release <- read.csv(unz(description=temp, filename = "releases_data_rsei_v2311.csv"))  # links to submission, has pounds and media
    
    # closes link
    unlink(temp)
    
    
    # sets folder name for storage
    folder <- "RSEI_Public_Release_Data_V2311_RY2021"
    
    if(savetodisk ==T ){
      print_color("Saving files to disk.\n","green")
      currDir <- getwd()
      setwd(directory)
      dir.create(folder)  # create folder to store info in. 
      setwd(paste(directory,folder,sep="/"))
      write.csv(Chems, "chemical_data_rsei_v2311.csv",row.names=FALSE)  # change naming to be dynamic.
      write.csv(Facility,"facility_data_rsei_v2311.csv",row.names=FALSE)
      write.csv(Submission,"submissions_data_rsei_v2311.csv",row.names=FALSE)
      write.csv(Elements, "elements_data_rsei_v2311.csv",row.names=FALSE)
      write.csv(Release,"releases_data_rsei_v2311.csv",row.names=FALSE)
      setwd(currDir)  # set working directory back to what it was before
    }
  } # close if current version statement
  
  if(rseiversion == "v2310"){
    # Extends download time to 500 seconds to prevent timing out
    options(timeout = seconds)
    
    # Opens a temporary file to store downloaded zip file in
    temp <- tempfile()
    
    #link to download zip file:
    download.file("https://gaftp.epa.gov/rsei/Older_Versions/Version2310_RY2020/Public_Release_Data/RSEIv2310_Public_Release_Data.zip",temp, quiet=FALSE)
    
    print_color("Download complete. Unzipping Files.\n", "green")
    
    # unzips and extracts each csv file necessary for RSEI data pull function
    Chems <- read.csv(unz(description=temp, filename = "chemical_data_rsei_v2310.csv"))
    Facility <- read.csv(unz(description=temp, filename = "facility_data_rsei_v2310.csv")) # has all Facility info
    Submission <- read.csv(unz(description=temp, filename = "submission_data_rsei_v2310.csv"))# - TRI data by year for facility. CAS links to chem table.  note that there is no s on submission for 23.10 data
    Elements <- read.csv(unz(description=temp, filename = "elements_data_rsei_v2310.csv"))  #RSEI scores, links to release table
    Release <- read.csv(unz(description=temp, filename = "release_data_rsei_v2310.csv"))  # links to submission, has pounds and media
    
    # closes link
    unlink(temp)
    
    # sets folder name for storage
    version <- "v2310"
    folder <- "RSEI_Public_Release_Data_V2310_RY2020"
    
    if(savetodisk ==T ){
      print_color("Saving files to disk.\n","green")
      currDir <- getwd()
      setwd(directory)
      dir.create(folder)  # create folder to store info in. 
      setwd(paste(directory,folder,sep="/"))
      write.csv(Chems, "chemical_data_rsei_v2310.csv",row.names=FALSE)  # change naming to be dynamic.
      write.csv(Facility,"facility_data_rsei_v2310.csv",row.names=FALSE)
      write.csv(Submission,"submission_data_rsei_v2310.csv",row.names=FALSE)
      write.csv(Elements, "elements_data_rsei_v2310.csv",row.names=FALSE)
      write.csv(Release,"release_data_rsei_v2310.csv",row.names=FALSE)
      setwd(currDir)  # set working directory back to what it was before
    }
  } # closes if statement
  
 
  # assign data frames to global environment
  assign('Chems',Chems,envir=.GlobalEnv)
  assign('Facility',Facility,envir=.GlobalEnv)
  assign('Submission',Submission,envir=.GlobalEnv)
  assign('Elements',Elements,envir=.GlobalEnv)
  assign('Release', Release,envir=.GlobalEnv)
  
  return(list(Chems,Facility,Submission, Elements, Release))
  
} # close function


