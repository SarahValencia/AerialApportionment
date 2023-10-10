#' @title RSEISearch
#'
#' @description This function allows users to access the EPAs' Risk-Screening Environmental Indicators (RSEI) Model database in order to obtain information on the total releases per facility per year, as well as the associated modeled risk and hazard scores. Users can search for specific time periods, facilities, or chemicals.
#'
#' @details The RSEI database includes data from the federally managed Toxic Release Inventory (TRI) program on over 25,000 facilities reporting hazardous emissions since the 1980s, as well as risk scores that account for the size of the chemical release, the fate and transport of the chemical through the environment, the size and location of the exposed population, and the chemical's toxicity. The aggregated version of these data is available for download on the RSEI website. The data are spread out over different tables, with crosswalks between them to link facilities and chemicals to emmissions data at the release level. This function allows users to either read in these tables if they have already been downloaded, or to download the necessary files, and then knits the tables together and filters them based on the search criteria provided to the function. These search criteria allow users to narrow these vast tables to specific years, chemicals, facility locations or types.
#'
#' This function uses the function GetRSEIData to download and unzip the five csv files (Facility, Chemical, Submission, Releases, and Elements) necessary to calculate the summed releases per facility per year of each chemical. Users can also point the function to where these csv files have been saved. The function requires files to have the names they were downloaded with.
#'
#' @param DownloadRSEI Logical. If True, the function GetRSEIData is called to download the necessary RSEI files from the EPA website. Defaults to TRUE.
#'
#' @param RSEIpath Path specifying location where RSEI csv files will to be saved if DownloadRSEI is true, or the path specifying the location of existing RSEI csv files are stored.
#'
#' @param RSEIversion "v2311" or "v2310" to specify whether the RSEI data files are from the current version (23.11) or the previous version (23.10). Defaults to "v2311". Earlier versions of RSEI data are not supported.
#'
#' @param StartYr The first year of RSEI data to be selected.
#'
#' @param EndYr The last year of RSEI data to be selected. Note that the final year of data is 2021 in version 23.11 and 2020 in version 23.10.
#'
#' @param FacilityName Character string or vector of character strings specifying a name or partial name of a TRI facility. Capitalization does not matter.
#'
#' @param Facility_ID_vec Character string or vector of character strings specifying a TRI facility ID.
#'
#' @param NAICS 6 digit numeric string representing a North American Industry Classification System (NAICS) code, or a vector of NAICS codes. Numbers provided to the NAICS argument are matched with those found in the column "Modeled NAICS". According to the RSEI data dictionary, a facility can report multiple NAICS codes. If more than one primary NAICS is reported by the facility, the most frequently reported primary NAICS for the most recent year is selected for the column "NAICS1", and the "Modeled NAICS" column is based on the NAICS1 but is modified based on additional information as necessary.
#'
#' @param State A character string or vector of character strings representing 2 letter state or territory codes.
#'
#' @param County Character string or vector of character strings specifying a name or partial name of a county. Capitalization does not matter.
#'
#' @param City Character string or vector of character strings specifying a name or partial name of a city. Capitalization does not matter.
#'
#' @param ZipCode 5 digit numeric string, or a vector of 5 digit numeric strings, representing zip codes.
#'
#' @param CASNumbers A numerical string or vector of numerical strings representing a CAS Number. A CAS Registry Number is a numeric identifier that can contain up to 10 digits, separated by hyphens. However hyphens, dashes, and spaces are not allowed, so please collapse the standard CAS number format to a digital string.
#'
#' @param ChemicalNames Character string or vector of character strings specifying a name or partial name of a chemical. Capitalization does not matter.
#'
#' @param rsei_Chem_Numbers A number representing a unique idenifyer in the RSEI Chemicals table. Chemical Numbers range from 1 to almost 900 at this time.
#'
#' @param returnLocation Logical. If true returns a spatial dataframe (sf object) of the location of TRI facilities, including street address and latitude/longitude. A .csv file and Rdata object are also saved to the working directory.  Defaults to FALSE.
#'
#' @param projectTo 4 digit numeric string specifying a EPSG coordinate system. If included, the Lat/Long for each facility location are projected to the provided coordinate system in the dataframe containing TRI location information that is created if returnLocation is true.
#'
#' @param wider Logical. If true, data is pivoted so that for each chemical-facility pair, the information about releases and the associated hazard scores per year is in a wide format where there are columns for each year. If false, information about releases/hazards per year is in a long format, with one row for each facility-chemical-year combination. Defaults to TRUE.
#'
#' @param Filename Optional character string specifying name of saved output file. File name will be appended to current date. If not supplied defaults to "RSEIDataPull".
#'
#' @param SaveTo Optional path where final RSEI datafiles should be saved. If NULL, output is not saved.

#' @returns This function returns a dataframe containing the summed chemical releases per facility per year, as well as the associated risk/hazard scores for those releases, based on the search criteria provided. Information on each chemical and facility is also included in the dataframe.
#'
#' There is also an option to return a second dataframe that contains the location information of the facilities returned in the search for use in spatial operations. This sf object contains the point geometry of the facilities along with their names, facility IDs, and addresses, projected into a desired coordinate system.
#'
#' @examples
#' #Download RSEI tables and find yearly releases for all chemicals released by the Ritepack Inc.
#'
#' RitePack <- RSEISearch(DownloadRSEI = TRUE, RSEIversion="v2310",  FacilityName = NULL,
#' Facility_ID_vec = "66024KMGBR52S15",  Filename = "RSEITest4", wider = FALSE)
#'
#' @export
RSEISearch <- function(DownloadRSEI= TRUE, RSEIpath = NULL, RSEIversion="v2311", StartYr=NULL, EndYr=NULL, FacilityName = NULL, Facility_ID_vec = NULL, NAICS=NULL, State = NULL, County = NULL,  City=NULL, ZipCode= NULL, CASNumbers= NULL, ChemicalNames = NULL, rsei_Chem_Numbers  = NULL, returnLocation = FALSE, projectTo = NULL, wider=TRUE, Filename = "RSEIDataPull", SaveTo = NULL){

  #### ## Set working directory
  if(!is.null(RSEIpath)){
    setwd(RSEIpath)
  } else {
    RSEIpath <- NA
  }

  if(DownloadRSEI == TRUE){
      GetRSEIData(rseiversion = RSEIversion, savetodisk = FALSE, directory = RSEIpath)  ## Calls function to download and parse zip file, loads tables for use.
  } else {  ## read in tables
    print_color("Reading in RSEI tables.\n","green")
    if(RSEIversion == "v2311"){
      Facility <- read.csv(paste("facility_data_rsei_",RSEIversion,".csv",sep="")) # has all Facility info
      Chems <- read.csv(paste("chemical_data_rsei_",RSEIversion,".csv",sep="")) # has CAS numbers and info about them
      # note that there is no s on submission for 23.10 data
      Submission <- read.csv(paste("submissions_data_rsei_",RSEIversion,".csv",sep="")) # - TRI data by year for facility. CAS links to chem table)
      Elements <- read.csv(paste("elements_data_rsei_",RSEIversion,".csv",sep=""))  #RSEI scores, links to release table
      Release <- read.csv(paste("releases_data_rsei_",RSEIversion,".csv",sep=""))  # links to submission, has pounds and media
    }
    if(RSEIversion == "v2310"){

      Chems <- read.csv("chemical_data_rsei_v2310.csv")
      Facility <- read.csv("facility_data_rsei_v2310.csv") # has all Facility info
      Submission <- read.csv("submission_data_rsei_v2310.csv")# - TRI data by year for facility. CAS links to chem table.  note that there is no s on submission for 23.10 data
      Elements <- read.csv("elements_data_rsei_v2310.csv")  #RSEI scores, links to release table
      Release <- read.csv("releases_data_rsei_v2310.csv")  # links to submission, has pounds and media

    } # closes if statement
  }

  print_color("Filtering tables.\n","green")
  ### Parse filter criteria
  lowerbound <- min(Submission$SubmissionYear)
  upperbound <- max(Submission$SubmissionYear)
  if(is.null(StartYr)){     # If no start year is provided, defaults to the lowerbound
    StartYr <- lowerbound
  } # close if statement
  if(is.null(EndYr)){     # If no end year is provided, defaults to the upperbound
    EndYr <- upperbound
  } # close if statement
  years <- StartYr:EndYr   #Vector of years to filter. Temporal filter happens early to reduce size of files.
  #check if years are outside bounds
  if(min(years)<lowerbound){
    print_color(paste("Warning: StartYr is before ", lowerbound,"\n",sep=""),"red")
  }
  if(max(years)>upperbound){
    print_color(paste("warning: EndYr is after ", upperbound, "\n",sep=""),"red")
  }

  # filter Submission table for years to reduce size of joins. Submission table will link to Facility
  # Link Submission to Facility Info - filter years here to reduce size of joins.
  Sub1 <- Submission %>%
    filter(SubmissionYear %in% years) %>%
    dplyr::select(SubmissionNumber, FacilityID, CAS, ChemicalNumber, SubmissionYear)

  # Search for chemicals
  Chems <- ChemSearch(ChemTab = Chems, CASnumbersearch = CASNumbers, chemnamesearch = ChemicalNames, rsei_chemnumber  = rsei_Chem_Numbers, AbbrevOutput = TRUE)

  ## Facility Search - narrow by state/county/city/zip - returns same table if no location filters
  Facility <- FacilitySearch(FacilityTab=Facility, facilityname = FacilityName, facilityID = Facility_ID_vec, NAICS  = NAICS,  statesearch = State, countysearch = County, citysearch = City, zipsearch = ZipCode,  AbbrevOutput = FALSE)

  # Pull useful info from Facility table and remove NAs in lat-long
  TRI_Facility <- Facility  %>%
    dplyr::select(FacilityID, Longitude, Latitude, FacilityName, Street, City, County, State, ZIPCode, FIPS, STFIPS, StackHeight, StackVelocity, StackDiameter, StackTemperature, ChromHexPercent, ModeledNAICS, NAICS1) # long first for use in projection

  ## Filter by Releases found in Sub1 to reduce size, then Pull out Submission number, release nuber, media, pounds in order to link to submission.
  REL <- Release %>%
    filter(SubmissionNumber %in% Sub1$SubmissionNumber) %>%
    dplyr::select(SubmissionNumber, ReleaseNumber,Media,PoundsReleased)

  ### Series of joins to knit tables together
  print_color("Knitting tables together.\n","green")

  # Submission has been reduced to certain years, Facility has also been reduced. Want to keep all Facilities.
  Step1<- right_join(Sub1,TRI_Facility,by = "FacilityID")
  Step2<- right_join(Step1, Chems, by= "ChemicalNumber")
  Step3 <- right_join(Step2, REL, by = "SubmissionNumber",multiple = "all")  #there are multiple releases reported per submission number. We want to keep all releases so using a right join.

  ## Filter Elements to include only the release numbers in Step3
  ElementsFiltered <- Elements %>% filter(ReleaseNumber %in% Step3$ReleaseNumber)

  # Multiple scores in elements per release, we want to group by release and sum, then merge into Step3 table by release number.
  ElementSummary <- ElementsFiltered %>%
    group_by(ReleaseNumber) %>%
    summarise(PoundsPT = sum(PoundsPT, na.rm = TRUE),
              Score = sum(Score, na.rm = TRUE),
              NCScore = sum(NCScore, na.rm = TRUE),
              CScore = sum(CScore, na.rm = TRUE),
              Hazard = sum(Hazard, na.rm = TRUE),
              HazardC = sum(HazardC, na.rm = TRUE),
              HazardNC = sum(HazardNC, na.rm = TRUE)
    )  %>%
    as.data.frame()

  Step4 <- left_join(Step3, ElementSummary, by="ReleaseNumber")

  #Multiple releases per chemical (but different media) - group by chemical, facilityID, year.
  Step5 <-  Step4 %>%
    unite(UniqueID,"ChemicalNumber","FacilityID","SubmissionYear", remove=F) %>%  #create a uniqueID that is chem per facility per year
    group_by(UniqueID) %>%
    summarise(
      SubmissionYear = first(SubmissionYear),
      Chemical = first(Chemical),
      CAS = first(CAS),    #need the first because otherwise you are passing a vector and it keeps all values even though they are the same.
      ChemicalNumber = first(ChemicalNumber),
      FacilityName= first(FacilityName),
      FacilityID = first(FacilityID),
      Street = first(Street),
      City = first(City),
      County = first(County),
      State = first(State),
      ZIPCode = first(ZIPCode),
      Longitude = first(Longitude),
      Latitude = first(Latitude),
      StackHeight = first(StackHeight),
      StackVelocity = first(StackVelocity),
      StackDiameter = first(StackDiameter),
      StackTemperature = first(StackTemperature),
      ChromHexPercent = first(ChromHexPercent),
      ModeledNAICS = first(ModeledNAICS),  # Note that we are only useing ModeledNAICS
      PoundsReleased = sum(PoundsReleased,na.rm=T),
      PoundsPT = sum(PoundsPT, na.rm = TRUE),
      Score = sum(Score, na.rm = TRUE),
      NCScore = sum(NCScore, na.rm = TRUE),
      CScore = sum(CScore, na.rm = TRUE),
      Hazard = sum(Hazard, na.rm = TRUE),
      HazardC = sum(HazardC, na.rm = TRUE),
      HazardNC = sum(HazardNC, na.rm = TRUE),
    )  %>%
    select(-UniqueID) %>%
    arrange(SubmissionYear) ## pivot_wider arranges columns in the order they appear so order by year to make years sequential.

  if(wider == TRUE){
    FinalRSEIData <- Step5 %>%
      filter(!is.na(SubmissionYear)) %>%
      pivot_wider(names_from = SubmissionYear,
                  values_from = c(PoundsReleased, PoundsPT, Score, NCScore, CScore, Hazard, HazardC, HazardNC))  %>%
      arrange(FacilityID)

  } else {
    FinalRSEIData <- Step5 %>%
      filter(!is.na(SubmissionYear)) %>%
      arrange(FacilityID)
  }

  # ## Change data sets to spatial object
  # FinalRSEIData_sf <- st_as_sf(FinalRSEIData, coords =  c("Longitude", "Latitude"))  ##long first for use in projection
  # st_crs(FinalRSEIData_sf) <- st_crs(4269)  #tri_buffers has no inherent CRS, assign to NAD83
  #
  #  if(!is.null(projectTo)){
  #   print_color("Re-projecting data.\n","green")
  #   FinalRSEIData_sf<-st_transform(FinalRSEIData_sf, crs=projectTo)
  # }# end if projectTo

  # Write out final dataframe
  if(!is.null(SaveTo)){
    CurrentDate <- Sys.Date()
    print_color("Saving data.\n","green")
    setwd(SaveTo)
    dir.create(paste(Filename,"_",CurrentDate,sep=""))
    setwd(paste(SaveTo,paste(Filename,"_",CurrentDate,sep=""),sep="/"))
    write.csv(FinalRSEIData,paste(Filename,"_",CurrentDate,".csv",sep=""),row.names=FALSE)
    saveRDS(FinalRSEIData, paste(Filename,"_",CurrentDate,".rds",sep=""))
    }

  # Check if returnLocation is true
  if(returnLocation == TRUE){

    # Pull out unique facility IDs and their locations, remove NAs
    TRI_Facility <- FinalRSEIData %>%
      group_by(FacilityID) %>%
      slice(1) %>%
      select(FacilityID,Longitude,Latitude,FacilityName,Street,City,County,State,ZIPCode) %>%
      filter(!is.na(Longitude)) %>%
      filter(!is.na(Latitude))

    ## Change data sets to spatial objects
    tri_locations <- st_as_sf(TRI_Facility, coords =  c("Longitude", "Latitude"))  ##long first for use in projection

    # Remove locations and flag if necessary
    TRI_Facility <- TRI_Facility %>%
      filter(!is.na(Longitude)) %>%
      filter(!is.na(Latitude))

    st_crs(tri_locations) <- st_crs(4269)  #tri_buffers has no inherent CRS, assign to NAD83
    if(!is.null(projectTo)){
      print_color("Re-projecting data.\n","green")
      tri_locations<-st_transform(tri_locations, crs=projectTo)
    }# end if projectTo

    if(!is.null(SaveTo)){
      write.csv(tri_locations,paste(Filename,"_locations_",CurrentDate,".csv",sep=""),row.names=FALSE)
      saveRDS(tri_locations, paste(Filename,"_locations_",CurrentDate,".rds",sep=""))
    }

    # Return list with desired info
    return(list(FinalRSEIData,tri_locations))
  } else {
    return(FinalRSEIData)
  }

} # End Function
