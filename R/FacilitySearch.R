#' @title FacilitySearch 
#' 
#' @description This function allows users to search the Facility table of the RSEI database, which contains identifying information on facilities that have reported emissions via the Toxic Release Inventory (TRI) program. The unfiltered Facility table is provided as an input, and the table is filtered to return rows matching the search criteria provided in the arguments. There is an option to return all columns of the table or an abbreviated table with only columns containing the facility name and location information.
#' 
#' @details The Facility table provided to the function is filtered sequentially in the order the search terms are listed in the arguments section below. If a search term has no matches and yields an empty dataframe, searches with subsequent arguments are not completed. Instead, a warning is provided and an empty dataframe is returned. 
#' 
#' @param FacilityTab The Facility table downloaded from the publicly available RSEI database and stored as a dataframe. 
#' 
#' @param facilityname Character string or vector of character strings specifying a name or partial name of a TRI facility. Capitalization does not matter.
#' 
#' @param facilityID Character string or vector of character strings specifying a TRI facility ID.
#' 
#' @param NAICS 6 digit numeric string representing a North American Industry Classification System (NAICS) code, or a vector of NAICS codes. Numbers provided to the NAICS argument are matched with those found in the column "Modeled NAICS". According to the RSEI data dictionary, a facility can report multiple NAICS codes. If more than one primary NAICS is reported by the facility, the most frequently reported primary NAICS for the most recent year is selected for the column "NAICS1", and the "Modeled NAICS" column is based on the NAICS1 but is modified based on additional information as necessary. 
#' 
#' @param statesearch A character string or vector of character strings representing 2 letter state or territory codes.
#' 
#' @param countysearch Character string or vector of character strings specifying a name or partial name of a county. Capitalization does not matter.
#' 
#' @param citysearch Character string or vector of character strings specifying a name or partial name of a city. Capitalization does not matter.
#' 
#' @param zipsearch 5 digit numeric string, or a vector of 5 digit numeric strings, representing zip codes.
#' 
#' @param AbbrevOutput - Logical, defaults to TRUE. If abbreviated the function returns the Facility ID, Name, Street, City, County, State, ZipCode, ModeledNAICS, and the Lat/Lon, otherwise all columns in the RSEI Facility table are returned.
#' 
#' @returns Dataframe containing rows that match the search criteria provided. If all search terms are NULL, the original Facility dataframe is returned. Note that if there are no rows matching the argument(s) provided an empty dataframe is returned and a warning is provided.
#' 
#' @examples
#' # If you are searching for a partial string, this returns facilities in the citys of Buena,
#' # Buena Vista, Buena Park, Aguas Buenas, and Lake Buena Vista.
#' # FacilitySearch(FacilityTab, citysearch = "buena")
#' 
#' # You can search for partial facility names
#' # FacilitySearch(FacilityTab = Facility, facilityname = c("chevron","monsanto")) 
#' 
#' # If you are searching for a city include the state for more accurate results.
#' # FacilitySearch(FacilityTab = Facility, citysearch = "oakland", statesearch= "NJ") 
#' 
#' @export
FacilitySearch <- function(FacilityTab, facilityname = NULL, facilityID = NULL, 
                           NAICS  = NULL,  statesearch = NULL, countysearch = NULL, 
                           citysearch = NULL, zipsearch = NULL,  AbbrevOutput = TRUE){
  
  
  ## Check if matching facility name is there
  if(!is.null(facilityname)){  
    #decapitilize search 
    searchTerms <- tolower(facilityname)
    
    # This code decapitalizes the names in the table to elimiminate needing to match capitilizations.
    FacilityTab <- FacilityTab[unlist(sapply(searchTerms, grep, tolower(FacilityTab$FacilityName), USE.NAMES = F)), ]
    
    if(dim(FacilityTab)[1] == 0){
      print_color("No matching facility names.\n","red")
      return(FacilityTab)
    }

  } ## end if searching for facility name
  
  ## Check if facility ID is contained in dataset.
  if(!is.null(facilityID)){
    if(any(!facilityID %in% FacilityTab$FacilityID)) {
      print_color("Warning: the following Facility IDs are not found in the RSEI Facility Table.\n","red")
      pos <-which(!facilityID %in% FacilityTab$FacilityID)
      print(facilityID[pos])
    }  #close warning
    #filter Facility table to include desired facilities
    FacilityTab <- FacilityTab %>% filter(FacilityID %in% facilityID) # keep rows that match these IDs
  }
  
  ## check that something is returned
  if(dim(FacilityTab)[1] == 0){
    print_color("No matching facility IDs.\n","red")
    return(FacilityTab)
  }
  
  ## Check if NAICS are there and if so, filter  - using modeled NAICS
  if(!is.null(NAICS)){       #  Check if NAICS argument was supplied
    if(any(!NAICS %in% Facility$ModeledNAICS)) {
      print_color("Warning: the following NAICS are not found in the RSEI Facility Table.\n","red")
      pos <-which(!NAICS %in% Facility$ModeledNAICS)
      print(NAICS[pos])
    } # if missing
    
    # filter NAICS in Facility Table if argument is provided
    FacilityTab <- FacilityTab %>% filter(ModeledNAICS %in% NAICS) # keep rows that match these IDs
  } #close if NAICS argument provided
  
  #Filter by state if provided before any other locational steps
  if(!is.null(statesearch)){
    FacilityTab <- FacilityTab %>% filter(State == statesearch)
    
    if(dim(FacilityTab)[1] == 0){
      print_color("No matching state names.\n","red")
      return(FacilityTab)
    }
  }
  
  ## Check if county search
  if(!is.null(countysearch)){  
    #decapitilize search 
    searchTerms <- tolower(countysearch)
    
    # This code decapitalizes the names in the table to elimiminate needing to match capitilizations.
    FacilityTab <- FacilityTab[unlist(sapply(searchTerms, grep, tolower(FacilityTab$County), USE.NAMES = F)), ]
    
    if(dim(FacilityTab)[1] == 0){
      print_color("No matching county names.\n","red")
      return(FacilityTab)
    }
    
  } ## end if searching for county
  
  ## Check if city search
  if(!is.null(citysearch)){  
    #decapitilize search 
    searchTerms <- tolower(citysearch)
    
    # This code decapitalizes the names in the table to elimiminate needing to match capitilizations.
    FacilityTab <- FacilityTab[unlist(sapply(searchTerms, grep, tolower(FacilityTab$City), USE.NAMES = F)), ]
    
    if(dim(FacilityTab)[1] == 0){
      print_color("No matching city names.\n","red")
      return(FacilityTab)
    }
    
  } ## end if searching for city
  
  
  # Zip code search
  if(!is.null(zipsearch)){  
    #check if numeric
    if(!is.numeric(zipsearch)){
      print_color("Please enter a numeric zip code.\n","red")
      return(FacilityTab)
      } else {
      
        FacilityTab <- FacilityTab %>% filter(ZIPCode == zipsearch)
    
    #If no matches, provide a warning
        if(dim(FacilityTab)[1] == 0){
          print_color("No matching zip code.\n","red")
          return(FacilityTab)
        } # close if no matches
      } # close else
  } ## end if searching for a zip
  
  ## Abbreviate if desired - using col names because if data is read in col numbers are shifted
  if(AbbrevOutput == TRUE){
    FacilityTab <- FacilityTab %>% select(FacilityID,FacilityName, Street, City, State, ZIPCode, FIPS, ModeledNAICS, Latitude, Longitude)
  } 
  
  return(FacilityTab)
} # end of function
