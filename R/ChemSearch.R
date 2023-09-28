#' @title ChemSearch
#' 
#' @description This function allows users to search the RSEI Chemicals table 
#' for chemical names, CAS Numbers, or RSEI Chemical numbers in order to obtain
#'  information about the chemical.
#' 
#' @details The RSEI Chemicals tables contains information on over 800 different 
#' chemicals. Users can search the "Chems" table using single words rather than 
#' entire chemical names, which can be long. The rows containing information 
#' matching each search term provided are returned in order to let users easily 
#' located specific RSEI database chemical numbers (a unique identifier and crosswalk 
#' to other tables) for use in other operations.
#' 
#' The Chemical table provided to the function is filtered sequentially in the order 
#' the search terms are listed in the arguments section below (CASnumbersearch, 
#' chemnamesearch, and then rsei_chemnumber). If a search term has no matches and 
#' yields an empty dataframe, searches with subsequent arguments are not completed. 
#' Instead, a warning is provided and an empty dataframe is returned. 
#' 
#' @param ChemTab The Chemicals table downloaded from the publicly available RSEI 
#' database and stored as a dataframe.
#' 
#' @param CASnumbersearch A numerical string or vector of numerical strings representing 
#' a CAS Number. A CAS Registry Number is a numeric identifier that can contain up 
#' to 10 digits, separated by hyphens. However hyphens, dashes, and spaces are not 
#' allowed, so please collapse the standard CAS number format to a digital string.
#' 
#' @param chemnamesearch Character string or vector of character strings specifying 
#' a name or partial name of a chemical. Capitalization does not matter.
#' 
#' @param rsei_chemnumber A number representing a unique idenifyer in the RSEI 
#' Chemicals table. Chemical Numbers range from 1 to almost 900 at this time.
#' 
#' @param AbbrevOutput - Logical, defaults to TRUE. If abbreviated the function returns 
#' the CASNumber, CAS Standard, ChemicalNumber, Chemical Name, FirstReportingYear, 
#' ToxicityClassOral, ToxicityClassInhale, and  ToxicityCategory. Otherwise all columns 
#' in the RSEI Chemicals table are returned.
#' 
#' @returns If there are no matching chemical names, the statement "No matching chemical 
#' names" is printed in the console. If there are chemicals that contain words that match 
#' the search terms provided the information for those chemicals in printed in the console
#' 
#' @examples
#' # Read in the Chemical table and save as "Chems" to pass to function.
#' # Chems <- read.csv("chemical_data_rsei_v2311.csv")
#' 
#' # Search for three CAS numbers, one of which is incorrect.
#' # ChemSearch(ChemTab = Chems, CASnumbersearch = c(79107,116063,0009))
#' 
#' # Search for all chemicals that contain the word "Chloride", and return an unabbreviated 
#' # table.
#' # ChemSearch(ChemTab = Chems, chemnamesearch = "Chloride", AbbrevOutput = F)
#' 
#' # Search for all chemicals that contain either word
#' # ChemSearch(ChemTab = Chems, chemnamesearch = c("benzoyl", "benzyl"))

#' @export 
ChemSearch <- function(ChemTab, CASnumbersearch = NULL, chemnamesearch = NULL, rsei_chemnumber  = NULL, AbbrevOutput = TRUE){
  
  if(!is.null(CASnumbersearch)){  
    
    #alert user to incorrect numbers
    if(any(!CASnumbersearch %in% ChemTab$CASNumber)) {
      print_color("Warning: the following CAS Numbers are not found in the RSEI Chem Table.\n","red")
      pos <-which(!CASnumbersearch %in% ChemTab$CASNumber)
      print(CASnumbersearch[pos])
    }  #close warning
    
    #filter Chem table to include desired CASnumbers
    ChemTab <- ChemTab %>% filter(CASNumber %in% CASnumbersearch) # keep rows that match these IDs
  }
  
  ## check that something is returned
  if(dim(ChemTab)[1] == 0){
    print_color("No matching CAS Numbers.\n","red")
    return(ChemTab)
  }
  
  # Search for chemical name. 
  if(!is.null(chemnamesearch)){  
    
    #decapitilize search terms
    searchTerms <- tolower(chemnamesearch)
    
    # This code decapitalizes the names in the table to elimiminate needing to match capitilizations.
    ChemTab <- ChemTab[unlist(sapply(searchTerms, grep, tolower(ChemTab$Chemical), USE.NAMES = F)), ]
    
    if(dim(ChemTab)[1] == 0){
      print_color("No matching chemical names.\n","red")
      return(ChemTab)
    }
  } #close search for chem name

  # Check if chemical numbers are in table
  if(!is.null(rsei_chemnumber)){
    missing <- rsei_chemnumber[!(rsei_chemnumber %in% ChemTab$ChemicalNumber)]
    if(length(missing) >0) {
      print_color(paste("Warning: the following Chemical Number is not found in the RSEI Chemicals table:", missing, "\n", sep=" "),"red")
      }
  
    # filter by chemical numbers if provided.
    ChemTab <- ChemTab %>% filter(ChemicalNumber %in% rsei_chemnumber)
  
    if(dim(ChemTab)[1] == 0){
      print_color("No matching chemical names.\n","red")
      return(ChemTab)
      }
    }# close search for chemical number
  
    ## Abbreviate if desired  - using names because if you read in data col numbers are shifted
    if(AbbrevOutput == TRUE){
      ChemTab <- ChemTab %>% select(CASNumber,CASStandard, ChemicalNumber, Chemical, FirstReportingYear,ToxicityClassOral, ToxicityClassInhale, ToxicityCategory)
    } 
  
  # return filtered table
  return(ChemTab)
} # close function
  