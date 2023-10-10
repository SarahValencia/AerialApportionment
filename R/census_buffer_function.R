#' @title CensusBuffer
#'
#' @description Function to overlay point locations on ACS data at the block group level, draw concentric polygons (buffers) around point locations at user specified radial distance, and apportion block group populations to each buffer based on the percentage of spatial overlap.
#'
#' @details This function is specifically designed to apportion ACS population data at the block group level to nearby TRI facilities in order to calculate the demography within a certain distance of those facilties.  It relies on spatial functions in the sf package to take the outputs of the PullCensusData function (ACS data at the block group level, projected into a metric coordinate system) and the RSEISearch function (TRI Facility location data along with associated attributes, including emmissions) to determine the community attributes within a user specified distance of those facilities. Users can enter a vector of buffers (distance in meters) to do multiple calculations at once. For each radial distance provided, the data are intersected to determine the the proportion of each block groups that intersects the circular buffer. The population variables are then multiplied by that proportion to determine the number of people that are within the buffer, and this is summed for all the block groups that intersect each buffer to give a total population the buffer. This assumes that populations are distributed uniformly across block groups, which is incorrect but in the absence of finer scale distribution data provides a best estimate for the number of people that live within a given distance of each TRI facility.
#'
#' This function is designed to be used with the output of the PullCensusData function, which pulls ACS data at desired geographical units, and the RSEISearch function, which produces a spatial dataframe of TRI reporting facilities as well as a dataframe of emissions data and RSEI scores for each chemical-facility combination.
#'
#' @param buffer_vec A radial distance for a concentric buffer to be drawn around a point location in meters, or a vector of radial distances.
#'
#' @param census_dat_proj A spatial dataframe (sf object) of ACS data at the block group level, projected into a metric coordinate system. This should be the output of the PullCensusData function with a geography of "block group".
#'
#' @param FacilityLocation_m A spatial dataframe (sf object) of TRI facilities. This should be the output of the RSEISearch function, with returnLocation set to TRUE.
#'
#' @param runTract Logical. If true, adds tract level summaries for demographic variables for comparison. The user must supply ACS data summarized at the tract level. Defaults to False.
#'
#' @param tract_dat_proj  A spatial dataframe (sf object) of ACS data at the tract level, projected into a metric coordinate system. This should be the output of the PullCensusData function with a geography of "tract".
#'
#' @param runCounty  Logical. If true, adds county level summaries for demographic variables for comparison. The user must supply ACS data summarized at the county level. Defaults to False.
#'
#' @param county_dat_proj = A spatial dataframe (sf object) of ACS data at the countylevel, projected into a metric coordinate system. This should be the output of the PullCensusData function with a geography of "county".
#'
#' @param variableNames An optional vector of variable names, most likely the vector of named variable codes provided to the function PullCensusData.
#'
#' @param EmissionData An optional dataframe of emissions/RSEI score information if the user would like this merged into the demographic tabulation results. This should be the output of the RSEISearch function.
#'
#' @param saveAs A character string specifying the name you would like the output file saved under. Defaults to "DemographicsNearTRIFacilities".
#'
#' @returns A wide format dataframe. If EmissionsData are included, these data are merged into the final output so that each row is a chemical-facility combination, and the columns include the summed emissions and associated scores as well as the demography in each buffer. If EmissionsData is not included, the output of this function is a wide format dataframe with a row for each facility and in the columns the tabulated ACS variables within each buffer distance of that facility location. If runTract or runCounty are true, it also includes the county or tract totals for each variable for comparison. Counts of the number of TRI facilities within the buffer/tract/county are also included.
#'
#'
#'
#' @export

CensusBuffer <- function(buffer_vec, census_dat_proj, FacilityLocation_m, runTract = FALSE, tract_dat_proj = NULL,runCounty = FALSE, county_dat_proj = NULL, variableNames = NULL, EmissionData = NULL, saveAs = "DemographicsNearTRIFacilities"){

  # calculate the area of each block group in m^2 - only needs to happen once
  census_dat_proj$BG_area <- st_area(census_dat_proj)
  TRI_agg <- distinct(FacilityLocation_m) # locations of each facility
  if(is.null(EmissionData)){
    df.list <- list()
    count <- 0
  } else{
    df.list <- list(EmissionData)
    count<-1
  }


  for (i in 1:length(buffer_vec)){  #loop through each buffer distance provided
    count <- count+1
    # create buffer around each facility
    tri_buffer <- st_buffer(TRI_agg, buffer_vec[i])

    # calculate the number of TRI facilities in each buffer by intersecting the total TRI dataset and the buffer.
    tri_buffer$Cluster <- lengths(st_intersects(tri_buffer, TRI_agg))   # out <- st_intersection(points, poly)

    # intersect - Returns a geometry representing the point-set intersection of census       data and buffer around TRI facilities
    print_color(paste("Determining amount of block group that falls within", buffer_vec[i], "m buffer","\n", sep=" "),"green")


    #BG_clip <- st_intersection(census_dat_proj, tri_buffer, warn=F) #warning because         function assumes attribute data associated with geographic pts is constant, but        TRI data only has geometry, no other attributes 9434 bg in first buffer

    BG_clip <- st_intersection(tri_buffer, census_dat_proj, left = TRUE)

    # area of each clip and store as an attribute
    BG_clip$area_clip <- st_area(BG_clip)

    # area proportion in each
    BG_clip$area_prop <- BG_clip$area_clip / BG_clip$BG_area

    # Figure out which columns are Estimates (to keep), and which have a median dollar value (not multiplied by area)
    columnnames <- paste(names(my_vars),"E",sep="")
    dropcols <- paste(names(my_vars),"M",sep="")

    # Drop MOE cols
    BG_clip_edit <- BG_clip %>% select(-any_of(dropcols))

    # determine number of columns to multiply by
    txt<-tibble(line=1:length(columnnames),names=columnnames)
    tidy_txt<- tidytext::unnest_tokens(tbl=txt, output = word, input=names,token=stringr::str_split, pattern = "_")
    # Find the lines with the search terms
    Indextbl <- tidy_txt %>% filter(word %in% "median")
    DollarCols <- Indextbl$line + 2 #First 2 are GEOID and Names

    ## save names of diff kinds of variables
    SumCols <- columnnames[-Indextbl$line]
    MeanCols <- columnnames[Indextbl$line]

    ## Use across command to multiply proportion area by population
    BG_clip_df <- BG_clip_edit %>%
      mutate(across(any_of(SumCols), function(x) x*area_prop))

    # now sum by facility ID - change to dataframe to use pipelines
    print_color("Summing totals for each facility\n", "green")

    BG_Sums <-
      BG_clip_df %>%
      group_by(FacilityID) %>%
      summarise_at(SumCols,sum,na.rm=T) #find those associated with each TRI facility's buffer and add up

    BG_Means <-  BG_clip_df %>%
      group_by(FacilityID) %>%   #find those associated with each TRI facility's buffer and take the mean
      summarise_at(MeanCols,mean,na.rm=T)

    # join to combine means and sums
    BG.vars <- full_join(data.frame(BG_Sums),data.frame(BG_Means),by="FacilityID") %>% select(-geometry.x)

    ## ideas to add - flag those dropped in join, add vector of arguments to determine sumarizing functions, select units for buffers for appropriate labeling. Consider dropping geom from BG_means and BG_sums

    # add suffix to label buffer distance
    colnames(BG.vars)[2:dim(BG.vars)[2]] <- paste(colnames(BG.vars)[2:dim(BG.vars)[2]], "_", buffer_vec[i]/1000, "km", sep = "")

    # Give it a name and add it to list
    assign(paste("BG.vars",buffer_vec[i],sep="_"),BG.vars)
    df.list[[count]] <- as.data.frame(BG.vars)


  } # loop over buffers



  ## Run run tract analysis if runTract == T
  if(runTract == TRUE){
    count <- count+1

    # calculate the number of TRI facilities in each tract
    TRIperTract <- lengths(st_intersects(tract_dat_proj,TRI_agg))
    TractCounts <- data.frame(tract_dat_proj$GEOID, tract_dat_proj$NAME, TRIperTract) #83000 tracts
    colnames(TractCounts)=c("GEOID","Tract","Counts")

    # Save Tract counts
    saveRDS(TractCounts, file = "TRI_Facility_per_Tract.rds")

    # determine which tracts have a facility
    print_color("Determining which tracts have TRI facilities\n","green")
    #Tract_clip <- st_intersection(tract_dat_proj, TRI_agg, warn=F) #warning because         function assumes attribute data associated with geographic pts is constant, but        TRI data only has geometry, no other attributes
    Tract_clip <- st_join(TRI_agg, tract_dat_proj, left = TRUE) # join uses intersecton command but keeps those that do not intersect as an NA.

    # Drop MOE cols
    Tract_clip_edit <- Tract_clip %>% select(-any_of(dropcols))

    # merge in counts
    Tract.vars <- left_join( data.frame(Tract_clip_edit), TractCounts, by =c ("GEOID"))

    # add suffix to label tract variables
    colnames(Tract.vars)[2:dim(Tract.vars)[2]] <- paste(colnames(Tract.vars)[2:dim(Tract.vars)[2]], "tract", sep = "_")

    #merge to tri_agg data
    df.list[[count]] <- Tract.vars %>% select(-colnames(Tract.vars)[2:10])

  } # if runTract

  ## Run run county analysis if runCounty == T
  if(runCounty == TRUE){
    count <- count+1

    # calculate the number of TRI facilities in each county
    TRIperCounty <- lengths(st_intersects(county_dat_proj,TRI_agg))
    CountyCounts <- data.frame(county_dat_proj$GEOID, county_dat_proj$NAME, TRIperCounty)
    colnames(CountyCounts)=c("GEOID","County","Counts")

    # Save County counts
    saveRDS(CountyCounts, file = "TRI_Facility_per_County.rds")

    # determine which counties have
    print_color("Determining which counties have TRI facilities\n","green")

    County_clip <- st_join(TRI_agg, county_dat_proj, left = TRUE) # join uses intersecton command but keeps those that do not intersect as an NA.

    # Drop MOE cols
    County_clip_edit <- County_clip %>% select(-any_of(dropcols))

    # merge in counts
    County.vars <- left_join( data.frame(County_clip_edit), CountyCounts, by =c ("GEOID")) %>% select(-c(2,3,4,5,6,7))

    # add suffix to label tract variables
    colnames(County.vars)[2:dim(County.vars)[2]] <- paste(colnames(County.vars)[2:dim(County.vars)[2]], "county", sep = "_")

    #Add to list
    df.list[[count]] <- County.vars

  } # if runCounty


  Final <- df.list %>% reduce(left_join,by="FacilityID")
  CurrDate <- Sys.Date()
  saveRDS(Final, file = paste(saveAs,"_",CurrDate,".rds", sep=""))
  write.csv(Final,paste(saveAs,"_",CurrDate,".csv", sep=""),row.names=FALSE)

  return(Final)

} # end of function
