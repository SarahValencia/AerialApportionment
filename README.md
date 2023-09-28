# AerialApportionment
Aerial Apportionment R Package - Functions for assigning populations to polluting facilities

This package was developed to help users assess the characteristics of communities that live near facilities that emit toxic chemicals, and to determine the risks they may face due to this proximity. 

Accessing Publicly Available Datasets
The data necessary to perform these operations comes from publicly available datasets. Emissions information is sourced from the EPAs Risk Screening Environmental Indicators (RSEI) database, which includes data from the federally managed Toxic Release Inventory (TRI) program on over 25,000 facilities reporting hazardous emissions since the 1980s, as well as risk scores that account for the size of the chemical release, the fate and transport of the chemical through the environment, the size and location of the exposed population, and the chemical's toxicity. Population information comes from the American Community Survey (ACS) at the Census Block Group-level. This package contains a suite of functions to access and organize these data, and to perform the spatial manipulations necessary to determine which demographics may be impacted by TRI facilities.

What is Aerial Apportionment?
Impacts are calculated using a method called aerial apportionment, in which circular polygons, or buffers, are drawn around each facility and overlaid on census data at the block group level. The population is weighted by the proportion of the area of the block group that is captured by the circle. The weighted populations of these units are then used to determine the aggregate demographic characteristics of perfectly circular neighborhoods within each buffer. 

Getting Started
Please see the vignette showing how these functions can be used to assess facilities reporting TRI emissions on Long Island, and the community characteristics around those facilities.


