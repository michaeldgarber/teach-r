#Revised August 4, 2022
#Note to self: adapted from previous work
#~Work/CDC/prj05983_R_for_GIS_User_Group/r_teach_4_sf_osmdata_20210709.R


# Demo of sf using  counties and pharmacies

# 0. Set-up-----------#######
# #Install or update packages.

# install.packages(c("osmdata", "tidycensus")) #packages needed for downloading data
# install.packages(c("sf", "tidyverse", "mapview")) #other packages used.

#Load packages
library(tidyverse) #always
library(osmdata) #For pulling data from OpenStreetMap
library(sf) #the set of functions for wrangling spatial data
library(tidycensus) #for pulling census data
library(mapview) #for quick interactive maps
library(viridis) #for its color scales

# Set census key for tidycensus if not already done.-
#census_api_key("YOUR API KEY GOES HERE")

# 1. Intro and overview---------------------
## Outline of the different sf object types--------
#https://r-spatial.github.io/sf/articles/sf1.html

# point
# linestring
# polygon
# multipoint
# multilinestring
# multipolygon
# geometry collection - set of geomtries of any type


# multi versions of all of those.

## Goal-------------
# Overall goal: Illustrate use of sf through some example analyses.

#1. Count the number of pharmacies in Fulton and Dekalb counties, according to OpenStreetMap.
#2. How many people live within 1/2 mile of these pharmacies in these counties?

#Miscellaneous methods used and things to note:
# Dissolve census tracts into counties using group_by() and summarise()
# Illustrate use of the st_area function to measure area.
# Illustrate use of st_union, which smushes together geometries into one.
# Illustrate use of st_intersection, which returns the geometry covered by both objects.
# Illustrate use of st_join, which allows for a spatial left join.
# Illustrate use of st_centroid, which finds the centroid of an sf object.
# Illustrate use of st_buffer() to create a buffer area around a polygon.
# Note that dplyr and sf verbs can be used in the same pipe.

#Some helpful links:
# All the vignettes here:
#https://r-spatial.github.io/sf/articles/sf1.html
#https://r-spatial.github.io/sf/articles/sf2.html
#https://r-spatial.github.io/sf/articles/sf3.html


# 2. Download and prepare census tract data for Georgia---------------
#note I'm writing out packagename::function to keep track, but that syntax is not necessary.
#i.e., you could write get_acs rather than tidycensus::get_acs.

#Run this to save the data locally for speed.
options(tigris_use_cache = TRUE) 

#Load population data for all of Georgia's census tracts
tract_ga =tidycensus::get_acs(
  year=2020,
  #make it wide form (rather than long-form, otherwise default) so variable names are in columns
  # https://cran.r-project.org/web/packages/tidycensus/tidycensus.pdf
  output = "wide",  
  geography = "tract",
  state ="GA",
  geometry = TRUE, #omit geometry for speed
  variables = c(
    pop  = "B01001_001")
) 

library(here)
setwd(here("data-processed"))
save(tract_ga, file = "tract_ga.RData")

tract_ga 
## Use st_transform and st_area---------------
#Make a new dataset so don't have to load data from API multiple times during workflow.
#once done editing, could link together in one pipe to simplify.
tract_ga_wrangle = tract_ga %>% 
  dplyr::rename(
    geo_id= GEOID,
    name_tract_county = NAME,
    pop = popE
  ) %>% 
  #Some resources on coordinate systems:
  #  https://www.nceas.ucsb.edu/sites/default/files/2020-04/OverviewCoordinateReferenceSystems.pdf
  sf::st_transform(4326) %>% #
  dplyr::mutate(
    #extract county 5-digit FIPS code
    #first 2 correspond to state. then the next 3 indicate the county.
    # Note Fulton is 13121
    # Dekalb is 13089
    #stringr is a tidyverse package
    county_fips = stringr::str_sub(geo_id, start=1, 5),
    area_4326 = sf::st_area(geometry), #returns area of type "units". 
    #measured in meters squared because of the coordinate system.
    area_m2 = as.numeric(area_4326) #convert to numeric. strip units
  ) %>% 
  #remove the population margin of error
  #For fun, convert it to a coordinate system that will output in feet.
  #https://www.spatialreference.org/ref/epsg/2239/
  #NAD83 / Georgia West (ftUS)
  sf::st_transform(2240) %>% 
  dplyr::mutate(
    area_2240 = sf::st_area(geometry),
    area_ft2 = as.numeric(area_2240),
    area_mi2 = area_ft2/27878400, #convert to square miles
    #calculate population density
    pop_dens_per_mi2 = pop/area_mi2
  ) %>% 
  dplyr::select(-popM) 

#Things to note:
# st_area and st_transform are functions in the sf package. They can be used together with dplyr.
# Note that we used dplyr functions (e.g., mutate) and sf functions (e.g., st_transform) in the same pipe.
# An sf object is a dataframe with special attributes.

#Look at the data.
tract_ga_wrangle
#-----check on the area measurements-----#

#Confirm the column type is as expected.
class(tract_ga_wrangle$area_4326) #note units
class(tract_ga_wrangle$area_m2) #numeric
class(tract_ga_wrangle$area_2240) #units
class(tract_ga_wrangle$area_ft) #numeric

## Map Georgia census tracts using ggplot--------------------
#Visualize population
#viridis plaettes: 
#https://cran.r-project.org/web/packages/viridis/vignettes/intro-to-viridis.html
tract_ga_wrangle %>% 
  ggplot2::ggplot(aes(fill = pop, color = pop))+ #color the fill and lines by population
  ggplot2::geom_sf()+ #Note ggplot2 has a geom option for mapping sf objects.
  ggplot2::scale_fill_viridis_b()+ #the fill color of the polygons
  ggplot2::scale_colour_viridis_b() #the line (border) color of the polygons


#Visualize population density
tract_ga_wrangle %>% 
  ggplot2::ggplot(aes(fill = pop_dens_per_mi2, color = pop_dens_per_mi2))+ #color the fill and lines by population
  ggplot2::geom_sf()+
  # ggplot2::scale_fill_viridis_b()+
  # ggplot2::scale_colour_viridis_b()
  viridis::scale_fill_viridis() + #easier to see with a continuous scale rather than the breaks
  viridis::scale_color_viridis()

# Different color palette, and add a nicer label, nicer coordinates
tract_ga_wrangle %>% 
  filter(atlanta_metro==1) %>% 
  filter(pop_dens_per_mi2 <25000) %>% 
  ggplot2::ggplot(aes(fill = pop_dens_per_mi2, color = pop_dens_per_mi2))+
  ggplot2::geom_sf()+
  #Write the same name in both, and it won't repeat.
  #\n for line break (carriage return)
  viridis::scale_fill_viridis(
    option="magma",
    name = "Pop. density \n(people per square mile)") + 
  viridis::scale_color_viridis(
    option="magma",
    name = "Pop. density \n(people per square mile)" 
  )+  theme_minimal()
## Summarize by county to create a geometry of counties----------------------
#Like a "dissolve" in ArcGIS
#summarize over county and keep geometry of counties
county_ga  = tract_ga_wrangle %>% 
  dplyr::group_by(county_fips) %>% 
  #any summary operation (e.g., average, standard deviation) will do. sum makes sense here.
  dplyr::summarise(pop= sum(pop, na.rm=TRUE)) %>% 
  dplyr::ungroup() %>% #so we're no longer doing grouped operations
  sf::st_transform(2240) %>% 
  dplyr::mutate(
    #Measure the area of each county
    area_2240 = st_area(geometry),
    area_ft2 = as.numeric(area_2240),
    area_mi2 = area_ft2/27878400, #convert to square miles
    pop_dens_per_mi2 = pop/area_mi2     #calculate population density
  ) 


## Map counties using ggplot ---------------------
#Visualize population
county_ga %>% 
  ggplot2::ggplot(aes(fill = pop))+
  ggplot2::geom_sf(color = "white")+
  viridis::scale_fill_viridis(name = "Population") 

#Visualize population density
county_ga %>% 
  ggplot(aes(fill = pop_dens_per_mi2))+
  geom_sf(color = "white")+
  viridis::scale_fill_viridis(name = "Population density \n(per square mile)") 



# 3. Download and manipulate pharmacies in Fulton and Dekalb County--------------------
# Eventual goal is to gather a dataset of all pharmacies in FUlton County and Dekalb County.
# First download all pharmacies in this bounding box that includes Fulton and Dekalb 
#and then restrict to Fulton and Dekalb County

#Get a picture of Fulton and Dekalb first.
#Fulton is 13121
#Dekalb is #13089
county_ga %>% 
  dplyr::filter(county_fips == "13121" | county_fips == "13089") %>% 
  mapview::mapview()

#Okay, in creating the bounding box, 
#Fulton will have the westernmost, northernmost, and southernmost coordinates.
#Dekalb has the easternmost point.
#We could approximate the coordinates manually by clicking in Google Maps (easier, maybe better) or 
#extract the coordinates with some sf functions.

## Use st_union to generate a geometry file for the smushed together version of Fulton and Dekalb counties------
fulton_dekalb_union = county_ga %>% 
  dplyr::filter(county_fips == "13121" | county_fips == "13089") %>% 
  sf::st_union() %>% #combine the two features into one.
  sf::st_as_sf() %>% #confirm it's an sf object again.
  sf::st_transform(4326) #be explicit about the coordinate system. step not necessary.

fulton_dekalb_union #note bounding box and geometry type
st_geometry(fulton_dekalb_union) #check geometry type specifically
fulton = county_ga %>%  
  dplyr::filter(county_fips == "13121")
dekalb = county_ga %>% 
  dplyr::filter(county_fips == "13089")

#Visualize st_unioned geometry of Fulton and Dekalb
#Point to highlight: several mapview layers can be combined using plus signs
# see https://r-spatial.github.io/mapview/articles/mapview_02-advanced.html
#These instances may be one of the few where not using a pipe has an avantage.
mapview(fulton, col.regions = "yellow")+ 
  mapview(dekalb, col.regions = "blue") + 
  mapview(fulton_dekalb_union, col.regions = "gray50") 

## Use st_bbox to get the bounding box for the Fulton-Dekalb sf object--------
bbox_fulton_dekalb = fulton_dekalb_union %>% 
  sf::st_bbox()      #returns the bounding box of this sf object

#Visualize bounding box
bbox_lon = c(bbox_fulton_dekalb$xmax, 
              bbox_fulton_dekalb$xmin, 
              bbox_fulton_dekalb$xmax, 
              bbox_fulton_dekalb$xmin) %>% 
  as_tibble() %>% 
  rename(lon = value)

bbox_lat = c(bbox_fulton_dekalb$ymax, bbox_fulton_dekalb$ymax,
             bbox_fulton_dekalb$ymin, bbox_fulton_dekalb$ymin) %>% 
  as_tibble() %>% 
  rename(lat = value)

bbox_fulton_dekalb_sf = bbox_lat  %>% 
  bind_cols(bbox_lon) %>% 
  st_as_sf(coords = c("lon", "lat"), crs = 4326) 

mapview(fulton_dekalb_union, col.regions = "gray50") +
  mapview(bbox_fulton_dekalb_sf)


## Use osmdata to download pharmacies in the bounding box------------------
#https://cran.r-project.org/web/packages/osmdata/vignettes/osmdata.html
#Info on opq function: https://www.rdocumentation.org/packages/osmdata/versions/0.1.5/topics/opq
#Note this code takes a while.
#I'm commenting out. I ran August 3, 2022
pharm= osmdata::opq (
  #Note the bbox argument can either be a string of maximal and minimal latitudes for the bounding box, or
  #it can be the name of a place, from which a bounding box will be derived.
  #For example, we could say:
#  bbox = "Georgia, USA" #the bounding box will be a rectangle based on the farthest apart corners.

  #So we know exactly what we're getting, let's use the values from the bounding box computed above.
  #The order it expects is c(xmin, ymin, xmax, ymax). 
  #Note it can be a character vector or a numeric vector.
  #Here, we are using a numeric vector:
   bbox = c(
     bbox_fulton_dekalb$xmin,
     bbox_fulton_dekalb$ymin,
     bbox_fulton_dekalb$xmax,
     bbox_fulton_dekalb$ymax)
  ) %>%
  osmdata::add_osm_feature(
    key = "amenity",
    value = "pharmacy") %>%
  osmdata::osmdata_sf() #return an sf object

library(here)
setwd(here("data-processed"))
save(pharm, file = "pharm.RData")

#The result of this is an object of class osmdata, which is a special type of list 
#comprised of elements containing the various types of geometry (points, polygons, etc.).
#see https://cran.r-project.org/web/packages/osmdata/vignettes/osmdata.html#3_The_osmdata_object
#Because we used the osmdata_sf code, each element in the list is an sf object.
pharm 
class(pharm)
#To access each sf object, we can use the dollar-sign operator.
class(pharm$osm_points)


#----Points------#
pharm_points = pharm$osm_points
#Note, for reference that this could also be done using a pipe, which could be useful
#if we wanted to incorporate this as a part of a workflow with multiple steps.
# Note the . as a stand in for the object because is an operator and not a function.
pharm_points = pharm %>% 
  .$osm_points 

#Visualize all of the points and compare with Fulton and Dekalb
mv_points = mapview(
  pharm_points,
  col.regions = "red",
  color = "red") 

mv_points+mapview(fulton_dekalb_union) #compare with fulton and dekalb

#-----Polygons-------#
pharm_polygons = pharm %>% .$osm_polygons
#north and piedmont and ponce and the beltline, and a few other places
# in southwest atlanta
mv_polygons =   pharm_polygons %>% 
  mapview(
    layer.name = "pharm_polygons",
    color = "blue",
    col.regions = "blue")

#-----MultiPolygons-------#
pharm_multipolygons = pharm %>% 
  .$osm_multipolygons
#This is at North and Piedmont.
mv_multipolygons = mapview(
  pharm_multipolygons,
  color = "purple",
  col.regions = "purple")


#------Visualize all three to see if there is overlap.------## 
mv_points + mv_polygons + mv_multipolygons

#Conclusion: many of the points are simply vertices of buildings which are coded as polygons.
#Where this is the case, let's remove the points and keep the polygons.
#It also seems to be the case that where there is overlap, the polygon contains the more valuable metadata.
#  There is also one coded as multipolygon.  It contains much of the metadata about the pharmacy (osm_id = 11918050), 
#which is at North and Piedmont. So for that one, let's keep the multipolygon and remove the polygon.



## Limit to the pharmacies in Fulton and Dekalb county---------------------
#Use st_intersection
#Could do this later as well but it makes sense to do it earlier so that we're working with a smaller dataset.
#Note this requires sf objects to be in the same coordinate system.
#Check that first.
sf::st_crs(pharm_points)
sf::st_crs(fulton_dekalb_union)
pharm_points_fd = pharm_points %>%  #fd for Fulton and Dekalb.
  #Return the intersection between the points and the unioned Fulton and Dekalb counties
  sf::st_intersection(fulton_dekalb_union) %>% 
  dplyr::mutate(
    type_point = 1, #indicator
    point_row_number = row_number())   # an identifier for subsequent linking

#Again note the use of an sf function (st_intersection) and a dplyr function (mutate) in the same pipe.

# Visualize to check
mv_points_fd = mapview(
  pharm_points_fd,
  col.regions = "orange",
  color = "orange") 

mapview(fulton_dekalb_union, col.regions = "gray50") + mv_points + mv_points_fd

#Can also confirm by noting that there are fewer rows in the intersected version
nrow(pharm_points_fd)
nrow(pharm_points)

#Do the same for the polygons. We  know the one multipolygon is in Fulton County based on our data exploration,
#but run the code anyway to be consistent.
pharm_polygons_fd = pharm_polygons %>% 
  #Return the intersection between the polygons and the unioned Fulton and Dekalb counties
  sf::st_intersection(fulton_dekalb_union) %>% 
  dplyr::mutate(
    type_polygon = 1, #indicator for the spatial join
    polygon_row_number = row_number())   # an identifier for subsequent linking

# pharm_multipolygons_fd = pharm_multipolygons %>%
#   st_intersection(fulton_dekalb_union)  #hmm, that gave an error.

#Googling suggests we need to adjust the precision... 
# https://github.com/r-spatial/sf/issues/1710
#(not sure what that means but it worked)
pharm_multipolygons_fd = pharm_multipolygons %>% 
  #  sf::st_set_precision(1e6) %>% 
  sf::st_make_valid() %>% 
  st_buffer(0) %>% #another trick that sometimes works but is not needed here.
  sf::st_intersection(fulton_dekalb_union) %>% 
  mutate(type_multipolygon=1)

nrow(pharm_multipolygons_fd)
# 4. Combine the pharmacies into a single dataset------------ 
#restricted to those in Fulton and Dekalb
#Conclusion: many of the points are simply vertices of buildings which are coded as polygons.
#Where this is the case, let's remove the points and keep the polygons.
# There is also one coded as multipolygon. It contains much of the metadata about the pharmacy (osm_id = 11918050),
#so let's keep that one and remove the polygons.

# Find points that join with polygons and exclude those points..

## st_buffer---------------
# Before finding intersections, make sure the points are large enough to overlap the polygons 
#(i.e., no false negatives).
#This may not be necessary here but is good insurance.

## Avoid duplicates.-----------------
#Also, because st_intersection is like a join, it will link all variables from each joining datasets.
# To avoid duplicate variable names, remove extraneous variables before joining.

pharm_points_fd_buff_20m = pharm_points_fd %>% 
  st_buffer(20) %>% #create a 20 m buffer around each point.
  rename(osm_id_point = osm_id) %>% #keep this but rename it so it doesn't create a duplicate variable name upon linking
  dplyr::select(osm_id_point, type_point, point_row_number, geometry) 

#Confirm same number of rows (i.e., no new rows added or subtracted due to buffer)
nrow(pharm_points_fd_buff_20m)
nrow(pharm_points_fd)
names(pharm_points_fd_buff_20m)
mapview(pharm_points_fd_buff_20m, col.regions = "orange") + mapview(pharm_points_fd)

## use st_join to perform the spatial join.--------------------
#A few ways this could be done:
# st_intersection (returns a geometry)
# st_intersects (does not return geometry)
# st_join - https://r-spatial.github.io/sf/reference/st_join.html

# st_join is the best for this specific problem, I think, where we essentially want a left join.
pharm_points_polygons_join = pharm_points_fd_buff_20m %>% 
  sf::st_join(
    pharm_polygons_fd, 
    left=TRUE, #yes, a left join. This is the default but good to be explicit.
    largest=TRUE) %>%  #default is false. this is useful to avoid duplicate values, but it does take some more time.
  
  #create an indicator variable to visualize
  dplyr::mutate(
    point_overlaps_polygon = case_when(
      type_polygon==1 ~ 1,
      TRUE ~ 0
    ))

names(pharm_points_polygons_join)
#confirm it's a left join without any new rows added.
nrow(pharm_points_polygons_join)
nrow(pharm_points_fd_buff_20m)
table(pharm_points_polygons_join$point_overlaps_polygon)
mapview(pharm_points_polygons_join, zcol = "point_overlaps_polygon")

## Remove points that joined with polygons from the point dataset.--------------
pharm_points_fd_nodupes = pharm_points_polygons_join %>% 
  filter(point_overlaps_polygon==0)

nrow(pharm_points_fd_nodupes)

## All of the points in one pipe---------------
#Note: we could do all of the above  in one step.
pharm_points_fd_nodupes = pharm_points_fd %>% 
  st_buffer(20) %>% #create a 20 m buffer around each point.
  rename(osm_id_point = osm_id) %>% #keep this but rename it so it doesn't create a duplicate variable name upon linking
  dplyr::select(osm_id_point, type_point, point_row_number, geometry) %>% 
  sf::st_join(
    pharm_polygons_fd, 
    left=TRUE, #yes, a left join. This is the default but good to be explicit.
    largest=TRUE) %>%  #default is false. this is useful to avoid duplicate values, but it does take some more time.
  dplyr::mutate(
    point_overlaps_polygon = case_when(
      type_polygon==1 ~ 1,
      TRUE ~ 0
    )) %>% 
  filter(point_overlaps_polygon==0)

names(pharm_points_fd_nodupes)

## Filter to all polygons that are not covered by the single multipolygon---------
#could do this manually but try programatically.
pharm_polygons_fd_no_multipolygon = pharm_polygons_fd %>% 
  rename(osm_id_polygon = osm_id) %>% 
  dplyr::select(osm_id_polygon, type_polygon, polygon_row_number, geometry) %>% 
  #Here, we can be more concise with our st_join function because we know there is just the one
  #multipolygon
  st_join(pharm_multipolygons_fd) %>% 
  filter(is.na(type_multipolygon)==TRUE)


nrow(pharm_polygons_fd)
nrow(pharm_polygons_fd_no_multipolygon)

mapview(pharm_polygons_fd, col.regions = "red") + 
  mapview(pharm_polygons_fd_no_multipolygon, col.regions = "black")+
  mapview(pharm_multipolygons_fd, col.regions = "orange")


## Combine all pharmacies together in one datset.----------------

### Use st_centroid to convert all to points--------
#For simplicity, first find the centroid of each of buffered points, the polygons, 
#and the multipolygon

#Also in this step, link the information back in using left_join
#that we removed from the points and polygons. Before doing so,
#remove geometry from old data.
pharm_points_fd_nogeo = pharm_points_fd %>% 
  st_set_geometry(NULL) %>% 
  as_tibble()
pharm_polygons_fd_nogeo = pharm_polygons_fd %>% 
  st_set_geometry(NULL)%>% 
  as_tibble()


pharm_points_fd_nodupes_centroid = pharm_points_fd_nodupes %>% 
  st_centroid() %>%
  dplyr::select(osm_id_point, geometry) %>% 
  rename(osm_id = osm_id_point) %>% 
  left_join(pharm_points_fd_nogeo, by = "osm_id") %>% 
  mutate(type_original = "point")

pharm_polygons_fd_no_multipolygon_centroid = pharm_polygons_fd_no_multipolygon %>% 
  st_centroid()  %>% 
  dplyr::select(osm_id_polygon, geometry) %>% 
  rename(osm_id = osm_id_polygon) %>% 
  left_join(pharm_polygons_fd_nogeo, by = "osm_id") %>% 
  mutate(type_original = "polygon")

# View(pharm_points_fd_nodupes_centroid)
# View(pharm_polygons_fd_no_multipolygon_centroid)

pharm_multipolygons_fd_centroid = pharm_multipolygons_fd %>% 
  st_centroid() %>% 
  mutate(type_original = "multipolygon")

#visualize
mapview(pharm_points_fd_nodupes_centroid, col.regions = "blue") +
  mapview(pharm_polygons_fd_no_multipolygon_centroid, col.regions = "red")+
  mapview(pharm_multipolygons_fd_centroid, col.regions = "orange")

### 4.7.2. Use bind_rows to stack them together into one dataset--------
pharm_fd_combined = pharm_points_fd_nodupes_centroid %>% 
  dplyr::bind_rows(
    pharm_polygons_fd_no_multipolygon_centroid,
    pharm_multipolygons_fd_centroid
  )

# how many pharmacies?
nrow(pharm_fd_combined) #66
mapview(pharm_fd_combined, zcol = "type_original") +  
  mapview(fulton_dekalb_union, col.regions = "gray50") 

# 5. Estimate population within a .5 mile of a pharmacy------------------------
## Create half-mile buffer around each pharmacy------------
pharm_fd_combined_halfmile = pharm_fd_combined %>% 
  sf::st_transform(2240) %>% 
  sf::st_buffer(5280/2)

mapview(pharm_fd_combined_halfmile)  

## Combine into a single sf object----------------------
#for simplicity, combine into one single object using st_union
pharm_fd_combined_halfmile_union = pharm_fd_combined_halfmile %>% 
  st_union() %>% 
  st_as_sf() 

nrow(pharm_fd_combined_halfmile_union) #1 observation

## Assess proportion each census tract is overlapped by this object.------------

### Find part of census tracts that overlaps the pharmacy buffer areas---------
tract_fulton_dekalb = tract_ga_wrangle %>% 
  #Filter to census tracts in Fulton and Dekalb County
  dplyr::filter(county_fips == "13121" | county_fips == "13089") 

tract_fulton_dekalb_int_pharm_buff = tract_fulton_dekalb %>% 
  sf::st_transform(2240) %>% #coordinate system in feet.
  st_intersection(pharm_fd_combined_halfmile_union) %>% 
  #measure the overlapping area for each census tract.
  mutate(
    area_overlap_ft2 = as.numeric(st_area(geometry)),
    #proportion of this overlapping slice of the whole census tract.
    prop_area_overlap_ft2 = area_overlap_ft2/area_ft2
  ) %>% 
  #just grab those two variables and then link with the original to compute
  #totals
  dplyr::select(geo_id, contains("overlap"), geometry) %>% 
  mutate(number_for_vis = row_number())

#are there duplicate census tracts?
n_distinct(tract_fulton_dekalb_int_pharm_buff$geo_id)
nrow(tract_fulton_dekalb_int_pharm_buff)

#Map the intersected pieces of census tracts
mapview(tract_fulton_dekalb_int_pharm_buff, zcol = "number_for_vis")

### Link that overlapping part with all of the census tracts----------
#Link with the unintersected version.
#remove geometry from these intersected pieces befoe linking.
tract_fulton_dekalb_int_pharm_buff_nogeo = tract_fulton_dekalb_int_pharm_buff %>% 
  st_set_geometry(NULL) %>% 
  as_tibble()

names(tract_fulton_dekalb)
tract_fulton_dekalb_linked = tract_fulton_dekalb %>% 
  left_join(tract_fulton_dekalb_int_pharm_buff_nogeo, by = "geo_id") %>% 
  #if the linked values are missing, make them zero
  mutate(
    prop_area_overlap_ft2_linked = case_when(
      is.na(prop_area_overlap_ft2)==TRUE ~ 0,
      TRUE ~ prop_area_overlap_ft2 
    ),
    #Assume population is uniformly distributed, and 
    #multiply this areal proportion by the population in the census tract.
    #Probably a reasonable assumption for census tracts. Maybe not for a county or state.
    pop_overlap = prop_area_overlap_ft2_linked*pop
  )

### 5.3.3. Sum over all values and calculate proportion of the population within a half-mile---
#Total overlapping population
tract_fulton_dekalb_linked %>% 
  st_set_geometry(NULL) %>% 
  mutate(dummy=1) %>% 
  group_by(dummy) %>% 
  summarise(
    pop = sum(pop, na.rm = TRUE),
    pop_overlap = sum(pop_overlap, na.rm=TRUE)
  ) %>% 
  ungroup() %>% 
  mutate(
    prop_pop_overlap = pop_overlap/pop
  )

#10% of the population in Fulton and Dekalb County lives within a half mile of a pharmacy.

# Future analysis:---------
#do this stratified by SES or social vulnerability category. 
#Looks like lots of pharmacies in higher-SES areas



