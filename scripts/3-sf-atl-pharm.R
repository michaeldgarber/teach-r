#Title: R for spatial data wrangling: a demo of sf through an 
#example analysis assessing population living near pharmacies in Atlanta
#Revised August 4, 2022


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
# tract_ga =tidycensus::get_acs(
#   year=2020,
#   #make it wide form (rather than long-form, otherwise default) so variable names are in columns
#   # https://cran.r-project.org/web/packages/tidycensus/tidycensus.pdf
#   output = "wide",  
#   geography = "tract",
#   state ="GA",
#   geometry = TRUE, #omit geometry for speed
#   variables = c(
#     pop  = "B01001_001")
# ) 

library(here)
setwd(here("data-processed"))
#save(tract_ga, file = "tract_ga.RData")
load("tract_ga.RData")
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
    area_m2 = as.numeric(area_4326) ,#convert to numeric. strip units
    #indicator for major Atlanta Metro Counties
    atlanta_metro = case_when(
      #character format even though they're numbers, so quote
      county_fips %in% c(
        "13121",#fulton 
        "13089",#dekalb
        "13135",#gwinnett
        "13067", #cobb
        "13063", #clayton
        "13097"  #douglas
      ) ~1,
      TRUE ~0)  
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
# Key packages and functions used:
#   
# osmdata to download pharmacies from OpenStreetMap
# 
# mapview
# 
# sf functions used
# st_union(): unify several features into one.
# st_as_sf(): convert an object that is not of class sf to sf.
# st_buffer(): create a buffer around the features in the sf object.
# st_bbox(): create a bounding box (four vertices representing the min and max longitude and latitude of the sf object)
# st_intersection(): return the overlapping geometry of two features.
# st_transform(): change the coordinate reference system of an sf object
# st_crs(): return the coordinate reference system of an object.
# st_join(): join two simple features based on whether their geometries overlap (spatial join).

#the usual dplyr verbs and some new ones:
# bind_cols() binds two dataframes or sf objects column-wise (adding width to data). 
  #Rows are matched by position. The binding datasets must have the same number of rows.
  #It differs from joins like left_join in that you don't need to match on a key.

#bind_rows() binds two dataframes or sf objects row-wise (adding length to the data)
#Columns are matched by name. Any columns that appear in one dataset but not the other are filled with NA


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
# pharm= osmdata::opq (
#   #Note the bbox argument can either be a string of maximal and minimal latitudes for the bounding box, or
#   #it can be the name of a place, from which a bounding box will be derived.
#   #For example, we could say:
# #  bbox = "Georgia, USA" #the bounding box will be a rectangle based on the farthest apart corners.
# 
#   #So we know exactly what we're getting, let's use the values from the bounding box computed above.
#   #The order it expects is c(xmin, ymin, xmax, ymax). 
#   #Note it can be a character vector or a numeric vector.
#   #Here, we are using a numeric vector:
#    bbox = c(
#      bbox_fulton_dekalb$xmin,
#      bbox_fulton_dekalb$ymin,
#      bbox_fulton_dekalb$xmax,
#      bbox_fulton_dekalb$ymax)
#   ) %>%
#   osmdata::add_osm_feature(
#     key = "amenity",
#     value = "pharmacy") %>%
#   osmdata::osmdata_sf() #return an sf object

library(here)
# setwd(here("data-processed"))
# save(pharm, file = "pharm.RData")
load("pharm.RData")

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

#-----Polygons-------#
pharm_polygons = pharm %>% .$osm_polygons
#north and piedmont and ponce and the beltline, and a few other places
# in southwest atlanta

#-----MultiPolygons-------#
pharm_multipolygons = pharm %>% 
  .$osm_multipolygons


### Visualize all three to see if there is overlap.--------------
mv_points =   pharm_points %>% 
  mapview(
    layer.name = "points",
    col.regions = "red",
    color = "red") 

mv_polygons =   pharm_polygons %>% 
  mapview(
    layer.name = "polygons",
    color = "blue",
    col.regions = "blue")

#This is at North and Piedmont.
mv_multipolygons = pharm_multipolygons %>% 
  mapview(
    layer.name = "multipolygons",
    color = "purple",
    col.regions = "purple")

#Compare with the bounding box and the Fulton-Dekalb county boundaries
mv_fulton_dekalb_union = fulton_dekalb_union %>% 
  mapview(
    layer.name = "fulton_dekalb_union",
    col.regions = "gray50")

mv_bbox = bbox_fulton_dekalb_sf %>% 
  mapview(
    layer.name = "bounding box",
    col.regions = "orange",
    color = "orange")

#Visualize all of the layerss
mv_points + mv_polygons+mv_multipolygons+
  mv_fulton_dekalb_union+
  mv_bbox

#Conclusion: many of the points are simply vertices of buildings which are coded as polygons.
#Where this is the case, let's remove the points and keep the polygons.
#It also seems to be the case that where there is overlap, 
#the polygon contains the more valuable metadata.
#  There is also one coded as multipolygon. 
#It contains much of the metadata about the pharmacy (osm_id = 11918050), 
#which is at North and Piedmont. So for that one, let's keep the multipolygon and remove the polygon.


## Restrict to the pharmacies in Fulton and Dekalb county---------------------
#Use st_intersection
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

#Do the same for the polygons. 
pharm_polygons_fd = pharm_polygons %>% 
  #Return the intersection between the polygons and the unioned Fulton and Dekalb counties
  sf::st_intersection(fulton_dekalb_union) %>% 
  dplyr::mutate(
    type_polygon = 1, #indicator for the spatial join
    polygon_row_number = row_number())   # an identifier for subsequent linking

pharm_polygons_fd %>% mapview()
#We  know the one multipolygon is in Fulton County based on our data exploration,
#but run the code anyway to be consistent.

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
# There is also one coded as multipolygon.
#It contains much of the metadata about the pharmacy (osm_id = 11918050),
#so let's keep that one and remove the polygons.

# Find points that join with polygons and exclude those points..

## st_buffer points---------------
# Before finding intersections, make sure the points are large enough to overlap the polygons 
#(i.e., no false negatives).
#This may not be necessary here but is good insurance.

# In addition, in this step, 
# I'm removing extraneous variable names using dplyr::select(). 
# st_intersection() is like a join and will link all variables from each of the joining datasets. 
# If the same variable name exists in both, it may cause issues.

pharm_points_fd_buff_20m = pharm_points_fd %>% 
  st_buffer(20) %>% #create a 20 m buffer around each point.
  #keep this but rename it so it doesn't create a duplicate variable name upon linking
  rename(osm_id_point = osm_id) %>% 
  dplyr::select(osm_id_point, type_point, point_row_number, geometry) 

#Confirm same number of rows (i.e., no new rows added or subtracted due to buffer)
nrow(pharm_points_fd_buff_20m)
nrow(pharm_points_fd)
names(pharm_points_fd_buff_20m)
mapview(pharm_points_fd_buff_20m, col.regions = "orange") + mapview(pharm_points_fd)

## use st_join to perform the spatial join.--------------------
#Some functions that could help with this problem:
  
#st_intersection() returns only the intersecting geometry, so, 
#while we could use it, it will take a couple more steps, 
#because we'd have to re-link the information from the overlap back with the main points dataset. 
#That is st_intersection() acts more like an inner join.

#st_intersects() returns a matrix of true/false values indicating whether the two features intersect at 
#that point in the dataset. This is valuable information that could theoretically be used here, 
#but I frankly don't use st_intersects() often because I find the matrix output difficult to work with. 
#It may be the fastest from a computational standpoint, though, because it doesn't return a geometry.

#Let's use st_join() for this specific problem. We want a left join, keeping all of the points 
#(and their geometry) and joining the information from the polygons which overlap those points. 
#Some other important notes about st_join():

#Its default behavior is a left join, which means if the points are x and the polygons are y, 
#all of the values from x will appear in the joined version, but not necessarily all of y.
#In addition, it's possible that multiple points overlap the same polygon. 
#For this exercise, we only want to know if a point was overlapped by any polygon. 
#We thus use the argument, largest=TRUE, to indicate that only the polygon with the 
#largest overlap will join. Note that this is not the default behavior. 
#The default behavior would be to include every combination of points and polygons, 
#which could repeat observations for the same point.

pharm_points_polygons_join = pharm_points_fd_buff_20m %>% 
  sf::st_join(
    pharm_polygons_fd, 
    left=TRUE, #yes, a left join. This is the default but good to be explicit.
    #default is false. this is useful to avoid duplicate values, but it does take some more time.
    largest=TRUE) %>%    
  #create an indicator variable to visualize
  dplyr::mutate(
    point_overlaps_polygon = case_when(
      type_polygon==1 ~ 1,
      TRUE ~ 0
    ))

#Save because for some reason this isn't working in RMarkdown
library(here)
setwd(here("data-processed"))
save(pharm_points_polygons_join, file = "pharm_points_polygons_join.RData")
save(pharm_points_fd_buff_20m, file = "pharm_points_fd_buff_20m.RData")
save(pharm_polygons_fd, file = "pharm_polygons_fd.RData")
nrow() #joined version, points
nrow(pharm_points_fd_buff_20m) #original version of points
nrow() #polygons

names(pharm_points_polygons_join)
#confirm it's a left join without any new rows added.
nrow(pharm_points_polygons_join)
nrow(pharm_points_fd_buff_20m)

#A base R way to check the overlap status
table(pharm_points_polygons_join$point_overlaps_polygon)

#the tidyverse way:
pharm_points_polygons_join %>% 
  st_set_geometry(NULL) %>% #for speed, convert to tibble
  as_tibble() %>% 
  group_by(point_overlaps_polygon) %>% 
  summarise(n=n())

pharm_points_polygons_join %>% 
  mapview(
    layer.name = "point_overlaps_polygon",
    zcol = "point_overlaps_polygon",
    col.regions = c("red", "blue") #define palette
  )

## Remove points that joined with polygons from the point dataset.--------------
pharm_points_fd_nodupes = pharm_points_polygons_join %>% 
  dplyr::filter(point_overlaps_polygon==0)

nrow(pharm_points_fd_nodupes)

## All of the points in one pipe---------------
#Note: we could do all of the above in one step.
pharm_points_fd_nodupes = pharm_points_fd %>% 
  sf::st_buffer(20) %>% #create a 20 m buffer around each point.
  dplyr::rename(osm_id_point = osm_id) %>% 
  dplyr::select(osm_id_point, type_point, point_row_number, geometry) %>% 
  sf::st_join(
    pharm_polygons_fd, 
    left=TRUE, 
    largest=TRUE) %>%  
  dplyr::mutate(
    point_overlaps_polygon = case_when(
      type_polygon==1 ~ 1,
      TRUE ~ 0
    )) %>% 
  dplyr::filter(point_overlaps_polygon==0)

names(pharm_points_fd_nodupes)

## Use a similar st_join() procedure for the one multipolygon.---- 
names(pharm_multipolygons_fd)
pharm_polygons_fd_no_multipolygon = pharm_polygons_fd %>% 
  dplyr::rename(osm_id_polygon = osm_id) %>% 
  dplyr::select(osm_id_polygon, type_polygon, polygon_row_number, geometry) %>% 
  sf::st_join(pharm_multipolygons_fd) %>% 
  #create indicator variable for whether the polygon
  #intersected the multipolygon
  dplyr::mutate(
    polygon_overlaps_multipolygon = case_when(
      type_multipolygon==1 ~ 1,
      TRUE ~ 0
    )) %>% 
  #restrict to those that didn't overlap.
  dplyr::filter(polygon_overlaps_multipolygon==0)


#How many did we lose?
nrow(pharm_polygons_fd)
nrow(pharm_polygons_fd_no_multipolygon)

mapview(pharm_polygons_fd, col.regions = "red") + 
  mapview(pharm_polygons_fd_no_multipolygon, col.regions = "black")+
  mapview(pharm_multipolygons_fd, col.regions = "orange")


## Combine all pharmacies together in one datset.----------------

### Use st_centroid to convert all to points--------
#For simplicity, first find the centroid of each of buffered points, the polygons, 
#and the multipolygon


names(pharm_points_fd_nodupes)
names(pharm_polygons_fd_no_multipolygon)
names(pharm_multipolygons_fd)
pharm_points_fd_nodupes_centroid = pharm_points_fd_nodupes %>% 
  dplyr::select(starts_with("osm_id")) %>% 
  st_as_sf() %>% 
  st_centroid() %>%
  mutate(type_original = "point")

pharm_polygons_fd_no_multipolygon_centroid = pharm_polygons_fd_no_multipolygon %>% 
  dplyr::select(starts_with("osm_id")) %>% 
  st_centroid()  %>% 
  mutate(type_original = "polygon")

pharm_multipolygons_fd_centroid = pharm_multipolygons_fd %>% 
  dplyr::select(starts_with("osm_id")) %>% 
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
nrow(pharm_fd_combined) 
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



