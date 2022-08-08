#Script to accompany

#~teach-r/docs/0-manage-packages.Rmd

#Functions to print information about packages, currently loaded and otherwise-------------
## base R---------
#general session info
utils::sessionInfo() 

#look at search path
base::search()
base::searchpaths()

#what packages have been loaded via a namespace?
base::loadedNamespaces()


#all installed packages
utils::installed.packages()

.libPaths()# returns the folder location in your computer where packages are stored.

## sessioninfo--------

sessioninfo::session_info()

sessioninfo::package_info()

sessioninfo::package_info(pkgs = "loaded")

sessioninfo::package_info(pkgs = "attached")

sessioninfo::package_info(pkgs = "installed")

# Functions to install, update, and remove packages (base R)---------

#install and control dependencies
#install.packages("package_name", dependencies = FALSE)

#update packages already loaded
#update.packages(checkBuilt = T, ask = F)

#detach packages from current session
library(here) #attach
base::detach("package:here", unload = TRUE) #detach
sessioninfo::package_info(pkgs = "loaded") #check

#unload a namespace - useful for packages that have not been attached
#base::unloadNamespace("package_name")

library(here)
library(ggplot2)
sessioninfo::package_info(pkgs = "attached")

sessioninfo::package_info(pkgs = "loaded")
sessioninfo::package_info(pkgs = "installed")

#remove a package
base::detach("package:here", unload = TRUE)
base::detach("package:mapview", unload = TRUE)
sessioninfo::package_info("attached")

remove.packages("sessioninfo")
remove.packages("sf")
remove.packages("tidycensus")
install.packages("tidycensus") #sf would import

install.packages("sessioninfo")


packages  = utils::available.packages()
#View(packages)
library(tidyverse) #so the pipe works
packages_tibble_tidycensus = packages %>% 
  dplyr::as_tibble() %>% 
  dplyr::filter(Package == "tidycensus")

#Just select these two fields.
packages_tibble_tidycensus %>% 
  dplyr::select(Package, Imports)