#Script to accompany

#~teach-r/docs/0-manage-packages.Rmd

#Functions to print information about packages, currently loaded and otherwise-------------
## base R---------
#general session info
utils::sessionInfo() 

#look at search path
base::search()

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