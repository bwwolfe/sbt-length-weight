#install package nzsf which has CCSBT region shapefiles
install.packages('devtools')
install.packages("maps")
library(devtools)
devtools::install_github(repo = "ropensci/rnaturalearthhires")
devtools::install_github(repo = "quantifish/nzsf", build_vignettes = F)
