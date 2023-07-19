#install package nzsf which has CCSBT region shapefiles
install.packages('devtools')
install.packages("maps")
library(devtools)
devtools::install_github(repo = "ropensci/rnaturalearthhires")
devtools::install_github(repo = "quantifish/nzsf", build_vignettes = F)
library(sf)
library(rnaturalearthhires)
library(nzsf)
library(shiny)

CCSBTs <- CCSBT[CCSBT$Area %in% 1:8, ] |> st_make_valid() |>
  st_union(by_feature = TRUE) |> st_shift_longitude()
the_map <- rnaturalearth::ne_countries(returnclass = "sf")

saveRDS(CCSBTs, "data/CCSBT_areas.RDS")
saveRDS(the_map, "data/the_map.RDS")
