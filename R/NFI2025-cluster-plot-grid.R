## Make the cluster/plot grid for the NFI 2025.
## Gaël Sola, FAO, June 2025


## Description ####

## The new design is based on NFMA plot 1 initial design with cluster center as middle point of NFMA plot 1.
## New clusters are crosses of 5 rectangular plots 50 x 20 m, with 100 m distance between plot center points.
## Plot 1 (center plot) ranges from 25 m south to 25 m north of NFMA plot 1 middle point. Its center is NFMA plot 1 middle point.
## Plot 2 (north plot) ranges from 50 m south of the NFMA plot 1 end point to the end point. Its center is 25 m south of NFMA plot 1 end point.
## Plot 3 (east plot) is at the same Y-position as Plot 1 but shifted 100 m to the east.
## Plot 4 (south plot) ranges from NFMA plot 1 start point to 50 m north of the start point.
## Plot 5 (west plot) is at the same Y-position as Plot 1 but shifted 100 m to the west.

## Initiation ####
source("R/get-pkg.R") 

if (!"data-core" %in% list.files()) stop("Missing core data, Run: 'source(R/get-data.R)' below before bulk running this script")

source("R/get-data.R")


if (!"NFI-grid" %in% list.files("results")) dir.create("results/NFI-grid")


##
## Load data #### 
##

## + NFMA data ####
tract_raw <- read_csv("data-core/NFMA/GMB-tract_raw.csv")

## + Get initial 5 arcminute grid and other spatial data
sf_grid_init <- st_read("data-core/grid_fromsharepoint/tracts_5x5_true_utm.shp") 

## + Country boundary ####
## Replaced with Iveren data, provided by forest department
#sf_country <- geodata::gadm("Gambia", level = 0, path = "data/Gambia") |> st_as_sf()
sf_country <- st_read("data-core/Admin boundries/LG_Areas.shp") |> st_transform(4326)



##
## Data preparation ####
##

## Add buffer to country boundaries for flexibility
sf_country_buff <- sf_country |> st_buffer(dist = 200)


## Make spatial object from tract raw data
sf_tract_center <- tract_raw |>
  select(
    ID.TRACT, 
    nfma_tc_lat = X12a.TractCenterLatitude, 
    nfma_tc_lon = X12b.TractCenterLongitude,
  ) |>
  mutate(
    nfma_tract_id = paste0("t", str_sub(ID.TRACT, start = 4)),
    xx = nfma_tc_lon, 
    yy = nfma_tc_lat
  ) |>
  select(nfma_tract_id, nfma_tc_lon, nfma_tc_lat, xx, yy) |>
  st_as_sf(coords = c("xx", "yy"), crs = 4326)


## REMOVE TRACT OUTSIDE BOUNDARIES FROM REPORT
sf_tract_center_visited <- sf_tract |>
  filter(!nfma_tract_id %in% paste0("t", c(141, 106, 113, 114, 103, 105))) |>
  filter(!nfma_tract_id %in% paste0("t0", c(75, 76, 11, 87, 91, 98, 48, 51, 52, 71)))


## CHECK
# tm_basemap(c("Esri.WorldGrayCanvas", "Esri.WorldImagery", "Esri.WorldTopoMap")) +
#   tm_shape(sf_grid_init) + tm_dots(fill = "grey10", size = 0.8) +
#   tm_shape(sf_tract_center) + tm_dots(fill = "red", size = 0.4) +
#   tm_shape(sf_tract_center_visited) + tm_dots(fill = "lightgreen", size = 0.4)




## 
## Make 2.5 arcminutes grid ####
##


## Starting point from initial grid
start_point <- sf_tract_center |> 
  filter(nfma_tract_id == "t152") %>%
  mutate(geometry = st_geometry(.) + c(-5/60, 0))

# start_point <- sf_grid_init |> 
#   filter(Name == 149) %>%
#   mutate(geometry = st_geometry(.) + c(-5/60, 0))

sf_grid25 <- st_make_grid(
  sf_country,
  cellsize = 2.5/60, 
  offset = st_coordinates(start_point), 
  square = T,
  what = "corners"
) |>
  st_as_sf() |>
  rename(geometry = x) |>
  st_filter(sf_country_buff) |>
  mutate(
    cluster_no = row_number(),
    cluster_id = case_when(
      cluster_no < 10 ~ paste0("c00", cluster_no),
      cluster_no < 100 ~ paste0("c0", cluster_no),
      TRUE ~ paste0("c", cluster_no)
    )
  )

## CHECK
tm_basemap(c("Esri.WorldGrayCanvas", "Esri.WorldImagery", "Esri.WorldTopoMap")) +
  tm_shape(sf_grid_init) + tm_dots(fill = "grey10", size = 0.8) +
  tm_shape(sf_tract_center) + tm_dots(fill = "red", size = 0.4) +
  tm_shape(sf_tract_center_visited) + tm_dots(fill = "lightgreen", size = 0.4) +
  tm_shape(sf_grid25) + tm_dots(fill = "pink3", size = 0.4, col = "black", lwd = 0.1)


## Add tract info to 2.5 arcmin grid
sf_tract_buff <- st_buffer(sf_tract, dist = 500)
sf_grid25_join <- st_join(sf_grid25, sf_tract_buff)



##
## Convert 2.5 arcminutes grid to UTM to get NFMA plot 1 converted to cross
##

## Get grid center points matching tract center location
grid25_m <- sf_grid25_join %>%
  mutate(
    tc_no = row_number(),
    tc_id = case_when(
      tc_no < 10 ~ paste0("c00", tc_no),
      tc_no < 100 ~ paste0("c0", tc_no),
      TRUE ~ paste0("c", tc_no)
    ),
    tc_lon = st_coordinates(.)[,1],
    tc_lat = st_coordinates(.)[,2],
  ) |>
  st_transform(crs = 32628) %>%
  mutate(
    tc_x = st_coordinates(.)[,1],
    tc_y = st_coordinates(.)[,2]
  ) |>
  as_tibble() |>
  select(-geometry) 


## Make the cross locations
ceo_grid25 <- grid25_m |>
  mutate(
    x_plot1 = tc_x - 250,
    y_plot1 = tc_y - 125,
    x_plot2 = x_plot1,
    y_plot2 = y_plot1 + 100,
    x_plot3 = x_plot1 + 100,
    y_plot3 = y_plot1,
    x_plot4 = x_plot1,
    y_plot4 = y_plot1 - 100,
    x_plot5 = x_plot1 - 100,
    y_plot5 = y_plot1,
  ) |> 
  select(cluster_no, cluster_id, nfma_tract_id, starts_with(c("x_", "y_"))) |>
  pivot_longer(cols = starts_with(c("x_", "y_")), names_to = c("xy", "plot_no"), names_pattern = "(.)_plot(.)", values_to = "coord") |>
  pivot_wider(names_from = xy, values_from = coord) |>
  mutate(
    plot_no = as.numeric(plot_no),
    plot_id = paste0(cluster_id, plot_no)
  ) |>
  select(cluster_no, cluster_id, nfma_tract_id, plot_no, plot_id, plot_x = x, plot_y = y)

## Convert back to lat/lon
sf_ceo_grid25 <- ceo_grid25 |>
  mutate(xx = plot_x, yy = plot_y) |>
  st_as_sf(coords = c("xx", "yy"), crs = 32628) |>
  st_transform(crs = 4326)

## Check
# tmap_mode("view") +
#   tm_shape(sf_country) + tm_lines() +
#   tm_shape(sf_tract) + tm_dots(size = 0.6) +
#   tm_shape(sf_ceo_grid25) + tm_dots(fill = "green", size = 0.4)


## Make table
ceo25_latlon <- sf_ceo_grid25 |>
  mutate(
    plot_lon = st_coordinates(sf_ceo_grid25)[,1],
    plot_lat = st_coordinates(sf_ceo_grid25)[,2]
  ) |>
  as_tibble() |>
  select(-geometry)

## Final tables
ceo25_intens <- ceo25_latlon |>
  filter(is.na(nfma_tract_id))

ceo5_corr <- ceo25_latlon |>
  filter(!is.na(nfma_tract_id))


##
## Convert NFMA tract center to cross points
##

ceo_grid_nfma <- sf_tract |>
  st_transform(32628) %>%
  mutate(
    tc_x = st_coordinates(.)[,1],
    tc_y = st_coordinates(.)[,2]
  ) |>
  as_tibble() |>
  select(-geometry) |>
  mutate(
    x_plot1 = tc_x - 250,
    y_plot1 = tc_y - 125,
    x_plot2 = x_plot1,
    y_plot2 = y_plot1 + 100,
    x_plot3 = x_plot1 + 100,
    y_plot3 = y_plot1,
    x_plot4 = x_plot1,
    y_plot4 = y_plot1 - 100,
    x_plot5 = x_plot1 - 100,
    y_plot5 = y_plot1,
  ) |> 
  select(nfma_tract_id, starts_with(c("x_", "y_"))) |>
  pivot_longer(cols = starts_with(c("x_", "y_")), names_to = c("xy", "plot_no"), names_pattern = "(.)_plot(.)", values_to = "coord") |>
  pivot_wider(names_from = xy, values_from = coord) |>
  mutate(
    plot_no = as.numeric(plot_no),
    plot_id = paste0(nfma_tract_id, plot_no)
  ) |>
  select(nfma_tract_id, plot_no, plot_id, plot_x = x, plot_y = y)

## Convert back to lat/lon
sf_ceo_grid_nfma <- ceo_grid_nfma |>
  mutate(xx = plot_x, yy = plot_y) |>
  st_as_sf(coords = c("xx", "yy"), crs = 32628) |>
  st_transform(4326) 

## make table
ceo5_nfma <- sf_ceo_grid_nfma |>
  mutate(
    plot_lon = st_coordinates(sf_ceo_grid_nfma)[,1],
    plot_lat = st_coordinates(sf_ceo_grid_nfma)[,2]
  ) |>
  as_tibble() |>
  select(-geometry)

## Check
sf_ceo5_nfma <- ceo5_nfma |>
  mutate(xx = plot_lon, yy = plot_lat) |>
  st_as_sf(coords = c("xx", "yy"), crs = 4326)

sf_ceo25_intens <- ceo25_intens |>
  mutate(xx = plot_lon, yy = plot_lat) |>
  st_as_sf(coords = c("xx", "yy"), crs = 4326)

sf_ceo5_corr <- ceo5_corr |>
  mutate(xx = plot_lon, yy = plot_lat) |>
  st_as_sf(coords = c("xx", "yy"), crs = 4326)

tmap_mode("view") +
  tm_shape(sf_tract) + tm_dots(size = 0.6) +
  tm_shape(sf_grid25) + tm_dots(size = 0.6, fill = "grey40") +
  tm_shape(sf_ceo5_nfma) + tm_dots(fill = "red", size = 0.4) +
  tm_shape(sf_ceo5_corr) + tm_dots(fill = "pink", size = 0.4) +
  tm_shape(sf_ceo25_intens) + tm_dots(fill = "green", size = 0.4)



##
## Write results
##

write_csv(ceo5_nfma, "results/NFI-grid/ceo5_NFMA.csv")
write_csv(ceo5_corr, "results/NFI-grid/ceo5_grid.csv")
write_csv(ceo25_intens, "results/NFI-grid/ceo25_intens.csv")

st_write(sf_ceo5_nfma, "results/NFI-grid/ceo5_NFMA.kml")
st_write(sf_ceo5_corr, "results/NFI-grid/ceo5_grid.kml")
st_write(sf_ceo25_intens, "results/NFI-grid/ceo25_intens.kml")


## stats
nrow(ceo5_nfma)/5
nrow(ceo25_intens)/5

nrow(ceo5_nfma) + nrow(ceo25_intens)
nrow(ceo5_nfma) + nrow(ceo25_intens) + nrow(ceo5_corr)




