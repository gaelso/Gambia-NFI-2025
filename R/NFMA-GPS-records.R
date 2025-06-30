## Extract GPS records from NFMA data base and analyse tree positioning
## Gaël Sola, FAO, June 2025


## Initiation 
source("R/get-pkg.R")

if (!"data-core" %in% list.files()) stop("Missing core data, Run: 'source(R/get-data.R)' below before bulk running this script")

source("R/get-data.R")

##
## Load data ####
##

## + NFMA data ####
path_nfma       <- list.files("data-core/NFMA", pattern = "GMB-.*\\.csv", full.names = T)
path_nfma_names <- list.files("data-core/NFMA", pattern = "GMB-.*\\.csv") |> str_remove("GMB-") |> 
  str_remove("\\.csv")

nfma <- map(path_nfma, read_csv, show_col_types = F)
names(nfma) <- path_nfma_names

walk(path_nfma_names, function(x){
  assign(paste0(x, "_init"), nfma[[x]], envir = globalenv())
})

rm(nfma, path_nfma, path_nfma_names)


##
## Country analysis - GPS records ####
##

## Convert plot GPS points to long table
plot_gps_records <- plot_gps_init |>
  pivot_longer(
    cols = starts_with("plot_gps"),
    names_to = c(".value", "location"),
    names_pattern = "plot_gps_(.)_(.*)" 
  ) |>
  arrange(tract_id, plot_id)

## Convert UTM to latlon
sf_plot_gps_records <- plot_gps_records |>
  filter(!is.na(x), !is.na(y)) |>
  mutate(xx = x, yy = y) |>
  st_as_sf(coords = c("xx", "yy"), crs = 32628) |>
  st_transform(crs = 4326)

plot_gps_records_latlon <- sf_plot_gps_records |>
  mutate(
    lon = st_coordinates(sf_plot_gps_records)[,1],
    lat = st_coordinates(sf_plot_gps_records)[,2]
  ) |>
  as_tibble() |>
  select(-geometry)

## Check
# ggplot(sf_plot_gps_records) + 
#   geom_sf(aes(color = location)) +
#   theme_bw()

## Save data
write_csv(sf_plot_gps_records, paste0("results/NFMA-plot-GPS-records.csv"))
st_write(sf_plot_gps_records, paste0("results/NFMA-plot-GPS-records.kml"))
