## Extract GPS records from NFMA data base and analyse tree positioning
## Gaël Sola, FAO, June 2025


## Initiation 
source("R/get-pkg.R")
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
ct$sf_plot_gps_records <- ct$plot_gps_records |>
  filter(!is.na(x), !is.na(y)) |>
  mutate(xx = x, yy = y) |>
  st_as_sf(coords = c("xx", "yy"), crs = 32628) |>
  st_transform(crs = 4326)

ct$plot_gps_records_latlon <- ct$sf_plot_gps_records |>
  mutate(
    lon = st_coordinates(ct$sf_plot_gps_records)[,1],
    lat = st_coordinates(ct$sf_plot_gps_records)[,2]
  ) |>
  as_tibble() |>
  select(-geometry)

## Check
ggplot(ct$sf_plot_gps_records) + 
  geom_sf(aes(color = location)) +
  theme_bw()

## Save data
write_csv(ct$sf_plot_gps_records, paste0("results/", init$country_name, "/", init$country_iso, "-plot-GPS-records.csv"))
st_write(ct$sf_plot_gps_records, paste0("results/", init$country_name, "/", init$country_iso, "-plot-GPS-records.kml"))


##
## Country analysis - tree position ####
##

## Checks
# table(ct$lus$lus)
# 
# tt <- ct$lus |> summarise(count = n(), .by = c(iso, plot_id))
# 
# table(tt$count)
# 
# length(unique(ct$lus$plot_id)) / 4

## For each cluster make tree location maps
tmp$list_tract <- ct$tree |>
  mutate(
    tract_id = str_sub(plot_id, end = -2)
  ) |>
  pull(tract_id) |>
  unique() |>
  sort()

tmp$gg_tracts <- map(tmp$list_tract, function(x){
  
  tract_id <- tmp$plot_no |> filter(tract_id == x) |> pull(tract_id) |> unique()
  
  ct$tree |> 
    filter(plot_id %in% paste0(x, 1:4)) |>
    mutate(
      plot_no = str_remove(plot_id, pattern = x),
      lus_id = paste0(plot_id, "0", lus_no)
    ) |>
    left_join(tmp$lus, by = join_by(lus_id)) |>
    ggplot(aes(tree_x, tree_y)) +
    geom_point(aes(color = tree_species_name, shape = lus)) +
    ggrepel::geom_text_repel(aes(label = paste0(tree_dbh, " cm")), min.segment.length = 0, size = 2) +
    theme_bw() + 
    coord_fixed(ratio = 1/2) +
    facet_wrap(~plot_no, nrow = 1) + 
    scale_y_continuous(breaks = 1:25 * 10) +
    scale_x_continuous(breaks = c(-10, 0, 10), minor_breaks = c(-5, 0, 5), limits = c(-10, 10)) +
    labs(
      subtitle = paste0("Tree DBH for Tract ", tract_id),
      color = "",
      shape = "",
      x = "X (m)",
      y = "Y (m)",
      caption = "Plot ratio y/x = 1/2"
    )
  
})

names(tmp$gg_tracts) <- tmp$list_tract

## Write all plots
walk(tmp$list_tract, function(x){
  
  ggsave(
    plot = tmp$gg_tracts[[x]], 
    filename = paste0("results/", init$country_name, "/", init$country_iso, "-tree-position-tract0-", x, ".png"),
    height = 17, width = 14, units = "cm", dpi = 300
  )
  
})