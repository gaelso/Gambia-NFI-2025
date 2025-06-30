## Correction and analysis of CEO campaign from May-June 2025 and comaprison with NFMA land use classes
## Gaël Sola, FAO, June 2025


## Initiation 
source("R/get-pkg.R")

if (!"data-core" %in% list.files()) stop("Missing core data, Run: 'source(R/get-data.R)' below before bulk running this script")
source("R/get-data.R")

if(!"CEO-comparison" %in% list.files("results")) dir.create("results/CEO-comparison")


##
## LOAD DATA ####
##


## + CEO corrected ####

ceo_plot <- read_csv("data-core/CEO_corr/ceo-Land-Cover-Assessment_Gambia_(All)_data.csv", show_col_types = F) |>
  rename(tract_no = track_no) |>
  mutate(
    `Select Forest Type?:Mangrove Forest` = case_when(
      ceo_id == 296 ~ 0,        ## 
      ceo_id %in% c(21:25) ~ 0, ## Tract 78
      ceo_id %in% c(26:30) ~ 0, ## Tract 79
      TRUE ~ `Select Forest Type?:Mangrove Forest`
    ),
    `Select Forest Type?:Closed Forest` = case_when(
      ceo_id == 296 ~ 88.89,
      ceo_id %in% c(21:25) ~ 0, ## Tract 78
      ceo_id %in% c(26:30) ~ 0, ## Tract 79
      TRUE ~ `Select Forest Type?:Closed Forest`
    ),
    `Select Forest Type?:Open Forest` = case_when(
      ceo_id %in% c(21:25) ~ 100,
      ceo_id %in% c(26:30) ~ 100,
      TRUE ~ `Select Forest Type?:Open Forest`
    ),
  )
ceo_plot


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

## + ROOTS ####

rs_root1 <- terra::rast("data-core/ROOTS/lc_gambia_2023_10m-0000000000-0000000000.tif")
rs_root2 <- terra::rast("data-core/ROOTS/lc_gambia_2023_10m-0000000000-0000032768.tif")

roots_class <- tibble(
  roots_lc_id = 0:18,
  roots_lc_name = c(
    "Closed Forest", "Open Forest", "Closed Shrubs", "Open Shrubs", "Tree Savanna", 
    "Shrub Savanna", "Grass Savanna", "Mangrove Forest", "Flooded Woody", "Swamp", 
    "Salt Marsh", "Orchards", "Annual Herb Crops (Rainfed)", "Herbaceous Crop (Irrigated)", 
    "Paddy Rice", "Beaches", "Built-up, non-linear", "Built-up linear/Barren", "Rivers"
  ),
  roots_lc_class = c(
    "closed forest", "open forest", rep("nonforest", 5), "mangrove forest",
    rep("nonforest", 11)
  )
)

## Checks
# plot(rs_root1)
# 
# ggplot() +
#   geom_spatraster(data = rs_root1) +
#   geom_spatraster(data = rs_root2)
# 
# freq(rs_root1)



## + Data preparation ####

lus <- lus_init
plot_gps <- plot_gps_init


##
## TASK 1: Land cover in NFMA ####
##

## Check
# tt <- lus |> filter(plot_no == 1)
# table(tt$lus_no)


## tract_no where multiple LC (2 or 3/4)

vec_lus2 <- lus |> 
  filter(plot_no == 1, lus_no == 2) |>
  pull(tract_no) |>
  unique()

vec_lus34 <- lus |>
  filter(plot_no == 1, lus_no %in% c(3, 4)) |>
  pull(tract_no) |>
  unique()


## Homogeneous LC

lus1 <- lus |> 
  filter(plot_no == 1, !tract_no %in% vec_lus2) |>
  select(tract_no, nfma_lu = lus, nfma_lu_class = lus_class) |>
  arrange(tract_no)

## 2 LC in the same NFMA plot 1

lus2 <- lus |>
  filter(plot_no == 1, tract_no %in% vec_lus2, !tract_no %in% vec_lus34) |>
  arrange(lus_id) |>
  pivot_wider(id_cols = c(tract_no, plot_no), names_from = lus_no, names_glue = "{.value}_lus{lus_no}", values_from = c(plot_length, lus, lus_class)) |>
  mutate(
    newplot1_lu = case_when(
      plot_length_lus1 >= 150 ~ lus_lus1,
      plot_length_lus2 >= 150 ~ lus_lus2,
      TRUE ~ "mix"
    ),
    newplot1_lu_class = case_when(
      lus_class_lus1 == lus_class_lus2 ~ lus_class_lus1,
      plot_length_lus1 >= 150 ~ lus_class_lus1,
      plot_length_lus2 >= 150 ~ lus_class_lus2,
      lus_class_lus1 == "nonforest" & lus_class_lus2 == "nonforest" ~ "mix nonforest",
      TRUE ~ "mix forest"
    ),
    newplot2_lu = case_when(
      plot_length_lus1 == 250 ~ lus_lus1,
      plot_length_lus2 >=  50 ~ lus_lus2,
      TRUE ~ "mix"
    ),
    newplot2_lu_class = case_when(
      lus_class_lus1 == lus_class_lus2 ~ lus_class_lus1,
      plot_length_lus1 == 250 ~ lus_class_lus1,
      plot_length_lus2 >=  50 ~ lus_class_lus2,
      lus_class_lus1 == "nonforest" & lus_class_lus2 == "nonforest" ~ "mix nonforest",
      TRUE ~ "mix forest"
    ),
    newplot4_lu = case_when(
      plot_length_lus1 >=  50 ~ lus_lus1,
      TRUE ~ "mix"
    ),
    newplot4_lu_class = case_when(
      lus_class_lus1 == lus_class_lus2 ~ lus_class_lus1,
      plot_length_lus1 >= 50 ~ lus_class_lus1,
      lus_class_lus1 == "nonforest" & lus_class_lus2 == "nonforest" ~ "mix nonforest",
    ),
  )

## Convert to long table to have 1 row equal; one new plot (1, 2 or 4)
newplot_lus2 <- lus2 |>
  select(-starts_with("plot_length"), -starts_with("lus_"), -ends_with("_lu_class"), -plot_no) |>
  pivot_longer(cols = c(ends_with("_lu")), names_to = "plot_no", values_to = "nfma_lu", names_pattern = "newplot(.)_lu") |>
  mutate(plot_no = as.numeric(plot_no)) 

newplot_lu_class2 <- lus2 |>
  select(-starts_with("plot_length"), -starts_with("lus_"), -ends_with("_lu"), -plot_no) |>
  pivot_longer(cols = c(ends_with("_lu_class")), names_to = "plot_no", values_to = "nfma_lu_class", names_pattern = "newplot(.)_lu_class") |>
  mutate(plot_no = as.numeric(plot_no))

## More than 3 or 4 LC, done manually at the end
lus34 <- lus |>
  filter(plot_no == 1, tract_no %in% vec_lus34) |>
  arrange(lus_id)

## Add info to CEO initial table
ceo_plot1 <- ceo_plot |>
  filter(plot_no %in% c(1, 2, 4), type == "nfma_track") |> 
  left_join(lus1, by = "tract_no") |>
  left_join(newplot_lus2, by = c("tract_no", "plot_no")) |>
  left_join(newplot_lu_class2, by = c("tract_no", "plot_no")) |>
  mutate(
    nfma_lu = case_when(
      !is.na(nfma_lu.x) ~ nfma_lu.x,
      !is.na(nfma_lu.y) ~ nfma_lu.y,
      TRUE ~ NA_character_
    ),
    nfma_lu_class = case_when(
      !is.na(nfma_lu_class.x) ~ nfma_lu_class.x,
      !is.na(nfma_lu_class.y) ~ nfma_lu_class.y,
      tract_no == 13 & plot_no == 4 ~ "open forest",
      tract_no == 13 & plot_no == 1 ~ "nonforest",
      tract_no == 13 & plot_no == 2 ~ "open forest",
      tract_no == 15 & plot_no == 4 ~ "nonforest",
      tract_no == 15 & plot_no == 1 ~ "mix nonforest",
      tract_no == 15 & plot_no == 2 ~ "nonforest",
      tract_no == 24 & plot_no == 4 ~ "mix nonforest",
      tract_no == 24 & plot_no == 1 ~ "mix nonforest",
      tract_no == 24 & plot_no == 2 ~ "mix nonforest",
      tract_no == 40 & plot_no == 4 ~ "nonforest",
      tract_no == 40 & plot_no == 1 ~ "mix nonforest",
      tract_no == 40 & plot_no == 2 ~ "mix nonforest",
      tract_no == 42 & plot_no == 4 ~ "open forest",
      tract_no == 42 & plot_no == 1 ~ "mix nonforest",
      tract_no == 42 & plot_no == 2 ~ "nonforest",
      tract_no == 58 & plot_no == 4 ~ "mix nonforest",
      tract_no == 58 & plot_no == 1 ~ "nonforest",
      tract_no == 58 & plot_no == 2 ~ "nonforest",
      tract_no == 90 & plot_no == 4 ~ "mix forest",
      tract_no == 90 & plot_no == 1 ~ "mix forest",
      tract_no == 90 & plot_no == 2 ~ "nonforest",
      tract_no == 120 & plot_no == 4 ~ "mix forest",
      tract_no == 120 & plot_no == 1 ~ "nonforest",
      tract_no == 120 & plot_no == 2 ~ "mix nonforest",
    )
  ) |>
  select(-ends_with(".x"), -ends_with(".y"))

table(ceo_plot1$nfma_lu_class, useNA = "ifany")


##
## TASK 2: Distance between NFMA plot center and CEO plot center ####
##

plot_gps2 <- plot_gps |>
  filter(plot_no == 1) |>
  rename_with(.cols = starts_with("plot_gps"), str_remove, "plot_gps_") |>
  select(-x__leavingroad, -y__leavingroad, -x_middlepoint, -y_middlepoint) |>
  filter(!is.na(x_startingpoint), !is.na(y_startingpoint), !is.na(x_endpoint), !is.na(y_endpoint))

plot_gps_latlon <- plot_gps2 |>
  pivot_longer(cols = c(starts_with("x"), starts_with("y")), names_to = c("coords", "location"), names_sep = "_") |>
  pivot_wider(names_from =  coords, values_from = value) |>
  st_as_sf(coords = c("x", "y"), crs = 32628) |>
  st_transform(crs = 4326) %>%
  mutate(
    lon = st_coordinates(.)[,1],
    lat = st_coordinates(.)[,2]
  ) |>
  as_tibble() |>
  select(-geometry) |>
  pivot_wider(names_from = location, values_from = c(lon, lat))


# starts <- plot_gps_latlon |> filter(location == "startingpoint")
# ends   <- plot_gps_latlon |> filter(location == "endpoint")
# 
# plot_gps2$bearing <- bearing(starts, ends)
# 
# plot_gps2$xy_plot2 <- destPoint(c(plot_gps2$x_endpoint, plot_gps2$y_endpoint), b = plot_gps2$bearing, d = 25)

plot_gps3 <- plot_gps_latlon |>
  rowwise() |>
  mutate(
    bearing = bearing(c(lon_startingpoint, lat_startingpoint), c(lon_endpoint, lat_endpoint)),
    plot2_lon = destPoint(c(lon_endpoint, lat_endpoint), b = bearing, d = -25)[,1],
    plot2_lat = destPoint(c(lon_endpoint, lat_endpoint), b = bearing, d = -25)[,2],
    plot4_lon = destPoint(c(lon_startingpoint, lat_startingpoint), b = bearing, d = 25)[,1],
    plot4_lat = destPoint(c(lon_startingpoint, lat_startingpoint), b = bearing, d = 25)[,2],
  ) |>
  ungroup()

newplot_gps24 <- plot_gps3 |>
  select(tract_id, starts_with("plot2"), starts_with("plot4")) |>
  pivot_longer(cols = c(starts_with("plot")), names_to = c("plot_no", "coords"), values_to = "value", names_pattern = "plot(.)_(.*)") |>
  pivot_wider(names_from = coords, values_from = value) |>
  mutate(
    tract_no = as.numeric(str_sub(tract_id, -3)),
    plot_no = as.numeric(plot_no)
  ) |>
  select(tract_no, plot_no, lon, lat)

newplot_gps1 <- plot_gps |>
  filter(plot_no == 1, !is.na(plot_gps_x_middlepoint), !is.na(plot_gps_y_middlepoint)) |>
  select(tract_id, plot_no, plot_gps_x_middlepoint, plot_gps_y_middlepoint) |>
  st_as_sf(coords = c("plot_gps_x_middlepoint", "plot_gps_y_middlepoint"), crs = 32628) |>
  st_transform(4326) %>%
  mutate(
    lon = st_coordinates(.)[,1],
    lat = st_coordinates(.)[,2]
  ) |>
  as_tibble() |>
  mutate(tract_no = as.numeric(str_sub(tract_id, -3))) |>
  select(tract_no, plot_no, lon, lat)


newplot_gps <- bind_rows(newplot_gps1, newplot_gps24)

## ADD to CEO
ceo_plot2 <- ceo_plot1 |> 
  left_join(newplot_gps, by = c("tract_no", "plot_no")) |>
  rename(nfma_lon = lon, nfma_lat = lat) |>
  rowwise() |>
  mutate(
    distance_ceo_nfma = distHaversine(c(center_lon, center_lat), c(nfma_lon, nfma_lat))
  )



##
## TASK 3: add ROOT maps ####
##

sf_ceo <- ceo_plot |>
  select(tract_no, plot_no, type, center_lon, center_lat) |>
  mutate(lon = center_lon, lat = center_lat) |>
  st_as_sf(coords = c("lon", "lat"), crs = 4326)

roots_lc_id1 <- terra::extract(rs_root1, vect(sf_ceo))[,2]
roots_lc_id2 <- terra::extract(rs_root2, vect(sf_ceo))[,2]

ceo_plot3 <- ceo_plot |>
  mutate(
    ceo_lu_class = case_when(
      `Select Forest Type?:Closed Forest` >= 7/9*100 ~ "closed forest",
      `Select Forest Type?:Open Forest`   >= 1/9*100 ~ "open forest",
      `Select Forest Type?:Mangrove Forest` > 0      ~ "mangrove forest",
      TRUE ~ "nonforest"
    ),
    ceo_lu_class60 = if_else(`Select Forest Type?:Open Forest` > 60, "closed forest", ceo_lu_class)
  ) |>
  bind_cols(roots_lc_id1 = roots_lc_id1, roots_lc_id2 = roots_lc_id2) |>
  mutate(roots_lc_id = if_else(!is.na(roots_lc_id1), roots_lc_id1, roots_lc_id2)) |>
  left_join(roots_class, by = "roots_lc_id") |>
  left_join(ceo_plot1) |>
  left_join(newplot_gps, by = c("tract_no", "plot_no")) |>
  rename(nfma_lon = lon, nfma_lat = lat) |>
  rowwise() |>
  mutate(
    distance_ceo_nfma = distHaversine(c(center_lon, center_lat), c(nfma_lon, nfma_lat))
  ) |>
  ungroup()

## Check outputs
ceo_plot3_reduced <- ceo_plot3 |>
  filter(plot_no %in% c(1, 2, 4), type == "nfma_track")

## + SAVE COMPARISON TABLE ####

write_csv(ceo_plot3, "results/CEO-comparison/ceo_extra.csv")
write_csv(ceo_plot3_reduced, "results/CEO-comparison/ceo_extra_reduced.csv")


##
## OUTPUT TABLES ####
##

## + nb plots per cluster in forest (from plot 1, 2  and 4) ####

t1 <- ceo_plot3_reduced |>
  filter(!nfma_lu_class %in% c("nonforest", "mix forest", "mix nonforest")) |>
  summarise(plot_count_nfma = n(), .by = c(type, tract_no, nfma_lu_class)) |>
  arrange(nfma_lu_class, tract_no) |>
  mutate(select_NFMA = if_else(plot_count_nfma > 1, T, F))

## + nb plot per cluster in forest from CEO ####

t2 <- ceo_plot3 |>
  filter(plot_no %in% c(1, 2, 4), type == "nfma_track", ceo_lu_class != "nonforest") |>
  summarise(plot_count_ceo = n(), .by = c(type, tract_no, ceo_lu_class)) |>
  arrange(desc(type), ceo_lu_class, tract_no) |>
  mutate(select_CEO = if_else(plot_count_ceo > 1, T, F))

t2a <- ceo_plot3 |>
  filter(plot_no %in% c(1, 2, 4), type == "nfma_track", ceo_lu_class60 != "nonforest") |>
  summarise(plot_count_ceo = n(), .by = c(type, tract_no, ceo_lu_class60)) |>
  arrange(desc(type), ceo_lu_class60, tract_no) |>
  mutate(select_CEO = if_else(plot_count_ceo > 1, T, F))

## + nb plot per cluster in forest from ROOTS ####

# table(ceo_plot3$roots_lc_class, useNA = "ifany")
# 
# t3 <- ceo_plot3 |>
#   filter(roots_lc_class != "nonforest") |>
#   summarise(plot_count_roots = n(), .by = c(type, tract_no, roots_lc_class)) |>
#   arrange(desc(type), roots_lc_class, tract_no) |>
#   mutate(select_ROOTS = if_else(plot_count_roots > 2, T, F))

## + Group table for selection ####

tmp_t2_reduced <- t2 |>
  summarise(max_plot = max(plot_count_ceo), .by = tract_no) |>
  mutate(keep = T)

t2_reduced <- t2 |>
  left_join(tmp_t2_reduced, by = c("tract_no", "plot_count_ceo" = "max_plot")) |>
  filter(keep) |>
  select(-keep)

tmp_t2a_reduced <- t2a |>
  summarise(max_plot = max(plot_count_ceo), .by = tract_no) |>
  mutate(keep = T)

t2a_reduced <- t2 |>
  left_join(tmp_t2a_reduced, by = c("tract_no", "plot_count_ceo" = "max_plot")) |>
  filter(keep) |>
  select(-keep)

## List cluster outside country
vec <- lus |>
  filter(lus == "outside country") |>
  pull(tract_no) |>
  unique()

t4 <- t1 |>
  arrange(desc(type), tract_no) |>
  full_join(t2_reduced, by = c("type", "tract_no")) |>
  full_join(t2a_reduced, by = c("type", "tract_no"), suffix = c("", "60")) |>
  filter(!tract_no %in% vec) |>
  arrange(nfma_lu_class, ceo_lu_class)

t4 |>
  filter(!is.na(nfma_lu_class)) |>
  nrow()

t4 |>
  filter(!is.na(nfma_lu_class), !is.na(ceo_lu_class)) |>
  nrow()

table(t4$nfma_lu_class, t4$ceo_lu_class, useNA = "ifany")


write_csv(t4, "results/CEO-comparison/t4.csv")

t4_corr <- t4 |>
  mutate(
    lu_corr = case_when(
      tract_no == 128 ~ "mangrove forest",
      tract_no ==  39 ~ "open forest", 
      tract_no ==  61 ~ "closed forest",
      tract_no ==  95 ~ "open forest", 
      tract_no == 132 ~ "nonforest", 
      tract_no ==  13 ~ "closed forest", 
      tract_no ==  68 ~ "open forest", 
      tract_no ==  74 ~ "open forest", 
      tract_no ==  97 ~ "open forest", 
      tract_no == 111 ~ "open forest", 
      tract_no == 116 ~ "open forest", 
      tract_no == 104 ~ "nonforest", 
      tract_no ==  84 ~ "nonforest", 
      tract_no == 123 ~ "nonforest", 
      tract_no == 124 ~ "open forest", ## Deforested but should visit
      tract_no == 134 ~ "open forest", ## Deforested/burned but should visit
      tract_no == 145 ~ "open forest", 
      tract_no ==  92 ~ "open forest", 
      tract_no ==  49 ~ "nonforest", 
      tract_no ==  55 ~ "nonforest",
      tract_no == 129 ~ "mangrove forest",
      !is.na(nfma_lu_class) & !is.na(ceo_lu_class) ~ nfma_lu_class,
      is.na(nfma_lu_class) & !is.na(ceo_lu_class)  ~ ceo_lu_class, ## Tested open forest in CEO with NA in NFMA could be open forest.  
      TRUE ~ "nonforest"
    ),
    comment_gs = case_when(
      tract_no == 128 ~ "confirmed mangrove",
      tract_no ==  39 ~ "seems it was always open forest", 
      tract_no ==  61 ~ "a portion of the 5 plots is bareland",
      tract_no ==  95 ~ "looks degraded, should visit", 
      tract_no == 132 ~ "2 plots in mangrove/shrubs, 3 plots in river", 
      tract_no ==  13 ~ "3/5 closed forest, 2/5 bareland", 
      tract_no ==  68 ~ "could be shrubland", 
      tract_no ==  74 ~ "could be bareland with few trees, highly deviated", 
      tract_no ==  97 ~ "could be shrubland", 
      tract_no == 111 ~ "could be shrubland", 
      tract_no == 116 ~ "could be shrubland", 
      tract_no == 104 ~ "90% river. NFMA data include palm trees in the river, fake data or ID error?", 
      tract_no ==  84 ~ "confirmed nonforest", 
      tract_no == 123 ~ "confirmed nonforest", 
      tract_no == 124 ~ "deforested but should visit",
      tract_no == 134 ~ "deforested and burned but should visit",
      tract_no == 145 ~ "confirmed open forest", 
      tract_no ==  92 ~ "could be shrubland, highly deviated", 
      tract_no ==  49 ~ "90% water, no trees in NFMA", 
      tract_no ==  55 ~ "90% salt pans, no trees in NFMA",
      tract_no == 129 ~ "could be shrubs, not visited in NFMA",
      !is.na(nfma_lu_class) & !is.na(ceo_lu_class) ~ "agreement NFMA CEO",
      is.na(nfma_lu_class) & !is.na(ceo_lu_class) ~ "Tested open forest in CEO with NA in NFMA were open forest",  
      TRUE ~ "not cross-checked"
    )
  )

write_csv(t4_corr, "results/CEO-comparison/t4_corr.csv")


## NFMA closed forest to remove for sure:
## 84, 120, 123, 124 (deforested since NFMA), 134 (burned, probably deforested), 
## KEEP 145 (should still be open forest)

## KEEP ALL open forest no matter what says CEO

## Mangrove: all checks out except 
## > 132 (water maybe 1 plot in MG)

## CEO but not NFMA
## Closed forest 14, 92
## > 13 closed forest but probably swamp forest (wooded wetland in NFMA)
## > 92 open forest / shrubland (shrub in NFMA) - highly deviated
## Mangrove forest 49, 55, 129
## > 49 90% water, inaccessible, no trees in NFMA
## > 55 90% salt pans, accessible, no trees in NFMA (not visited most likely)
## > 129 mangrove forest should go, no record in NFMA
## Open forest
## First few tested had trees in NFMA but described as shrub / open shrubs.


## Close forest
##  3 matching CEO - NFMA 
## 14 CEO not in NFMA
## 4 NFMA not in CEO


## Check nb clusters

t1 |>
  filter(select_NFMA) |>
  summarise(count_clusters = n(), .by = c(nfma_lu_class))

t2 |>
  filter(select_CEO) |>
  summarise(count_clusters = n(), .by = c(ceo_lu_class))

t2a |>
  filter(select_CEO) |>
  summarise(count_clusters = n(), .by = c(ceo_lu_class60))

## Check
table(ceo_plot$`Select Forest Type?:Open Forest`, useNA = "ifany")
