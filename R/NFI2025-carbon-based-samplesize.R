
## Calculate land use class AGB and use it for Neymann allocation of plots per land cover class
## Gaël Sola, FAO, June 2025


## Initiation 
source("R/get-pkg.R")
source("R/get-data.R")

if (!"ceo_extra.csv" %in% list.files("results/CEO-comparison")) source("R/NFI20205-ceo-analysis.R")


##
## Load data ####
##

## + Chave E and WD ####
rs_E <- terra::rast("data-anci/E.bil")
#plot(rs_E)

wd_init <- read_csv("data-anci/wdData.csv", show_col_types = F) |>
  select(wd_no = Number, wd_family_name = Family, wd_species_name = Binomial, wd_gcm3 = `Wood density (g/cm^3), oven dry mass/fresh volume`, wd_region = Region)
#table(wd_init$wd_region)

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

## + CEO corrected data ####
## > Requires running ceo-analysis.R to get the full corrections and 
##   comparison of NFMA and, ROOTS and CEO land use

ceo_extra <- read_csv("results/CEO-comparison/ceo_extra.csv")
ceo_tract_corr <- read_csv("results/CEO-comparison/t4_corr.csv") |>
  select(tract_no, type, lu_corr) |>
  distinct() |>
  arrange(tract_no)

table(ceo_tract_corr$lu_corr, useNA = "ifany")

## Solve duplicates in CEO tract corr
dup_tract <- ceo_tract_corr |>
  summarise(count = n(), .by = tract_no) |>
  filter(count > 1) |>
  pull(tract_no)

ceo_tract_corr <- ceo_tract_corr |> filter(!(tract_no %in% dup_tract & !str_detect(lu_corr, "likely")))

## + Get country boundary ####
## NOT NEEDED
# sf_country <- st_read("data-core/Admin boundries/LG_Areas.shp") |> 
#   st_transform(4326)
# # plot(sf_country)
# 
# sf_country2 <- sf_country |> summarise()
# 
# gb_area <- as.numeric(st_area(sf_country2))/10000




## 
## PRELIMINARY CALCULATIONS ####
##

## + Update CEO land use with corrections from t4 ####
max_nfma <- ceo_extra |>
  filter(type == "nfma_track") |>
  pull(tract_no) |>
  max()


ceo_corr <- ceo_extra |>
  left_join(ceo_tract_corr, by = join_by(tract_no, type)) |>
  arrange(desc(type), tract_no) |>
  mutate(
    lu_class_final = case_when(
      str_detect(lu_corr, "likely") ~ ceo_lu_class,
      is.na(lu_corr) ~ ceo_lu_class,
      TRUE ~ lu_corr
    ),
    tract_no_new = if_else(type == "nfma_track", tract_no, tract_no + max_nfma),
    tract_id = case_when(
      type == "nfma_track" & tract_no < 10   ~ paste0("nfma00", tract_no), 
      type == "nfma_track" & tract_no < 100  ~ paste0("nfma0" , tract_no),
      type == "nfma_track" & tract_no >= 100 ~ paste0("nfma"  , tract_no),
      type != "nfma_track" & tract_no < 10   ~ paste0("00", tract_no), 
      type != "nfma_track" & tract_no < 100  ~ paste0("intens0" , tract_no),
      type != "nfma_track" & tract_no >= 100 ~ paste0("intens"  , tract_no),
      TRUE ~ NA_character_
    )
  )


## + Make species WD averages for Africa ####

wd_sp <- wd_init |> 
  filter(wd_region == "Africa (tropical)") |>
  group_by(wd_species_name) |>
  summarise(
    count_avg_species = n(),
    wd_avg_species = mean(wd_gcm3), 
    wd_std_species = sd(wd_gcm3),
    .groups = "drop"
  )

wd_gn <- wd_init |> 
  filter(wd_region == "Africa (tropical)") |>
  mutate(wd_genus_name = word(wd_species_name)) |>
  group_by(wd_genus_name) |>
  summarise(
    count_avg_genus = n(),
    wd_avg_genus = mean(wd_gcm3), 
    wd_std_genus = sd(wd_gcm3),
    .groups = "drop"
  )

wd_fm <- wd_init |> 
  filter(wd_region == "Africa (tropical)") |>
  group_by(wd_family_name) |>
  summarise(
    count_avg_family = n(),
    wd_avg_family = mean(wd_gcm3), 
    wd_std_family = sd(wd_gcm3),
    .groups = "drop"
  )

wd_unk <- wd_init |> 
  filter(wd_region == "Africa (tropical)") |>
  summarise(
    count_avg_unknown = n(),
    wd_avg_unknown = mean(wd_gcm3), 
    wd_std_unknown = sd(wd_gcm3),
    .groups = "drop"
  )


## + Get land use section areas ####
lus <- lus_init |>
  mutate(area_ha = round(plot_length * plot_width / 10000, 3)) |>
  select(tract_no, plot_no, lus_no, lus, lus_class, area_ha)

## + Get E at plot level ####
sf_plot_gps <- plot_gps_init |>
  mutate(
    tract_no = as.numeric(str_sub(tract_id, -3)),
    plot_no  = as.numeric(str_sub(plot_id, -1)),
    x = plot_gps_x_middlepoint, 
    y = plot_gps_y_middlepoint
  ) |>
  select(tract_no, plot_no, x, y) |>
  filter(!is.na(x), !is.na(y)) |>
  st_as_sf(coords = c("x", "y"), crs = 32628) |>
  st_transform(crs = 4326)

tmp_E <- terra::extract(rs_E, vect(sf_plot_gps))

## CHECK
# ggplot() +
#   geom_spatraster(data = rs_E) +
#   geom_sf(data = sf_plot_gps) +
#   coord_sf(
#     xlim = st_bbox(sf_plot_gps)[c("xmin", "xmax")],
#     ylim = st_bbox(sf_plot_gps)[c("ymin", "ymax")]
#     )

plot_E <- sf_plot_gps |>
  bind_cols(E = tmp_E[,2]) |>
  as_tibble() |>
  select(-geometry)


## + Get LUS and E at tree level ####

tree <- tree_init |>
  mutate(
    tract_no = as.numeric(str_sub(plot_id, 8, 10)),
    plot_no = as.numeric(str_sub(plot_id, start = -1)),
    tree_genus_name = word(tree_species_name),
    tree_species_epithet = word(tree_species_name, 2),
    tree_species_short = paste(tree_genus_name, tree_species_epithet)
  ) |>
  select(country, iso, tract_no, plot_no, lus_no, everything()) |>
  left_join(lus, by = join_by(tract_no, plot_no, lus_no)) |>
  left_join(plot_E, by = join_by(tract_no, plot_no)) |>
  left_join(wd_sp, by = join_by(tree_species_short == wd_species_name)) |>
  left_join(wd_gn, by = join_by(tree_genus_name == wd_genus_name)) |>
  mutate(
    tree_wd = case_when(
      !is.na(wd_avg_species) ~ wd_avg_species,
      !is.na(wd_avg_genus) ~ wd_avg_genus,
      TRUE ~ wd_unk$wd_avg_unknown
    ),
    tree_wd_level = case_when(
      !is.na(wd_avg_species) ~ "species",
      !is.na(wd_avg_genus) ~ "genus",
      TRUE ~ "regional"
    ),
    tree_height_chave = exp(0.243^2/2)* exp(0.893 - E + 0.760*log(tree_dbh) - 0.0340*(log(tree_dbh))^2),
    tree_h_ciup = tree_height_chave * exp(1.96 * 0.243),
    tree_h_cilo = tree_height_chave * exp(-1.96 * 0.243),
    tree_agb_chave_noh = exp(-1.803 - 0.976*E + 0.976*log(tree_wd) + 2.673*log(tree_dbh) - 0.0299*(log(tree_dbh))^2),
    tree_agb_chave = 0.0673 * (tree_wd * tree_dbh^2 * tree_height_top)^0.976
  )


## Checks
# tree |>
#   filter(plot_no == 1) |>
#   ggplot(aes(x = tree_dbh)) +
#   geom_point(aes(y = tree_height_top))

tree |>
  mutate(tree_health = as.character(tree_health)) |>
  filter(tree_dbh < 100) |>
  filter(plot_no == 1) |>
  ggplot(aes(x = tree_dbh)) +
  geom_point(aes(y = tree_height_top, colour = tree_health), alpha = 0.6) +
  facet_wrap(~tree_health)

# tree |>
#   filter(tree_dbh < 100) |>
#   filter(plot_no == 1) |>
#   ggplot(aes(x = tree_dbh)) +
#   geom_point(aes(y = tree_height_top, colour = as.character(tree_health))) +
#   geom_line(aes(y = tree_height_chave, colour = plot_id)) +
#   geom_line(aes(y = tree_h_ciup, colour = plot_id)) +
#   geom_line(aes(y = tree_h_cilo, colour = plot_id)) +
#   theme(legend.position = "none")

# table(tree$tree_health, useNA = "ifany")

# tree |>
#   filter(!is.na(tree_health)) |>
#   ggplot(aes(x = tree_dbh)) +
#   geom_point(aes(y = tree_agb_chave))



## 
## MAIN CALCULATIONS ####
##

## CHECK - Tree positioning > OK
# lus_full <- lus |>
#   filter(plot_no == 1, area_ha == 0.5) |>
#   mutate(lus_id = tract_no * 100 + plot_no * 10 + lus_no) |>
#   pull(lus_id)
# 
# tree_pos <- tree |> 
#   mutate(lus_id = tract_no * 100 + plot_no * 10 + lus_no) |>
#   filter(lus_id %in% lus_full, tree_dbh >= 20)
# 
# tree_pos |>
#   ggplot() +
#   geom_point(aes(x = tree_x, y = tree_y), alpha = 0.3) +
#   coord_fixed(ratio = 0.5)

## + Cstock per land cover class ####

plot_lus_agb <- tree |>
  filter(!is.na(tree_health)) |>
  #filter(plot_no == 1) |>
  #group_by(tract_no, lus_class, lus,  area_ha) |>
  group_by(tract_no, plot_no, lus_class, lus,  area_ha) |>
  summarise(
    count = n(),
    lus_agb = sum(tree_agb_chave) / 1000, 
    .groups = "drop" 
  ) 

tract_agb <- plot_lus_agb |>
  group_by(tract_no, lus_class, lus) |>
  summarise(
    tree_count = sum(count),
    area_ha = sum(area_ha),
    plot_agb = sum(lus_agb), 
    .groups = "drop"
  ) |>
  mutate(
    plot_agb_ha = round(plot_agb / area_ha,  3)
  ) |>
  filter(!is.na(plot_agb_ha))

tract_agb |>
  ggplot(aes(x = lus_class)) +
  geom_jitter(aes(y = plot_agb_ha), alpha = 0.6, col = "darkred") +
  geom_boxplot(aes(y = plot_agb_ha), fill = NA) +
  coord_flip()

class_agb <- tract_agb |>
  group_by(lus_class) |>
  summarise(
    tract_count = n(),
    agb_ha = mean(plot_agb_ha),
    agb_sd = sd(plot_agb_ha)
  )
class_agb

overall_agb <- tract_agb |>
  filter(lus_class != "nonforest") |>
  summarise(
    tract_count = n(),
    agb_ha = mean(plot_agb_ha),
    agb_sd = sd(plot_agb_ha)
  )
overall_agb



## 
## SAMPLING DESIGN: sample size ####
##

## + Overall sample size ####
E <- 20
t <- 1.96 

n <- ceiling(((overall_agb$agb_sd / overall_agb$agb_ha * 100) * t/E)^2)
n

## + Stratified sampling with Neyman allocation ####
table(ceo_corr$lu_class_final)

n_ceoplot <- nrow(ceo_corr)

strata_weight <- ceo_corr |>
  filter(lu_class_final != "nonforest") |>
  summarise(ceo_count = n(), .by = "lu_class_final") |>
  mutate(weight = ceo_count / n_ceoplot) |>
  left_join(class_agb, by = join_by(lu_class_final == lus_class)) |>
  mutate(
    WhSh = weight * agb_sd,
    mean_prop = weight * agb_ha
  )

sum_WhSh <- sum(strata_weight$WhSh)
mean_st  <- sum(strata_weight$mean_prop)

n_st <- ceiling(t^2 * sum_WhSh^2 / (mean_st * E / 100)^2)
n_st

## + fixed sample size allocation ####
n_cost <- 120

strata_ss <- strata_weight |> 
  mutate(
    nh = ceiling(n_cost * WhSh / sum_WhSh),
    neq = ceiling(n_cost / nrow(strata_weight)),
    n_mix = ceiling((nh + neq) / 2)
  )
strata_ss

sum(strata_ss$nh)
sum(strata_ss$n_mix)
E_st <-  t * sum_WhSh / (mean_st / 100 * sqrt(n_cost))
E_st


## Check CEO on NFMA and assign ####
ceo_tract_init <- ceo_corr |> 
  summarise(count_plot = n(), .by = c(tract_no_new, tract_no, type, tract_id, lu_class_final)) |>
  arrange(tract_no_new)

ceo_tract_max <- ceo_tract_init |>
  summarise(count_max = max(count_plot), .by = tract_no_new)  |>
  mutate(majority = TRUE)

ceo_tract <- ceo_tract_init |>
  left_join(ceo_tract_max, by = join_by(tract_no_new, count_plot == count_max)) |>
  filter(majority)

nrow(ceo_tract) == length(unique(ceo_corr$tract_no_new))

## Find dup
tract_dup <- ceo_tract |>
  summarise(count = n(), .by = tract_no_new) |>
  filter(count > 1) |>
  pull(tract_no_new)

tt <- ceo_corr |> filter(tract_no_new %in% tract_dup)

## Reclassify mix of closed and open forest into open forest
## Reclassify mix of mangrove and closed forest into closed forest
tract_corr_dup <- tibble(
    tract_no_new   = c(90, 219, 349, 525, 565, 640, 661),
    tract_no       = c(90, 67, 197, 373, 413, 488, 509),
    type           = c("nfma_track", rep("intense_cluster", 6)),
    tract_id       = c("nfma090", "intens067", "intens197", "intens373", "intens413", "intens488", "intens509"),
    lu_class_final = c(rep("open forest", 5), "closed forest", "open forest"),
    count_plot = c(rep(3, 5), 4, 3),
    majority = rep(TRUE, 7)
    )

ceo_tract_final <- ceo_tract |>
  filter(!tract_no_new %in% tract_dup) |>
  bind_rows(tract_corr_dup) |>
  arrange(tract_no_new)

nrow(ceo_tract_final) == length(unique(ceo_corr$tract_no_new))

write_csv(ceo_tract_final, "results/tract-final-allocation.csv")


table(ceo_tract_final$lu_class_final)
table(ceo_tract_final$type, ceo_tract_final$lu_class_final)


t_nfma <- ceo_tract_final |> filter(tract_no_new <= 152)
table(t_nfma$lu_class_final, useNA = "ifany")

t_intens <- ceo_tract_final |> filter(tract_no_new > 152)
table(t_intens$lu_class_final, useNA = "ifany")

##
## Define cluster selection ####
##

## Closed forest
assign_plot <- function(.strata_name, .ceo, .ceo_tract, .samplesize){
  
  ## !!! FOR TESTING ONLY
  # .strata_name = "open forest"
  # .ceo = ceo_corr
  # .ceo_tract = ceo_tract_final
  # .samplesize = 67
  ## !!!
  
  ## Subset CEO data from desired strata
  ceo_tract_nfma <- .ceo_tract |> filter(lu_class_final == .strata_name, type == "nfma_track")
  ceo_tract_intens <- .ceo_tract |> filter(lu_class_final == .strata_name, type == "intense_cluster")
  
  if (nrow(ceo_tract_sub) <= .samplesize) {
    ## take all samples 
    strata_sample <- ceo_corr |> 
      filter(tract_no_new %in% ceo_tract_nfma$tract_no_new) |>
      select(tract_no_new, tract_no, type, plot_no, center_lon, center_lat, lu_class_final) |>
      mutate(lu_cluster = .strata_name)
    
  } else {
    ## take all NFMA and take the rest randomly from INTENS
    strata_sample1 <- ceo_corr |> 
      filter(tract_no_new %in% ceo_tract_nfma$tract_no_new) |>
      select(tract_no_new, tract_no, type, plot_no, center_lon, center_lat, lu_class_final) |>
      mutate(lu_cluster = .strata_name)
    
    sample_tract_intens <- sample(ceo_tract_intens$tract_no_new, size = (.samplesize - nrow(ceo_tract_nfma)))
    strata_sample2 <- ceo_corr |> 
      filter(tract_no_new %in% sample_tract_intens) |>
      select(tract_no_new, tract_no, type, plot_no, center_lon, center_lat, lu_class_final) |>
      mutate(lu_cluster = .strata_name)
    
    strata_sample <- bind_rows(strata_sample1, strata_sample2)
  }
  
  
} ## End function

table(ceo_tract_final$lu_class_final)
strata_ss
set.seed(44)

cf_plot <- assign_plot(.strata_name = "closed forest", .samplesize = 24, .ceo = ceo_corr, .ceo_tract = ceo_tract_final)
mg_plot <- assign_plot(.strata_name = "mangrove forest", .samplesize = 29, .ceo = ceo_corr, .ceo_tract = ceo_tract_final)
of_plot <- assign_plot(.strata_name = "open forest", .samplesize = 67, .ceo = ceo_corr, .ceo_tract = ceo_tract_final)

NFI_PLOT <- bind_rows(cf_plot, mg_plot, of_plot)

NFI_CLUSTER <- NFI_PLOT |>
  filter(plot_no == 1) |>
  select(-lu_class_final, -plot_no)

table(NFI_CLUSTER$lu_cluster)

write_csv(NFI_PLOT, "results/NFI_PH2_PLOT.csv")
write_csv(NFI_CLUSTER, "results/NFI_PH2_CLUSTER.csv")
