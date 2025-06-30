## Make maps of tree positions from NFMA plots
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


## Data preparation 
lus <- lus_init |> select(tract_no, plot_no, lus_no, lus_code, lus, lus_class)

tree <- tree_init |>
  mutate(
    tract_no = as.numeric(str_sub(plot_id, 8, 10)),
    plot_no = as.numeric(str_sub(plot_id, -1))
  ) |>
  select(country, iso, plot_id, tract_no, plot_no, lus_no, tree_no, everything()) |>
  left_join(lus, by = join_by(tract_no, plot_no, lus_no))


##
## Country analysis - tree position ####
##

## Checks
# table(lus$lus)
# 
# tt <- lus |> summarise(count = n(), .by = c(tract_no, plot_no))
# 
# table(tt$count)
# 
# length(unique(lus$plot_id)) / 4

## For each cluster make tree location maps
list_tract <- tree |>
  mutate(
    tract_id = str_sub(plot_id, end = -2)
  ) |>
  pull(tract_id) |>
  unique() |>
  sort()

gg_tracts <- map(list_tract, function(x){
  
  #tract_id <- plot_no |> filter(tract_id == x) |> pull(tract_id) |> unique()
  
  tree |> 
    filter(plot_id %in% paste0(x, 1:4)) |>
    mutate(
      plot_no = str_remove(plot_id, pattern = x),
      lus_id = paste0(plot_id, "0", lus_no)
    ) |>
    ggplot(aes(tree_x, tree_y)) +
    geom_point(aes(color = tree_species_name, shape = lus), size = 2) +
    ggrepel::geom_text_repel(aes(label = paste0(tree_dbh, " cm")), min.segment.length = 0, size = 2) +
    theme_bw() + 
    coord_fixed(ratio = 1/2) +
    facet_wrap(~plot_no, nrow = 1) + 
    scale_y_continuous(breaks = 0:25 * 10, minor_breaks = NULL, limits = c(5, 245)) +
    scale_x_continuous(breaks = c(-10, 0, 10), minor_breaks = c(-5, 0, 5), limits = c(-11, 11)) +
    labs(
      subtitle = paste0("Tree position in plots 1-4 for Tract: ", str_sub(x, 8, 10)),
      color = "",
      shape = "",
      x = "Distance to plot center line (X, in m)",
      y = "Distance along the plot center line (Y, in m)",
      caption = "Tree DBH in annotation\nPlot ratio y/x = 1/2"
    )
  
})

names(gg_tracts) <- list_tract

## Write all plots

if (!"tree-maps" %in% list.files("results")) dir.create("results/tree-maps")

walk(list_tract, function(x){
  
  tract_no <- str_sub(x, 8, 10)
  
  ggsave(
    plot = gg_tracts[[x]], 
    filename = paste0("results/tree-maps/GMB-tree-position-tract-", tract_no, ".png"),
    height = 17, width = 14, units = "cm", dpi = 300
  )
  
})
