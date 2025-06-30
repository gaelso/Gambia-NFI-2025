# Gambia-NFI-2025
Analysis of historical NFMA inventory and CEO survey to prepare a re-measurement campaign of forest field plots

## NFMA data

The following tables are prepared from the 'NFI-modules-2021-data' R project:
- GMB-tract.csv
- GMB-tract_raw.csv
- GMB-plot.csv
- GMB-plot_gps.csv
- GMB-lus.csv
- GMB-tree.csv

These tables regroup the main information of tract, plot, land use sections and tree 
collected during the NFMA campaign in the Gambia 

The 'NFI-modules-2021-data' R project makes a grouped analysis of the main NFAM surveys across
three continents.

In the original NFMA grid, the tract center point positions are shifted from a 5 arcminutes grid. 
These coordinates are in tract_raw table or in the tract_5x5_true_utm shapefile.

!!! Important:
the intensified grid at 2.5 arcminutes grid distance is not shifted and aligned to the southwestern most point, i.e. tract 152.


## CEO survey

The CEO survey was collect in May 2025 over a 5 arc min grid overlapping NFMA plot 1 
of NFMA tracts and an intensification grid at 2.5 arc min.

Survey link on Collect Earth Online: https://app.collect.earth/review-institution?institutionId=5770

The collected data went through a first round of correction to make the csv files used in R (see 'data-core/CEO_corr').


## ROOTS map
The ROOTS map was initially downloaded directly from the Google Earth Engine recipe. See the info note:
https://drive.google.com/file/d/1uaa-Q4bmcccQ-6rGK-SdBSr2HngHGH9m/view
And the GEE recipe: 
https://code.earthengine.google.com/8568e16d66b811173e31620a2e47764c

To download the raster data, go to the GEE right panel > unsubmitted tasks > Run lc_gambia_2023_10m

## Administrative boundaries and Land use land cover 2023

These two spatial data were provided by the forest administration as shapefile.


## Ancillary data

The carbon stock was calculated with Chave et al., 2014 pan-tropical model:
https://onlinelibrary.wiley.com/doi/abs/10.1111/gcb.12629

The environmental factor based tree height model was used to check for unrealistic tree height, but the 
general aboveground biomass equation was used with measured tree height.

Wood density at species or genus level were derived from Zanne et al 2009 global wood density database,
with species and genus averages calculated from the Tropical Africa region.


## Proposed NFI design

The proposed updated design is a 2 stage stratified sampling with an hybrid semi-variance optimization combining
Neyman allocation and equal allocation to ensure a minimum number of clusters per strata.

The cluster/plot design partially covers NFMA plot 1 of each NFMA tract, with a 5 plots per clusters in a cross shape.
Plot 1, 2 and 4 cover the middle, end and start of NFMA plot 1 while plot 3 and 5 are shifted 100 m east and west of the NFMA plot 1 middle point.

Plots are rectangles of 50 x 20 m and the distance between plot centers is 100 m.

The plot size for 20% allowable error on biomass is ~120 plots. Their distribution is based on the 5 arcmin NFMA grid, with an intensification of extra cluster randomly on the 2.5 arcminute grid 


