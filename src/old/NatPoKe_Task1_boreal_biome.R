# NatPoKe Task1 for Meeting: Tuesday 14th January 2024
# From: Jorinde
# Tasks:
# 1. Load one scenario in R, try to crop it, mask it to the boreal biome
# 2. Try to change its cell resolution and project in two different coordinate systems

# packages
library(terra)
library(ggplot2)
library(dplyr)
library(sf)

# import data scenario
PESLC_PNAS<-rast("data/pnas_lulc_dist20240207a/pnas_lulc_seals5_gtap1_rcp45_ssp2_2030_PESLC.tif")
PESLC_PNAS

# import data ecosystem biome
shapefile_path<-"data/6kcchn7e3u_official_teow/official/wwf_terr_ecos.shp"
biome_sf<-st_read(shapefile_path)
biome_terra<-vect(shapefile_path)

# inspect biome data
st_geometry_type(biome_sf)
st_crs(biome_sf)


### WWF ecosystem data needs to be croped to boreal biome - how to define the boreal biome?
names(biome_terra)
biome_column<-biome_terra$BIOME
print(biome_column)




# crop the scenario raster to the biome extent
biome_terra<-project(biome_terra, crs(PESLC_PNAS))
PESLC_PNAS_cropped_biome<-crop(PESLC_PNAS, biome_terra)
res(PESLC_PNAS_cropped_biome)
res(biome_terra)

# simplify vector layer
biome_terra<-aggregate(biome_terra,dissolve=TRUE)

# reduce raster size by changing cell resolution to factor 100
PESLC_PNAS_cropped_biome_res<-aggregate(PESLC_PNAS_cropped_biome, fact=100, fun=mean)
res(PESLC_PNAS_cropped_biome_res)

# mask the scenario with biome_terra
PESLC_PNAS_masked_biome<-mask(PESLC_PNAS_cropped_biome_res, biome_terra)

# project into two different coordinate systems: 
# UTM 33N (central Europe and nothern Europe)
crs(PESLC_PNAS_masked_biome)
PESLC_PNAS_biome_utm33N<-project(PESLC_PNAS_masked_biome, crs("EPSG:32633"))
crs(PESLC_PNAS_biome_utm33N)

# UTM 6N (North America)
PESLC_PNAS_biome_utm6N<-project(PESLC_PNAS_masked_biome, crs("EPSG:32606"))
crs(PESLC_PNAS_biome_utm6N)

# check scenario raster and crs
PESLC_PNAS_biome_utm33N
PESLC_PNAS_biome_utm6N



# convert to data frame
PESLC_PNAS_biome_utm33N_df<-as.data.frame(PESLC_PNAS_biome_utm33N, xy=TRUE)
str(PESLC_PNAS_biome_utm33N_df)

# plot data
ggplot()+
  geom_raster(data=PESLC_PNAS_biome_utm33N_df,
              aes(x=x,y=y, fill =pnas_lulc_seals5_gtap1_rcp45_ssp2_2030_PESLC))+
  scale_fill_viridis_c(name="?Amount of habitat preserved")+
  xlab("Longitude")+
  ylab("Latitude")+
  ggtitle("Payment of Ecosystem Services - domestic carbon forest")+
  coord_quickmap()



# ?To plot a map, number of bins needs to be corrected according to LULC types?, histogram and bins of the data
PESLC_PNAS_boreal_res
ggplot()+
  geom_histogram(data=PESLC_PNAS_boreal_res_df, aes(pnas_lulc_seals5_gtap1_rcp45_ssp2_2030_PESLC))


