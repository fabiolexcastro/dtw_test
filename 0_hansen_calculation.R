##################################
#### FOREST LOSS CALCULATIONS ####
##################################

# code by Raphael Ganzenmueller (LMU, CIAT)
# mail: ganzenmueller.r@posteo.de
# date: Feb 2020

#=====================================================

#libraries
library(raster)
library(gdalUtils)
library(rgdal)
library(sf)
library(tidyverse)

#=====================================================
# set working directory and pathes

# set working directory
setwd("~/colombia/publication_analysis")

# input path rasters
path_input_raster <- "data/hansen_v1-6"

# input path mun shapefile
path_input_shp <- "data/WGS84_MGN2019_00_COLOMBIA/ADMINISTRATIVO"

# input path natreg shapefile
path_input_shp_natreg <- "data/Municipios_Regiones_COL/"

# output path
path_output <- "output/data"

#=====================================================
# load files
#=====================================================

# load raster files
r1 <- raster(file.path(path_input_raster, "Hansen_GFC-2018-v1.6_lossyear_00N_070W.tif"))
r2 <- raster(file.path(path_input_raster, "Hansen_GFC-2018-v1.6_lossyear_00N_080W.tif"))
r3 <- raster(file.path(path_input_raster, "Hansen_GFC-2018-v1.6_lossyear_10N_070W.tif"))
r4 <- raster(file.path(path_input_raster, "Hansen_GFC-2018-v1.6_lossyear_10N_080W.tif"))
r5 <- raster(file.path(path_input_raster, "Hansen_GFC-2018-v1.6_lossyear_10N_090W.tif"))
r6 <- raster(file.path(path_input_raster, "Hansen_GFC-2018-v1.6_lossyear_20N_080W.tif"))
r7 <- raster(file.path(path_input_raster, "Hansen_GFC-2018-v1.6_lossyear_20N_090W.tif"))

# load shapefile
s_mun_orig <- st_read(dsn = file.path(path_input_shp, "MGN_MPIO_POLITICO.shp"))

# load natural regions shapefile
s_natreg_orig <- st_read(dsn = file.path(path_input_shp_natreg, "Municipios_Regiones_COL.shp"))

#=====================================================
# data preparation
#=====================================================

# from mun shapefile select relevant data, and project
s_mun <- s_mun_orig %>%  
  st_transform(crs=crs(r1)) %>%
  select(id=MPIO_CCNCT, dpt=DPTO_CNMBR, mun=MPIO_CNMBR, geometry) %>%
  arrange(id) 

### Change id, dpt, mun to character!! <- Warning message: Column `id` joining character vector and factor, coercing into character vector 

# create list of municipalities
mun_list <- s_mun%>%
  tbl_df() %>%
  select(id, dpt, mun) %>%
  mutate(id = as.character(id),
         dpt = as.character(iconv(dpt, to='ASCII//TRANSLIT')),
         mun = as.character(iconv(mun, to='ASCII//TRANSLIT')))

# create shape with colombia borders
scol <- s_mun%>%
  as("Spatial") %>%
  raster::aggregate() %>% 
  st_as_sf()

# shape as spatial polygon df
scol_spatial <- scol %>%
  as("Spatial")

# merge raster
r <- merge(r1, r2, r3, r4, r5, r6, r7)

# crop raster to colombia extent
r_crop <- crop(r, extent(scol))

# crop raster to colombia border shape
r_mask <- mask(r_crop, scol_spatial)

# save masked raster
writeRaster(r_mask, 
            file=file.path(path_output, "hansen_v1-6_mask.tif"), 
            format="GTiff", 
            overwrite=TRUE, 
            progress="text")

# load masked raster
#r_mask <- raster(file.path(path_output, "hansen_v1-6_mask.tif"))

#=====================================================
# calculate forest loss in whole colombia
#=====================================================

# zonal statistics
acol <- area(r_mask)
zcol <- zonal(acol, r_mask, 'sum')

# compare raster area and shape area
abs(sum(zcol[,2]) - (area(as(scol, "Spatial"))/1000000)) # difference in km2
abs(1-(sum(zcol[,2]) / (area(as(scol, "Spatial"))/1000000)))*100 # difference in percent

# compare raster area and wikipedia area entry
abs(sum(zcol[,2]) - 1141748) # difference in km2
abs(1-(sum(zcol[,2]) / 1141748))*100 # difference in percent

#=====================================================
# calculate forest loss in natural regions
#=====================================================

# prepare natreg data
shp_natreg <- s_natreg_orig %>%
  st_transform(crs=crs(r1)) %>%
  transmute(natreg = iconv(RegNat_Nom, to='ASCII//TRANSLIT'), 
            natreg = case_when(natreg == "Amazonia" ~ "Amazonas",
                               natreg == "Andina" ~ "Andes",
                               natreg == "Pacifico" ~ "Pacifico",
                               natreg == "Orinoquia" ~ "Orinoquia",
                               natreg == "Caribe" ~ "Caribe")) %>%
  group_by(natreg) %>%
  summarize(do_union=TRUE)

# function to look over natreg to extract forest loss areas 
df.natreg.function <- function(nnatreg){

    # select one natural region
  snatreg = shp_natreg %>%
    filter(natreg == nnatreg)

  # crop raster to colombia extent
  rnatreg_crop <- crop(r_mask, extent(snatreg))

  # crop raster to colombia border shape
  rnatreg_mask <- mask(rnatreg_crop, snatreg)

  # calculate zonal statistics
  anatreg <- area(rnatreg_mask)
  znatreg <- zonal(anatreg, rnatreg_mask, 'sum')
  dnatreg <- znatreg %>%
    tbl_df() %>%
    mutate(df = ifelse(nchar(zone) == 1, paste("df0", zone, "_ha", sep=""), paste("df", zone, "_ha", sep="")),
           area_ha = sum*100) %>%
    pivot_wider(id_cols=c(df, area_ha), names_from = df, values_from = area_ha) %>%
    transmute(gr=nnatreg, 
              no_df_ha = ifelse(exists("df00_ha"), df00_ha, 0),
              df01_ha = ifelse(exists("df01_ha"), df01_ha, 0),
              df02_ha = ifelse(exists("df02_ha"), df02_ha, 0),
              df03_ha = ifelse(exists("df03_ha"), df03_ha, 0),
              df04_ha = ifelse(exists("df04_ha"), df04_ha, 0),
              df05_ha = ifelse(exists("df05_ha"), df05_ha, 0),
              df06_ha = ifelse(exists("df06_ha"), df06_ha, 0),
              df07_ha = ifelse(exists("df07_ha"), df07_ha, 0),
              df08_ha = ifelse(exists("df08_ha"), df08_ha, 0),
              df09_ha = ifelse(exists("df09_ha"), df09_ha, 0),
              df10_ha = ifelse(exists("df10_ha"), df10_ha, 0),
              df11_ha = ifelse(exists("df11_ha"), df11_ha, 0),
              df12_ha = ifelse(exists("df12_ha"), df12_ha, 0),
              df13_ha = ifelse(exists("df13_ha"), df13_ha, 0),
              df14_ha = ifelse(exists("df14_ha"), df14_ha, 0),
              df15_ha = ifelse(exists("df15_ha"), df15_ha, 0),
              df16_ha = ifelse(exists("df16_ha"), df16_ha, 0),
              df17_ha = ifelse(exists("df17_ha"), df17_ha, 0),
              df18_ha = ifelse(exists("df18_ha"), df18_ha, 0),
              shape_area_ha = area(as(snatreg, "Spatial"))/10000,
              raster_area_ha = sum(znatreg[,2])*100,
              sr_diff_area_ha = abs(shape_area_ha - raster_area_ha),
              sr_diff_ratio = abs(1-(shape_area_ha/raster_area_ha)))
  }

# run function over all municipalities
df_natreg <- do.call(rbind.data.frame, lapply(shp_natreg$natreg, df.natreg.function))

# compare raster and shape area
abs(sum(df_natreg$raster_area_ha)/100 - sum(df_natreg$shape_area_ha)/100) # difference in km2
abs(1-sum(df_natreg$raster_area_ha) / sum(df_natreg$shape_area_ha))*100 # difference in percent

# compare raster area and wikipdia area entry
abs((sum(df_natreg$raster_area_ha)/100) - 1141748) # difference in km2
abs(1-((sum(df_natreg$raster_area_ha)/100) / 1141748))*100 # difference in percent

#=====================================================
# calculate forest loss in municipalities
#=====================================================

df.mun.function <- function(munid){
  # select one municipality
  smun <- s_mun %>%
    filter(id == munid)
  
  # crop raster to colombia extent
  rmun_crop <- crop(r_mask, extent(smun))
  
  # crop raster to colombia border shape
  rmun_mask <- mask(rmun_crop, smun)
  
  # calculate zonal statistics
  amun <- area(rmun_mask)
  zmun <- zonal(amun, rmun_mask, 'sum')
  dmun <- zmun %>%
    tbl_df() %>%
    mutate(df = ifelse(nchar(zone) == 1, paste("df0", zone, "_ha", sep=""), paste("df", zone, "_ha", sep="")),
           area_ha = sum*100) %>%
    pivot_wider(id_cols=c(df, area_ha), names_from = df, values_from = area_ha) %>%
    transmute(id=munid, 
              no_df_ha = ifelse(exists("df00_ha"), df00_ha, 0),
              df01_ha = ifelse(exists("df01_ha"), df01_ha, 0),
              df02_ha = ifelse(exists("df02_ha"), df02_ha, 0),
              df03_ha = ifelse(exists("df03_ha"), df03_ha, 0),
              df04_ha = ifelse(exists("df04_ha"), df04_ha, 0),
              df05_ha = ifelse(exists("df05_ha"), df05_ha, 0),
              df06_ha = ifelse(exists("df06_ha"), df06_ha, 0),
              df07_ha = ifelse(exists("df07_ha"), df07_ha, 0),
              df08_ha = ifelse(exists("df08_ha"), df08_ha, 0),
              df09_ha = ifelse(exists("df09_ha"), df09_ha, 0),
              df10_ha = ifelse(exists("df10_ha"), df10_ha, 0),
              df11_ha = ifelse(exists("df11_ha"), df11_ha, 0),
              df12_ha = ifelse(exists("df12_ha"), df12_ha, 0),
              df13_ha = ifelse(exists("df13_ha"), df13_ha, 0),
              df14_ha = ifelse(exists("df14_ha"), df14_ha, 0),
              df15_ha = ifelse(exists("df15_ha"), df15_ha, 0),
              df16_ha = ifelse(exists("df16_ha"), df16_ha, 0),
              df17_ha = ifelse(exists("df17_ha"), df17_ha, 0),
              df18_ha = ifelse(exists("df18_ha"), df18_ha, 0),
              shape_area_ha = area(as(smun, "Spatial"))/10000,
              raster_area_ha = sum(zmun[,2])*100,
              sr_diff_area_ha = abs(shape_area_ha - raster_area_ha),
              sr_diff_ratio = abs(1-(shape_area_ha/raster_area_ha)))
}

# run function over all municipalities
df_mun <- do.call(rbind.data.frame, lapply(mun_list$id, df.mun.function))

# compare raster and shape area
abs(sum(df_mun$raster_area_ha)/100 - sum(df_mun$shape_area_ha)/100) # difference in km2
abs(1-sum(df_mun$raster_area_ha) / sum(df_mun$shape_area_ha))*100 # difference in percent

# compare raster area and wikipdia area entry
abs((sum(df_mun$raster_area_ha)/100) - 1141748) # difference in km2
abs(1-((sum(df_mun$raster_area_ha)/100) / 1141748))*100 # difference in percent

# compare shape area and wikipedia area entry
abs((sum(df_mun$shape_area_ha)/100) - 1141748) # differnce in km2
abs(1-((sum(df_mun$shape_area_ha)/100) / 1141748))*100 # difference in percent

# compare raster single municipalities and raster colombia
abs((sum(df_mun$raster_area_ha)/100) - sum(zcol[,2])) # difference in km2
abs(1-((sum(df_mun$raster_area_ha)/100) / sum(zcol[,2])))*100 # difference in percent

# compare shape single municipalities and shape colombia
abs((sum(df_mun$shape_area_ha)/100) - (area(scol_spatial)/1000000)) # difference in km2 # difference due to rounding?!
abs(1-((sum(df_mun$shape_area_ha)/100) / (area(scol_spatial)/1000000)))*100 # difference in percent

#=====================================================
# save files
#=====================================================

# save municipality list
saveRDS(mun_list, file.path(path_output, "mun_list.rds"))

# save mun shapefile
saveRDS(s_mun, file.path(path_output, "shp_mun.rds"))
#writeOGR(s_mun %>% as("Spatial"), path_output, "shp_mun.shp" , driver="ESRI Shapefile")

# save natreg shapefile
saveRDS(shp_natreg, file.path(path_output, "shp_natreg.rds"))

# save forest loss municipality data
saveRDS(df_mun, file.path(path_output,"hansen_v1-6_mun_ha.rds"))

# save forest loss municipality data
saveRDS(df_natreg, file.path(path_output,"hansen_v1-6_natreg_ha.rds"))

#=====================================================
####END####