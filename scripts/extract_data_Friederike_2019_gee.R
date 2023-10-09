# Friederike 2018-2019
library(here)
library(terra)
library(data.table)
library(dplyr)
library(stringr)
source(local=TRUE, r"{scripts/functions.R}")

### Load data
#### Load raster data
rpast <- rast(im2list_11bands(r"{Y:\Andrea\wsf-sat\data\s2_mosaic_gee\s2_2018\gee_2018_11bands_mistrikli.tif}"))
rpresent <- rast(im2list_11bands(r"{Y:\Andrea\wsf-sat\data\s2_mosaic_gee\s2_2019\gee_2019_11bands_mistrikli.tif}"))

names(rpast) <- paste(names(rpast), '_past', sep="")
names(rpresent) <- paste(names(rpresent), '_present', sep="")

#### Load geometry data
# Geometries
damage_polygons <- vect(r"{D:\wsf-sat\data\validation\Friedericke_2019\fried_2019_kartiergebiet_zeitangepasst_nachkartiert_260421.gpkg}", layer = "filter025")
no_damage_points <- vect(here("data", "Referenz_NichtSchaden_randompoints_Friedericke_2019.gpkg"))

damage_polygons_table <- data.table(as.data.frame(damage_polygons))
no_damage_points_table <- data.table(as.data.frame(no_damage_points))

#### Prepare geometry data
# Use objektart to classify in damage class, then drop it 
damage_polygons_table <- 
  damage_polygons_table %>%
  select("Objektart") %>% 
  mutate(damage_type = case_when(
    Objektart == 10 ~ 3, # windwurf geräumt to Freifläche
    Objektart == 20 ~ 2, # windwurf ungeräumt to liegendes Totholz
    Objektart == 21 ~ 3, # kahlflächen ohne ursache to Freifläche
    Objektart ==30 ~ 1 # stehendes Totholz to stehendes Totholz
  ))  %>% 
  select(-"Objektart") %>%
  mutate(damage_class = 1) %>% 
  mutate(year = 2019)

#### Calculate difference bands
rdiff <- rpresent - rpast
names(rdiff) <- str_replace(names(rdiff), "_present", "_diff")

#writeRaster(rdiff, filename=here("output", "Friedericke_2019_bands.tif"), filetype="GTiff", datatype='INT4S', overwrite=TRUE)  


#### Calculate indices and differences
# List with rasters
indices_past <- calculate_indices(rpast)
indices_present <- calculate_indices(rpresent)

names(indices_past) <- paste(names(indices_past), '_past', sep="")
names(indices_present) <- paste(names(indices_present), '_present', sep="")

# Multiband rasters
rindices_past <- rast(indices_past)
rindices_present <- rast(indices_present) 

rindices_diff <- rindices_present - rindices_past
names(rindices_diff) <- str_replace(names(rindices_diff), "_present", "_diff")

#writeRaster(rindices_past, filename=here("output", "Friedericke_2019_indices_past.tif"), filetype="GTiff", datatype='INT4S', overwrite=TRUE)  
#writeRaster(rindices_present, filename=here("output", "Friedericke_2019_indices_present.tif"), filetype="GTiff", datatype='INT4S', overwrite=TRUE)  
#writeRaster(rindices_diff, filename=here("output", "Friedericke_2019_indices_diff.tif"), filetype="GTiff", datatype='INT4S', overwrite=TRUE)  

#### Prepare extract raster
rextract <- c(rpast, rpresent, rdiff, rindices_past, rindices_present, rindices_diff)
rm(rpast, rpresent, rdiff, rindices_past, rindices_present, rindices_diff)

#### Extract data to points
extract_values_damage <- extract(rextract, damage_polygons) 
extract_values_no_damage <- extract(rextract, no_damage_points) 

damage_polygons_table$ID <- rownames(damage_polygons_table) # write rowid in column
extract_values_damage <- merge(extract_values_damage, damage_polygons_table)

extract_values_no_damage$year <- 2019 
extract_values_no_damage$damage_type <- NA 
extract_values_no_damage$damage_class <- 0  
extract_values_no_damage$ID <- extract_values_no_damage$ID + max(extract_values_damage$ID)

extract_values_all <- rbind(extract_values_damage, extract_values_no_damage) 
write.csv2(extract_values_all, here("output", "reference_Friedericke_2019.csv"), row.names=FALSE) 
