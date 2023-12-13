### Harz 2020-2021
  
library(here)
library(terra)
library(data.table)
library(dplyr)
library(stringr)
source(local=TRUE, r"{scripts/functions.R}")

### Load data

#### Load raster data
rpast <- rast(im2list("D:\\wsf-sat\\data\\validation\\Harz_2021\\Merge_UNC_UPC_2020_GTiff_shift_TC.tif"))
rpresent <- rast(im2list("D:\\wsf-sat\\data\\validation\\Harz_2021\\Merge_UNC_UPC_2021_GTiff_shift_TC.tif"))
names(rpast) <- paste(names(rpast), '_past', sep="")
names(rpresent) <- paste(names(rpresent), '_present', sep="")

#### Load geometry data

# Geometries
damage_polygons <- vect("D:\\wsf-sat\\data\\validation\\Harz_2021\\Harz_Freiflaeche_stehendesTotholz_2021.gpkg", layer="filter025")
no_damage_points <- vect(here("data", "Referenz_NichtSchaden_randompoints_Harz_2021.gpkg"))

damage_polygons_table <- data.table(as.data.frame(damage_polygons))
no_damage_points_table <- data.table(as.data.frame(no_damage_points))

#### Prepare geometry data


# Damage classes: 1 -\> stehendes Totholz, 2 -\> lliegendes Totholz, 3-\> Freifläche

damage_polygons_table <- damage_polygons_table %>% 
  rename(year = Jahr, damage_type = Typ_wsfsat) %>% 
  select("year", "damage_type")  %>% 
  mutate(damage_class = 1)

#### Calculate difference bands

rdiff <- rpresent - rpast
names(rdiff) <- str_replace(names(rdiff), "_present", "_diff")

#writeRaster(rdiff, filename=here("output", "Harz_2021_bands.tif"), filetype="GTiff", datatype='INT4S', overwrite=TRUE)  

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

#writeRaster(rindices_past, filename=here("output", "Harz_2021_indices_past.tif"), filetype="GTiff", datatype='INT4S', overwrite=TRUE)  
#writeRaster(rindices_present, filename=here("output", "Harz_2021_indices_present.tif"), filetype="GTiff", datatype='INT4S', overwrite=TRUE)  
#writeRaster(rindices_diff, filename=here("output", "Harz_2021_indices_diff.tif"), filetype="GTiff", datatype='INT4S', overwrite=TRUE)  

#### Prepare extract raster

rextract <- c(rpast, rpresent, rdiff, rindices_past, rindices_present, rindices_diff)
rm(rpast, rpresent, rdiff, rindices_past, rindices_present, rindices_diff)

#### Extract data to points

extract_values_damage <- extract(rextract, damage_polygons) 
extract_values_no_damage <- extract(rextract, no_damage_points) 

damage_polygons_table$ID <- rownames(damage_polygons_table) # write rowid in column
extract_values_damage <- merge(extract_values_damage, damage_polygons_table)

extract_values_no_damage$year <- 2021 
extract_values_no_damage$damage_type <- NA 
extract_values_no_damage$damage_class <- 0  
extract_values_no_damage$ID <- extract_values_no_damage$ID + max(extract_values_damage$ID)

extract_values_all <- rbind(extract_values_damage, extract_values_no_damage) 
write.csv2(extract_values_all, here("output", "reference_Harz_2021_lokal.csv"), row.names=FALSE) 
