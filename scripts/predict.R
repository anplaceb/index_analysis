library(terra)
#library(tidyverse)
library(tidymodels)
library(here)
#library(RStoolbox)
library(stringr)
library(ranger)
library(randomForest)
library(data.table)

memory.limit(9999999999)
# Load data to train models
#d1 <- fread(here("output", "reference_Friedericke_2018_clean.csv"), dec = ",")
#d2 <- fread(here("output", "reference_Harz_2021_clean.csv"), dec = ",")
#df <- rbind(d1,d2)

#df <- df %>%
#  group_by(damage_class) %>%
#  sample_n(10000)  %>%
#  ungroup

#df <- 
#  df %>% 
#  select(-Jahr, -damage_type) %>% 
#  mutate(damage_class = as.factor(damage_class))

# Load rasters
rpast <- rast("D:\\wsf-sat\\data\\validation\\Friedericke_2018\\Merge_UNC_UNB_20170823_GTiff_shift_TC.tif")
rpresent <- rast("D:\\wsf-sat\\data\\validation\\Friedericke_2018\\Merge_UNC_UNB_20180719_20180724_GTiff_shift_TC_na.tif")
names(rpast) <- c("blau_past", "green_past", "red_past", "re1_past", "re2_past", "re3_past", "nir_past", "nir2_past", "swir1_past", "swir2_past")
names(rpresent) <- c("blau_present", "green_present", "red_present", "re1_present", "re2_present", "re3_present", "nir_present", "nir2_present", "swir1_present", "swir2_present")


###########
# Calculate spatial indices and prepare raster to predict
norm_diff <- function(x,y){round(((x-y)/(x+y)*100 + 100),0)}
swir2_past <- rpast[[10]]
swir2_present <- rpresent[[10]]
nir2_past <- rpast[[8]]
nir2_present <- rpresent[[8]]

nbr_past <- lapp(x = c(nir2_past, swir2_past), fun=norm_diff)
nbr_present <- lapp(x = c(nir2_present, swir2_present), fun=norm_diff)
nbr_diff <- nbr_present - nbr_past

swir1_past <- rpast[[9]]
swir1_present <- rpresent[[9]]
swir1_diff <- swir1_present - swir1_past

# to convert from numeric to int to avoid error in predict
writeRaster(nbr_diff, filename=here("output", "nbr_diff_2018_2017.tif"), filetype="GTiff", datatype='INT4S', overwrite=TRUE)
nbr_diff <- rast(here("output", "nbr_diff_2018_2017.tif"))

raster2predict <- c(nbr_diff, swir1_diff, swir1_past)

names(raster2predict) <- c("nbr_diff", "swir1_diff", "swir1_past")

rm(rpast, rpresent, norm_diff, swir2_past, nir2_past, nir2_present, nbr_past,
   nbr_present, nbr_diff, swir1_past, swir1_present, swir1_diff, swir2_present)

# Models
# Load ranger model trained tidymodels 
tuned_model <- readRDS(here("output", "model_10000.Rdata"))
(extract_preprocessor(tuned_model))
class(extract_spec_parsnip(tuned_model))
class(extract_fit_parsnip(tuned_model) )




# Predict on raster
# With tidymodels model
# The problem is that a model produced by the parsnip package always returns a tibble when the prediction type is type="class". raster.predict expects a matrix to be returned. You can get around this by providing a function to raster.predict that converts the returned parsnip::predicted model to a matrix.
fun<-function(...){
  p<-predict(...)
  return(as.matrix(as.numeric(p[, 1, drop=T]))) 
}

#ext(vect("D:\\wsf-sat\\methods\\reference_data\\Friedericke_2018\\Friedericke_2018_damage_areas.gpkg"))

#new_ext <- c(527000,585000,5710000,5745000)
#raster2predict_small <- crop(raster2predict, new_ext)
#plot(raster2predict_small)
#rm(raster2predict)
#rm(new_ext)
pred <- terra::predict(object= raster2predict, 
                       model = tuned_model,
                       fun = fun,
                       type = "class",
                       na.rm=TRUE)
plot(pred)
writeRaster(pred, here('output', 'predictions_rf_Friedericke_2018_ganzeKarte.tif'), 
            filetype="GTiff", datatype='FLT4S', overwrite=TRUE)



###########
# Train ranger model ranger
ranger_model <- ranger(
  formula         = damage_class ~ nbr_diff + swir1_diff + swir1_past , 
  data            = df[complete.cases(df),], 
  num.trees       = 872,
  mtry            = 1,
  importance      = 'impurity', 
  class.weights   = c(0.9, 0.1)
)
#  Train random forest 
rf <- randomForest(damage_class ~ nbr_diff + swir1_diff + swir1_past, 
                   data=na.omit(df), ntree=872, mtry=1)


rm(df, d1,d2)
# With ranger model from ranger works, function to access the predictions
pred <- terra::predict(object = raster2predict, model = ranger_model,
                       fun = function(mod, dat, ...) {
  library(ranger); predict(mod, dat, ...)$predictions}, na.rm=TRUE)


pred <- terra::predict(raster2predict, rf, na.rm=TRUE)


pred <- terra::predict(raster2predict, extract_fit_parsnip(tuned_model),  fun = function(model, ...) predict(model, ...)$predictions,  na.rm=TRUE)

writeRaster(pred, here('output', 'predictions.tif'), filetype="GTiff", datatype='INT2S')








