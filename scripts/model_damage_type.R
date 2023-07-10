## Packages, seed and data
library(tidyverse)
library(tidymodels)
library(caret)
library(here)
library(data.table)
library(vip)

set.seed(123)

d1 <- fread(here("output", "reference_Friedericke_2018_clean.csv"), dec = ",")
d2 <- fread(here("output", "reference_Harz_2021_clean.csv"), dec = ",")

d2$ID <- d2$ID + max(d1$ID)
df <- rbind(d1, d2)


df <- df %>%
  group_by(damage_type) %>%
  sample_n(10000)  %>%
  ungroup

df <- 
  df %>% 
  select(-Jahr, -damage_class) %>% 
  mutate(damage_type = as.factor(damage_type))

## Modelisation
# Initial split
df_split <- group_initial_split(df[complete.cases(df),], ID)
df_train <- training(df_split)
df_test <- testing(df_split)

# Models
model_rf <- 
  rand_forest(mtry = tune(), trees = tune(), min_n = tune()) %>% 
  set_engine("ranger", importance = "impurity") %>% 
  set_mode("classification")

model_xgboost <- 
  boost_tree(mtry = tune(), trees = tune(), min_n = tune()) %>% 
  set_engine("xgboost", importance = "impurity") %>% 
  set_mode("classification")

# Grid of hyperparameters
grid_rf <- 
  grid_max_entropy(        
    mtry(range = c(3, 5)), 
    trees(range = c(800, 1000)),
    min_n(range = c(2, 4)),
    size = 10) 

# Workflow
wkfl_rf <- 
  workflow() %>% 
  add_formula(damage_type ~ . - ID) %>% 
  add_model(model_rf)

# Cross validation method
cv_folds <- group_vfold_cv(df_train, v = 5, group = ID)
cv_folds

my_metrics <- metric_set(accuracy, sens)
# roc_auc, accuracy, sens, spec 

rf_fit <- tune_grid(
  wkfl_rf,
  resamples = cv_folds,
  grid = grid_rf,
  metrics = my_metrics,
  control = control_grid(verbose = TRUE) # don't save prediction (imho)
)

rf_fit
collect_metrics(rf_fit)
autoplot(rf_fit, metric = "accuracy")
show_best(rf_fit, metric = "accuracy")
select_best(rf_fit, metric = "accuracy")

wkfl_rf %>% 
  finalize_workflow(select_best(rf_fit, metric = "accuracy")) %>% 
  fit(data = df_train)  %>% 
  extract_fit_parsnip() %>% 
  vip(num_features = 10)

tuned_model <-  
  wkfl_rf %>% 
  finalize_workflow(select_best(rf_fit, metric = "accuracy")) %>% 
  fit(data = df_train)

(tuned_model)
#saveRDS(tuned_model, file=here("output", "model_10000.Rdata"))

df_test$prediction <- predict(tuned_model, df_test)[[1]]

pred <- df_test[,c("damage_type", "prediction")]
confusionMatrix(pred$prediction, pred$damage_type )

(extract_preprocessor(tuned_model))
class(extract_spec_parsnip(tuned_model))
class(extract_fit_parsnip(tuned_model) )

###########

names(pred)
names(df_test)
view <- df_test %>% 
  select("damage_type", "prediction", "ID") %>% 
  filter(damage_type!=prediction)
            
write.csv2(view, here("output", "rf_wrongprediction_damagetype.csv"), row.names=FALSE) 
