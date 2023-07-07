
library(caret)
library(here)
library(data.table)
library(dplyr)
library(ggplot2)


min_max_norm <- function(x) {
  (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
}

mean_diff <- function(value_damage, value_nodamage){
  mean1 <- lapply(value_damage, mean)
  mean2 <- lapply(value_nodamage, mean)
  diff <- mapply('-', mean1, mean2, SIMPLIFY=FALSE)
  abs_diff <- lapply(diff, abs)
  return(data.table(t(as.data.frame(abs_diff))))
}

sd_mean <- function(value_damage, value_nodamage){
  sd1 <- lapply(value_damage, sd)
  sd2 <- lapply(value_nodamage, sd)
  sd <- as.data.frame(rbind(unlist(sd1),unlist(sd2)))
  mean <- data.table(as.data.frame(colMeans(sd)))
  
  return(mean)
}

d1 <- fread(here("output", "reference_Friedericke_2018_clean.csv"))
d2 <- fread(here("output", "reference_Harz_2021_clean.csv"))
d <- rbind(d1,d2)

rm(d1,d2)
d <- d %>% mutate_at("damage_class", as.factor)

# only columns diff values
#d_diff <- d[ , c(grepl("diff" , names( d ))), with=FALSE ] 
#d <- cbind(d_diff, d[,"damage_class"]) # bind column klasse again
d2norm <- d %>% select(!c(ID, damage_class, Jahr, damage_type))

d_norm <- as.data.frame((lapply(d2norm, min_max_norm))) 
d_norm <- cbind(d_norm, d[,"damage_class"]) # bind damage class column again
d_norm <- data.table(na.omit(d_norm))


pop_nodam <- d_norm[d_norm$damage_class==0, -c("damage_class")] 
pop_dam <- d_norm[d_norm$damage_class==1, -c("damage_class")]

datamean <- mean_diff(pop_dam, pop_nodam)
datasd <- sd_mean(pop_dam, pop_nodam)

names <- names(pop_dam)
dataplot <- cbind(names, datamean, datasd)
names(dataplot) <- c("var", "diff", "sd")
dataplot$ratio <- dataplot$diff / dataplot$sd

ggplot(dataplot,aes(x=diff,y=sd,col=var,label=var))+geom_point()+geom_text(hjust=0, vjust=0)


ggplot(dataplot, aes(x = reorder(var, -ratio), y = ratio, fill=var)) + geom_col() + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + xlab("variable")

