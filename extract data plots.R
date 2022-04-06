# extracting plot level data ----------------------------------------------

library(raster)
library(glcm)
library(rgdal)
library(caret)
library(randomForest)
library(exactextractr)
library(sf)
library(spatialEco)
library(landscapemetrics)
library(tidyverse)

rasterOptions(maxmemory = 2e+10) # for VM


# functions ---------------------------------------------------------------

loadshapelist<-function(dir, extension = '.gpkg'){
  
  z<-getwd()
  setwd(dir)
  
  #
  print('listing files in raster folder') 
  temp<-list.files(pattern = paste0('*',extension), full.names=FALSE)
  
  #
  print('compiling rater names')
  names_plots<-temp
  names_plots<-str_replace(names_plots, extension,'')
  
  #
  print('load all rasters into list')
  allshapes <- lapply(temp, st_read)
  
  #
  print('set stack names')
  names(allshapes)<-names_plots
  
  setwd(z)
  return(allshapes)
  
}

# load data ---------------------------------------------------------------

#predictions

models_all<-readRDS('outputs/single_model/lists/single_model.rds')

#load shapes single

plots_all_sep<-loadshapelist('data/plots/', extension = '.shp')

#merge all

plots_all_sep_merge<-bind_rows(plots_all_sep, .id = 'plot')%>%st_transform(crs = crs(models_all$ashburton))

#add garden 

plots_all<-plots_all_sep_merge%>%mutate(garden = case_when(grepl('ash', plot, ignore.case = T) ~ 'ashburton',
                                                             grepl('bal', plot, ignore.case = T) ~ 'balwyn',
                                                             grepl('box', plot, ignore.case = T) ~ 'boxhill',
                                                             grepl('ess', plot, ignore.case = T) ~ 'essendon',
                                                             grepl('fle', plot, ignore.case = T) ~ 'flemington',
                                                             grepl('haw', plot, ignore.case = T) ~ 'hawthorn',
                                                             grepl('jol', plot, ignore.case = T) ~ 'jolimont',
                                                             grepl('rus', plot, ignore.case = T) ~ 'rushall',
                                                             grepl('sla', plot, ignore.case = T) ~ 'slater',
                                                             grepl('wes', plot, ignore.case = T) ~ 'westernbrunswick'))


#sort and split

plots_all_list<-plots_all%>%select(19, 1)%>%group_split(garden)%>%setNames(unique(plots_all$garden))

# crop layers -------------------------------------------------------------

#test with one

ash_plots<-plots_all_list$ashburton%>%group_split(plot)

names(ash_plots)<-plots_all_list$ashburton$plot

ash<-models_all$ashburton

out <- list()
for (i in 1:length(ash_plots)) {
  x <- crop(ash, ash_plots[[i]])
  x <- mask(x, ash_plots[[i]])
  out[[i]] <- x
}

names(out)<-names(ash_plots)


#split all polygons

plots_split<-map(plots_all_list, group_split, plot)

names_key<-list(plots_all_list$ashburton$plot,
                plots_all_list$balwyn$plot,
                plots_all_list$boxhill$plot,
                plots_all_list$essendon$plot,
                plots_all_list$flemington$plot,
                plots_all_list$hawthorn$plot,
                plots_all_list$jolimont$plot,
                plots_all_list$rushall$plot,
                plots_all_list$slater$plot,
                plots_all_list$westernbrunswick$plot)%>%setNames(unique(plots_all$garden))


#function for all

crop_mask_list<-function(plot_list, raster){
  out <- list()
  
  for (i in 1:length(plot_list)) {
    x <- crop(raster, plot_list[[i]])
    x <- mask(x, plot_list[[i]])
    out[[i]] <- x
  }
 
  names(out)<-names(plot_list)
  return(out)
}

test<-crop_mask_list(ash_plots, ash)

#all  

crop_all_plots<-map2(plots_split, models_all, crop_mask_list)

unlist_all<-unlist(crop_all_plots)  

names(unlist_all)<-plots_all$plot


# spatial metrics ---------------------------------------------------------

raster_stats<-function(class_raster){
  
  print('creating data frame')
  rast_df<-class_raster%>%as.data.frame(xy = T, na.rm = T)
  
  rast_df$ID<-1
  
  res<-res(class_raster)[1]
  
  print('tallying class area')
  rast_area<-rast_df%>%group_by(layer)%>%
    summarise(area = sum(ID), aream2 = (area*res^2))%>%
    mutate(sumA = sum(area), per = 100*area/sumA)%>%rename(class = layer)
  
  print('getting landscape metrics')
  landmetrics<-calculate_lsm(class_raster, what = c("lsm_c_np", 'lsm_c_clumpy'),
                             directions = 8)
  
  landmetrics_wide<-pivot_wider(landmetrics, names_from = c('metric'), values_from = c('value'))%>%select(3, 5, 6)
  
  print('binding')
  
  metrics_combined<-inner_join(rast_area, landmetrics_wide, by = 'class')
  
  print('done')
  return(metrics_combined)
  
}

#test

ash_metrics_test<-raster_stats(unlist_all$ashburtont1)
 
#all

#extract all

metrics_landscape_plots<-lapply(unlist_all, raster_stats)

#combine

metrics_landscape_plots_bind<-bind_rows(metrics_landscape_plots, .id = 'plot')

write.csv(metrics_landscape_plots_bind, 'outputs/single_model/area_metrics+landscape_plots.csv')

#some stats

ggplot(metrics_landscape_plots_bind, aes(x = as.factor(class), y = aream2))+
  geom_boxplot()+
  theme_bw()

ggplot(metrics_landscape_plots_bind, aes(x = as.factor(class), y = clumpy))+
  geom_boxplot()+
  theme_bw()

ggplot(metrics_landscape_plots_bind, aes(x = as.factor(class), y = np))+
  geom_boxplot()+
  theme_bw()













