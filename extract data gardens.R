
# data extract all gardens ------------------------------------------------

library(raster)
library(glcm)
library(rgdal)
library(caret)
library(randomForest)
library(exactextractr)
library(sf)
library(spatialEco)
library(tidyverse)

rasterOptions(maxmemory = 2e+10) # for VM


# trial on one garden > function development -----------------------------------------------------

#load raster

ash<-stack('outputs/stacks_all/ashburton.tif')
balwyn<-stack('outputs/stacks_all/balwyn.tif')

#assign names

names<-c('Red', 'Green', 'Blue', 'mean', 'variance', 'homogeneity', 'contrast', 'dissimilarity', 'entropy', 'second_moment',
                           'CHM')
names(ash)<-names
names(balwyn)<-names



#load roi

roi_ash<-st_read('data/roi/gardens_new/ashburton.gpkg')

roi_balwyn<-st_read('data/roi/gardens_new/balwyn.gpkg')

# extract data

lc_ash<-exact_extract(ash, roi_ash, fun = NULL)

for( i in seq_along(lc_ash)){
  
  lc_ash[[i]]$ID <- seq_along(lc_ash)[i]
  
}

ash_table<-dplyr::bind_rows(lc_ash)%>%dplyr::select(-12)

ash_table$lc <- as.factor(roi_ash$lc[ash_table$ID])

#function test

extract_garden_data<-function(raster, mask){
  
  lc<-exact_extract(raster, mask, fun = NULL)
  
  for( i in seq_along(lc)){
    
    lc[[i]]$ID <- seq_along(lc)[i]
    
  }
  
  table<-dplyr::bind_rows(lc)%>%dplyr::select(-12)
  
  table$lc <- as.factor(mask$lc[table$ID])
  
  return(as.tibble(table))
}

ash_extract<-extract_garden_data(ash, roi_ash)

bal_extract<-extract_garden_data(balwyn, roi_balwyn)


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

loadrasterlist_select<-function(rasterdir, func = stack, bands = NULL, bandnames){
  
  z<-getwd()
  setwd(rasterdir)
  
  #
  print('listing files in raster folder') 
  temp<-list.files(pattern = '*.tif', full.names=FALSE)
  
  #
  print('compiling rater names')
  names_plots<-temp
  names_plots<-str_replace(names_plots,'.tif','')
  names_band<-bandnames
  
  #
  print('load all rasters into list')
  allrasters <- lapply(temp, func, bands = bands)
  
  #
  print('set stack names')
  allrasters<-lapply(allrasters, setNames, names_band)
  names(allrasters)<-names_plots
  
  setwd(z)
  return(allrasters)
  
}

extract_garden_data<-function(raster, mask){
  
  lc<-exact_extract(raster, mask, fun = NULL)
  
  for( i in seq_along(lc)){
    
    lc[[i]]$ID <- seq_along(lc)[i]
    
  }
  
  table<-dplyr::bind_rows(lc)%>%dplyr::select(-12)
  
  table$lc <- as.factor(mask$lc[table$ID])
  
  return(as.tibble(table))
}


# load data ---------------------------------------------------------------


allshapes<-loadshapelist('data/roi/gardens_new')

names<-c('Red', 'Green', 'Blue', 'mean', 'variance', 'homogeneity', 'contrast', 'dissimilarity', 'entropy', 'second_moment',
         'CHM')

allrasters<-loadrasterlist_select('outputs/stacks_all', 
                                  func = stack, 
                                  bands = 1:11,
                                  bandnames = names)


# extract to list ---------------------------------------------------------

all_data<-map2(allrasters, allshapes, extract_garden_data)

#combine to dataframe

all_data_df<-bind_rows(all_data, .id = 'garden')

#save

write.csv(all_data_df, 'outputs/all_data.csv', row.names = F)

#subset

all_data_df_sample<-na.omit(all_data_df)%>%group_by(garden, as.factor(lc))%>%sample_n(2000)

write.csv(all_data_df_sample, 'outputs/all_data_sample.csv', row.names = F)



















