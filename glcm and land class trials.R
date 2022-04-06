### glcm and land classification trials ###

library(raster)
library(glcm)
library(rgdal)
library(caret)
library(randomForest)
library(exactextractr)
library(sf)
library(spatialEco)
library(patchwork)
library(tidyverse)


# simple trial ------------------------------------------------------------


#load trial ortho

ortho_west<-stack('data/monika/ortho/westernbrunswick.tif', bands = 1:3) #1:3, excluding 4 which is alpha band

names(ortho_west)<-c('Red', 'Green', 'Blue')

plotRGB(ortho_west, r=1, g=2, b=3, stretch = 'lin')

#crop to garden area

west_shape<-st_read('data/monika/shapefiles/westbrunsgarden.shp')

ortho_west_garden<-crop(ortho_west, west_shape)
ortho_west_garden<-mask(ortho_west_garden, west_shape)

plotRGB(ortho_west_garden, r=1, g=2, b=3, stretch = 'lin')

# extract simple texture

system.time(tex<-glcm(raster(ortho_west_garden, layer = 2)))

#refine moving window size

system.time(tex_31x31<-glcm(raster(ortho_west_garden, layer = 2), window = c(31,31)))

#save

#dir.create('outputs/glcm+landclass')

writeRaster(tex,'outputs/glcm+landclass/westernbrunswick_texture_3x3.tif', overwrite = T)

writeRaster(tex_31x31,'outputs/glcm+landclass/westernbrunswick_texture_31x31.tif', overwrite = T)

plot(tex[[1]])
plot(tex_31x31[[1]])


#names

names(tex)<-c('mean', 'variance', 'homogeneity', 'contrast', 'dissimilarity', 'entropy', 'second_moment',
                'correlation')

names(tex_31x31)<-c('mean', 'variance', 'homogeneity', 'contrast', 'dissimilarity', 'entropy', 'second_moment',
              'correlation')


#stack (3x3 moving window)

model<-stack(ortho_west_garden, tex)

#load roi

roi<-st_read('data/monika/shapefiles/glcm_landclass/west_roi.shp')

plotRGB(model, 1,2,3, stretch = 'lin')
plot(roi, add = T, col = roi$lc)

#extract data

lc<-extract(model,roi, df=TRUE)

lc$lc <- as.factor(roi$lc[lc$ID])

lc2<-na.omit(lc)%>%filter_all(is.finite)%>%sample_n(10000)

names(lc2)

rf <- randomForest(lc ~ Red+Green+Blue+mean+variance+homogeneity+contrast+dissimilarity+
                     entropy+second_moment, data=lc2, importance=TRUE)

print(rf)
varImpPlot(rf)

#create dublicate raster

img_class<-model[[-11]]

img_class<-na.omit(img_class)

#predict model to raster

img_pred <- predict(img_class, model=rf, inf.rm = T)


colors <- c(rgb(200, 100, 0, maxColorValue=255),  # Bare
            rgb(0, 255, 150, maxColorValue=255),  # Veg
            rgb(192, 192, 192, maxColorValue=255)) # Infra

colhex<-c(col = "#AA4371", col = "#00FF00", col = '#4DB3E6' )


plot(img_pred, col = colors)

writeRaster(img_pred, 'outputs/glcm+landclass/class_3x3_nocor.tif', overwrite = T)


# trial essendon, more classes --------------------------------------------


ortho_ess<-stack('data/monika/ortho/essendon.tif', bands = 1:3) #1:3, excluding 4 which is alpha band

names(ortho_ess)<-c('Red', 'Green', 'Blue')

plotRGB(ortho_ess, r=1, g=2, b=3, stretch = 'lin')


#crop to garden area

ess_shape<-st_read('data/monika/shapefiles/essendongarden.shp')

ortho_ess_garden<-crop(ortho_ess, ess_shape)
ortho_ess_garden<-mask(ortho_ess_garden, ess_shape)

plotRGB(ortho_ess_garden, r=1, g=2, b=3, stretch = 'lin')

# extract simple texture

system.time(tex_ess<-glcm(raster(ortho_ess_garden, layer = 2)))

#refine moving window size

system.time(tex_31x31_ess<-glcm(raster(ortho_ess_garden, layer = 2), window = c(31,31)))

writeRaster(tex_ess,'outputs/glcm+landclass/essendon_texture_3x3.tif', overwrite = T)

writeRaster(tex_31x31_ess,'outputs/glcm+landclass/essendon_texture_31x31.tif', overwrite = T)

plot(tex_ess[[1]])
plot(tex_31x31_ess[[1]])


#names

names<-c('mean', 'variance', 'homogeneity', 'contrast', 'dissimilarity', 'entropy', 'second_moment',
              'correlation')

names(tex_ess)<-names
names(tex_31x31_ess)<-names

#stack (3x3 moving window)

model_ess<-stack(ortho_ess_garden, tex_ess)

#load roi

roi_ess<-st_read('data/monika/shapefiles/glcm_landclass/ess_roi.shp')

plotRGB(model_ess, 1,2,3, stretch = 'lin')
plot(roi_ess, add = T, col = roi_ess$lc)

#extract data

system.time(lc_ess<-exact_extract(model_ess,roi_ess, fun = NULL))

for( i in seq_along(lc_ess)){
  
  lc_ess[[i]]$ID <- seq_along(lc_ess)[i]
  
}

lc_ess_table<-dplyr::bind_rows(lc_ess)%>%dplyr::select(-12)

lc_ess_table$lc <- as.factor(roi_ess$lc[lc_ess_table$ID])

lc_ess_table_sample<-na.omit(lc_ess_table)%>%filter_all(is.finite)%>%sample_n(10000)

#build model

names(lc_ess_table_sample)

rf_ess <- randomForest(lc ~ Red+Green+Blue+mean+variance+homogeneity+contrast+dissimilarity+
                     entropy+second_moment, data=lc_ess_table_sample, importance=TRUE)



print(rf_ess)
varImpPlot(rf_ess)

#create dublicate raster

img_class_ess<-model_ess[[-11]]

img_class_ess<-na.omit(model_ess)

#predict model to raster

img_pred_ess <- predict(img_class_ess, model=rf_ess, inf.rm = T, progress = 'text')

colors_ess <- c(rgb(200, 100, 0, maxColorValue=255),  # Bare
            rgb(0, 255, 150, maxColorValue=255),  # Veg
            rgb(192, 192, 192, maxColorValue=255),
            ) # Infra

plot(img_pred_ess)

writeRaster(img_pred_ess, 'outputs/glcm+landclass/ess_class_3x3_nocor.tif', overwrite = T)

#test model performance

test <- exact_extract(img_pred_ess,roi_ess, fun = NULL) 

for( i in seq_along(test)){
  
  test[[i]]$ID <- seq_along(test)[i]
  
}

test_table<-dplyr::bind_rows(test)%>%dplyr::select(-2)

test_table$lc <- as.factor(roi_ess$lc[test_table$ID])


testProbs <- data.frame(
  obs = as.factor(test_table$lc),
  pred = as.factor(test_table$value)) %>% 
  mutate(correct = ifelse(obs == pred, 1, 0))%>%na.omit()

confMatrix <- confusionMatrix(testProbs$obs, testProbs$pred)
confMatrix

stats<-confMatrix[["byClass"]]%>%as.data.frame()%>%rownames_to_column('landclass')

write.csv(stats, 'outputs/glcm+landclass/essendon_stats.csv', row.names = F)




# trial 3 essendon, fewer classes -----------------------------------------

# # put together rasters and change to utm projection # # 

#RGB

ortho_ess<-stack('data/monika/ortho/essendon.tif', bands = 1:3) #1:3, excluding 4 which is alpha band

names(ortho_ess)<-c('Red', 'Green', 'Blue')

ess_shape<-st_read('data/monika/shapefiles/essendongarden.shp')

ortho_ess_garden<-crop(ortho_ess, ess_shape)
ortho_ess_garden<-mask(ortho_ess_garden, ess_shape)

#texture

tex_ess<-stack('outputs/glcm+landclass/essendon_texture_3x3.tif')

#names

names<-c('mean', 'variance', 'homogeneity', 'contrast', 'dissimilarity', 'entropy', 'second_moment',
         'correlation')

names(tex_ess)<-names

#reproject

proj<- '+proj=utm +zone=55 +south +datum=WGS84 +units=m +no_defs'

ortho_ess_garden <- raster::projectRaster(ortho_ess_garden, crs = proj)

tex_ess <- raster::projectRaster(tex_ess, crs = proj)


#stack 

model_ess<-stack(ortho_ess_garden, tex_ess)

names(model_ess)<-c('Red', 'Green', 'Blue', 'mean', 'variance', 'homogeneity', 'contrast', 'dissimilarity', 'entropy', 'second_moment',
           'correlation')

#writeRaster(model_ess,'outputs/glcm+landclass/essendon_modeling.tif', overwrite = T)

# load raster

model_ess<-stack('outputs/glcm+landclass/essendon_modeling.tif')

names(model_ess)<-c('Red', 'Green', 'Blue', 'mean', 'variance', 'homogeneity', 'contrast', 'dissimilarity', 
                    'entropy', 'second_moment', 'correlation')

# load rois

roi_ess_6<-st_read('data/glcm_landclass/ess_6.gpkg')

# extract data 

lc_ess_6<-exact_extract(model_ess,roi_ess_6, fun = NULL)

for( i in seq_along(lc_ess_6)){
  
  lc_ess_6[[i]]$ID <- seq_along(lc_ess_6)[i]
  
}

lc_ess_table<-dplyr::bind_rows(lc_ess_6)%>%dplyr::select(-12)

lc_ess_table$lc <- as.factor(roi_ess_6$lc[lc_ess_table$ID])

#take an equal sample of each class

lc_ess_table_sample<-na.omit(lc_ess_table)%>%filter_all(is.finite)%>%group_by(lc)%>%sample_n(3000)

#remove correlation here (has NAs in raster)

lc_ess_table_sample_nocor<-lc_ess_table%>%select(-11)%>%na.omit()%>%filter_all(is.finite)%>%group_by(lc)%>%sample_n(3000)


#build model

names(lc_ess_table_sample)

rf_ess_6 <- randomForest(lc ~ Red+Green+Blue+mean+variance+homogeneity+contrast+dissimilarity+
                         entropy+second_moment+correlation, data=lc_ess_table_sample, importance=TRUE)


rf_ess_6_nocor <- randomForest(lc ~ Red+Green+Blue+mean+variance+homogeneity+contrast+dissimilarity+
                           entropy+second_moment, data=lc_ess_table_sample_nocor, importance=TRUE)

print(rf_ess_6)
plot(rf_ess_6)
varImpPlot(rf_ess_6)

print(rf_ess_6_nocor)
plot(rf_ess_6_nocor)
varImpPlot(rf_ess_6_nocor)

#create dublicate raster

img_class_ess<-model_ess
img_class_ess_nocor<-model_ess[[-11]]

img_class_ess<-na.omit(img_class_ess)
img_class_ess_nocor<-na.omit(img_class_ess_nocor)


#predict model to raster


beginCluster()

img_pred_ess<-clusterR(img_class_ess, raster::predict, args = list(model = rf_ess_6, inf.rm = T), progress = 'text')

endCluster()

beginCluster()

img_pred_ess_nocor<-clusterR(img_class_ess_nocor, 
                             raster::predict, args = list(model = rf_ess_6_nocor, inf.rm = T), progress = 'text')

endCluster()

#check

colors <- c(rgb(0, 0, 255, maxColorValue=255),  # impervious
            rgb(255, 0, 0, maxColorValue=255),  # bare ground (paths)
            rgb(255, 255, 0, maxColorValue=255),  # Bare plots
            rgb(0, 155, 0, maxColorValue=255), #trees
            rgb(255,255,100, maxColorValue = 255), #crops
            rgb(0,155,155, maxColorValue = 255))  # grasses + needs

plot(img_pred_ess, col = colors)
plot(img_pred_ess_nocor, col = colors)


#writeRaster(img_pred_ess, 'outputs/glcm+landclass/ess_6.tif', overwrite = T)
writeRaster(img_pred_ess_nocor, 'outputs/glcm+landclass/ess_6_nocor.tif', overwrite = T)

# test performance

test <- exact_extract(img_pred_ess_nocor,roi_ess_6, fun = NULL) 

for( i in seq_along(test)){
  
  test[[i]]$ID <- seq_along(test)[i]
  
}

test_table<-dplyr::bind_rows(test)%>%dplyr::select(-2)

test_table$lc <- as.factor(roi_ess_6$lc[test_table$ID])


testProbs <- data.frame(
  obs = as.factor(test_table$lc),
  pred = as.factor(test_table$value)) %>% 
  mutate(correct = ifelse(obs == pred, 1, 0))%>%na.omit()

confMatrix <- confusionMatrix(testProbs$obs, testProbs$pred)
confMatrix

stats<-confMatrix[["byClass"]]%>%as.data.frame()%>%rownames_to_column('landclass')

write.csv(stats, 'outputs/glcm+landclass/essendon_6_stats_nocor.csv', row.names = F)

# plot it - work in progress

conf<-confMatrix[["table"]]%>%as.data.frame()

conf_rast<-rasterFromXYZ(conf)

plot(conf_rast)


# extract metrics ---------------------------------------------------------


raster_stats_area<-function(class_raster, pred_raster){
  
  rast_df<-class_raster%>%as.data.frame(xy = T, na.rm = T)
  
  rast_df$ID<-1
  
  res<-res(class_raster)[1]
  
  rast_area<-rast_df%>%group_by(layer)%>%
    summarise(area = sum(ID), aream2 = (area*res^2))%>%
    mutate(sumA = sum(area), per = 100*area/sumA)%>%rename(class = layer)

  
  return(rast_area)
  
}

ess_area<-raster_stats_area(img_pred_ess)

write.csv(ess_area, 'outputs/glcm+landclass/ess_6_area.csv', row.names = F)



# include CHM -------------------------------------------------------------


ortho_ess<-stack('data/monika/ortho/essendon.tif', bands = 1:3) #1:3, excluding 4 which is alpha band

names(ortho_ess)<-c('Red', 'Green', 'Blue')

ess_shape<-st_read('data/monika/shapefiles/essendongarden.shp')

ortho_ess_garden<-crop(ortho_ess, ess_shape)
ortho_ess_garden<-mask(ortho_ess_garden, ess_shape)

#texture

tex_ess_raw<-stack('outputs/glcm+landclass/essendon_texture_3x3.tif')

#names

names<-c('mean', 'variance', 'homogeneity', 'contrast', 'dissimilarity', 'entropy', 'second_moment',
         'correlation')

names(tex_ess_raw)<-names

#CHM

chm_ess_raw<-raster('outputs/essendon.tif')

names(chm_ess_raw)<-'CHM'

# stack together because these match in CRS

chm_re<-resample(chm_ess_raw, tex_ess_raw)

tex_chm<-stack(tex_ess_raw, chm_re)

#remove correlation here

tex_chm<-tex_chm[[-8]]

#reproject

proj<-'+proj=utm +zone=55 +south +datum=WGS84 +units=m +no_defs'

ortho_ess_garden <- raster::projectRaster(ortho_ess_garden, crs = proj)

tex_chm_re <- raster::projectRaster(tex_chm, crs = proj)

#stack 

model_ess<-stack(ortho_ess_garden, tex_chm_re)

names(model_ess)<-c('Red', 'Green', 'Blue', 'mean', 'variance', 'homogeneity', 'contrast', 'dissimilarity', 'entropy', 'second_moment',
                    'CHM')

#plot(model_ess[[11]])

#writeRaster(model_ess,'outputs/glcm+landclass/essendon_modeling_chm.tif', overwrite = T)

#get ROI data and extract

model_ess<-stack('outputs/glcm+landclass/essendon_modeling_chm.tif')

roi_ess_6<-st_read('data/roi/ess_6.gpkg')

# extract data 

lc_ess_6<-exact_extract(model_ess,roi_ess_6, fun = NULL)

for( i in seq_along(lc_ess_6)){
  
  lc_ess_6[[i]]$ID <- seq_along(lc_ess_6)[i]
  
}

lc_ess_table<-dplyr::bind_rows(lc_ess_6)%>%dplyr::select(-12)

lc_ess_table$lc <- as.factor(roi_ess_6$lc[lc_ess_table$ID])

#take an equal sample of each class

lc_ess_table_sample<-na.omit(lc_ess_table)%>%filter_all(is.finite)%>%group_by(lc)%>%sample_n(3000)

summary(lc_ess_table_sample$lc)

#build model

names(lc_ess_table_sample)

rf_ess_6 <- randomForest(lc ~ Red+Green+Blue+mean+variance+homogeneity+contrast+dissimilarity+
                           entropy+second_moment+CHM, data=lc_ess_table_sample, importance=TRUE)


print(rf_ess_6)
plot(rf_ess_6)
varImpPlot(rf_ess_6)


#create dublicate raster

img_class_ess<-model_ess

img_class_ess<-na.omit(img_class_ess)


beginCluster()

img_pred_ess<-clusterR(img_class_ess, raster::predict, args = list(model = rf_ess_6, inf.rm = T), progress = 'text')

endCluster()

#check

colors <- c(rgb(0, 0, 255, maxColorValue=255),  # impervious
            rgb(255, 0, 0, maxColorValue=255),  # bare ground (paths)
            rgb(255, 255, 0, maxColorValue=255),  # Bare plots
            rgb(0, 155, 0, maxColorValue=255), #trees
            rgb(255,255,100, maxColorValue = 255), #crops
            rgb(0,155,155, maxColorValue = 255))  # grasses + needs


plot(img_pred_ess, col = colors)


writeRaster(img_pred_ess, 'outputs/glcm+landclass/ess_6_chm.tif', overwrite = T)

# test performance

ess_pred<-raster('outputs/glcm+landclass/ess_6_chm.tif')

test <- exact_extract(ess_pred, roi_ess_6, fun = NULL) 

for( i in seq_along(test)){
  
  test[[i]]$ID <- seq_along(test)[i]
  
}

test_table<-dplyr::bind_rows(test)%>%dplyr::select(-2)

test_table$lc <- as.factor(roi_ess_6$lc[test_table$ID])


testProbs <- data.frame(
  obs = as.factor(test_table$lc),
  pred = as.factor(test_table$value)) %>% 
  mutate(correct = ifelse(obs == pred, 1, 0))%>%na.omit()

confMatrix_ess <- confusionMatrix(testProbs$obs, testProbs$pred)
confMatrix_ess

stats<-confMatrix[["byClass"]]%>%as.data.frame()%>%rownames_to_column('landclass')

write.csv(stats, 'outputs/glcm+landclass/essendon_6_stats_chm.csv', row.names = F)

## extract area metrics ####

raster_stats_area<-function(class_raster, pred_raster){
  
  rast_df<-class_raster%>%as.data.frame(xy = T, na.rm = T)
  
  rast_df$ID<-1
  
  res<-res(class_raster)[1]
  
  rast_area<-rast_df%>%group_by(layer)%>%
    summarise(area = sum(ID), aream2 = (area*res^2))%>%
    mutate(sumA = sum(area), per = 100*area/sumA)%>%rename(class = layer)
  
  
  return(rast_area)
  
}

ess_area_chm<-raster_stats_area(img_pred_ess)

write.csv(ess_area_chm, 'outputs/glcm+landclass/ess_6_area_chm.csv', row.names = F)


# smoothing ---------------------------------------------------------------

ess_pred<-raster('outputs/glcm+landclass/ess_6_chm.tif')

smooth_mask<-st_read('data/glcm_landclass/ess_6_smooth.gpkg')

ess_pred_trial<-crop(ess_pred, smooth_mask)
ess_pred_trial<-mask(ess_pred_trial, smooth_mask)

plot(ess_pred_trial)

# focal approach

ker <- matrix(1,3,3)

# ess_pred_smooth_core <- focal(ess_pred_trial, w = ker, fun = modal, progress = 'text')
# 
# plot(ess_pred_smooth_core)

beginCluster()

ess_pred_smooth<-clusterR(ess_pred_trial, raster::focal, args = list(w= ker, fun = modal), 
                          progress = 'text') #this creates gaps

endCluster()

plot(ess_pred_smooth, col = colors)
plot(ess_pred_smooth_core, col = colors)
plot(ess_pred_trial, col = colors)


#fill NAs with initial classification

ess_pred_smooth_fill<-cover(ess_pred_smooth, ess_pred_trial)

plot(ess_pred_smooth, col = colors)
plot(ess_pred_smooth_fill, col = colors)
#plot(ess_pred_smooth_core, col = colors)

writeRaster(ess_pred_smooth, 'outputs/glcm+landclass/ess_6_chm_focal_maj_5.tif')

#gaussian smoothing

ess_pred_smooth<-raster.gaussian.smooth(ess_pred_trial, sigma = 2, n = 3)*10

plot(ess_pred_smooth)
plot(ess_pred_trial)

writeRaster(ess_pred_smooth, 'outputs/glcm+landclass/ess_6_chm_gauss.tif', overwrite = T)


