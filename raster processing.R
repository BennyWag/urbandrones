
# create all raster stacks ------------------------------------------------

library(raster)
library(glcm)
library(rgdal)
library(caret)
library(randomForest)
library(progress)
library(exactextractr)
library(sf)
library(spatialEco)
library(stringr)
library(purrr)
library(dplyr)


#rasterOptions(maxmemory = 2e+10) # for VM

# crop all orthos ---------------------------------------------------------

croprasterlist<-function(rasterdir, cropdir, outdir, bandnames, bands){
  
  #
  print('saving project wd, setting file wd')
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
  allrasters <- lapply(temp, stack, bands = bands)
  
  #
  print('set stack names')
  allrasters<-lapply(allrasters, setNames, names_band)
  names(allrasters)<-names_plots
  
  #
  print('load shapes')
  setwd(z)
  setwd(cropdir)
  temp<-list.files(pattern = '*.shp', full.names=FALSE)
  
  #
  print('compiling shape names and setting')
  names_shape<-temp
  names_shape<-str_replace(names_shape,'.shp','')
  
  allshapes <- lapply(temp, st_read)
  names(allshapes)<-names_chms
  
  #
  print('cropping and masking rasters')
  
  cropmask<-function(raster, mask){
    
    crop<-crop(raster, mask)
    mask<-mask(crop, mask)
    
    return(mask)
    
  }
  
  allrasters_crop<-map2(allrasters, allshapes, cropmask)  
  
  #
  print('saving rasters')
  setwd(z)
  setwd(outdir)
  mapply(writeRaster, allrasters_crop, names(allrasters_crop), bylayer=F, 'GTiff', options="COMPRESS=LZW")
  
  #
  print('resetting wd, clearing ram')
  setwd(z)
  gc()
  memory.size(max=F)
  
  #
  print('All done')
  
}


croprasterlist(rasterdir ='data/ortho', 
               cropdir = 'data/shapefiles',
               outdir = 'outputs/ortho_crop',
               bandnames = c('Red', 'Green', 'Blue'),
               bands = 1:3)



# derive texture for all orthos based on green band -----------------------

loadrasterlist_select<-function(rasterdir, func,  bands, bandnames){
  
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

#load

all_green<-loadrasterlist_select(rasterdir = 'outputs/ortho_crop',
                                 func = raster,
                                 bands = 2,
                                 bandnames = 'Green')

#loop through processing

tex<-list()

pb <- progress_bar$new(total = length(all_green))

for (i in all_green){
  
  pb$tick()
  
  texture = glcm(i, statistics = 
                   c("mean", "variance", "homogeneity", "contrast", "dissimilarity", "entropy", 
                     "second_moment"))
  
  names<-c('mean', 'variance', 'homogeneity', 'contrast', 'dissimilarity', 'entropy', 'second_moment')
  
  names(texture)<-names

  
  tex[[length(tex)+1]] = texture
  
}

names(tex)<-names(all_green)

#save

saverasterlist<-function(list, outdir){
  
  x<-getwd()
  
  setwd(outdir)
  
  mapply(writeRaster, list, names(list), bylayer=F, 'GTiff', options="COMPRESS=LZW")
  
  setwd(x)
  
  gc()
  memory.size(max=F)
  
}

saverasterlist(tex, 'outputs/texture')


# reproject all and combine -----------------------------------------------

#stack

ortho<-loadrasterlist_select(rasterdir = 'outputs/ortho_crop',
                             func = stack,
                             bands = 1:3,
                             bandnames = c('Red', 'Green', 'Blue'))


CHMs<-loadrasterlist_select(rasterdir = 'data/chm',
                            func = raster,
                            bands = 1,
                            bandnames = 'CHM')

tex<-loadrasterlist_select(rasterdir = 'outputs/texture',
                           func = stack,
                           bands = 1:7,
                           bandnames = c('mean', 'variance', 'homogeneity', 'contrast', 
                                         'dissimilarity', 'entropy', 'second_moment'))

extent(CHMs[[1]])
extent(tex[[1]])

#resample

chm_re<-map2(CHMs, tex, resample)

tex_CHM<-map2(tex, chm_re, stack)

tex_CHM[[1]]

#reproject

proj<-'+proj=utm +zone=55 +south +datum=WGS84 +units=m +no_defs'

ortho_re <- lapply(ortho, projectRaster, crs = proj, progress = 'text')

tex_chm_re <- lapply(tex_CHM, projectRaster, crs = proj, progress = 'text')

#final stack and save

stack_all<-map2(ortho_re, tex_chm_re, stack)

saverasterlist(stack_all, 'outputs/stacks_all')












