
# all gardens modeling ----------------------------------------------------

library(raster)
library(glcm)
library(rgdal)
library(caret)
library(corrplot)
library(randomForest)
library(exactextractr)
library(sf)
library(spatialEco)
library(broom)
library(cvms)
library(patchwork)
library(landscapemetrics)
library(tidyverse)

rasterOptions(maxmemory = 2e+10) # for VM


# functions ---------------------------------------------------------------

extract_varimp<-function(model, title = NULL){
  
  var.imp <- data.frame(importance(model, type=1))
  var.imp$Variables <- row.names(var.imp)%>%as.factor()
  rownames(var.imp)<-1:nrow(var.imp)
  varimp <- var.imp[order(var.imp$MeanDecreaseAccuracy,decreasing = T),]
  
  var.imp2 <- data.frame(importance(model, type=2))
  var.imp2$Variables <- row.names(var.imp2)%>%as.factor()
  rownames(var.imp2)<-1:nrow(var.imp2)
  varimp2 <- var.imp2[order(var.imp2$MeanDecreaseGini,decreasing = T),]
  
  imp<-ggplot(varimp, aes(x = reorder(Variables,MeanDecreaseAccuracy), y = MeanDecreaseAccuracy))+
    geom_bar(stat = "summary", fun.y = 'mean', fill = 'darkblue')+
    coord_flip()+
    ylab('Mean decrease Accuracy')+xlab('')+
    ggtitle(title)+
    geom_text(aes(label=round(MeanDecreaseAccuracy, digits = 3)), position=position_dodge(width=0.9), hjust=1, color = 'white')+
    theme_bw()+
    theme(plot.title = element_text(size =18, face='bold'),
          axis.title.y = element_text(size = 16, face = 'bold'),
          axis.title.x = element_text(size = 16, face = 'bold'),
          axis.text.x = element_text(size = 15, face = 'bold'), 
          axis.text.y = element_text(size = 15, face = 'bold'),
          legend.title = element_text(size = 16),
          legend.text = element_text(size = 15))
  
  imp2<-ggplot(varimp2, aes(x = reorder(Variables,MeanDecreaseGini), y = MeanDecreaseGini))+
    geom_bar(stat = "summary", fun.y = 'mean', fill = 'darkblue')+
    coord_flip()+
    ylab('Mean decrease Gini')+xlab('')+
    ggtitle(title)+
    geom_text(aes(label=round(MeanDecreaseGini, digits = 3)), position=position_dodge(width=0.9), hjust=1, color = 'white')+
    theme_bw()+
    theme(plot.title = element_text(size =18, face='bold'),
          axis.title.y = element_text(size = 16, face = 'bold'),
          axis.title.x = element_text(size = 16, face = 'bold'),
          axis.text.x = element_text(size = 15, face = 'bold'), 
          axis.text.y = element_text(size = 15, face = 'bold'),
          legend.title = element_text(size = 16),
          legend.text = element_text(size = 15))
  
  plot<- imp / imp2
  
  return(plot)
  
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

saverasterlist<-function(list, outdir){
  
  x<-getwd()
  
  setwd(outdir)
  
  mapply(writeRaster, list, names(list), bylayer=F, 'GTiff', options="COMPRESS=LZW")
  
  setwd(x)
  
  gc()
  memory.size(max=F)
  
}

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

garden_performance<-function(pred_raster, mask){
  
  test <- exact_extract(pred_raster, mask, fun = NULL) 
  
  for( i in seq_along(test)){
    
    test[[i]]$ID <- seq_along(test)[i]
    
  }
  
  test_table<-dplyr::bind_rows(test)%>%dplyr::select(-2)
  
  test_table$lc <- as.factor(mask$lc[test_table$ID])
  
  
  testProbs <- data.frame(
    obs = as.factor(test_table$lc),
    pred = as.factor(test_table$value))
  
  confMatrix <- confusionMatrix(testProbs$obs, testProbs$pred)
  confMatrix
  
  stats<-confMatrix[["byClass"]]%>%as.data.frame()%>%rownames_to_column('landclass')
  
  #tidy
  
  conf_plot_ess<-broom::tidy(confMatrix$table)%>%mutate(ID = row_number())
  
  plot<-plot_confusion_matrix(conf_plot_ess, 
                              targets_col = "Reference", 
                              predictions_col = "Prediction", 
                              counts_col = 'n',
                              add_normalized = F,
                              add_col_percentages = T,
                              add_row_percentages = T,
                              add_arrows = F,
                              add_counts = F,
                              palette = 'Greys')
  
  list<-list(confMatrix, plot)
  
  names(list)<-c('matrix', 'plot')
  
  return(list)
}


# universal data ----------------------------------------------------------

names<-c('Red', 'Green', 'Blue', 'mean', 'variance', 'homogeneity', 'contrast', 'dissimilarity', 'entropy', 'second_moment',
         'CHM')

imp_vars<-c("Red","Green","Blue","mean","variance","CHM")

imp_vars2<-c("Green","Blue","mean","contrast","entropy","CHM")


colors <- c(rgb(0, 0, 255, maxColorValue=255),  # impervious
            rgb(255, 0, 0, maxColorValue=255),  # bare ground (paths)
            rgb(255, 150, 255, maxColorValue=255),  # Bare plots
            rgb(0, 155, 0, maxColorValue=255), #trees
            rgb(255,255,100, maxColorValue = 255), #crops
            rgb(0,155,155, maxColorValue = 255))  # grasses + needs

allshapes<-loadshapelist('data/roi/gardens_new')

allrasters<-loadrasterlist_select('outputs/stacks_all', 
                                  func = stack, 
                                  bands = 1:11,
                                  bandnames = names)

essendon_single<-garden_performance(ess_pred, roi_ess_6)

# split data 1 ------------------------------------------------------------



#all data
#gardens_data<-read.csv('outputs/all_data.csv')%>%mutate(lc = as.factor(lc))%>%as.tibble()

#sample
gardens_sample<-read.csv('outputs/all_data_sample.csv')%>%mutate(lc = as.factor(lc))%>%as.tibble()

garden_sample_subset<-na.omit(gardens_sample)%>%group_by(garden, lc)%>%sample_n(600)

summary(as.factor(garden_sample_subset$lc))

summary(as.factor(garden_sample_subset$garden))


# subsetting ------------------------------------------------------------------

train <- createDataPartition(garden_sample_subset$lc, time=1, p = 0.8, list=F)
train_subset <- garden_sample_subset[train,]
test_subset <- garden_sample_subset[-train,]

summary(as.factor(train_subset$lc))
summary(as.factor(test_subset$lc))


# build small model ------------------------------------------------------

#n samples = nrow(train_subset)

rf_1<-randomForest(lc ~ Red+Green+Blue+mean+variance+homogeneity+contrast+dissimilarity+
                           entropy+second_moment+CHM, data=train_subset, importance=TRUE)


rf_1
plot(rf_1)
varImpPlot(rf_1)

importance(rf_1, type=2)



#visualize var imp


extract_varimp(rf_1)


# test --------------------------------------------------------------------

test_subset$pred<-predict(rf_1, test_subset)

confMatrix_rf1 <- confusionMatrix(as.factor(test_subset$lc), test_subset$pred)
confMatrix_rf1
stats_rf1<-confMatrix_rf1[["byClass"]]%>%as_tibble()%>%rownames_to_column('lc')


# visualize confusion matrix ---------------------------------------------------------------


#see https://cran.r-project.org/web/packages/cvms/vignettes/creating_a_confusion_matrix.html

conf_plot<-broom::tidy(confMatrix_rf1$table)%>%mutate(ID = row_number())

plot_confusion_matrix(conf_plot, targets_col = "Reference", predictions_col = "Prediction", counts_col = 'n')

#tweak

plot_confusion_matrix(conf_plot, 
                      targets_col = "Reference", 
                      predictions_col = "Prediction", 
                      counts_col = 'n',
                      add_normalized = T,
                      add_col_percentages = F,
                      add_row_percentages = F,
                      palette = 'Oranges')


# check corrleation -------------------------------------------------------

cor<-garden_sample_subset%>%ungroup()%>%select(3:12)

cor_mat<-cor(cor, use = 'complete.obs', method = 'pearson')

corrplot(cor_mat, method = "number", type="lower", order="hclust", tl.cex = 0.75, tl.col="black", tl.srt = 45)

corrplot(cor(cor, use = 'complete.obs', method = 'pearson'),method = 'number', type = 'lower',
         tl.cex = 1, cl.cex = 1.2, cl.ratio = 0.1, tl.col="black", tl.srt = 90)

corrplot(cor(cor, use = 'complete.obs', method = 'pearson'),method = 'number', type = 'lower')

highlyCor <- findCorrelation(cor_mat, cutoff = 0.9)
noncor <- cor_mat[,-highlyCor]

#extract non correlated variable names

noncor_vars<-c(names(as.data.frame(noncor)))

gardens_nocor<-garden_sample_subset%>%ungroup()%>%select(all_of(noncor_vars))

corrplot(cor(gardens_nocor, use = 'complete.obs', method = 'pearson'),method = 'number', type = 'lower',
         tl.cex = 1, cl.cex = 1.2, cl.ratio = 0.1, tl.col="black", tl.srt = 90)


# model on non-corrleated pairs only --------------------------------------

noncor_dataset<-garden_sample_subset%>%dplyr::select(1, 14, all_of(noncor_vars))

noncor_dataset_modeling<-garden_sample_subset%>%ungroup()%>%dplyr::select(14, all_of(noncor_vars))

#subset train/test

train_noncor <- createDataPartition(noncor_dataset_modeling$lc, time=1, p = 0.8, list=F)
train_subset_noncor <- noncor_dataset_modeling[train_noncor,]
test_subset_noncor <- noncor_dataset_modeling[-train_noncor,]

summary(as.factor(train_subset_noncor$lc))
summary(as.factor(test_subset_noncor$lc))

#build model

rf_2<-randomForest(lc ~ ., data=train_subset_noncor, importance=TRUE)


rf_2
plot(rf_2)
varImpPlot(rf_2)

extract_varimp(rf_2)

#test performance

test_subset_noncor$pred<-predict(rf_2, test_subset_noncor)

confMatrix_rf2 <- confusionMatrix(as.factor(test_subset_noncor$lc), test_subset_noncor$pred)
confMatrix_rf2
stats_rf2<-confMatrix_rf2[["byClass"]]%>%as_tibble()%>%rownames_to_column('lc')

#plot

conf_plot_rf_2<-broom::tidy(confMatrix_rf2$table)%>%mutate(ID = row_number())

plot_confusion_matrix(conf_plot_rf_2, 
                      targets_col = "Reference", 
                      predictions_col = "Prediction", 
                      counts_col = 'n',
                      add_normalized = T,
                      add_col_percentages = F,
                      add_row_percentages = F,
                      palette = 'Oranges')


#compare

compare<-left_join(conf_plot, conf_plot_rf_2, by = 'ID', copy = F)%>%select(1:3,7)%>%rename(all_n = 3, noncor_n = 4)%>%mutate(diff = all_n - noncor_n)


# model on high performing predictors -------------------------------------

# based on gini (FINAL MODEL!) -----------------------------------------------------------


imp_vars<-data.frame(importance(rf_1, type=2))%>%
  rownames_to_column()%>%
  filter(MeanDecreaseGini>1300)

imp_vars<-imp_vars$rowname

#compile data

highperf_dataset<-garden_sample_subset%>%dplyr::select(1, 14, all_of(imp_vars))

highperf_dataset_modeling<-garden_sample_subset%>%ungroup()%>%dplyr::select(14, all_of(imp_vars))

#subset train/test

train_highperf <- createDataPartition(highperf_dataset_modeling$lc, time=1, p = 0.8, list=F)
train_subset_highperf  <- highperf_dataset_modeling[train_highperf,]
test_subset_highperf  <- highperf_dataset_modeling[-train_highperf,]

#build model

rf_3<-randomForest(lc ~ ., data=train_subset_highperf, importance=TRUE)

rf_3
plot(rf_3)
varImpPlot(rf_3)

extract_varimp(rf_3)

#test performance

test_subset_highperf$pred<-predict(rf_3, test_subset_highperf)

confMatrix_rf3 <- confusionMatrix(as.factor(test_subset_highperf$lc), test_subset_highperf$pred)
confMatrix_rf3
stats_rf3<-confMatrix_rf3[["byClass"]]%>%as_tibble()%>%rownames_to_column('lc')

stats_rf3_overall<-confMatrix_rf3[["overall"]]%>%as.data.frame()%>%rownames_to_column()%>%rename(value = 2, metric = 1)


#plot

conf_plot_rf_3<-broom::tidy(confMatrix_rf3$table)%>%mutate(ID = row_number())

plot_confusion_matrix(conf_plot_rf_3, 
                      targets_col = "Reference", 
                      predictions_col = "Prediction", 
                      counts_col = 'n',
                      add_normalized = T,
                      add_col_percentages = F,
                      add_row_percentages = F,
                      palette = 'Oranges')


universal_model_plot<-plot_confusion_matrix(conf_plot_rf_3, 
                      targets_col = "Reference", 
                      predictions_col = "Prediction", 
                      counts_col = 'n',
                      add_normalized = F,
                      add_col_percentages = T,
                      add_row_percentages = T,
                      add_arrows = F,
                      add_counts = T,
                      palette = 'Greys')

universal_model_plot+
  labs(title = 'Overall confusion matrix')

summary(as.factor(test_subset_highperf$lc))

ggsave('overall.pdf', path = 'outputs/performance/universal/overall/',
       width = 15.6, height = 15.6, units = 'cm', dpi = 600)

write.csv(stats_rf3, 'outputs/performance/universal/overall/model_perf.csv', row.names = F)
write.csv(stats_rf3_overall, 'outputs/performance/universal/overall/model_perf_overall.csv', row.names = F)

#calculate TSS

stats_rf3_TSS<-stats_rf3%>%mutate(TSS = (Sensitivity + Specificity)-1)

write.csv(stats_rf3_TSS, 'outputs/performance/universal/overall/model_perf_TSS.csv', row.names = F)


# based on mean decr. acc ---------------------------------------------------------------

imp_vars2<-data.frame(importance(rf_1, type=1))%>%
  rownames_to_column()%>%
  filter(MeanDecreaseAccuracy>63)

imp_vars2<-imp_vars2$rowname

#compile data

highperf_dataset2<-garden_sample_subset%>%dplyr::select(1, 14, all_of(imp_vars2))

highperf_dataset_modeling2<-garden_sample_subset%>%ungroup()%>%dplyr::select(14, all_of(imp_vars2))

#subset train/test

train_highperf2 <- createDataPartition(highperf_dataset_modeling2$lc, time=1, p = 0.8, list=F)
train_subset_highperf2  <- highperf_dataset_modeling2[train_highperf2,]
test_subset_highperf2  <- highperf_dataset_modeling2[-train_highperf2,]

#build model

rf_4<-randomForest(lc ~ ., data=train_subset_highperf2, importance=TRUE)

rf_4
plot(rf_4)
varImpPlot(rf_4)

extract_varimp(rf_4)

#test performance

test_subset_highperf2$pred<-predict(rf_4, test_subset_highperf2)

confMatrix_rf4 <- confusionMatrix(as.factor(test_subset_highperf2$lc), test_subset_highperf2$pred)
confMatrix_rf4
stats_rf4<-confMatrix_rf4[["byClass"]]%>%as_tibble()%>%rownames_to_column('lc')

#plot

conf_plot_rf_4<-broom::tidy(confMatrix_rf4$table)%>%mutate(ID = row_number())

plot_confusion_matrix(conf_plot_rf_4, 
                      targets_col = "Reference", 
                      predictions_col = "Prediction", 
                      counts_col = 'n',
                      add_normalized = T,
                      add_col_percentages = F,
                      add_row_percentages = F,
                      palette = 'Oranges')


# predict to one garden ---------------------------------------------------

ash<-stack('outputs/stacks_all/ashburton.tif')
balwyn<-stack('outputs/stacks_all/balwyn.tif')


names<-c('Red', 'Green', 'Blue', 'mean', 'variance', 'homogeneity', 'contrast', 'dissimilarity', 'entropy', 'second_moment',
         'CHM')

names(ash)<-names
names(balwyn)<-names


beginCluster()

pred_ash<-clusterR(ash, raster::predict, args = list(model = rf_3, inf.rm = T), progress = 'text')
pred_bal<-clusterR(balwyn, raster::predict, args = list(model = rf_3, inf.rm = T), progress = 'text')

endCluster()


plot(pred_ash, col = colors)
plot(pred_bal, col = colors)

# smoothing ---------------------------------------------------------------


# focal approach

ker <- matrix(1,3,3)
ker2 <- matrix(1,5,5)


beginCluster()

pred_ash_smooth<-clusterR(pred_ash, raster::focal, args = list(w= ker, fun = modal), 
                          progress = 'text') #this creates gaps

endCluster()

beginCluster()

pred_ash_smooth_5<-clusterR(pred_ash, raster::focal, args = list(w= ker2, fun = modal), 
                          progress = 'text') #this creates gaps

endCluster()

beginCluster()

pred_bal_smooth<-clusterR(pred_bal, raster::focal, args = list(w= ker, fun = modal), 
                          progress = 'text') #this creates gaps

endCluster()


plot(pred_ash_smooth, col = colors)
plot(pred_ash_smooth_5, col = colors)
plot(pred_bal_smooth, col = colors)


pred_ash_smooth_fill<-cover(pred_ash_smooth, pred_ash)
pred_bal_smooth_fill<-cover(pred_bal_smooth, pred_bal)


plot(pred_ash, col = colors)
plot(pred_ash_smooth_fill, col = colors)

plot(pred_bal, col = colors)
plot(pred_bal_smooth_fill, col = colors)

# r <- raster(nrows = 120, ncol = 120, xmn=0)
# r[] <- sample(3, ncell(r), replace=TRUE)
# plot(r)
# 
# a<-focal(r, w=matrix(1,1,1), fun = modal)    # 3x3 moving window
# plot(a)

# gauusian smoothing

pred_ash_smooth_gauss<-raster.gaussian.smooth(pred_ash, sigma = 2, n = 5)*10

plot(pred_ash, col = colors)
plot(pred_ash_smooth_fill, col = colors)
plot(pred_ash_smooth_gauss, col = colors)


# predicting to all as list ----------------------------------------------------

#prediction

all_predict<-list()

#take it from here

for (i in allrasters){
  
  beginCluster()
  pred<-clusterR(i, raster::predict, args = list(model = rf_3, inf.rm = T), progress = 'text')
  endCluster()
  
  all_predict[[length(all_predict)+1]] <- pred
  
}

names(all_predict)<-names(allrasters)

#test plots

plot(all_predict$hawthorn, col = colors)

#save

# dir.create('outputs/universal_model')
# dir.create('outputs/universal_model/pred_rasters')
# dir.create('outputs/universal_model/lists')

saverasterlist(all_predict, 'outputs/universal_model/pred_rasters')

saveRDS(all_predict, 'outputs/universal_model/lists/univers_model.rds')


# test on the garden level ------------------------------------------------

#one garden

# test performance

test <- exact_extract(all_predict$essendon, allshapes$essendon, fun = NULL) 

for( i in seq_along(test)){
  
  test[[i]]$ID <- seq_along(test)[i]
  
}

test_table<-dplyr::bind_rows(test)%>%dplyr::select(-2)

test_table$lc <- as.factor(allshapes$essendon$lc[test_table$ID])


testProbs <- data.frame(
  obs = as.factor(test_table$lc),
  pred = as.factor(test_table$value)) %>% 
  mutate(correct = ifelse(obs == pred, 1, 0))%>%na.omit()

confMatrix <- confusionMatrix(testProbs$obs, testProbs$pred)
confMatrix

stats<-confMatrix[["byClass"]]%>%as.data.frame()%>%rownames_to_column('landclass')

#tidy

conf_plot_ess<-broom::tidy(confMatrix$table)%>%mutate(ID = row_number())

ess_plot<-plot_confusion_matrix(conf_plot_ess, 
                      targets_col = "Reference", 
                      predictions_col = "Prediction", 
                      counts_col = 'n',
                      add_normalized = F,
                      add_col_percentages = T,
                      add_row_percentages = F,
                      add_arrows = F,
                      palette = 'Oranges')

ess_plot+labs(title = 'Essendon')


#extract for all > function


#test function

# compare to non-universal model

essendon_uni<-garden_performance(all_predict$essendon, allshapes$essendon)
essendon_uni$plot
essendon_single$plot

#extract for all

pred_all_perf<-map2(all_predict, allshapes, garden_performance)

pred_all_perf2<-map2(all_predict, allshapes, garden_performance)


pred_all_perf[["essendon"]][["plot"]]+labs(title = 'Essendon')
  
pred_all_perf[["boxhill"]][["plot"]] +labs(title = 'Boxhill')

roi_ess_6<-st_read('data/roi/ess_6.gpkg')

ess_pred<-raster('outputs/glcm+landclass/ess_6_chm.tif')

# extract single garden performance (!!!!) ---------------------------------------

#get matrices only

pred_all_perf_mat<-unlist(pred_all_perf)

pred_all_perf[["ashburton"]][["matrix"]]

pred_all_perf_mat<-list()
pred_all_perf_mat_overall<-list()


for (i in seq_along(pred_all_perf)){
  
  a<-pred_all_perf[[i]][["matrix"]]
  
  b<-a[["byClass"]]%>%as_tibble()%>%rownames_to_column('lc')
  
  c<-a[["overall"]]%>%as.data.frame()%>%rownames_to_column()%>%rename(value = 2, metric = 1)
  
  
  pred_all_perf_mat[[length(pred_all_perf_mat)+1]] <- b
  pred_all_perf_mat_overall[[length(pred_all_perf_mat_overall)+1]] <- c
  
  
}

#set names and bind

names(pred_all_perf_mat)<-names(pred_all_perf)
names(pred_all_perf_mat_overall)<-names(pred_all_perf)

perf_garden_merge<-bind_rows(pred_all_perf_mat, .id = 'garden')
perf_garden_overallmerge<-bind_rows(pred_all_perf_mat_overall, .id = 'garden')

write.csv(perf_garden_merge, 'outputs/performance/universal/by_garden/model_perf_gardens.csv', row.names = F)
write.csv(perf_garden_overallmerge, 'outputs/performance/universal/by_garden/model_perf_overall_gardens.csv', row.names = F)

perf_garden_merge_TSS<-perf_garden_merge%>%mutate(TSS = (Sensitivity + Specificity)-1)

write.csv(perf_garden_merge_TSS, 'outputs/performance/universal/by_garden/model_perf_gardens_TSS.csv', row.names = F)

#remove 0 for TSS stats (sd)
perf_garden_merge_TSS_nozero<-perf_garden_merge_TSS[-c(18),]

#confusion plots

gardens1<-pred_all_perf2$ashburton$plot+labs(title = 'Ashburton')
gardens2<-pred_all_perf2$balwyn$plot+labs(title = 'Balwyn', y =' ')
gardens3<-pred_all_perf2$boxhill$plot+labs(title = 'Boxhill', y = '')
gardens4<-pred_all_perf2$essendon$plot+labs(title = 'Essendon', y = '')
gardens5<-pred_all_perf2$flemington$plot+labs(title = 'Flemington')
gardens6<-pred_all_perf2$hawthorn$plot+labs(title = 'Hawthorn', x = '')
gardens7<-pred_all_perf2$jolimont$plot+labs(title = 'Jolimont', y = '', x = '')
gardens8<-pred_all_perf2$rushall$plot+labs(title = 'Rushall', y = '', x = '')
gardens9<-pred_all_perf2$slater$plot+labs(title = 'Slater', x = '')
gardens10<-pred_all_perf2$westernbrunswick$plot+labs(title = 'Westernbrunswick', y = '', x = '')

gardens1 + gardens2 + gardens3 + gardens4 + gardens5 + 
  gardens6 + gardens7 + gardens8 + gardens9 + gardens10 +
  plot_layout(nrow = 2, byrow = T)

ggsave('gardens.pdf', path = 'outputs/performance/universal/by_garden/',
       width = 42, height = 29.7, units = 'cm', dpi = 600)


# increase sample size - 3000 -------------------------------------------

#single garden approach used 3000 of each

summary(lc_ess_table_sample$lc)

# combined used 600 each so far

summary(as.factor(garden_sample_subset$lc))

#sample 3000 each

garden_sample_subset2<-na.omit(gardens_sample)%>%group_by(garden, lc)%>%sample_n(3000, replace = T)

summary(as.factor(garden_sample_subset2$lc))

summary(as.factor(garden_sample_subset2$lc[garden_sample_subset2$garden == 'essendon']))

summary(as.factor(garden_sample_subset2$garden))


# build model -------------------------------------------------------------

#compile data

highperf_dataset2<-garden_sample_subset2%>%dplyr::select(1, 14, all_of(imp_vars))

highperf_dataset_modeling2<-garden_sample_subset2%>%ungroup()%>%dplyr::select(14, all_of(imp_vars))

#subset train/test

train_highperf2 <- createDataPartition(highperf_dataset_modeling2$lc, time=1, p = 0.8, list=F)
train_subset_highperf2  <- highperf_dataset_modeling2[train_highperf2,]
test_subset_highperf2  <- highperf_dataset_modeling2[-train_highperf2,]

#build model

rf_3000<-randomForest(lc ~ ., data=train_subset_highperf2, importance=TRUE)

rf_3000
plot(rf_3000)
varImpPlot(rf_3000)

extract_varimp(rf_3000)


# evaluate model ----------------------------------------------------------


test_subset_highperf2$pred<-predict(rf_3000, test_subset_highperf2)

confMatrix_rf3000 <- confusionMatrix(as.factor(test_subset_highperf2$lc), test_subset_highperf2$pred)
confMatrix_rf3000
#stats_rf3000<-confMatrix_rf3000[["byClass"]]%>%as_tibble()%>%rownames_to_column('lc')

#plot

conf_plot_rf_3000<-broom::tidy(confMatrix_rf3000$table)%>%mutate(ID = row_number())

plot_confusion_matrix(conf_plot_rf_3000, 
                      targets_col = "Reference", 
                      predictions_col = "Prediction", 
                      counts_col = 'n',
                      add_normalized = F,
                      add_col_percentages = T,
                      add_row_percentages = F,
                      palette = 'Oranges')

# predict to plots --------------------------------------------------------


all_predict3000<-list()

for (i in allrasters){
  
  beginCluster()
  pred<-clusterR(i, raster::predict, args = list(model = rf_3000, inf.rm = T), progress = 'text')
  endCluster()
  
  all_predict3000[[length(all_predict3000)+1]] <- pred
  
}

names(all_predict3000)<-names(allrasters)

#test plots

plot(ess_pred, col = colors)
plot(all_predict$essendon, col = colors)
plot(all_predict3000$essendon, col = colors)


# test plot performance ---------------------------------------------------

pred_all_perf_3000<-map2(all_predict3000, allshapes, garden_performance)

pred_all_perf[["ashburton"]][["plot"]] +labs(title = 'Ashburton')
pred_all_perf_3000[["ashburton"]][["plot"]] +labs(title = 'Ashburton')

pred_all_perf[["balwyn"]][["plot"]] +labs(title = 'Balwyn')
pred_all_perf_3000[["balwyn"]][["plot"]] +labs(title = 'Balwyn')

pred_all_perf[["boxhill"]][["plot"]] +labs(title = 'Boxhill')
pred_all_perf_3000[["boxhill"]][["plot"]] +labs(title = 'Boxhill')

pred_all_perf[["essendon"]][["plot"]]+labs(title = 'Essendon')
pred_all_perf_3000[["essendon"]][["plot"]]+labs(title = 'Essendon')
essendon_single$plot+labs(title = 'Essendon')


# increase ntrees ---------------------------------------------------------

#build model

rf_3000_trees<-randomForest(lc ~ ., data=train_subset_highperf2, importance=TRUE, ntree = 1000)

rf_3000_trees
plot(rf_3000_trees)
varImpPlot(rf_3000_trees)

extract_varimp(rf_3000_trees)

#predict

all_predict3000_trees<-list()

for (i in allrasters){
  
  beginCluster()
  pred<-clusterR(i, raster::predict, args = list(model = rf_3000_trees, inf.rm = T), progress = 'text')
  endCluster()
  
  all_predict3000_trees[[length(all_predict3000_trees)+1]] <- pred
  
}

names(all_predict3000_trees)<-names(allrasters)

#test

pred_all_perf_3000_trees<-map2(all_predict3000_trees, allshapes, garden_performance)

pred_all_perf[["ashburton"]][["plot"]] +labs(title = 'Ashburton')
pred_all_perf_3000[["ashburton"]][["plot"]] +labs(title = 'Ashburton')
pred_all_perf_3000_trees[["ashburton"]][["plot"]] +labs(title = 'Ashburton')


pred_all_perf[["balwyn"]][["plot"]] +labs(title = 'Balwyn')
pred_all_perf_3000[["balwyn"]][["plot"]] +labs(title = 'Balwyn')
pred_all_perf_3000_trees[["balwyn"]][["plot"]] +labs(title = 'Balwyn')


pred_all_perf[["boxhill"]][["plot"]] +labs(title = 'Boxhill')
pred_all_perf_3000[["boxhill"]][["plot"]] +labs(title = 'Boxhill')
pred_all_perf_3000_trees[["boxhill"]][["plot"]] +labs(title = 'Boxhill')


pred_all_perf[["essendon"]][["plot"]]+labs(title = 'Essendon')
pred_all_perf_3000[["essendon"]][["plot"]]+labs(title = 'Essendon')
pred_all_perf_3000_trees[["essendon"]][["plot"]] +labs(title = 'Essendon')

essendon_single$plot+labs(title = 'Essendon')


# increase sample size - 6000 -------------------------------------------

#sample 6000 each

garden_sample_subset3<-na.omit(gardens_sample)%>%group_by(garden, lc)%>%sample_n(6000, replace = T)

summary(as.factor(garden_sample_subset3$lc))

summary(as.factor(garden_sample_subset3$lc[garden_sample_subset3$garden == 'essendon']))

summary(as.factor(garden_sample_subset3$garden))

#modeling dataset and plet

highperf_dataset3<-garden_sample_subset3%>%dplyr::select(1, 14, all_of(imp_vars))

highperf_dataset_modeling3<-garden_sample_subset3%>%ungroup()%>%dplyr::select(14, all_of(imp_vars))

#subset train/test

train_highperf3 <- createDataPartition(highperf_dataset_modeling3$lc, time=1, p = 0.8, list=F)
train_subset_highperf3  <- highperf_dataset_modeling3[train_highperf3,]
test_subset_highperf3  <- highperf_dataset_modeling3[-train_highperf3,]

#build model

rf_6000<-randomForest(lc ~ ., data=train_subset_highperf3, importance=TRUE)

rf_6000
plot(rf_6000)
varImpPlot(rf_6000)

extract_varimp(rf_6000)

#predict

all_predict6000<-list()

for (i in allrasters){
  
  beginCluster()
  pred<-clusterR(i, raster::predict, args = list(model = rf_6000, inf.rm = T), progress = 'text')
  endCluster()
  
  all_predict6000[[length(all_predict6000)+1]] <- pred
  
}

names(all_predict6000)<-names(allrasters)

#test

pred_all_perf_6000<-map2(all_predict6000, allshapes, garden_performance)

pred_all_perf[["ashburton"]][["plot"]] +labs(title = 'Ashburton')
pred_all_perf_3000[["ashburton"]][["plot"]] +labs(title = 'Ashburton')
pred_all_perf_3000_trees[["ashburton"]][["plot"]] +labs(title = 'Ashburton')
pred_all_perf_6000[["ashburton"]][["plot"]] +labs(title = 'Ashburton')


pred_all_perf[["balwyn"]][["plot"]] +labs(title = 'Balwyn')
pred_all_perf_3000[["balwyn"]][["plot"]] +labs(title = 'Balwyn')
pred_all_perf_3000_trees[["balwyn"]][["plot"]] +labs(title = 'Balwyn')
pred_all_perf_6000[["balwyn"]][["plot"]] +labs(title = 'Balwyn')


pred_all_perf[["boxhill"]][["plot"]] +labs(title = 'Boxhill')
pred_all_perf_3000[["boxhill"]][["plot"]] +labs(title = 'Boxhill')
pred_all_perf_3000_trees[["boxhill"]][["plot"]] +labs(title = 'Boxhill')
pred_all_perf_6000[["boxhill"]][["plot"]] +labs(title = 'Boxhill')


pred_all_perf[["essendon"]][["plot"]]+labs(title = 'Essendon')
pred_all_perf_3000[["essendon"]][["plot"]]+labs(title = 'Essendon')
pred_all_perf_3000_trees[["essendon"]][["plot"]] +labs(title = 'Essendon')
pred_all_perf_6000[["essendon"]][["plot"]] +labs(title = 'Essendon')

essendon_single$plot+labs(title = 'Essendon')



# use entire garden data to create samples --------------------------------

#create subset

#gardens_data<-read.csv('outputs/all_data.csv')%>%mutate(lc = as.factor(lc))%>%as_tibble()

#sample

#garden_sample_subset_full<-na.omit(gardens_data)%>%group_by(garden, lc)%>%sample_n(4500, replace = F)

#write.csv(garden_sample_subset_full, 'outputs/all_data_sample_4500.csv', row.names = F)

garden_sample_subset_full<-read.csv('outputs/all_data_sample_4500.csv')%>%mutate(lc = as.factor(lc))#%>%as_tibble()

summary(as.factor(garden_sample_subset_full$lc))

summary(as.factor(garden_sample_subset_full$lc[garden_sample_subset_full$garden == 'essendon']))

summary(as.factor(garden_sample_subset_full$garden))

#build data

highperf_dataset_all<-garden_sample_subset_full%>%dplyr::select(1, 14, all_of(imp_vars))

highperf_dataset_modeling_all<-garden_sample_subset_full%>%ungroup()%>%dplyr::select(14, all_of(imp_vars))

#subset train/test

train_highperf_all <- createDataPartition(highperf_dataset_modeling_all$lc, time=1, p = 0.8, list=F)
train_subset_highperf_all  <- highperf_dataset_modeling_all[train_highperf_all,]
test_subset_highperf_all  <- highperf_dataset_modeling_all[-train_highperf_all,]

#build model

rf_unique<-randomForest(lc ~ ., data=train_subset_highperf_all, importance=TRUE)

rf_unique
plot(rf_unique)
varImpPlot(rf_unique)

extract_varimp(rf_unique)

#predict

all_predict_unique<-list()

for (i in allrasters){
  
  beginCluster()
  pred<-clusterR(i, raster::predict, args = list(model = rf_unique, inf.rm = T), progress = 'text')
  endCluster()
  
  all_predict_unique[[length(all_predict_unique)+1]] <- pred
  
}

names(all_predict_unique)<-names(allrasters)

#test

pred_all_perf_unique<-map2(all_predict_unique, allshapes, garden_performance)

pred_all_perf[["ashburton"]][["plot"]] +labs(title = 'Ashburton')
pred_all_perf_unique[["ashburton"]][["plot"]] +labs(title = 'Ashburton')


pred_all_perf[["balwyn"]][["plot"]] +labs(title = 'Balwyn')
pred_all_perf_unique[["balwyn"]][["plot"]] +labs(title = 'Balwyn')


pred_all_perf[["boxhill"]][["plot"]] +labs(title = 'Boxhill')
pred_all_perf_unique[["boxhill"]][["plot"]] +labs(title = 'Boxhill')


pred_all_perf[["essendon"]][["plot"]]+labs(title = 'Essendon')
pred_all_perf_unique[["essendon"]][["plot"]] +labs(title = 'Essendon')
essendon_single$plot+labs(title = 'Essendon')


# test on single gardens --------------------------------------------------

#seperate data into list

garden_samples<-read.csv('outputs/all_data_sample_4500.csv')%>%mutate(lc = as.factor(lc))#%>%as_tibble()

gardens_sep<-garden_samples%>%group_split(garden)

gardens_sep_df<-lapply(gardens_sep, as.data.frame)

names(gardens_sep_df)<-names(allrasters)

#try on one

ash_data<-gardens_sep$ashburton

ash_data_3000<-ash_data%>%group_by(lc)%>%sample_n(3000)

summary(ash_data_3000$lc)

#build modeling data

#build data

dataset_ash<-ash_data_3000%>%dplyr::select(14, 1:12)

dataset_modeling_ash<-ash_data_3000%>%ungroup()%>%dplyr::select(14, 2:12)

#subset train/test

train_ash <- createDataPartition(dataset_modeling_ash$lc, time=1, p = 0.8, list=F)
train_subset_ash  <- dataset_modeling_ash[train_ash,]
test_subset_ash  <- dataset_modeling_ash[-train_ash,]

#build model

rf_ash<-randomForest(lc ~ ., data=train_subset_ash, importance=TRUE)

rf_ash
plot(rf_ash)
varImpPlot(rf_ash)

extract_varimp(rf_ash)

#predict

beginCluster()
pred_ash<-clusterR(allrasters$ashburton, raster::predict, args = list(model = rf_ash, inf.rm = T), progress = 'text')
endCluster()

#test

pred_ash_perf<-garden_performance(pred_ash, allshapes$ashburton)

pred_all_perf[["ashburton"]][["plot"]] +labs(title = 'Ashburton')
pred_all_perf_unique[["ashburton"]][["plot"]] +labs(title = 'Ashburton')
pred_ash_perf$plot+labs(title = 'Ashburton single')

#function

sep_models<-function(dataset){
  
  data_sample<-dataset%>%group_by(lc)%>%sample_n(3000)%>%droplevels()
  
  #build modeling data

  dataset_all<-data_sample%>%dplyr::select(14, 1:12)
  
  dataset_modeling<-data_sample%>%ungroup()%>%dplyr::select(14, 2:12)
  
  #subset train/test
  
  train <- createDataPartition(dataset_modeling$lc, time=1, p = 0.8, list=F)
  train_subset  <- dataset_modeling[train,]
  test_subset  <- dataset_modeling[-train,]
  
  #build model
  
  rf<-randomForest(lc ~ ., data=train_subset, importance=TRUE)
  
  #list_data<-list(dataset_all, dataset_modeling, train_subset, test_subset, rf)

  #return(list_data)
  
  return(rf)
  
}

#test

#ash<-sep_models(gardens_sep_df$ashburton)
#box<-sep_models(gardens_sep_df$boxhill)
#models for all

sep_models_all<-lapply(gardens_sep_df, sep_models)

#predictions for all

# all_predict_unique<-list()
# 
# for (i in allrasters){
#   
#   beginCluster()
#   pred<-clusterR(i, raster::predict, args = list(model = rf_unique, inf.rm = T), progress = 'text')
#   endCluster()
#   
#   all_predict_unique[[length(all_predict_unique)+1]] <- pred
#   
# }
# 
# names(all_predict_unique)<-names(allrasters)

predict_sep<-function(raster, model){
  
  beginCluster()
  pred<-clusterR(raster, raster::predict, args = list(model = model, inf.rm = T), progress = 'text')
  endCluster()
  
  return(pred)
  
}

#test

predict_ash<-predict_sep(allrasters$ashburton, sep_models_all$ashburton)

plot(predict_ash, col = colors)

#predict to all

preds_single_all<-map2(allrasters, sep_models_all, predict_sep)

#evaluate

pred_single_perf<-map2(preds_single_all, allshapes, garden_performance)


pred_all_perf[["ashburton"]][["plot"]] +labs(title = 'Ashburton')
pred_single_perf[["ashburton"]][["plot"]] +labs(title = 'Ashburton')


pred_all_perf[["balwyn"]][["plot"]] +labs(title = 'Balwyn')
pred_single_perf[["balwyn"]][["plot"]] +labs(title = 'Balwyn')


pred_all_perf[["boxhill"]][["plot"]] +labs(title = 'Boxhill')
pred_single_perf[["boxhill"]][["plot"]] +labs(title = 'Boxhill')


pred_all_perf[["essendon"]][["plot"]]+labs(title = 'Essendon')
pred_single_perf[["essendon"]][["plot"]] +labs(title = 'Essendon')
essendon_single$plot+labs(title = 'Essendon')


# save single preds -------------------------------------------------------

#dir.create('outputs/single_model')
#dir.create('outputs/single_model/pred_rasters')
#dir.create('outputs/single_model/lists')

saverasterlist(preds_single_all, 'outputs/single_model/pred_rasters')

saveRDS(preds_single_all, 'outputs/single_model/lists/single_model.rds')


# extract area fractions all sites ----------------------------------------------

#function and test

raster_stats_area<-function(class_raster){
  
  rast_df<-class_raster%>%as.data.frame(xy = T, na.rm = T)
  
  rast_df$ID<-1
  
  res<-res(class_raster)[1]
  
  rast_area<-rast_df%>%group_by(layer)%>%
    summarise(area = sum(ID), aream2 = (area*res^2))%>%
    mutate(sumA = sum(area), per = 100*area/sumA)%>%rename(class = layer)
  
  
  return(rast_area)
  
}

ess_area_test<-raster_stats_area(preds_single_all$essendon)


#extract all

metrics_all_single<-lapply(preds_single_all, raster_stats_area)

#combine

metrics_all_single_bind<-bind_rows(metrics_all_single, .id = 'garden')


write.csv(metrics_all_single_bind, 'outputs/single_model/area_metrics.csv')


# with landscapemetrics --------------------------------------------------------

#one site

ess_landmetrics<-calculate_lsm(preds_single_all$essendon, what = c("lsm_c_np", 'lsm_c_clumpy'),
                       directions = 8)


ess_landmetrics_wide<-pivot_wider(ess_landmetrics, names_from = c('metric'), values_from = c('value'))%>%select(3, 5, 6)

#bind

ess_metrics_combined<-inner_join(ess_area_test, ess_landmetrics_wide, by = 'class')

#combine in function

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

ess_area_test2<-raster_stats(preds_single_all$essendon)

#extract all

#preds_single_all<-readRDS('outputs/single_model/lists/single_model.rds')


metrics_landscape_single<-lapply(preds_single_all, raster_stats)

#combine

metrics_landscale_single_bind<-bind_rows(metrics_landscape_single, .id = 'garden')

write.csv(metrics_landscale_single_bind, 'outputs/single_model/area_metrics+landscape.csv')

#some stats

ggplot(metrics_landscale_single_bind, aes(x = as.factor(class), y = aream2))+
  geom_boxplot()+
  theme_bw()

ggplot(metrics_landscale_single_bind, aes(x = as.factor(class), y = clumpy))+
  geom_boxplot()+
  theme_bw()

ggplot(metrics_landscale_single_bind, aes(x = as.factor(class), y = np))+
  geom_boxplot()+
  theme_bw()

metrics_landscale_single_bind%>%group_by(class)%>%summarise(min = min(clumpy),
                                                            mean = mean(clumpy),
                                                            max = max(clumpy),
                                                            sd = sd(clumpy))
