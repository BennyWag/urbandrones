# figs+tables -------------------------------------------------------------

library(raster)
library(sf)
library(cvms)
library(caret)
library(randomForest)
library(patchwork)
library(ggspatial)
library(RStoolbox)
library(viridis)
library(tidyverse)

#dir.create('outputs/figures')

# variable importance -----------------------------------------------------

extract_varimp_edit<-function(model, title = NULL){
  
  var.imp <- data.frame(importance(model, type=1))
  var.imp$Variables <- row.names(var.imp)%>%as.factor()
  rownames(var.imp)<-1:nrow(var.imp)
  varimp <- var.imp[order(var.imp$MeanDecreaseAccuracy,decreasing = T),]
  
  var.imp2 <- data.frame(importance(model, type=2))
  var.imp2$Variables <- row.names(var.imp2)%>%as.factor()
  rownames(var.imp2)<-1:nrow(var.imp2)
  varimp2 <- var.imp2[order(var.imp2$MeanDecreaseGini,decreasing = T),]
  
  imp<-ggplot(varimp, aes(x = reorder(Variables,MeanDecreaseAccuracy), y = MeanDecreaseAccuracy))+
    geom_bar(stat = "summary", fun.y = 'mean', fill = 'black')+
    coord_flip()+
    ylab('Mean decrease Accuracy')+xlab('')+
    ggtitle(title)+
    geom_text(aes(label=round(MeanDecreaseAccuracy, digits = 3)), position=position_dodge(width=0.9), hjust=1, color = 'white')+
    theme_bw()+
    theme(plot.title = element_text(size =18, face='bold'),
          axis.title.y = element_text(size = 16, face = 'bold'),
          axis.title.x = element_text(size = 16, face = 'bold'),
          axis.text.x = element_text(size = 15, face = 'bold', color = 'black'), 
          axis.text.y = element_text(size = 15, face = 'bold', color = 'black'),
          legend.title = element_text(size = 16),
          legend.text = element_text(size = 15))
  
  imp2<-ggplot(varimp2, aes(x = reorder(Variables,MeanDecreaseGini), y = MeanDecreaseGini))+
    geom_bar(stat = "summary", fun.y = 'mean', fill = 'black')+
    coord_flip()+
    ylab('Mean decrease Gini')+xlab('')+
    ggtitle(title)+
    geom_text(aes(label=round(MeanDecreaseGini, digits = 3)), position=position_dodge(width=0.9), hjust=1, color = 'white')+
    theme_bw()+
    theme(plot.title = element_text(size =18, face='bold'),
          axis.title.y = element_text(size = 16, face = 'bold'),
          axis.title.x = element_text(size = 16, face = 'bold'),
          axis.text.x = element_text(size = 15, face = 'bold', color = 'black'), 
          axis.text.y = element_text(size = 15, face = 'bold', color = 'black'),
          legend.title = element_text(size = 16),
          legend.text = element_text(size = 15))
  
  plot<- imp / imp2
  
  return(plot)
  
}

extract_varimp_split<-function(model, title = NULL){
  
  var.imp <- data.frame(importance(model, type=1))
  var.imp$Variables <- row.names(var.imp)%>%as.factor()
  rownames(var.imp)<-1:nrow(var.imp)
  varimp <- var.imp[order(var.imp$MeanDecreaseAccuracy,decreasing = T),]
  
  var.imp2 <- data.frame(importance(model, type=2))
  var.imp2$Variables <- row.names(var.imp2)%>%as.factor()
  rownames(var.imp2)<-1:nrow(var.imp2)
  varimp2 <- var.imp2[order(var.imp2$MeanDecreaseGini,decreasing = T),]
  
  imp<-ggplot(varimp, aes(x = reorder(Variables,MeanDecreaseAccuracy), y = MeanDecreaseAccuracy))+
    geom_bar(stat = "summary", fun.y = 'mean', fill = 'black')+
    coord_flip()+
    ylab('Mean decrease Accuracy')+xlab('')+
    ggtitle(title)+
    geom_text(aes(label=round(MeanDecreaseAccuracy, digits = 3)), position=position_dodge(width=0.9), hjust=1, color = 'white')+
    theme_bw()+
    theme(plot.title = element_text(size =18, face='bold'),
          axis.title.y = element_text(size = 16, face = 'bold'),
          axis.title.x = element_text(size = 16, face = 'bold'),
          axis.text.x = element_text(size = 15, face = 'bold', color = 'black'), 
          axis.text.y = element_text(size = 15, face = 'bold', color = 'black'),
          legend.title = element_text(size = 16),
          legend.text = element_text(size = 15))
  
  imp2<-ggplot(varimp2, aes(x = reorder(Variables,MeanDecreaseGini), y = MeanDecreaseGini))+
    geom_bar(stat = "summary", fun.y = 'mean', fill = 'black')+
    coord_flip()+
    ylab('Mean decrease Gini')+xlab('')+
    ggtitle(title)+
    geom_text(aes(label=round(MeanDecreaseGini, digits = 3)), position=position_dodge(width=0.9), hjust=1, color = 'white')+
    theme_bw()+
    theme(plot.title = element_text(size =18, face='bold'),
          axis.title.y = element_text(size = 16, face = 'bold'),
          axis.title.x = element_text(size = 16, face = 'bold'),
          axis.text.x = element_text(size = 15, face = 'bold', color = 'black'), 
          axis.text.y = element_text(size = 15, face = 'bold', color = 'black'),
          legend.title = element_text(size = 16),
          legend.text = element_text(size = 15))
  
  list<-list(imp,imp2)
  names(list)<-c('acc', 'gini')
  
  return(list)
  
}

#both

extract_varimp_edit(rf_3)

ggsave('Varimp1.pdf',path = 'outputs/figures/', width = 20, height = 15.6, units = 'cm', dpi = 600)

#seperate

varimp<-extract_varimp_split(rf_3)

varimp$acc

ggsave('Varimp2.pdf',path = 'outputs/figures/', width = 20, height = 15.6, units = 'cm', dpi = 600)

varimp$gini

ggsave('Varimp3.pdf',path = 'outputs/figures/', width = 20, height = 15.6, units = 'cm', dpi = 600)


# stats -------------------------------------------------------------------

names_classes<-c('Impervious', 'Paths', 'Soil', 'Trees', 'Crops', 'Grasses & \n weeds')

colors_lc <- c(rgb(96, 96, 96, maxColorValue=255),  # impervious
            rgb(255, 215, 40, maxColorValue=255),  # bare ground (paths)
            rgb(51, 0, 0, maxColorValue=255),  # Bare plots
            rgb(0, 100, 0, maxColorValue=255), #trees
            rgb(0,200,0, maxColorValue = 255), #crops
            rgb(255,20,150, maxColorValue = 255))  # grasses + needs

plot(pred_ash, col = colors_lc) #test colors spatially


#garden level

metrics_landscale_single_bind<-read.csv('outputs/single_model/area_metrics+landscape.csv')


stats_garden_1<-ggplot(metrics_landscale_single_bind, aes(x = as.factor(class), y = aream2,
                                          color = as.factor(class)))+
  geom_boxplot(outlier.size = 2, show.legend = F)+
  scale_x_discrete(labels = names_classes)+
  scale_y_continuous(name = expression(Area~(m^{'2'})), limits = c(0,1500))+
  scale_color_manual(values = colors_lc)+
  theme_bw()+
  theme(plot.title = element_text(size =18, face='bold'),
        axis.title.y = element_text(size = 16),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(), 
        axis.text.y = element_text(size = 15, face = 'bold', color = 'black'),
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 15))


stats_garden_2<-ggplot(metrics_landscale_single_bind, aes(x = as.factor(class), y = clumpy, color = as.factor(class)))+
  geom_boxplot(outlier.size = 2, show.legend = F)+
  scale_x_discrete(labels = names_classes)+
  scale_y_continuous(name = 'Spatial \n aggregation', labels = scales::percent_format(accuracy = 1))+
  scale_color_manual(values = colors_lc)+
  theme_bw()+
  theme(plot.title = element_text(size =18, face='bold'),
        axis.title.y = element_text(size = 16),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(), 
        axis.text.y = element_text(size = 15, face = 'bold', color = 'black'),
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 15))


stats_garden_3<-ggplot(metrics_landscale_single_bind, aes(x = as.factor(class), y = np, color = as.factor(class)))+
  geom_boxplot(outlier.size = 2, show.legend = F)+
  scale_x_discrete(labels = names_classes)+
  scale_y_continuous(name = 'Number of \n patches')+
  scale_color_manual(values = colors_lc)+
  theme_bw()+
  theme(plot.title = element_text(size =18, face='bold'),
        axis.title.y = element_text(size = 16),
        axis.title.x = element_blank(),
        axis.text.x = element_text(size = 15, face = 'bold', color = 'black'), 
        axis.text.y = element_text(size = 15, face = 'bold', color = 'black'),
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 15))

stats_garden_1 / stats_garden_2 / stats_garden_3 + 
  plot_annotation(tag_levels = 'A')

ggsave('Stats_garden.pdf',path = 'outputs/figures/', width = 20, height = 15.6, units = 'cm', dpi = 600)


#plot level

stats_plot_1<-ggplot(metrics_landscape_plots_bind, aes(x = as.factor(class), y = aream2, color = as.factor(class)))+
  geom_boxplot(outlier.size = 2, show.legend = F)+
  scale_x_discrete(labels = names_classes)+
  scale_y_continuous(name = expression(Area~(m^{'2'})))+
  scale_color_manual(values = colors_lc)+
  theme_bw()+
  theme(plot.title = element_text(size =18, face='bold'),
        axis.title.y = element_text(size = 16),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(), 
        axis.text.y = element_text(size = 15, face = 'bold', color = 'black'),
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 15))


stats_plot_2<-ggplot(metrics_landscape_plots_bind, aes(x = as.factor(class), y = clumpy, color = as.factor(class)))+
  geom_boxplot(outlier.size = 2, show.legend = F)+
  scale_x_discrete(labels = names_classes)+
  scale_y_continuous(name = 'Spatial \n aggregation', labels = scales::percent_format(accuracy = 1))+
  scale_color_manual(values = colors_lc)+
  theme_bw()+
  theme(plot.title = element_text(size =18, face='bold'),
        axis.title.y = element_text(size = 16),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(), 
        axis.text.y = element_text(size = 15, face = 'bold', color = 'black'),
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 15))


stats_plot_3<-ggplot(metrics_landscape_plots_bind, aes(x = as.factor(class), y = np, color = as.factor(class)))+
  geom_boxplot(outlier.size = 2, show.legend = F)+
  scale_x_discrete(labels = names_classes)+
  scale_y_continuous(name = 'Number of \n patches')+
  scale_color_manual(values = colors_lc)+
  theme_bw()+
  theme(plot.title = element_text(size =18, face='bold'),
        axis.title.y = element_text(size = 16),
        axis.title.x = element_blank(),
        axis.text.x = element_text(size = 15, face = 'bold', color = 'black'), 
        axis.text.y = element_text(size = 15, face = 'bold', color = 'black'),
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 15))

stats_plot_1 / stats_plot_2 / stats_plot_3 + 
  plot_annotation(tag_levels = 'A')

ggsave('Stats_plot.pdf',path = 'outputs/figures/', width = 20, height = 15.6, units = 'cm', dpi = 600)


# panel graph maps --------------------------------------------------------

essendon_plots<-allrasters$essendon

essendon_shapes<-allshapes$essendon

#rgb ortho

ortho<-ggRGB(essendon_plots, r = 3, g = 2, b = 1, ggLayer = F)+
  scale_x_continuous(expand = c(0, 0), breaks = c(144.9355, 144.9358, 144.9361),                     
                     labels = scales::number_format(accuracy = 0.0001,
                                                    decimal.mark = '.')) +
  scale_y_continuous(expand = c(0, 0), breaks = c(-37.7745, -37.7748, -37.7751),
                     labels = scales::number_format(accuracy = 0.0001,
                                                    decimal.mark = '.')) +
  geom_sf(data=essendon_shapes, size = 1.2, color = NA, fill = NA)+
  labs(x = '', y = '', title = 'Orthomosaic')+
  theme_bw()+
  theme(axis.text.x = element_text(size = 12, face = 'bold'), 
        axis.text.y = element_text(size = 12, face = 'bold'))+
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.0001, "cm"), pad_y = unit(0.7, "cm"),
                         style = north_arrow_fancy_orienteering)+
  annotation_scale(location = 'bl', style = 'ticks', text_col = 'black')

ortho_panel<-ggRGB(essendon_plots, r = 3, g = 2, b = 1, ggLayer = F)+
  scale_x_continuous(expand = c(0, 0), breaks = c(144.9355, 144.9358, 144.9361),                     
                     labels = scales::number_format(accuracy = 0.0001,
                                                    decimal.mark = '.')) +
  scale_y_continuous(expand = c(0, 0), breaks = c(-37.7745, -37.7748, -37.7751),
                     labels = scales::number_format(accuracy = 0.0001,
                                                    decimal.mark = '.')) +
  geom_sf(data=essendon_shapes, size = 1.2, color = NA, fill = NA)+
  labs(x = '', y = '')+
  theme_bw()+
  theme(axis.text.x = element_text(size = 12, face = 'bold'), 
        axis.text.y = element_text(size = 12, face = 'bold'))+
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.0001, "cm"), pad_y = unit(0.7, "cm"),
                         style = north_arrow_fancy_orienteering)+
  annotation_scale(location = 'bl', style = 'ticks', text_col = 'black')


ggsave('input_Ortho.pdf',path = 'outputs/figures/', width = 20, height = 15.6, units = 'cm', dpi = 600)

#chm

essendon_chm<-essendon_plots$CHM

#CHM_df<-essendon_chm%>%as.data.frame(xy = T, na.rm = T)%>%rename('CHM' = 3)

CHM_sample<-sampleRegular(essendon_chm, size = 5e5, asRaster = TRUE) %>%
  as.data.frame(xy = T, na.rm = T)%>%rename('CHM' = 3)

CHM_sample<-CHM_sample%>%mutate(CHM_fix = case_when(CHM>3 ~ 3, TRUE ~ CHM))


chm<-ggplot()+
  geom_raster(data = CHM_sample, aes(x = x, y = y, fill = CHM_fix))+
  geom_sf(data=essendon_shapes, size = 1.2, color = NA, fill = NA)+
  scale_fill_viridis(option = 'D', breaks = c(0,1.5,3), labels = c('0', '1.5', '>3'))+
  scale_x_continuous(expand = c(0, 0), breaks = c(144.9355, 144.9358, 144.9361),                     
                     labels = scales::number_format(accuracy = 0.0001,
                                                    decimal.mark = '.')) +
  scale_y_continuous(expand = c(0, 0), breaks = c(-37.7745, -37.7748, -37.7751),
                     labels = scales::number_format(accuracy = 0.0001,
                                                    decimal.mark = '.')) +
  labs(x = '', y = '', title = 'CHM height')+
  theme_bw()+
  theme(axis.text.x = element_text(size = 12, face = 'bold'), 
        axis.text.y = element_text(size = 12, face = 'bold'),
        legend.title = element_blank(),
        legend.position = c(0.08,0.35),
        legend.background = element_rect(fill=F))+
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.0001, "cm"), pad_y = unit(0.7, "cm"),
                         style = north_arrow_fancy_orienteering)+
  annotation_scale(location = 'bl', style = 'ticks', text_col = 'black')

chm_panel<-ggplot()+
  geom_raster(data = CHM_sample, aes(x = x, y = y, fill = CHM_fix))+
  geom_sf(data=essendon_shapes, size = 1.2, color = NA, fill = NA)+
  scale_fill_viridis(option = 'D', breaks = c(0,1.5,3), labels = c('0', '1.5', '>3'))+
  scale_x_continuous(expand = c(0, 0), breaks = c(144.9355, 144.9358, 144.9361),                     
                     labels = scales::number_format(accuracy = 0.0001,
                                                    decimal.mark = '.')) +
  scale_y_continuous(expand = c(0, 0), breaks = c(-37.7745, -37.7748, -37.7751),
                     labels = scales::number_format(accuracy = 0.0001,
                                                    decimal.mark = '.')) +
  labs(x = '', y = '')+
  theme_bw()+
  theme(axis.text.x = element_text(size = 12, face = 'bold'), 
        axis.text.y =  element_blank(),
        legend.title = element_blank(),
        legend.position = c(0.08,0.35),
        legend.background = element_rect(fill=F))+
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.0001, "cm"), pad_y = unit(0.7, "cm"),
                         style = north_arrow_fancy_orienteering)+
  annotation_scale(location = 'bl', style = 'ticks', text_col = 'black')

ggsave('input_CHM.pdf',path = 'outputs/figures/', width = 20, height = 15.6, units = 'cm', dpi = 600)


#texture

essendon_tex<-essendon_plots$mean

tex_sample<-sampleRegular(essendon_tex, size = 5e5, asRaster = TRUE) %>%
  as.data.frame(xy = T, na.rm = T)%>%rename('tex' = 3)

texture<-ggplot()+
  geom_raster(data = tex_sample, aes(x = x, y = y, fill = tex))+
  geom_sf(data=essendon_shapes, size = 1.2, color = NA, fill = NA)+
  scale_fill_viridis(option = 'D', labels = scales::percent_format(accuracy = 1))+
  scale_x_continuous(expand = c(0, 0), breaks = c(144.9355, 144.9358, 144.9361),                     
                     labels = scales::number_format(accuracy = 0.0001,
                                                    decimal.mark = '.')) +
  scale_y_continuous(expand = c(0, 0), breaks = c(-37.7745, -37.7748, -37.7751),
                     labels = scales::number_format(accuracy = 0.0001,
                                                    decimal.mark = '.')) +
  labs(x = '', y = '', title = 'Texture averages')+
  theme_bw()+
  theme(axis.text.x = element_text(size = 12, face = 'bold'), 
        axis.text.y = element_text(size = 12, face = 'bold'),
        legend.title = element_blank(),
        legend.position = c(0.1,0.35),
        legend.background = element_rect(fill=F))+
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.0001, "cm"), pad_y = unit(0.7, "cm"),
                         style = north_arrow_fancy_orienteering)+
  annotation_scale(location = 'bl', style = 'ticks', text_col = 'black')

texture_panel<-ggplot()+
  geom_raster(data = tex_sample, aes(x = x, y = y, fill = tex))+
  geom_sf(data=essendon_shapes, size = 1.2, color = NA, fill = NA)+
  scale_fill_viridis(option = 'D', labels = scales::percent_format(accuracy = 1))+
  scale_x_continuous(expand = c(0, 0), breaks = c(144.9355, 144.9358, 144.9361),                     
                     labels = scales::number_format(accuracy = 0.0001,
                                                    decimal.mark = '.')) +
  scale_y_continuous(expand = c(0, 0), breaks = c(-37.7745, -37.7748, -37.7751),
                     labels = scales::number_format(accuracy = 0.0001,
                                                    decimal.mark = '.')) +
  labs(x = '', y = '')+
  theme_bw()+
  theme(axis.text.x = element_text(size = 12, face = 'bold'), 
        axis.text.y = element_blank(),
        legend.title = element_blank(),
        legend.position = c(0.1,0.35),
        legend.background = element_rect(fill=F))+
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.0001, "cm"), pad_y = unit(0.7, "cm"),
                         style = north_arrow_fancy_orienteering)+
  annotation_scale(location = 'bl', style = 'ticks', text_col = 'black')

ggsave('input_texture.pdf',path = 'outputs/figures/', width = 20, height = 15.6, units = 'cm', dpi = 600)

#all

ortho_panel + chm_panel + texture_panel + 
  plot_annotation(tag_levels = 'A')

ggsave('input_all.pdf',path = 'outputs/figures/', width = 35, height = 15, units = 'cm', dpi = 600)


# confusion matrices ------------------------------------------------------

#overall

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

universal_model_plot

ggsave('confmat_model.pdf', path = 'outputs/figures/',
       width = 15.6, height = 15.6, units = 'cm', dpi = 600)

#by plot

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

ggsave('confmat_gardens.pdf', path = 'outputs/figures/',
       width = 42, height = 29.7, units = 'cm', dpi = 600)


# prediction example ------------------------------------------------------

essendon_pred<-all_predict$essendon

plot(essendon_pred, col = colors_lc)

ess_df<-essendon_pred%>%as.data.frame(xy = T, na.rm = T)%>%rename('LC' = 3)

ess_df_sample<-sampleRegular(essendon_pred, size = 5e5, asRaster = TRUE) %>%
  as.data.frame(xy = T, na.rm = T)%>%rename('LC' = 3)


ggplot()+
  geom_raster(data = ess_df_sample, aes(x = x, y = y, fill = as.factor(LC)))+
  geom_sf(data=essendon_shapes, size = 1.2, color = NA, fill = NA)+
  scale_fill_manual(values = colors_lc, labels = names_classes)+
  scale_x_continuous(expand = c(0, 0), breaks = c(144.9355, 144.9358, 144.9361),                     
                     labels = scales::number_format(accuracy = 0.0001,
                                                    decimal.mark = '.')) +
  scale_y_continuous(expand = c(0, 0), breaks = c(-37.7745, -37.7748, -37.7751),
                     labels = scales::number_format(accuracy = 0.0001,
                                                    decimal.mark = '.')) +
  labs(x = '', y = '', title = '')+
  theme_bw()+
  theme(axis.text.x = element_text(size = 12, face = 'bold'), 
        axis.text.y = element_text(size = 12, face = 'bold'),
        legend.title = element_blank(),
        legend.position = c(0.13,0.35),
        legend.background = element_rect(fill=F))+
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.0001, "cm"), pad_y = unit(0.7, "cm"),
                         style = north_arrow_fancy_orienteering)+
  annotation_scale(location = 'bl', style = 'ticks', text_col = 'black')


ggsave('output_ess.pdf',path = 'outputs/figures/', width = 20, height = 15.6, units = 'cm', dpi = 600)

#rgb inset

ess_detail<-st_read('data/roi/ess_6_smooth.gpkg')

ess_detail_4326<-st_read('data/roi/ess_6_smooth.gpkg')%>%st_transform(crs = 4326)


#crop

essendon_rgb_crop<-crop(essendon_plots,ess_detail)

ess_shapes_crop<-st_crop(essendon_shapes,ess_detail)
 
#inset map 

output_detail<-ggRGB(essendon_rgb_crop, r = 3, g = 2, b = 1, ggLayer = F)+
  geom_sf(data=ess_shapes_crop, aes(fill = as.factor(lc)), size = 1.2,
          color = 'black', show.legend = F, alpha = 0.5)+
  scale_fill_manual(values = colors_lc, labels = names_classes)+
  scale_x_continuous(expand = c(0, 0), breaks = c(144.93575, 144.93589, 144.936),                     
                     labels = scales::number_format(accuracy = 0.0001,
                                                    decimal.mark = '.')) +
  scale_y_continuous(expand = c(0, 0), breaks = c(-37.77465, -37.77475, -37.77485),
                     labels = scales::number_format(accuracy = 0.0001,
                                                    decimal.mark = '.')) +
  labs(x = '', y = '', title = '')+
  theme_bw()+
  theme(axis.text.x = element_text(size = 12, face = 'bold'), 
        axis.text.y = element_text(size = 12, face = 'bold'),
        legend.title = element_blank())

#main map with crop extent

ess_bbox<-st_as_sfc(st_bbox(ess_detail))

output_class<-ggplot()+
  geom_raster(data = ess_df_sample, aes(x = x, y = y, fill = as.factor(LC)))+
  geom_sf(data=ess_bbox, size = 1.2, color = 'darkblue', fill = NA)+
  scale_fill_manual(values = colors_lc, labels = names_classes)+
  scale_x_continuous(expand = c(0, 0), breaks = c(144.9355, 144.9358, 144.9361),                     
                     labels = scales::number_format(accuracy = 0.0001,
                                                    decimal.mark = '.')) +
  scale_y_continuous(expand = c(0, 0), breaks = c(-37.7745, -37.7748, -37.7751),
                     labels = scales::number_format(accuracy = 0.0001,
                                                    decimal.mark = '.')) +
  labs(x = '', y = '', title = '')+
  theme_bw()+
  theme(axis.text.x = element_text(size = 12, face = 'bold'), 
        axis.text.y = element_text(size = 12, face = 'bold'),
        legend.title = element_blank(),
        legend.position = c(0.15,0.35),
        legend.background = element_rect(fill=F))+
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.0001, "cm"), pad_y = unit(0.7, "cm"),
                         style = north_arrow_fancy_orienteering)+
  annotation_scale(location = 'bl', style = 'ticks', text_col = 'black')

output_class + output_detail +   plot_annotation(tag_levels = 'A')

ggsave('output_ess_detail.pdf',path = 'outputs/figures/', width = 35, height = 15, units = 'cm', dpi = 600)

#other way around

output_detail + output_class +   plot_annotation(tag_levels = 'A')

ggsave('output_ess_detail2.pdf',path = 'outputs/figures/', width = 35, height = 15, units = 'cm', dpi = 600)


# tables ------------------------------------------------------------------

#dir.create('outputs/tables')

#universal model

write.csv(stats_rf3_overall, 'outputs/tables/model_perf_overall.csv', row.names = F)

stats_rf3_TSS<-stats_rf3%>%mutate(TSS = (Sensitivity + Specificity)-1)

write.csv(stats_rf3_TSS, 'outputs/tables/model_perf_TSS.csv', row.names = F)

#garden level

write.csv(perf_garden_overallmerge, 'outputs/tables/model_perf_overall_gardens.csv', row.names = F)

perf_garden_merge_TSS<-perf_garden_merge%>%mutate(TSS = (Sensitivity + Specificity)-1)

write.csv(perf_garden_merge_TSS, 'outputs/tables/model_perf_gardens_TSS.csv', row.names = F)

#area metrics - garden level

write.csv(metrics_landscale_single_bind, 'outputs/tables/area_metrics+landscape_gardens.csv', row.names = F)

# - plot level

write.csv(metrics_landscape_plots_bind, 'outputs/tables/area_metrics+landscape_plots.csv', row.names = F)


