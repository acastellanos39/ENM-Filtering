###Making Figures 4 and 5 - the 25 panel anomaly map madness (and additional 25 panel predictive suitability maps)
###requires locality information for all datasets for each species and uses a lot of the same code as Figure 3
packages <- c("reshape2", "ggplot2", "dismo", "ENMeval", "foreach", "rasterVis", "cowplot", "viridis", "rgeos", "grid", "gtable", "ggpubr")
#lapply(packages, install.packages)
lapply(packages, library, character.only = T)

ROWS <- lapply(list.files(path = "~/Desktop/SDM/Dipo", full.names = T), function(x) read.csv(x, header = T, row.names = 1)) #read in each locality csv file for the particular species (I found it easiest to put them all in their own folder and use list.files to grab them all at once)
PCA <- foreach(i = 1:length(ROWS)) %do% {
  lapply(1:ncol(ROWS[[i]]), function(x) PRES[ROWS[[i]][, x] == 1, ])
}  
#Use a foreach loop to grab make nested lists of each dataset
cl <- makeCluster(4, "SOCK") #running the models for each dataset is easiest with parallel processing, otherwise run it when you don't need R
registerDoSNOW(cl)
M <- foreach(i = 1:length(ROWS), .packages = 'dismo') %dopar% {
  m <- lapply(PCA[[i]], function(x) maxent(PREDI, x[, 1:2], ABSV[, 1:2]))
}
#runs all models
MP <- foreach(i = 1:length(unlist(M)), .packages = "dismo") %dopar% {
  mp <- predict(PREDI, unlist(M)[[i]], ext = EXT, progress = '')
}
#grabs all predictions

DIFF <- lapply(MP, function(x) x - MXP)
#creates the anomaly for all predictions
#Make sure to check this to determine the appropriate scale to use for your anomaly maps

VEC <- c(73:77, 1, 3, 5, 7, 9, 10, 12, 14, 16, 18, 19, 21, 23, 25, 27, 55, 57, 59, 61, 63) #grabs the 25 Raster object positions that will be used for the figure

PLOTS <- lapply(VEC, function(x) gplot(MP[[x]]) + geom_tile(aes(fill = value)) + scale_fill_viridis(breaks = seq(0, 1, 0.2), limits = c(0, 1), na.value = NA, name = "Suitability") + geom_polygon(data = BORD, aes(x = long, y = lat, group = group), fill = NA, col = "grey30", size = 0.05) + geom_polygon(data = BOUND, aes(x = long, y = lat, group = group), fill = NA, col = "black", size = 0.2) + scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0)) + coord_fixed() + theme(axis.text =  element_blank(), axis.ticks = element_blank(), axis.title = element_blank(), panel.border = element_rect(fill = NA, color = "grey50"), panel.background = element_rect(fill = "grey90"), panel.grid = element_blank(), legend.position = "none", plot.margin = unit(c(0, 0, 0, 0), "cm")))
#generates a list of plots of predictive suitability

PLOTS <- lapply(VEC, function(x) gplot(DIFF[[x]]) + geom_tile(aes(fill = value)) + scale_fill_gradient2(low = "red", high = "darkcyan", limits = c(-0.75, 0.75), na.value = NA, name = "Mean Filter Difference") + geom_polygon(data = BORD, aes(x = long, y = lat, group = group), fill = NA, col = "grey30", size = 0.05) + geom_polygon(data = BOUND, aes(x = long, y = lat, group = group), fill = NA, col = "black", size = 0.2) + scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0)) + coord_fixed() + theme(axis.text =  element_blank(), axis.ticks = element_blank(), axis.title = element_blank(), panel.border = element_rect(fill = NA, color = "grey50"), panel.background = element_rect(fill = "grey90"), panel.grid = element_blank(), legend.position = "none", plot.margin = unit(c(0, 0, 0, 0), "cm")))
#generates a list of plots of anomaly differences between unfiltered and filtered predictions

LEG <- get_legend(gplot(MP[[1]]) + geom_tile(aes(fill = value)) + scale_fill_viridis(breaks = seq(0, 1, 0.2), limits = c(0, 1), na.value = NA, name = "Suitability") + theme(legend.background = element_blank(), legend.title.align = 0.5, legend.title = element_text(size = 10, angle = 90), legend.text = element_text(size = 8)) + guides(fill = guide_colorbar(ticks = F, barwidth = 1, barheight = 17, title.position = "left", direction = "vertical")))
#generates the legend for the predicted suitability maps

LEG <- get_legend(gplot(DIFF[[1]]) + geom_tile(aes(fill = value)) + scale_fill_gradient2(low = "red", high = "darkcyan", limits = c(-1, 1), breaks = seq(-1, 1, 0.25), na.value = NA, name = "Mean Filter Difference") + theme(legend.background = element_blank(), legend.title.align = 0.5, legend.title = element_text(size = 10, angle = 90), legend.text = element_text(size = 8)) + guides(fill = guide_colorbar(ticks = F, barwidth = 1, barheight = 17, title.position = "left", direction = "vertical")))
#generates the legend for the anomaly maps

LABS <- c("Geographic", "BIO1 and 12", "BIO6 and 12", "PCA 2 Axes", "PCA 4 Axes") #the labels to be placed for each row of maps
LAB <- lapply(LABS, function(x) ggdraw() + draw_label(x, angle = 90, fontface = "bold"))
#draws the labels


plot_grid(plot_grid(plotlist = LAB, ncol = 1), plot_grid(plot_grid(plotlist = PLOTS[1:5], ncol = 5, labels = c("1km", "5km", "10km", "25km", "50km"), label_x = 0, hjust = -0.3, label_fontface = "plain", label_size = 12, label_colour = "white"), plot_grid(plotlist = PLOTS[6:25], ncol = 5, labels = rep(c("100", "50", "30", "20", "10"), 5), label_x = 0, hjust = -0.5, label_fontface = "plain", label_size = 12, label_colour = "white"), nrow = 2, rel_heights = c(0.25, 1)), LEG, ncol = 3, rel_widths = c(0.06, 1, 0.15))
#generates the final figure

ggsave("FigureA8.png", width = 8, height = 8)
#Aud is 8 x 8.5
#Dipo is 7.5 x 8.5
#Loph is 6.5 x 8.5
#Cerc is 8 x 8 

