###Making Figure 3 - a 16 panel figure with four panels for each species (AUC, OR, TSS, SchD)
###requires the results.csv file for each species to make the first three panels and then the various filter localities to make the Schoener's D panel

packages <- c("ggplot2", "dismo", "ENMeval", "foreach", "cowplot", "viridis", "rgeos", "grid", "gtable", "ggpubr", "tidyverse", "doSNOW")
lapply(packages, library, character.only = T)
setwd("~/Desktop/SDM") #or wherever your working directory is
DATA <- read.csv("Dipo_results.csv", header = T) #each "results" csv file contains all statistics (columns) of each of the 100 iterations for each filter type (rows). This can be subset by using either the filter column (filter type) or type column (filter size) or both
DATA$type <- as.factor(DATA$type) #type will often be turned into numeric because that's what it seems like

#This function will create each panel individually because the problem is that there are different x axis scales depending on whether the environmental filter (p1 below) or spatial filter (p2 below) is used. 
#It uses the DATA object from a given results.csv file and will create a panel for a specific evaluation statistic (stat). You have to set the max and min for the y axis and specify whether you want a legend added. 
create.panel <- function(stat, max, min, legend = F) {
  
  if(stat == "Schoener's D") {
    p1 <- SCHD %>% dplyr::filter(filter != "spatial") %>% ggplot(aes(x = type, y = values, group = filter)) + geom_line(aes(col = filter), size = 1.2) +  scale_x_discrete(name = "Filters", limits = c("100", "75", "50", "40", "30", "25", "20", "15", "10"), labels = c("100", "75", "50", "40", "30", "25", "20", "15", "10")) + theme(legend.position = c(0.1, 0.2)) + scale_color_manual(values = COLS, name = "Filter Type") + scale_y_continuous(name = "Schoener's D", limits = c(0.5, 1), breaks = seq(0.5, 1, 0.1)) + panel_border(col = "black", size = 0.5)
    p2 <- SCHD %>% filter(filter == "spatial") %>% ggplot(aes(x = type, y = values, group = 1)) + geom_line(size = 1.2) + scale_x_discrete(name = "Distance\n", limits = c("1km", "5km", "10km", "25km", "50km"), labels = c("1km", "5km", "10km", "25km", "50km"), position = "top") + scale_y_continuous(name = "Schoener's D", limits = c(0.5, 1), breaks = seq(0.5, 1, 0.1)) + panel_border(col = "black", size = 0.5)
  } else{
  
  stat.name <- function(stat) {
    if(stat == "AUC") {
      NAME <- "AUC"
    }
    if(stat == "AUC.diff") {
      NAME <- "AUC Difference"
    }
    if(stat == "ORate.MPT") {
      NAME <- "Omission Rate\n(Minimum Presence Threshold)"
    }
    if(stat == "ORate.10PT") {
      NAME <- "Omission Rate (10% Threshold)"
    }
    if(stat == "TSS.MPT") {
      NAME <- "True Skills Statistic\n(Minimum Presence Threshold)"
    }
    if(stat == "TSS.10PT") {
      NAME <- "TSS (10% Threshold)"
    }
    NAME
  }
  
  NAME <- stat.name(stat)
  COLS <- c("#332288", "#88CCEE", "#44AA99", "#117733", "#999933", "#DDCC77", "#CC6677", "#AA4499")
  
  UNF <- DATA %>% filter(filter == "unfiltered") %>% summarize_(mm = paste0("min(", stat, ")"), mx = paste0("max(", stat, ")")) 
  FILT <- cbind.data.frame(level = c("100", "75", "50", "40", "30", "25", "20", "15", "10"), mm = rep(UNF$mm, 9), mx = rep(UNF$mx, 9))
FILT$level <- as.numeric(FILT$level)
  
  p1 <- DATA %>% dplyr::filter(!filter %in% c("spatial", "unfiltered")) %>% group_by(type, filter) %>% summarize_(mean = paste0("mean(", stat, ")"), sd = paste0("sd(", stat, ")")) %>% ggplot(aes(x = type, y = mean, group = filter)) + geom_ribbon(data = FILT, aes(x = level, ymin = mm, ymax = mx), fill = "grey20", alpha = 0.15, inherit.aes = F) + geom_line(aes(col = filter), size = 1.2) +  scale_x_discrete(name = "Filters", limits = c("100", "75", "50", "40", "30", "25", "20", "15", "10"), labels = c("100", "75", "50", "40", "30", "25", "20", "15", "10")) + geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd, width = 0.1, col = filter)) + theme(legend.position = c(0.1, 0.8)) + scale_color_manual(values = COLS, name = "Filter Type") + scale_y_continuous(name = NAME, limits = c(min, max), breaks = seq(min, max, 0.05)) + panel_border(col = "black", size = 0.5)
  p2 <- DATA %>% filter(filter == "spatial") %>% group_by(type) %>% summarize_(mean = paste0("mean(", stat, ")"), sd = paste0("sd(", stat, ")")) %>% ggplot(aes(x = type, y = mean, group = 1)) + geom_line(size = 1.2) + scale_x_discrete(name = "Distance\n", limits = c("1km", "5km", "10km", "25km", "50km"), labels = c("1km", "5km", "10km", "25km", "50km"), position = "top") + geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd, width = 0.1)) + theme() + scale_y_continuous(name = NAME, limits = c(min, max), breaks = seq(min, max, 0.05)) + panel_border(col = "black", size = 0.5)
  
}
  
  if(legend == F){
    p1 <- p1 + theme(legend.position = "none")
    p2 <- p2 + theme(legend.position = "none")
  }
  g1 <- ggplotGrob(p1)
  g2 <- ggplotGrob(p2)
  pp <- c(g1$layout[g1$layout$name %in% "panel", 1:4])
  index <- which(g2$layout$name %in% "panel")
  alt.panel <- g2$grobs[[index]]
  g1 <- gtable_add_grob(g1, alt.panel, pp$t, pp$l, pp$t, pp$r, clip = "off")
  index <- which(g2$layout$name == "axis-t")
  xaxis <- g2$grobs[[index]]
  xaxis$children[[1]]$y <- unit.c(unit(0, "npc"), unit(0, "npc"))
  g1 <- gtable_add_rows(g1, g2$heights[g2$layout[index, ]$t], pp$t-1)
  g1 <- gtable_add_grob(g1, xaxis, pp$t, pp$l, pp$t, pp$r, clip = "off", name = "topxlab")
  index <- which(g2$layout$name == "xlab-t")
  title <- g2$grobs[[index]]
  g1 <- gtable_add_rows(g1, g2$heights[g2$layout[index, ]$t], pp$t-6)
  g1 <- gtable_add_grob(g1, title, pp$t, pp$l, pp$t, pp$r, clip = "off")
  g1
}
g1 <- create.panel("AUC", 0.9, 0.8, legend = T)
g2 <- create.panel("ORate.10PT", 0.3, 0.05, legend = F)
g3 <- create.panel("TSS.10PT", 0.7, 0.4, legend = F)
#g1 <- create.panel("AUC.diff", 0.2, 0, legend = T)
#g2 <- create.panel("ORate.MPT", 0.25, 0, legend = F)
#g3 <- create.panel("TSS.MPT", 0.65, 0.2, legend = F)


DATA <- read.csv("Aud_results.csv", header = T)
a1 <- create.panel("AUC", 0.90, 0.65, legend = T)
a2 <- create.panel("ORate.10PT", 0.2, 0.05, legend = F)
a3 <- create.panel("TSS.10PT", 0.65, 0.25, legend = F)
#a1 <- create.panel("AUC.diff", 0.15, 0, legend = T)
#a2 <- create.panel("ORate.MPT", 0.1, 0, legend = F)
#a3 <- create.panel("TSS.MPT", 0.25, 0, legend = F)


DATA <- read.csv("Loph_results.csv", header = T)
l1 <- create.panel("AUC", 0.95, 0.8, legend = T)
l2 <- create.panel("ORate.10PT", 0.3, 0.05, legend = F)
l3 <- create.panel("TSS.10PT", 0.8, 0.4, legend = F)
#l1 <- create.panel("AUC.diff", 0.2, 0, legend = T)
#l2 <- create.panel("ORate.MPT", 0.2, 0, legend = F)
#l3 <- create.panel("TSS.MPT", 0.65, 0.15, legend = F)

DATA <- read.csv("Cerc_results.csv", header = T)
c1 <- create.panel("AUC", 0.9, 0.6, legend = T)
c2 <- create.panel("ORate.10PT", 0.3, 0.05, legend = F)
c3 <- create.panel("TSS.10PT", 0.6, 0.1, legend = F)
#c1 <- create.panel("AUC.diff", 0.15, 0, legend = T)
#c2 <- create.panel("ORate.MPT", 0.1, 0, legend = F)
#c3 <- create.panel("TSS.MPT", 0.25, 0, legend = F)

###The Schoener's D panel is more complicated because you need predictions from each species for all 78 datasets. 

###Dipodomys compactus
PRES <- read.csv("Dipo_presence.csv", header = T)[, -1] #reads in presence data
ABSV <- read.csv("Dipo_background.csv", header = T, row.names = 1) #reads in background data
coordinates(PRES) <- ~lon + lat #turns the presence data into a SpatialPointsDataFrame
EXT <- extent(c(PRES@bbox[, 1] - 1, PRES@bbox[, 2] + 1)[c(1, 3, 2, 4)]) #increases bbox by 1 degree and turns into an extent object to create the study extent
PRES <- read.csv("Dipo_presence.csv", header = T)[, -1] #reread the presence data as a data.frame
PRED <- list.files(path = "~/Desktop/SDM/wc_30s", pattern = "bil", full.names = T) #read in your predictor variables wherever they are (use the path argument for this)
PRED <- stack(PRED) #stack them into a RasterStack
PREDI <- stack(PRED$bio_2, PRED$bio_4, PRED$bio_6, PRED$bio_10, PRED$bio_13, PRED$bio_14, PRED$bio_18) #subset the variables to remove highly correlated ones
MXNT <- maxent(PREDI, PRES[, 1:2], ABSV[, 1:2]) #run the model for the unfiltered dataset
MXP <- predict(PREDI, MXNT, ext = EXT, progress = '') #grab the unfiltered prediction
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
SCH <- calc.niche.overlap(stack(c(MXP, MP))) #creates a stack and calculates Schoener's D of said RasterStack
SCHD <- cbind.data.frame(filter = c(as.vector(sapply(c("bio1_12", "bio2_18", "pca2", "pca2r", "pca3", "pca3r", "pca4", "pca4r"), function(x) rep(x, 9))), rep("spatial", 5)), type = c(rep(c("100", "75", "50", "40", "30", "25", "20", "15", "10"), 8), c("1km", "5km", "10km", "25km", "50km")), values = SCH[-1, 1]) #puts it in a handy data.frame format for easy plotting
g4 <- create.panel("Schoener's D", 1, 0.5, legend = F)


###Icterus graduacauda
PRES <- read.csv("Aud_presence.csv", header = T)[, -1]
ABSV <- read.csv("Aud_background.csv", header = T, row.names = 1)
coordinates(PRES) <- ~lon + lat
EXT <- extent(c(PRES@bbox[, 1] - 1, PRES@bbox[, 2] + 1)[c(1, 3, 2, 4)])
PRES <- read.csv("Aud_presence.csv", header = T)[, -1]
PRED <- list.files(path = "~/Desktop/SDM/wc2-5", pattern = "bil", full.names = T) 
PRED <- stack(PRED)
PREDI <- stack(PRED$bio1, PRED$bio2, PRED$bio4, PRED$bio8, PRED$bio9, PRED$bio10, PRED$bio12, PRED$bio14, PRED$bio15)
MXNT <- maxent(PREDI, PRES[, 1:2], ABSV[, 1:2])
MXP <- predict(PREDI, MXNT, ext = EXT, progress = '')
ROWS <- lapply(list.files(path = "~/Desktop/SDM/Aud", full.names = T), function(x) read.csv(x, header = T, row.names = 1))
PCA <- foreach(i = 1:length(ROWS)) %do% {
  lapply(1:ncol(ROWS[[i]]), function(x) PRES[ROWS[[i]][, x] == 1, ])
} 
M <- foreach(i = 1:length(ROWS), .packages = 'dismo') %dopar% {
  m <- lapply(PCA[[i]], function(x) maxent(PREDI, x[, 1:2], ABSV[, 1:2]))
}
MP <- foreach(i = 1:length(unlist(M)), .packages = "dismo") %dopar% {
  mp <- predict(PREDI, unlist(M)[[i]], ext = EXT, progress = '')
}
SCH <- calc.niche.overlap(stack(c(MXP, MP)))
SCHD <- cbind.data.frame(filter = c(as.vector(sapply(c("bio1_12", "bio2_4", "pca2", "pca2r", "pca3", "pca3r", "pca4", "pca4r"), function(x) rep(x, 9))), rep("spatial", 5)), type = c(rep(c("100", "75", "50", "40", "30", "25", "20", "15", "10"), 8), c("1km", "5km", "10km", "25km", "50km")), values = SCH[-1, 1])
a4 <- create.panel("Schoener's D", 1, 0.5, legend = F)

###Lophuromys woosnami
PRES <- read.csv("Loph_presence.csv", header = T, row.names = 1)[, 2:3]
ABSV <- read.csv("Loph_background.csv", header = T)
coordinates(PRES) <- ~decimalLongitude + decimalLatitude
EXT <- extent(c(PRES@bbox[, 1] - 1, PRES@bbox[, 2] + 1)[c(1, 3, 2, 4)])
PRES <- read.csv("Loph_presence.csv", header = T)[, -1]
PRED <- list.files(path = "~/Desktop/SDM/wc_30s", pattern = "bil", full.names = T) 
PRED <- stack(PRED)
PREDI <- stack(PRED$bio_2, PRED$bio_4, PRED$bio_5, PRED$bio_7, PRED$bio_13, PRED$bio_15, PRED$bio_18, PRED$bio_19)
MXNT <- maxent(PREDI, PRES[, 1:2], ABSV[, 1:2])
MXP <- predict(PREDI, MXNT, ext = EXT, progress = '')
ROWS <- lapply(list.files(path = "~/Desktop/SDM/Loph", full.names = T), function(x) read.csv(x, header = T, row.names = 1))
PCA <- foreach(i = 1:length(ROWS)) %do% {
  lapply(1:ncol(ROWS[[i]]), function(x) PRES[ROWS[[i]][, x] == 1, ])
} 
M <- foreach(i = 1:length(ROWS), .packages = 'dismo') %dopar% {
  m <- lapply(PCA[[i]], function(x) maxent(PREDI, x[, 1:2], ABSV[, 1:2]))
}
MP <- foreach(i = 1:length(unlist(M)), .packages = "dismo") %dopar% {
  mp <- predict(PREDI, unlist(M)[[i]], ext = EXT, progress = '')
}
SCH <- calc.niche.overlap(stack(c(MXP, MP)))
SCHL <- cbind.data.frame(filter = c(as.vector(sapply(c("bio1_12", "bio5_15", "pca2", "pca2r", "pca3", "pca3r", "pca4", "pca4r"), function(x) rep(x, 9))), rep("spatial", 5)), type = c(rep(c("100", "75", "50", "40", "30", "25", "20", "15", "10"), 8), c("1km", "5km", "10km", "25km", "50km")), values = SCH[-1, 1])
l4 <- create.panel("Schoener's D", 1, 0.5, legend = F)

###Cercotrichas leucophrys
PRES <- read.csv("Cerc_presence.csv", header = T, row.names = 1)[, 1:2]
ABSV <- read.csv("Cerc_background.csv", header = T)
coordinates(PRES) <- ~lon + lat
EXT <- extent(c(PRES@bbox[, 1] - 1, PRES@bbox[, 2] + 1)[c(1, 3, 2, 4)])
PRES <- read.csv("Cerc_presence.csv", header = T, row.names = 1)[, 1:2]
PRED <- list.files(path = "~/Desktop/SDM/wc2-5", pattern = "bil", full.names = T) 
PRED <- stack(PRED)
PREDI <- stack(PRED$bio15, PRED$bio17, PRED$bio18, PRED$bio19, PRED$bio3, PRED$bio6, PRED$bio7, PRED$bio10, PRED$bio12)
MXNT <- maxent(PREDI, PRES[, 1:2], ABSV[, 1:2])
MXP <- predict(PREDI, MXNT, ext = EXT, progress = '')
ROWS <- lapply(list.files(path = "~/Desktop/SDM/Cerc", full.names = T), function(x) read.csv(x, header = T, row.names = 1))
PCA <- foreach(i = 1:length(ROWS)) %do% {
  lapply(1:ncol(ROWS[[i]]), function(x) PRES[ROWS[[i]][, x] == 1, ])
} 
M <- foreach(i = 1:length(ROWS), .packages = 'dismo') %dopar% {
  m <- lapply(PCA[[i]], function(x) maxent(PREDI, x[, 1:2], ABSV[, 1:2]))
}
MP <- foreach(i = 1:length(unlist(M)), .packages = "dismo") %dopar% {
  mp <- predict(PREDI, unlist(M)[[i]], ext = EXT, progress = '')
}
SCHD <- calc.niche.overlap(stack(c(MXP, MP)))
SCHDC <- cbind.data.frame(filter = c(as.vector(sapply(c("bio1_12", "bio6_12", "pca2", "pca2r", "pca3", "pca3r", "pca4", "pca4r"), function(x) rep(x, 9))), rep("spatial", 5)), type = c(rep(c("100", "75", "50", "40", "30", "25", "20", "15", "10"), 8), c("1km", "5km", "10km", "25km", "50km")), values = SCHD[-1, 1])
c4 <- create.panel("Schoener's D", 1, 0.5, legend = F)

#######Putting it all together
LABS <- c("Dipodomys\ncompactus", "Icterus\ngraduacauda", "Lophuromys\nwoosnami", "Cercotrichas\nleucophrys")
#create the species name labels

LAB <- lapply(LABS, function(x) ggdraw() + draw_label(x, angle = 90, fontface = "bold.italic", size = 18))
#draws the labels to add them to the side

plot_grid(plot_grid(plotlist = LAB, ncol = 1), plot_grid(g1, g2, g3, g4, a1, a2, a3, a4, l1, l2, l3, l4, c1, c2, c3, c4, ncol = 3), ncol = 2, rel_widths = c(0.05, 1))

ggsave("Figure3Test.pdf", width = 18, height = 22)

#for Figure A1, change legend in the function to c(0.1, 0.8) from c(0.1, 0.25)

plot_grid(plot_grid(plotlist = LAB, ncol = 1), plot_grid(g1, g2, g3, a1, a2, a3, l1, l2, l3, c1, c2, c3, ncol = 3), ncol = 2, rel_widths = c(0.05, 1))
ggsave("FigureA1.pdf", width = 12, height = 18)
