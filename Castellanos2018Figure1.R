###How to make Figure 1 - four panel map of the occurrences used for each species

packages <- c("tidyverse", "sp", "raster", "cowplot", "rgeos")
lapply(packages, library, character.only = T)

#it helps to start with the widespread species for each one so that you only need to grab country and elevation data once
setwd("~/Desktop/SDM") #easiest to have all the presence data in one centralized folder (maybe dedicated to this figure because this will download A LOT of files for the country boundaries)
PRES <- read.csv("Aud_presence.csv", header = T)[, -1] #read in coordinates
coordinates(PRES) <- ~lon + lat #make a SpatialPointsDataFrame
EXT <- extent(c(PRES@bbox[, 1] - 2, PRES@bbox[, 2] + 2)[c(1, 3, 2, 4)]) #add 2 degrees (for mapping purposes) and turn into an extent object (for cropping purposes in the near future)
PRES <- read.csv("Aud_presence.csv", header = T)[, -1] #read it back in as a data.frame because ggplot2 is hateful

#grab the elevation data
ELEV <- lapply(c("USA", "MEX", "BLZ", "GTM"), function(x) raster::getData(name = "alt", country = x))
ELEV <- list(ELEV[[1]][[1]], ELEV[[2]], ELEV[[3]], ELEV[[4]])
ELEV <- do.call(merge, ELEV)
ELEV <- crop(ELEV, EXT)
#make sure that the elevation data isn't pixelated when used in ggplot2 (may not be needed depending on what you are using)
alt <- cbind.data.frame(coordinates(ELEV), value = values(ELEV))

#grab country boundaries
BORD <- lapply(c("USA", "MEX", "BLZ", "GTM"), function(x) raster::getData(name = "GADM", country = x, level = 0))
BORD <- do.call(bind, BORD)
BORD <- crop(BORD, EXT)

#create the plot
AUD <- ggplot(alt) + geom_raster(aes(x = x, y = y, fill = value)) + scale_fill_gradientn(colors = c("white", "black"), name = "Elevation (m)", na.value = "grey95", breaks = c(500, 1500, 2500, 3500, 4500, 5500), labels = c("500", "1,500", "2,500", "3,500", "4,500", "5,500")) + geom_polygon(data = BORD, aes(x = long, y = lat, group = group), fill = NA, color = "black", size = 0.175) + geom_point(data = PRES[, 1:2], aes(x = lon, y = lat), col = "#88000090", pch = 19,  size = 2) +  scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0)) + ylab("/n") + guides(fill = guide_colorbar(barwidth = 1.25, barheight = 10, title.position = "top", direction = "vertical", ticks = F)) + coord_fixed() + theme(axis.text =  element_text(size = 11, face = "bold", color = "black"), axis.title = element_blank(), panel.border = element_rect(fill = NA), panel.background = element_rect(fill = "grey90", color = "grey50"), panel.grid = element_blank(), legend.background = element_blank(), legend.title = element_text(size = 10), legend.text = element_text(size = 9))

DIPO <- read.csv("Dipo_presence.csv", header = T)[, -1]
coordinates(DIPO) <- ~lon + lat
EXT <- extent(c(DIPO@bbox[, 1] - 2, DIPO@bbox[, 2] + 2)[c(1, 3, 2, 4)])
DIPO <- read.csv("Dipo_presence.csv", header = T)[, -1]

DIP <- ggplot(alt) + geom_raster(aes(x = x, y = y, fill = value)) + scale_fill_gradientn(colors = c("white", "black"), name = "Elevation (m)", na.value = "grey95", breaks = c(0, 1500, 3000, 4500, 5500), labels = c("0", "1,500", "3,000", "4,500", "5,500")) + geom_polygon(data = BORD, aes(x = long, y = lat, group = group), fill = NA, color = "black", size = 0.20) + geom_point(data = DIPO[, 1:2], aes(x = lon, y = lat), col = "#88000090", pch = 19, size = 2.5) + scale_color_manual(name = "", values = "#88000090", labels = "Occurences") + scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(breaks = seq(24, 30, 2), expand = c(0, 0)) + ylab("/n") + guides(fill = guide_colorbar(barwidth = 1, barheight = 4.45, title.position = "top", direction = "vertical", ticks = F)) + coord_fixed(xlim = EXT[1:2], ylim = EXT[3:4]) + theme(axis.text =  element_text(size = 11, face = "bold", color = "black"), axis.title = element_blank(), panel.border = element_rect(fill = NA), panel.background = element_rect(fill = "grey90", color = "grey50"), panel.grid = element_blank(), legend.position = c(0.84, 0.2), legend.background = element_blank(), legend.title = element_text(size = 10), legend.text = element_text(size = 8))

#creates a dummy plot that is simply used to grab the "occurence" legend dot
LEG <- ggplot() + geom_point(data = DIPO[, 1:2], aes(x = lon, y = lat, col = "#88000090"), pch = 19, size = 2.5) + scale_color_manual(name = "", values = "#88000090", labels = "Occurences", position = "right") + theme(legend.margin = margin(1, 1, 1, 1), legend.background = element_blank(), legend.text = element_text(size = 10))

#grabs this legend object
LEG2 <- get_legend(LEG)

CERC <- read.csv("Cerc_presence.csv", header = T, row.names = 1)[, 1:2]
coordinates(CERC) <- ~lon + lat
EXT <- extent(c(CERC@bbox[, 1] - 2, CERC@bbox[, 2] + 2)[c(1, 3, 2, 4)])
CERC <- read.csv("Cerc_presence.csv", header = T, row.names = 1)[, 1:2]

ELEV <- lapply(c("COD", "RWA", "UGA", "BDI", "TZA", "ETH", "KEN", "CAF", "COG", "GAB", "AGO", "NAM", "ZMB", "BWA", "MOZ", "ZWE", "SOM", "SSD", "CMR", "LSO", "GNQ", "SDN", "TCD", "NGA", "MWI", "MDG", "DJI"), function(x) raster::getData(name = "alt", country = x))
#South Africa is strange with its elevation data
ZAF <- raster::getData(name = "alt", country = "ZAF")
ELEV <- do.call(merge, ELEV)
ELEV <- list(ELEV, ZAF[[1]])
ELEV <- do.call(merge, ELEV)
ELEV <- crop(ELEV, EXT)
balt <- cbind.data.frame(coordinates(ELEV), value = values(ELEV))

BORD <- lapply(c("COD", "RWA", "UGA", "BDI", "TZA", "ETH", "KEN", "CAF", "COG", "GAB", "AGO", "NAM", "ZMB", "BWA", "ZAF", "MOZ", "ZWE", "SOM", "SSD", "CMR", "LSO", "GNQ", "SDN", "TCD", "NGA", "MWI", "MDG", "DJI"), function(x) raster::getData(name = "GADM", country = x, level = 0))
BORD <- do.call(bind, BORD)
BORD <- crop(BORD, EXT)

CER <- ggplot(balt) + geom_raster(aes(x = x, y = y, fill = value)) + scale_fill_gradientn(colors = c("white", "black"), name = "Elevation (m)", na.value = "grey95", breaks = c(500, 1500, 2500, 3500, 4500, 5500), labels = c("500", "1,500", "2,500", "3,500", "4,500", "5,500")) + geom_polygon(data = BORD, aes(x = long, y = lat, group = group), fill = NA, color = "black", size = 0.175) + geom_point(data = CERC[, 1:2], aes(x = lon, y = lat), col = "#88000090", pch = 19,  size = 2) +  scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0)) + ylab("/n") + guides(fill = guide_colorbar(barwidth = 1.25, barheight = 10, title.position = "top", direction = "vertical", ticks = F)) + coord_fixed() + theme(axis.text =  element_text(size = 11, face = "bold", color = "black"), axis.title = element_blank(), panel.border = element_rect(fill = NA), panel.background = element_rect(fill = "grey90", color = "grey50"), panel.grid = element_blank(), legend.margin = margin(1, 1, 1, 1), legend.background = element_blank(), legend.title = element_text(size = 10), legend.text = element_text(size = 9))

#grabs the elevation legend from here (the full range of elevation is found for this species)
LEG1 <- get_legend(CER)

LOPH <- read.csv("Loph_presence.csv", header = T, row.names = 1)[, 2:3]
coordinates(LOPH) <- ~decimalLongitude + decimalLatitude
EXT <- extent(c(LOPH@bbox[, 1] - 2.5, LOPH@bbox[, 2] + 2.5)[c(1, 3, 2, 4)])
LOPH <- read.csv("Loph_presence.csv", header = T, row.names = 1)[, 2:3]

LOP <- ggplot(balt) + geom_raster(aes(x = x, y = y, fill = value)) + scale_fill_gradientn(colors = c("white", "black"), name = "Elevation (m)", na.value = "grey95", breaks = c(500, 1500, 2500, 3500, 4500, 5500), labels = c("500", "1,500", "2,500", "3,500", "4,500", "5,500")) + geom_polygon(data = BORD, aes(x = long, y = lat, group = group), fill = NA, color = "black", size = 0.175) + geom_point(data = LOPH[, 1:2], aes(x = decimalLongitude, y = decimalLatitude), col = "#88000090", pch = 19,  size = 2) +  scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0), breaks = c(3, 1, -1, -3, -5)) + ylab("/n") + guides(fill = guide_colorbar(barwidth = 1.25, barheight = 10, title.position = "top", direction = "vertical", ticks = F)) + coord_fixed(xlim = EXT[1:2], ylim = EXT[3:4]) + theme(axis.text =  element_text(size = 11, face = "bold", color = "black"), axis.title = element_blank(), panel.border = element_rect(fill = NA), panel.background = element_rect(fill = "grey90", color = "grey50"), panel.grid = element_blank(), legend.position = c(0.835, 0.5), legend.background = element_blank(), legend.title = element_text(size = 10), legend.text = element_text(size = 9))

#puts each of these map panels together
FIG <- plot_grid(DIP + theme(legend.position = "none"), AUD + theme(legend.position = "none"), LOP + theme(legend.position = "none"), CER + theme(legend.position = "none"), nrow = 2, labels = c("A", "B", "C", "D"), hjust = 0, label_fontface = "bold", label_size = 14, label_colour = "black", rel_widths = c(1, 1, 1, 1), axis = "tblr")

#adds an area of blank space on the right to put the legends in
calt <- plot_grid(FIG, NULL, nrow = 1, rel_widths = c(1, 0.25))
#draws the legends in this blank spot
ggdraw(calt) + draw_plot(LEG2, 0.8, 0.1) + draw_plot(LEG1, 0.8, -0.1)
ggsave("Figure1.pdf", width = 8, height = 8)
