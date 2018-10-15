packages <- c("parallel", "dismo", "raster", "adehabitatHR", "doSNOW", "foreach", "rgdal", "sqldf", "maps", "testthat", "roxygen2", "sf", "magrittr", "tidyverse", "reshape2")
lapply(packages, library, character.only = T)
#performs k-fold cross validation when given presence coordinates, background coordinates, k, predictor variables, and any maxent arguments (if you so choose). 
#This function will grab the mean AUC, difference in training and test AUC, omission rate (using the minimum presence threshold and 10% presence threshold), and true skills statistic (using the same thresholds). Likewise, it grabs the mean percent contribution and permutation importance of each predictor variable. The mean is calculated from each of the k runs. 

detectCores()
cl <- makeCluster(10, "SOCK")
registerDoSNOW(cl)
PRES <- read.csv("Dipo_presence.csv", header = T, row.names = 1)[, 1:2]
ABSV <- read.csv("Dipo_background.csv", header = T, row.names = 1)[, 1:2]
MCP <- mcp(SpatialPoints(PRES[, 1:2], proj4string = CRS("+proj=longlat +datum=WGS84")), percent = 100)
EXT <- extent(c(MCP@bbox[, 1] - 1, MCP@bbox[, 2] + 1)[c(1, 3, 2, 4)])
PRED <- list.files(path = "~/Desktop/SDM/bio_30s_bil", pattern = "bil", full.names = T)
PRED <- stack(PRED)
PRED <- stack(PRED$bio_2, PRED$bio_4, PRED$bio_6, PRED$bio_10, PRED$bio_13, PRED$bio_14, PRED$bio_18)


cross.valid <- function(presence, background, k, predictors, args = NULL) {
	group <- list(kfold(presence, k), kfold(background, k))
	TRP <- lapply(1:k, function(x) presence[which(group[[1]] != x), ])
	TRA <- lapply(1:k, function(x) background[which(group[[2]] != x), ])
	TEP <- lapply(1:k, function(x) presence[which(group[[1]] == x), ])
	TEA <- lapply(1:k, function(x) background[which(group[[2]] == x), ])
	STAT <- foreach(i = 1:k, .packages= 'dismo') %dopar% {
	m <- maxent(predictors, TRP[[i]], TRA[[i]], args = args)
	me <- evaluate(TEP[[i]], TEA[[i]], m, predictors)
	stat <- c(me@auc, abs(me@auc - m@results["Training.AUC", ]), abs(sum(me@presence > as.data.frame(m@results)["Minimum.training.presence.logistic.threshold", ])/length(me@presence) - 1), abs(sum(me@presence > as.data.frame(m@results)["X10.percentile.training.presence.logistic.threshold", ])/length(me@presence) - 1), (me@TPR[which(me@t > as.data.frame(m@results)["Minimum.training.presence.logistic.threshold", ])[1]] + me@TNR[which(me@t > as.data.frame(m@results)["Minimum.training.presence.logistic.threshold", ])[1]] - 1), (me@TPR[which(me@t > as.data.frame(m@results)["X10.percentile.training.presence.logistic.threshold", ])[1]] + me@TNR[which(me@t > as.data.frame(m@results)["X10.percentile.training.presence.logistic.threshold", ])[1]] - 1))
	pred.stat <- c(m@results[paste(names(predictors), ".contribution", sep = ""), ], m@results[paste(names(predictors), ".permutation.importance", sep = ""), ])
	list(stat, pred.stat)
}
	STATS <- apply(t(sapply(STAT, '[[', 1)), 2, mean)
	names(STATS) <- c("AUC", "AUC.diff", "ORate.MPT", "ORate.10PT", "TSS.MPT", "TSS.10PT")
	PRED.STAT <- apply(sapply(STAT, '[[', 2), 1, mean)
	list(STATS, PRED.STAT)
}

stopCluster(cl)


###Example
FILT <- read.csv("PCA2FilterLocalities_Dipo.csv", header = T, row.names = 1) #read in the .csv of filtered localities
EFILT <- lapply(1:9, function(x) PRES[which(FILT[, x] == 1), ]) #grab only those presence points from each filtered dataset
PCA2 <- foreach(x = 1:9, .packages = c('dismo', 'foreach')) %dopar% {
	foreach(i = 1:100, .packages = c('dismo', 'foreach')) %dopar% {
	cross.valid(EFILT[[x]], ABSV, 5, PRED, seed = i, args = c("linear=true", "quadratic=true", "product=false", "threshold=false", "hinge=false"))
}
}
NAME <- c("100", "75", "50", "40", "30", "25", "20", "15", "10") #names of the environmental filter distances
RES <- lapply(1:9, function(x) cbind.data.frame(do.call(rbind, purrr::map(PCA2[[x]], 1)), do.call(rbind, purrr::map(PCA2[[x]], 2)), type = rep(NAME[x], 100))) #grabs the results of each iteration and binds them together for each distance
RESU <- cbind(do.call(rbind, RES), filter = "pca2") #binds all the distances for one filter type together

###Way to run the environmental filter for all 9 grid sizes 
sample.envR <- function(coords, filters, scale = c(100, 75, 50, 40, 30, 25, 20, 15, 10)) 
{
f <- lapply(filters, function(x) sapply(scale, function(y) diff(range(x))/y))
lapply(1:length(scale), function(x) envSample(coords, filter = filters, res = lapply(f, "[[", x), do.plot = F))
}
EFILT <- sample.envR(PRES[, 1:2], list(PRES$bio6, PRES$bio12))
NUMS <- lapply(1:9, function(y) sapply(1:dim(EFILT[[y]])[1], function(x) which(EFILT[[y]][x, 1] == PRES[, 1] & EFILT[[y]][x, 2] == PRES[, 2])))
RNUMS <- matrix(0, 3157, 9)
for (i in 1:length(NUMS)) {
	for (j in 1:length(NUMS[[i]])) {
		RNUMS[NUMS[[i]][j], i] <- 1
	}
}
colnames(RNUMS) <- c(100, 75, 50, 40, 30, 25, 20, 15, 10)
rownames(RNUMS) <- rownames(PRES)


#Random Sampling as described in the Supplemental
PRES <- read.csv("Aud_presence.csv", header = T)[, 2:3]
cloud.samp <- function(presence, number, start, end) {
	foreach(i = 1:number) %do% {
		set.seed(i)
presence[sample(rownames(presence), sample(start:end, 1)), ]

	}
}
CLO <- cloud.samp(PRES, 5000, 52, 625)




