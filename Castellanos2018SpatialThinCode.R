########Reduced spThin::thin function that gets rid of the aesthetically pleasing wrapper and halves computation time. Similar to most of the filters, it takes the presence points, a set distance, and a number of repetitions##############

library(fields)

thinR <- function (presence, distance, reps) 
{
    reduced.rec.dfs <- list()
    DIST <- rdist.earth(x1 = presence, miles = FALSE)
    diag(DIST) <- NA
   for (Rep in 1:reps) {
        rec.df <- presence
        DistMat <- DIST
        while (min(DistMat, na.rm = TRUE) < distance & nrow(rec.df) > 1) 
        {
            CloseRecs <- which(DistMat < distance, arr.ind = TRUE)[, 1]
            RemoveRec <- as.numeric(names(which(table(CloseRecs) == max(table(CloseRecs)))))
            if (length(RemoveRec) > 1) {
                RemoveRec <- sample(RemoveRec, 1)
            }
            rec.df <- rec.df[-RemoveRec, ]
            DistMat <- DistMat[-RemoveRec, -RemoveRec]
            if (length(DistMat) == 1) {
                break
            }
        }
        colnames(rec.df) <- c("Longitude", "Latitude")
        reduced.rec.dfs[[Rep]] <- rec.df
    }
    list(table(unlist(lapply(reduced.rec.dfs, nrow))), sort(table(unlist(lapply(reduced.rec.dfs, rownames))), decreasing = T))
}

#THIN1 <- thinR(PRES[, 1:2], 1, 1000)
#THIN10 <- PRES[names(THIN10[[2]][1:117]), ]

####Function for choosing the appropriate filtered points (often you have points chosen an equal number of times that are close by one another and should only choose one instead of both)
sp.choose <- function(thin.results, distance, reps) {
	NUM <- as.numeric(names(which.max(thin.results[[1]])))
	NUMS <- NUM - length(which(thin.results[[2]] == reps))
	NAMES <- names(which(thin.results[[2]] != reps))
	DIST <- rdist.earth(PRES[NAMES, 1:2], miles = F)
	PAIR <- lapply(1:length(NAMES), function(x) NAMES[which(DIST[, x] != 0 & DIST[, x] <= distance)])
	NAME <- NULL
	for(i in 1:length(NAMES)) {
	NAME <- c(NAME, names(which.max(c(thin.results[[2]][as.character(PAIR[[i]])], thin.results[[2]][NAMES[i]]))))
	#NAME <- NAMES[!NAMES %in% c(PAIR[[i]], NAMES[i])[c(PAIR[[i]], NAMES[i]) != MAX]]
	NAME <- NAME[!duplicated(NAME)]
	if(length(NAME) == NUMS) break 
	}
	c(names(which(thin.results[[2]] == 1000)), NAME)
	}	
	
	
THIN <- foreach(i = c(1, 5, 10, 25, 50), .packages = "fields") %dopar% {
	PRES[sp.choose(thinR(PRES[, 1:2], i, 1000), i, 1000), 1:2]
}
NUMS <- lapply(1:5, function(y) sapply(1:dim(THIN[[y]])[1], function(x) which(THIN[[y]][x, 1] == PRES[, 1] & THIN[[y]][x, 2] == PRES[, 2])))
RNUMS <- matrix(0, 3157, 5)
for (i in 1:length(NUMS)) {
	for (j in 1:length(NUMS[[i]])) {
		RNUMS[NUMS[[i]][j], i] <- 1
	}
}
colnames(RNUMS) <- c(1, 5, 10, 25, 50)
rownames(RNUMS) <- rownames(PRES)
write.csv(RNUMS, "SpatialFilterLocalities_Cerc.csv")

NAME <- c("1km", "5km", "10km", "25km", "50km")

SPCROSS <- foreach(x = 1:5, .packages = c('dismo', 'foreach')) %dopar% {
	foreach(i = 1:100, .packages = c('dismo', 'foreach')) %dopar% {
	cross.valid(THIN[[x]], ABSV, 5, PRED, seed = i, args = c("linear=true", "quadratic=true", "product=false", "threshold=false", "hinge=false"))
}
}
RES <- lapply(1:5, function(x) cbind.data.frame(do.call(rbind, purrr::map(SPCROSS[[x]], 1)), do.call(rbind, purrr::map(SPCROSS[[x]], 2)), type = rep(NAME[x], 100)))
RESU <- cbind(do.call(rbind, RES), filter = "spatial")
