swissMrP <- function(x, ...) UseMethod("swissMrP")

summary.swissMrP <-
function(object,...){
	if (dim(as.matrix(object))[2]==1) uncert <- 0
	if (dim(as.matrix(object))[2]!=1) uncert <- 1
	
	cantons.name <- c("ZH", "BE", "LU", "UR", "SZ", "OW", "NW", "GL", "ZG", "FR", "SO", "BS", "BL", "SH", "AR", "AI", "SG", "GR", "AG", "TG", "TI", "VD", "VS", "NE", "GE", "JU")

	xx <- rowMeans(as.matrix(object))
	xx1 <- data.frame(cantons.name,xx)
	xx2 <- xx1[order(xx1[,2])[c(1,13,14,26)],]
	cat(paste("   min: ",xx2[1,1]," ",round(xx2[1,2],2),"%\n" ))
	cat(paste("median: ",xx2[2,1]," ",round(xx2[2,2],2),"%\n" ))
	cat(paste("median: ",xx2[3,1]," ",round(xx2[3,2],2),"%\n" ))
	cat(paste("   max: ",xx2[4,1]," ",round(xx2[4,2],2),"%\n" ))

		
}
