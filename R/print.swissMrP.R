

swissMrP <- function(x, ...) UseMethod("swissMrP")

print.swissMrP <- function(x, ...){
	
	if(class(x)!="swissMrP")
		stop("print.swissMrP only defined for objects of class swissMrP")
		
	if (dim(as.matrix(x))[2]==1) uncert <- 0
	if (dim(as.matrix(x))[2]!=1) uncert <- 1
	
	class(x) <- 'oops'
	cantons.name <- c("ZH", "BE", "LU", "UR", "SZ", "OW", "NW", "GL", "ZG", "FR", "SO", "BS", "BL", "SH", "AR", "AI", "SG", "GR", "AG", "TG", "TI", "VD", "VS", "NE", "GE", "JU")


	# w/o uncertainty
	if (uncert==0){
		preport <- round(x,2)
		reportL <- data.frame(cbind(cantons.name,preport))
		colnames(reportL) <- c("Canton", "Prediction")		
		cat("===========================\n")		
		print(reportL,row.names=FALSE)			
		cat("---------------------------\n")		
		cat("(No uncertainty computed.)\n")		
		cat("===========================\n")		
		
		}	

	# w/ uncertainty
	if (uncert==1){
		preport <- round(rowMeans(x),2)
		reportL <- data.frame(cbind(cantons.name,preport))
		colnames(reportL) <- c("Canton", "Prediction")		
		cat("===========================\n")		
		print(reportL,row.names=FALSE)			
		cat("---------------------------\n")		
		cat("(Uncertainty computed.)\n")		
		cat("===========================\n")		
		
		}	
		
}
