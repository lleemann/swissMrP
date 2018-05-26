plot.swissMrP <-
function(x, conf.int=0.95, col="black", design.dot=TRUE, lab, ...){
		uncert <- FALSE
		if(missing(lab)) lab <- "Degree of Estimated Support"
		if (length(x)>26) uncert <- TRUE
		if (uncert==FALSE){
			order.c <- order(x)
			cantons.name <- c("ZH", "BE", "LU", "UR", "SZ", "OW", "NW", "GL", "ZG", "FR", "SO", "BS", "BL", "SH", "AR", "AI", "SG", "GR", "AG", "TG", "TI", "VD", "VS", "NE", "GE", "JU")
			span <- x[order.c][26]-x[order.c][1]
			if (design.dot==TRUE){
				 plot(x[order.c],c(1:26), bty="n",col=col,pch=19,ylab="",xlab=lab, yaxt="n", xlim=c(x[order.c][1]-0.05,x[order.c][1]+0.05+span), ...)
			axis(2,at=c(1:26),labels=cantons.name[order.c], tick=FALSE, las=2)
			}
			if (design.dot==FALSE) {
				plot(x[order.c],c(1:26), bty="n",col="white",pch=19,ylab="",xlab=lab, yaxt="n", xlim=c(x[order.c][1]-0.05,x[order.c][1]+0.05+span), ...)
			text(x[order.c],c(1:26),c("ZH", "BE", "LU", "UR", "SZ", "OW", "NW", "GL", "ZG", "FR", "SO", "BS", "BL", "SH", "AR", "AI", "SG", "GR", "AG", "TG", "TI", "VD", "VS", "NE", "GE", "JU")[order.c], col=col)
			#axis(2,at=c(1:26),labels=cantons.name[order.c], tick=FALSE, las=2)
			}
		}
		if (uncert==TRUE){
			#print(length(rowMeans(x)[order.c]))
			order.c <- order(rowMeans(x))
			cantons.name <- c("ZH", "BE", "LU", "UR", "SZ", "OW", "NW", "GL", "ZG", "FR", "SO", "BS", "BL", "SH", "AR", "AI", "SG", "GR", "AG", "TG", "TI", "VD", "VS", "NE", "GE", "JU")
			
			if (design.dot==TRUE){
			plot(rowMeans(x)[order.c],c(1:26), bty="n",col=col,pch=19,ylab="",xlab=lab, yaxt="n", xlim=c(min(x)-0.05,max(x)+0.05), ...)
			axis(2,at=c(1:26),labels=cantons.name[order.c], tick=FALSE, las=2)
			n.sim <- dim(x)[2]
			ql <-(1-conf.int)/2
			qu <-(1-conf.int)/2 + conf.int
			lb <- floor(ql*n.sim)
			ub <- ceiling(qu*n.sim)
			CI <- matrix(NA,26,2)
			for (t in 1:26){
				CI[t,1] <- sort(x[t,])[lb]
				CI[t,2] <- sort(x[t,])[ub]
			}
			CI.order <- CI[order.c,]
			for (t in 1:26){
				segments(CI.order[t,1],t,CI.order[t,2],t,lwd=0.5, col=col)
			}
			text(max(x)-0.1,2,paste("CI%=", conf.int))
		}
		if (design.dot==FALSE){
			plot(rowMeans(x)[order.c],c(1:26), bty="n",col="white",pch=19,ylab="",xlab=lab, yaxt="n", xlim=c(min(x)-0.05,max(x)+0.05), ...)
			text(rowMeans(x)[order.c],c(1:26),c("ZH", "BE", "LU", "UR", "SZ", "OW", "NW", "GL", "ZG", "FR", "SO", "BS", "BL", "SH", "AR", "AI", "SG", "GR", "AG", "TG", "TI", "VD", "VS", "NE", "GE", "JU")[order.c], col=col)
			n.sim <- dim(x)[2]
			ql <-(1-conf.int)/2
			qu <-(1-conf.int)/2 + conf.int
			lb <- floor(ql*n.sim)
			ub <- ceiling(qu*n.sim)
			CI <- matrix(NA,26,2)
			for (t in 1:26){
				CI[t,1] <- sort(x[t,])[lb]
				CI[t,2] <- sort(x[t,])[ub]
			}
			CI.order <- CI[order.c,]
			for (t in 1:26){
				segments(CI.order[t,1],t,CI.order[t,2],t,lwd=0.5, col=col)
			}
			text(max(x)-0.1,2,paste("CI%=", conf.int))
		}
		}


	}
