map.MrP <-
function(x, colors.m, threshold, main, labels=TRUE, legend.text1, legend.text2, ...){
	
		if(class(x)!="swissMrP")
		stop("map.MrP only defined for objects of class swissMrP")

    k <- 200
		if(missing(main)) main <- ""
		if(missing(legend.text1)) legend.text1 <- ""
		if(missing(legend.text2)) legend.text2 <- ""
		if(missing(colors.m))  	colors.m <- c(rgb(165,0,38,k,maxColorValue=255),rgb(215, 48,39,k,maxColorValue=255), rgb(244, 109, 67,k,maxColorValue=255), rgb(253, 174, 97,k,maxColorValue=255), rgb(254, 224, 139,k,maxColorValue=255), rgb(217, 239, 139,k,maxColorValue=255), rgb(166, 217, 106,k,maxColorValue=255), rgb(102, 189, 99,k,maxColorValue=255), rgb(26, 152, 80,k,maxColorValue=255),rgb(0, 104, 55,k,maxColorValue=255))

    if(missing(threshold)) threshold <- seq(0,1,by=1/(length(colors.m)))
    
    if(length(x) != 26){
      x <- rowMeans(x)
    }

		# reading-in shape files
		#data("zip1.rda")
		KT <- zip1$KT
		data.in <- cbind(as.vector(x),c(1:26))
		data1 <- data.in[KT,]
		zip1$data <- data1
		col.pointer <- rep(NA,26)
		
		for (i in 1:26){
				col.pointer[i] <- max(which(zip1$data[i,1]>threshold))
				}
	
	sp::plot(zip1, col= colors.m[col.pointer], main=main, ...)
	title(main=main, line=2)
  
  # legend
  if (missing(threshold)==TRUE){
    l.number <- ceiling((length(colors.m))/2)
    r.number <- (length(colors.m))-l.number
    lab1 <- c(NA)
    for (i in 1:l.number){
      step <- 1/(length(colors.m))
      a <- 0 + (i-1)*step
      b <- step*i
      lab1 <- c(lab1,paste(round(100*a,0),"-",round(100*b,0),"%",sep=""))
      if(i==l.number) lab1 <- lab1[-1]
    }
    
    lab2 <- c(NA)
    for (i in 1:r.number){
      step <- 1/(length(colors.m))
      a <- 0 + (l.number + i-1)*step
      b <- step*(l.number +i)
      lab2 <- c(lab2,paste(round(100*a,0),"-",round(100*b,0),"%",sep=""))
      if(i==r.number) lab2 <- lab2[-1]
    }
  }
    
	if(missing(threshold)==FALSE){
      l.number <- ceiling((length(threshold)-1)/2)
      r.number <- length(threshold)-l.number -1      
      lab1 <- c(NA)
      for (i in 1:l.number){
        a <- i
        b <- i+1
        lab1 <- c(lab1,paste(round(100*threshold[a],0),"-",round(100*threshold[b],0),"%",sep=""))
        if(i==l.number) lab1 <- lab1[-1]
      }
      lab2 <- c(NA)    
      for (i in (l.number+1):(l.number+r.number)){
        a <- i
        b <- i+1
        lab2 <- c(lab2,paste(round(100*threshold[a],0),"-",round(100*threshold[b],0),"%",sep=""))
        if(i==(r.number+l.number-1)) lab2 <- lab2[-1]
      }
		
		}  
	legend(x=490577,y=104297, legend=lab1, fill=colors.m[1:l.number], bty="n", xpd=NA)
	legend(x=750577,y=104297,legend=lab2, fill=colors.m[(l.number+1):(l.number+r.number)], bty="n", xpd=NA)
	if(labels==TRUE){
			text(660000,74297,legend.text1, xpd=NA)
			text(660000,63297,legend.text2, xpd=NA)
			} 
	}
