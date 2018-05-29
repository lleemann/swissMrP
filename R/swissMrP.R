swissMrP <-
  function(response.model, augment.data=NA, augment.row=0, uncertainty=FALSE, Number.sim=1000, region){
    
    ptm <- proc.time()
    alarm <-0
    aug <- 1
    if(missing(augment.data)) aug <- 0
    if (missing(region)) region.pointer <- c(4,2,6,6,6,6,6,5,6,2,2,3,3,5,5,5,5,5,3,5,7,1,1,2,1,2)
    if (missing(region)==FALSE) region.pointer <- region
    
    
    # read in RESPONSE model
    model.response <- response.model
    beta.fe <- fixef(model.response)
    alpha.re <- ranef(model.response)
    
    cantons.name <- c("ZH", "BE", "LU", "UR", "SZ", "OW", "NW", "GL", "ZG", "FR", "SO", "BS", "BL", "SH", "AR", "AI", "SG", "GR", "AG", "TG", "TI", "VD", "VS", "NE", "GE", "JU")
    canton.pointer <- c(1:26)
    
    alpha.var <- names(model.response@flist)
    check.a <- c("age", "cantonnr", "education", "region", "woman")==sort(alpha.var) #
    S <- length(which(check.a==FALSE))
    
    if (S!=0) stop(gettextf("The names of the random effects do not match! \n  Please use 'age', 'cantonnr', 'education', 'region', and 'woman'.")) # 
    
    beta.var <- names(fixef(model.response))
    
    # get the data for L2
    dim22 <- dim(model.response@frame)[2]
    dim21 <- dim22 - 4 # - 4
    data.level2 <- model.response@frame[,-c(1,dim21:dim22)]
    Knr <- model.response@frame$cantonnr
    
    #print(dim22)
    #print(dim21)  
    dimX <- 2
    #print((dim(model.response@frame)[2]))
    #print(dim(data.level2)[2])
    ifelse(dim(model.response@frame)[2]>7, dimX <- dim(data.level2)[2] + 1, dimX <- dimX) #>7
    #print(dimX)
    X2.26 <- matrix(NA,26,dimX)
    #print(X2.26)
    # more than one X on L2
    if (dim(model.response@frame)[2]>7){	#>7
      for (ww in 1:26){
        #	print(colMeans(data.level2[Knr==ww,]))
        X2.26[ww,] <- c(1,colMeans(data.level2[Knr==ww,])) 
      }
    }
    # exactly one X on L2
    if (dim(model.response@frame)[2]==7){	#==7				
      for (ww in 1:26){
        X2.26[ww,] <- c(1,mean(data.level2[Knr==ww])) 
      }
    }
    # No X on L2
    if (dim(model.response@frame)[2]==6){	#==6			
      X2.26 <- as.matrix(rep(1,26))
    }
    
    #print(X2.26)
    ## Augment data step:
    if (aug==1){
      X2.26[augment.row,] <- augment.data
    }
    
    
    if (uncertainty==TRUE){
      if (Number.sim<25){
        stop(gettextf("To estimate the uncertainty one needs a sufficient number of simulations. \n  You chose a too low value for 'Number.sim'.")) 
      }
      
      # use SIM command to get N.sim draws of RE's & FE's
      N.sim <- Number.sim
      sim.block <- sim(model.response,N.sim)
      educ.a.block <- ranef(sim.block)$education[,,1]
      age.a.block <- ranef(sim.block)$age[,,1]
      woman.a.block <- ranef(sim.block)$woman[,,1]
      ctnr.a.block0 <- ranef(sim.block)$cantonnr[,,1]
      
      #### FIX MISSING CANTONS
      if (aug==1){
        
        
      }
      #####
      
      ctnr.a.block <- matrix(0,N.sim,26)
      for (r in as.numeric(colnames(ctnr.a.block0))){
        dd <- which(as.numeric(colnames(ctnr.a.block0))==r)
        ctnr.a.block[,r] <- ctnr.a.block0[,dd]
      }
      
      if (dim(ranef(sim.block)$region[,,1])[2]==7) region.a.block <- ranef(sim.block)$region[,,1]
      #if (dim(ranef(sim.block)$region[,,1])[2]==6) region.a.block <- cbind(ranef(sim.block)$region[,,1],0)
      #### FIX FOR MISSING REGION
      
      if (dim(ranef(sim.block)$region[,,1])[2]!=7){
        region.a.block <- ranef(sim.block)$region[,,1]
        mis.region <- which(!c(1,2,3,4,5,6,7) %in%as.numeric(names(table(model.response@frame$region))))
        region.fix <- matrix(0,N.sim, 7)
        b <- 0
        for (a in 1:7){
          c <- a+b
          if(!a%in%mis.region){ region.fix[,a] <- region.a.block[,c]}
          if(a%in%mis.region){ 
            region.fix[a] <- 0
            b <- b+1
          }
        }
        region.a.block <- region.fix
      }
      
      ####
      
      
      
      
      beta.a.block <- fixef(sim.block)
      
      # Help objects for the census information (educ6, age4, woman2)
      education.pointer <- c(rep(1,8),rep(2,8),rep(3,8),rep(4,8),rep(5,8),rep(6,8))		# for each of the 48 SV
      age.pointer <- rep(c(1,1,2,2,3,3,4,4),6)												# ditto
      woman.pointer <- rep(c(1,2),24)											
      
      check.vector <- matrix(NA,48,N.sim) # remember structure of census: educ (6), age (4), woman (2)
      for (j in 1:N.sim){
        for (i in 1:48){
          check.vector[i,j] <- educ.a.block[j,education.pointer[i]] + age.a.block[j,age.pointer[i]] + woman.a.block[j,woman.pointer[i]]
        }
      }
      
      # create individual level contribution on latent, 48*24
      ylat.lev1.48.26 <- rbind(check.vector, check.vector, check.vector, check.vector, check.vector, check.vector, check.vector, check.vector, check.vector, check.vector, check.vector, check.vector, check.vector, check.vector, check.vector, check.vector, check.vector, check.vector, check.vector, check.vector, check.vector, check.vector, check.vector, check.vector, check.vector, check.vector)
      
      # create ceontext level contribution on latent, 48*24
      ylat.lev2 <-  X2.26 %*% t(beta.a.block)  + t(ctnr.a.block) + t(region.a.block)[region.pointer,]
      ylat.lev2.48.26 <- matrix(NA, 1248, N.sim)
      for (zz in 1:26){
        aa <- (zz-1)*48 + 1
        bb <- zz*48
        for (jj in aa:bb){
          ylat.lev2.48.26[jj,] <- ylat.lev2[zz,] 											
        }
      }
      
      y.lat <- ylat.lev2.48.26 + ylat.lev1.48.26
      
      # read-in census data
      #data("census.RData")
      census48 <- census$census48.MAZH2013
      colStandard <- function(x){
        x <- x/sum(x)
      }
      weight.48.26 <- apply(census48,2,colStandard)
      
      # generate predictions
      phat.weighted <- matrix(NA, 1248, N.sim)
      for (ss in 1:N.sim){
        phat.weighted[,ss] <- pnorm(y.lat[,ss]) * weight.48.26
      }
      
      # prediction per canton:
      phat.canton <- matrix(NA,26,N.sim)
      for (nn in 1:N.sim){
        for (tt in 1:26){
          aa <- (tt-1)*48 + 1
          bb <- tt*48
          phat.canton[tt, nn] <- sum(phat.weighted[aa:bb, nn])
        }
      }
      
      # Estimate is mean of all simulations
      preport <- 100*round(rowMeans(phat.canton),3)
      preport <- paste(preport,rep("%",26), sep=" ")
      
      #testi <- rowMeans(phat.canton)
      #if (NaN %in% testi) {
      #  alarm <- 1
      #  missing.canton <- which(is.na(testi))
      #}
      # end of uncertainty==TRUE	
    }
    
    
    
    if (uncertainty==FALSE){
      # Read out coefs
      educ.a.block <- alpha.re$education[,,1]
      age.a.block <- alpha.re$age[,,1]
      woman.a.block <- alpha.re$woman[,,1]
      ctnr.a.block0 <- alpha.re$cantonnr[,,1]
      ctnr.a.block <- rep(0,26)
      keeper <- which(as.numeric(c(1:26) %in% response.model@frame$cantonnr)==1)
      a <- 0
      for (r in 1:26){
        if (r %in% keeper){
          ctnr.a.block[r] <- ctnr.a.block0[r-a]
        } 
        if ((r %in% keeper)==FALSE){
          ctnr.a.block[r] <- 0
          a <- a+1
        }	
      }
      
      if (length(alpha.re$region[,,1])==7) region.a.block <- alpha.re$region[,,1]
      #### FIX FOR MISSING REGION
      
      if (length(alpha.re$region[,,1])!=7){
        region.a.block <- alpha.re$region[,,1]
        mis.region <- which(!c(1,2,3,4,5,6,7) %in%as.numeric(names(table(model.response@frame$region))))
        region.fix <- rep(0,7)
        b <- 0
        for (a in 1:7){
          c <- a+b
          if(!a%in%mis.region){ region.fix[a] <- region.a.block[c]}
          if(a%in%mis.region){ 
            region.fix[a] <- 0
            b <- b+1
          }
        }
        region.a.block <- region.fix
      }
      
      ####
      beta.a.block <- beta.fe
      
      # Help objects for the census information (educ6, age4, woman2)
      education.pointer <- c(rep(1,8),rep(2,8),rep(3,8),rep(4,8),rep(5,8),rep(6,8))		# for each of the 48 SV
      age.pointer <- rep(c(1,1,2,2,3,3,4,4),6)												# ditto
      woman.pointer <- rep(c(1,2),24)											
      
      check.vector <- rep(NA,48) # remember structure of census: educ (6), age (4), woman (2)
      for (i in 1:48){
        check.vector[i] <- educ.a.block[education.pointer[i]] + age.a.block[age.pointer[i]] + woman.a.block[woman.pointer[i]]
      }
      
      # create individual level contribution on latent, 48*26
      ylat.lev1.48.26 <- rep(check.vector, 26)
      
      # create ceontext level contribution on latent, 48*24
      #print(region.a.block)
      ylat.lev2 <-  X2.26 %*% as.matrix(beta.a.block) + ctnr.a.block   + region.a.block[region.pointer]
      #			ylat.lev2 <-  X2.26 %*% beta.a.block
      ylat.lev2.48.26 <- rep(NA, 1248)
      for (zz in 1:26){
        aa <- (zz-1)*48 + 1
        bb <- zz*48
        for (jj in aa:bb){
          ylat.lev2.48.26[jj] <- ylat.lev2[zz] 											
        }
      }
      
      y.lat <- ylat.lev2.48.26 + ylat.lev1.48.26
      
      # read-in census data
      #data("census.RData")
      census48 <- census$census48.MAZH2013
      colStandard <- function(x){
        x <- x/sum(x)
      }
      weight.48.26 <- apply(census48,2,colStandard)
      
      # generate predictions
      phat.weighted <- pnorm(y.lat) * weight.48.26
      
      # prediction per canton:
      phat.canton <- rep(NA,26)
      for (tt in 1:26){
        aa <- (tt-1)*48 + 1
        bb <- tt*48
        phat.canton[tt] <- sum(phat.weighted[aa:bb])
      }
      
      preport <- 100*round(phat.canton,3)
      preport <- paste(preport,rep("%",26), sep=" ")
      
      
      #testi <- phat.canton
      #if (NaN %in% testi) {
      #  alarm <- 1
      #  missing.canton <- which(is.na(testi))
      #}
      
      # end of uncertainty==FALSE	
    }
    
   
    
    
    
    
    
     if (length(ranef(response.model)$cantonnr[,,1])!=26){
      alarm <- 1
      missing.canton <- cantons.name[which(!  c(1:26) %in% as.numeric(rownames(ranef(response.model)$cantonnr)) )]
    } 
    
    if (length(fixef(response.model))==1) {alarm <- 0}
    if (alarm==1){
      N.missing <- length(as.vector(missing.canton))
      TEXT <- paste(missing.canton,sep=" ", collapse=" / ")
      print(TEXT)
      writeLines(paste("Your survey model's data not contain any respondents from canton(s):", TEXT, "\n(see option 'augment.data'; Predictions are still made for the other cantons.)", sep=" "))
      
    } 
    time1 <- proc.time() - ptm
    
    class(phat.canton) <- "swissMrP"
    return(phat.canton)
    
  }

