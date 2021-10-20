require(ggplot2)
require(segmented)
require(VGAM)

geom.text.size = 8; theme.size = (14/5)*geom.text.size
esperc <- 0.01 # for linear segmented regression, starting for estimation from 1 percentile value (98% winsorization)
span <- 0.8 # loess smoothing span
nsmall <- 2
show <- FALSE
width <- 24; height <- 10

savefig <- function(fileName, dpi, width, height, units, type){
  #' Function to save figures
  #' @param fileName file name of the figure
  #' @param dpi resolution, e.g., 300
  #' @param width figure width in inches (units)
  #' @param height figure height in inches (units)
  #' @param units unit name for width and height, e.g., 'in'
  #' @param type figure type: 'png', png figure; 'pdf', pdf figure; 'both', both png and pdf figures
  
  if(type=="png"){ file <- paste(fileName, ".png", sep=""); ggsave(file, dpi = dpi, width = width, height = height, units = units) }
  if(type=="pdf"){ file <- paste(fileName, ".pdf", sep=""); ggsave(file, dpi = dpi, width = width, height = height, units = units) }
  if(type=="both"){
    file <- paste(fileName, ".png", sep=""); ggsave(file, dpi = dpi, width = width, height = height, units = units)
    file <- paste(fileName, ".pdf", sep=""); ggsave(file, dpi = dpi, width = width, height = height, units = units)
  }
}

calMSE_AIC <- function(my.pred, my.model, df, X, Y){
  #' calculate mean squared error and AIC
  #' @param my.pred model prediction
  #' @param my.model regression model
  #' @param df raw data
  #' @param X x feature name
  #' @param Y y feature name
  #' @return return a dataframe with MSE and AIC as columns
  resDF <- data.frame()
  
  mse <- 0; count <- 0
  for(i in 1:nrow(my.pred)){
    x <- my.pred[i, X] 
    y.pred <- my.pred[i, Y]; y.raw <- mean(df[df[,X]==x, Y], na.rm=TRUE)
    if(!is.na(y.raw)){ 
      count <- count + 1; mse <- mse + (y.pred-y.raw)^2
    }
  }
  mse <- mse/count
  
  if(inherits(my.model, "loess")){ aic<-NA #aic <- loess.aic(my.model) 
  }else{ aic <- AIC(my.model) }
  
  resDF <- rbind(resDF, data.frame(MSE=mse, AIC=aic))
  return(resDF)
}

drawSegLoess <- function(df, X, Y, xlab, ylab, yRange, yrate, title, FileName, show, noSeg){
  #' draw main effect: loess and segment regression, and record MSE, AIC information of different models
  #' @param df dataset
  #' @param X X variable (independent)
  #' @param Y Y variable (dependent)
  #' @param xlab x-axis label
  #' @param ylab y-axis label
  #' @param yRange y-axis range
  #' @param yrate 0.85
  #' @param title figure title
  #' @param FileName figure file name and the csv file name
  #' @param show whether show summary of models or not
  #' @param noSeg how many segmented points to be estimated
  
  newdf <- df[!is.na(df[,Y]) & !is.na(df[,X]), ] # remove nan
  Xside <- newdf[,X]; Yside <- newdf[,Y]
  xlim <- c(floor(min(newdf[,X])), floor(max(newdf[,X]))+1)
  
  # baseline model: loess regression
  my.loess <- loess(Yside ~ Xside, data = newdf, span=span) # smoothing span
  if(show) print(summary(my.loess))
  my.loess.pred <- data.frame(Y = predict(my.loess, newdf), X = Xside) 
  names(my.loess.pred)[1] <- Y; names(my.loess.pred)[2] <- X
  
  # linear regression
  my.lm <- lm(Yside ~ Xside, data = newdf)
  if(show) print(summary(my.lm))
  coef <- summary(my.lm)$coefficients[2,1]; err <- summary(my.lm)$coefficients[2,2]; ci <- coef + c(-1,1)*err*qt(0.975, 42)
  my.lm.pred <- data.frame(Y = predict(my.lm, newdf), X = Xside)
  names(my.lm.pred)[1] <- Y; names(my.lm.pred)[2] <- X
  
  # segmented linear regression
  newesperc <- esperc
  if(noSeg==1) result <- try(my.seg <- segmented(my.lm, seg.Z = ~Xside, psi = list(Xside=c(quantile(Xside, newesperc, na.rm = TRUE))), 
                                    data = newdf, control = seg.control(tol = 1e-8, it.max = 500)), silent = TRUE)
  if(noSeg==2) result <- try(my.seg <- segmented(my.lm, seg.Z = ~Xside, psi = list(Xside=c(quantile(Xside, newesperc, na.rm = TRUE), quantile(Xside, 1-newesperc, na.rm=TRUE))), 
                                                data = newdf, control = seg.control(tol = 1e-8, it.max = 500)), silent = TRUE)
  while((newesperc<1.0)&(class(result)[1]=='try-error')){ 
    newesperc <- newesperc + esperc
    if(noSeg==1) result <- try(my.seg <- segmented(my.lm, seg.Z = ~Xside, psi = list(Xside=c(quantile(Xside, newesperc, na.rm = TRUE))), 
                                                   data = newdf, control = seg.control(tol = 1e-8, it.max = 500)), silent = TRUE)
    if(noSeg==2) result <- try(my.seg <- segmented(my.lm, seg.Z = ~Xside, psi = list(Xside=c(quantile(Xside, newesperc, na.rm = TRUE), quantile(Xside, 1-newesperc, na.rm = TRUE))), 
                                                   data = newdf, control = seg.control(tol = 1e-8, it.max = 500)), silent = TRUE)
  }
  if((class(result)[1]=="try-error")|(class(result)[1]=='lm')){ noConvSeg <- TRUE 
  }else{
    noConvSeg <- FALSE
    if(show) print(summary(my.seg))
    my.seg.pred <- data.frame(Y = fitted(my.seg), X = Xside)
    names(my.seg.pred)[1] <- Y; names(my.seg.pred)[2] <- X
  }
  
  # # logistic regression
  # my.logis <- vglm(ordered(Yside) ~ Xside, data = newdf, family = acat(parallel=TRUE))
  # if(show) print(summary(my.logis))
  # my.logis.pred <- data.frame(Y = (fitted(my.logis)%*%(1:dim(fitted(my.logis))[2]))[order(my.logis@x[,2])], X = sort(my.logis@x[,2]))
  # names(my.logis.pred)[1] <- Y; names(my.logis.pred)[2] <- X
  
  # draw figures: loess and segreg fit
  if(!noConvSeg){
    # axis_x <- my.seg$psi[, 2]
    # # axis_y <- predict(my.seg, data.frame(Xside=my.seg$psi[, 2]))
    # if(noSeg==1) axis_y <- unique(slope(my.seg)$Xside[,1]*axis_x + intercept(my.seg)$Xside[,1])[1]
    # if(noSeg==2) axis_y <- predict(my.seg, data.frame(Xside=my.seg$psi[, 2]))
    # txtdat <- data.frame(axis_x=axis_x, axis_y=axis_y, 
    #                      label=paste("(", format(round(axis_x,nsmall),nsmall=nsmall), 
    #                                  ", ", format(round(axis_y,nsmall),nsmall=nsmall), ")"))
     
    label <- paste("Lm: Int.:", toString(round(my.lm$coefficients[[1]], digits=nsmall)), 
                   "\nSlope:", toString(round(my.lm$coefficients[[2]], digits=nsmall)), "[", toString(round(ci[1], digits=nsmall)), ",", toString(round(ci[2], digits=nsmall)), "]", 
                   "\nSegLm: SegPoint:", toString(round(my.seg$psi[, 2], digits=nsmall)), 
                   "\nSlopes:", toString(round(slope(my.seg)$Xside[,1], digits=nsmall)), sep=" ") 
    xpos <- 0; ypos <- (yRange[2] - yRange[1])*yrate + yRange[1]
    labeldata <- data.frame(x=xpos, y=ypos, lab=label)
    
    # draw figure
    ggplot(newdf, aes(x=Xside, y=Yside)) +  
      stat_smooth(method='loess', formula = y ~ x, span=span, se=TRUE, color="blue", size=1.5) +
      geom_line(data = my.lm.pred, aes(x=my.lm.pred[,X], y=my.lm.pred[,Y]), color="black", size=1.5) + 
      geom_line(data = my.seg.pred, aes(x=my.seg.pred[,X], y=my.seg.pred[,Y]), color="red", size=1.5) + 
      #geom_text(data = txtdat, aes(x=axis_x, y=axis_y, label=label), angle = 0, size=geom.text.size/1.5) +
      #geom_line(data = my.logis.pred, aes(x=my.logis.pred[,X], y=my.logis.pred[,Y]), color="green", size=1.5) + 
      geom_text(data = labeldata, aes(x=xpos, y=ypos, label=lab), vjust=1, size=geom.text.size/1.2) +
      coord_cartesian(ylim = yRange) +
      ggtitle(title) + xlab(xlab) + ylab(ylab) + xlim(xlim) + 
      theme_bw() + 
      theme(plot.title=element_text(size=theme.size), axis.title=element_text(size=theme.size, face="bold"), 
            axis.text.x=element_text(hjust=0.5, size=theme.size), axis.text.y=element_text(size=theme.size),
            panel.grid.major=element_line(colour="grey", linetype="dotted", size=1),
            legend.title=element_text(size=theme.size), legend.text=element_text(size=theme.size), legend.position=c(0.8,0.4))
    savefig(FileName, 300, 10, 8, "in", "both")
  
    # calculate MSE and other measure
    # compare based on average SSE
    res <- data.frame("lm.seg.axis_x"=paste(axis_x, collapse=';'), "lm.seg.axis_y"=paste(axis_y, collapse=";")
                      , "lm_MSE"=calMSE_AIC(my.lm.pred, my.lm, newdf, X, Y)$MSE, "lm_AIC"=calMSE_AIC(my.lm.pred, my.lm, newdf, X, Y)$AIC
                      , "lm.seg_MSE"=calMSE_AIC(my.seg.pred, my.seg, newdf, X, Y)$MSE, "lm.seg_AIC"=calMSE_AIC(my.seg.pred, my.seg, newdf, X, Y)$AIC 
                      #, "logis_MSE"=calMSE_AIC(my.logis.pred, my.logis, newdf, X, Y)$MSE, "logis_AIC"=calMSE_AIC(my.logis.pred, my.logis, newdf, X, Y)$AIC
                      , "loess_MSE"=calMSE_AIC(my.loess.pred, my.loess, newdf, X, Y)$MSE, "loess_AIC"=calMSE_AIC(my.loess.pred, my.loess, newdf, X, Y)$AIC)
    write.csv(res, paste(FileName, ".csv", sep=""), row.names = FALSE)
  }else{
    label <- paste("Lm: Int.:", toString(round(my.lm$coefficients[[1]], digits=nsmall)), 
                   "\nSlope:", toString(round(my.lm$coefficients[[2]], digits=nsmall)), "[", toString(round(ci[1], digits=3)), ",", toString(round(ci[2], digits=3)), "]", sep=" ") 
    xpos <- 0; ypos <- (yRange[2] - yRange[1])*yrate + yRange[1]
    labeldata <- data.frame(x=xpos, y=ypos, lab=label)
    
    # draw figure
    ggplot(newdf, aes(x=Xside, y=Yside)) +  
      stat_smooth(method='loess', formula = y ~ x, span=span, se=TRUE, color="blue", size=1.5) +
      geom_line(data = my.lm.pred, aes(x=my.lm.pred[,X], y=my.lm.pred[,Y]), color="black", size=1.5) + 
      #geom_line(data = my.logis.pred, aes(x=my.logis.pred[,X], y=my.logis.pred[,Y]), color="green", size=1.5) + 
      geom_text(data = labeldata, aes(x=xpos, y=ypos, label=lab), vjust=1, size=geom.text.size/1.2) +
      coord_cartesian(ylim = yRange) +
      ggtitle(title) + xlab(xlab) + ylab(ylab) + xlim(xlim) + 
      theme_bw() + 
      theme(plot.title=element_text(size=theme.size), axis.title=element_text(size=theme.size, face="bold"), 
            axis.text.x=element_text(hjust=0.5, size=theme.size), axis.text.y=element_text(size=theme.size),
            panel.grid.major=element_line(colour="grey", linetype="dotted", size=1),
            legend.title=element_text(size=theme.size), legend.text=element_text(size=theme.size), legend.position=c(0.8,0.4))
    savefig(FileName, 300, 10, 8, "in", "both")
    
    # calculate MSE and other measure
    # compare based on average SSE
    res <- data.frame("lm.seg.axis_x"=NA, "lm.seg.axis_y"=NA
                      , "lm_MSE"=calMSE_AIC(my.lm.pred, my.lm, newdf, X, Y)$MSE, "lm_AIC"=calMSE_AIC(my.lm.pred, my.lm, newdf, X, Y)$AIC
                      , "lm.seg_MSE"=NA, "lm.seg_AIC"=NA 
                      #, "logis_MSE"=calMSE_AIC(my.logis.pred, my.logis, newdf, X, Y)$MSE, "logis_AIC"=calMSE_AIC(my.logis.pred, my.logis, newdf, X, Y)$AIC
                      , "loess_MSE"=calMSE_AIC(my.loess.pred, my.loess, newdf, X, Y)$MSE, "loess_AIC"=calMSE_AIC(my.loess.pred, my.loess, newdf, X, Y)$AIC)
    write.csv(res, paste(FileName, ".csv", sep=""), row.names = FALSE)
  }
}

# draw interactions
Draw_Int <- function(regType, number_interval, cut_number_type, D, X, Y, Z, 
                     numQuartile, LabQuartile, yRange, yrate, freeY, xlab, ylab, title, figName){
  #' draw interaction effect: loess and linear regression
  #' @param regType regression type for each subgroup, 'linear' or 'logistic'
  #' @param numer_interval separate groups based on number of subjects or percentile of skill value
  #' @param cut_number_type 0 or 1
  #' @param D data set
  #' @param X word property (independent)
  #' @param Y skill property (independent)
  #' @param Z eye-tracking measure (dependent)
  #' @param numQuartile number of subgroups (2 or 4)
  #' @param LabQuartile labels of subgroups
  #' @param yRange y-axis range
  #' @param yrate 0.85
  #' @param freeY 0 or 1
  #' @param xlab x-aixs label
  #' @param ylab y-axis label
  #' @param title figure title
  #' @param figName figure file name
  
  Dcopy <- D[(!is.na(D[,X]))&(!is.na(D[,Y])),]
  # create quartiles
  if(number_interval == "number"){
    # cut equal number
    if(cut_number_type==0) quartiles <- cut_number(Dcopy[,Z], n=numQuartile)
    if(cut_number_type==1) quartiles <- cut_number(Dcopy[,Z] + seq_along(Dcopy[,Z])*.Machine$double.eps, n=numQuartile)
  }
  if(number_interval == "interval"){
    # cut equal interval
    if(cut_number_type==0) quartiles <- cut_interval(Dcopy[,Z], n=numQuartile)
    if(cut_number_type==1) quartiles <- cut_interval(Dcopy[,Z] + seq_along(Dcopy[,Z])*.Machine$double.eps, n=numQuartile)
  }
  Dcopy <- cbind(Dcopy, quartiles)
  # get linear regression model
  tileLabel <- c() # store lm and seg lm parameters
  my.seg.all <- data.frame() # store fitted seg lm curves
  res <- data.frame() # store AIC, MSE of lm and seg lm
  for(i in 1:numQuartile){
    subD <- Dcopy[Dcopy$quartiles == levels(quartiles)[i], ]
    if(regType == "linear"){
      Xside <- subD[,X]; Yside <- subD[,Y]
      
      # baseline model: loess regression
      my.loess <- loess(Yside ~ Xside, data = subD, span=span) # smoothing span
      my.loess.pred <- data.frame(Y = predict(my.loess, subD), X = Xside) 
      names(my.loess.pred)[1] <- Y; names(my.loess.pred)[2] <- X
      
      # train linear regression model
      lm <- lm(Yside ~ Xside, data=subD)
      coef <- summary(lm)$coefficients[2,1]; err <- summary(lm)$coefficients[2,2]; ci <- coef + c(-1,1)*err*qt(0.975, 42)
      my.lm.pred <- data.frame(Y = predict(lm, subD), X = Xside)
      names(my.lm.pred)[1] <- Y; names(my.lm.pred)[2] <- X
      
      # segmented linear regression
      newesperc <- esperc
      result <- try(my.seg <- segmented(lm, seg.Z = ~Xside, psi = list(Xside=c(quantile(Xside, newesperc, na.rm = TRUE))), 
                                        data = subD, control = seg.control(tol = 1e-8, it.max = 500)), silent = TRUE)
      while((newesperc<1.0)&(class(result)[1]=='try-error')){ 
        newesperc <- newesperc + esperc
        result <- try(my.seg <- segmented(lm, seg.Z = ~Xside, psi = list(Xside=c(quantile(Xside, newesperc, na.rm = TRUE))), 
                                          data = subD, control = seg.control(tol = 1e-8, it.max = 500)), silent = TRUE)
      }
      if((class(result)[1]=="try-error")|(class(result)[1]=='lm')){ noConvSeg <- TRUE
      }else{
        noConvSeg <- FALSE
        my.seg.pred <- data.frame(quartiles = levels(quartiles)[i], Y = fitted(my.seg), X = Xside)
        names(my.seg.pred)[2] <- Y; names(my.seg.pred)[3] <- X
        my.seg.pred$quartiles <- factor(my.seg.pred$quartiles)
        my.seg.all <- rbind(my.seg.all, my.seg.pred)
      }
      
      if(!noConvSeg){
        axis_x <- my.seg$psi[, 2]
        tempres <- data.frame("quartiles"=levels(quartiles)[i], "lm.seg.axis_x"=paste(axis_x, collapse=';')
                              , "lm_MSE"=calMSE_AIC(my.lm.pred, lm, subD, X, Y)$MSE, "lm_AIC"=calMSE_AIC(my.lm.pred, lm, subD, X, Y)$AIC
                              , "lm.seg_MSE"=calMSE_AIC(my.seg.pred, my.seg, subD, X, Y)$MSE, "lm.seg_AIC"=calMSE_AIC(my.seg.pred, my.seg, subD, X, Y)$AIC 
                              , "loess_MSE"=calMSE_AIC(my.loess.pred, my.loess, subD, X, Y)$MSE, "loess_AIC"=calMSE_AIC(my.loess.pred, my.loess, subD, X, Y)$AIC)
        tempres$quartiles <- factor(tempres$quartiles)
      }else{
        tempres <- data.frame("quartiles"=levels(quartiles)[i], "lm.seg.axis_x"=NA
                             , "lm_MSE"=calMSE_AIC(my.lm.pred, lm, subD, X, Y)$MSE, "lm_AIC"=calMSE_AIC(my.lm.pred, lm, subD, X, Y)$AIC
                             , "lm.seg_MSE"=NA, "lm.seg_AIC"=NA 
                             , "loess_MSE"=calMSE_AIC(my.loess.pred, my.loess, subD, X, Y)$MSE, "loess_AIC"=calMSE_AIC(my.loess.pred, my.loess, subD, X, Y)$AIC)
        tempres$quartiles <- factor(tempres$quartiles)
      }
      res <- rbind(res, tempres)
      if(!noConvSeg){
        label <- paste("Lm: Int.:", toString(round(lm$coefficients[[1]], digits=3)), 
                     "\nSlope:", toString(round(lm$coefficients[[2]], digits=3)), "[", toString(round(ci[1], digits=3)), ",", toString(round(ci[2], digits=3)), "]", 
                     "\nSegLm: Seg.Point:", toString(round(my.seg$psi[, 2], digits=3)), 
                     "\nSlopes:", toString(round(slope(my.seg)$Xside[,1], digits=3)), sep=" ") 
      }else{
        label <- paste("Lm\nInt.:", toString(round(lm$coefficients[[1]], digits=3)), 
                   "\nSlope:", toString(round(lm$coefficients[[2]], digits=3)), "[", toString(round(ci[1], digits=3)), ",", toString(round(ci[2], digits=3)), "]", sep=" ") 
      }
      tileLabel <- c(tileLabel, label)
    }
    if(regType == "logistic"){
      Xside <- subD[,X]; Yside <- subD[,Y]
      
      # baseline model: loess regression
      my.loess <- loess(Yside ~ Xside, data = subD, span=span) # smoothing span
      my.loess.pred <- data.frame(Y = predict(my.loess, subD), X = Xside) 
      names(my.loess.pred)[1] <- Y; names(my.loess.pred)[2] <- X
      
      # train logistic regression model
      glm <- glm(subD[,Y] ~ subD[,X], family=binomial(link='logit'), data=subD)
      coef <- summary(glm)$coefficients[2,1]; err <- summary(glm)$coefficients[2,2]; ci <- coef + c(-1,1)*err*qt(0.975, 42)
      my.glm.pred <- data.frame(Y = predict(glm, subD), X = Xside)
      names(my.glm.pred)[1] <- Y; names(my.glm.pred)[2] <- X
      
      
      # train linear regression model
      lm <- lm(Yside ~ Xside, data=subD)
      coef <- summary(lm)$coefficients[2,1]; err <- summary(lm)$coefficients[2,2]; ci <- coef + c(-1,1)*err*qt(0.975, 42)
      my.lm.pred <- data.frame(Y = predict(lm, subD), X = Xside)
      names(my.lm.pred)[1] <- Y; names(my.lm.pred)[2] <- X
      
      # segmented linear regression
      newesperc <- esperc
      result <- try(my.seg <- segmented(lm, seg.Z = ~Xside, psi = list(Xside=c(quantile(Xside, newesperc, na.rm = TRUE))), 
                                        data = subD, control = seg.control(tol = 1e-8, it.max = 500)), silent = TRUE)
      while((newesperc<1.0)&(class(result)[1]=='try-error')){ 
        newesperc <- newesperc + esperc
        result <- try(my.seg <- segmented(lm, seg.Z = ~Xside, psi = list(Xside=c(quantile(Xside, newesperc, na.rm = TRUE))), 
                                          data = subD, control = seg.control(tol = 1e-8, it.max = 500)), silent = TRUE)
      }
      if((class(result)[1]=="try-error")|(class(result)[1]=='lm')){ noConvSeg <- TRUE
      }else{
        noConvSeg <- FALSE
        my.seg.pred <- data.frame(quartiles = levels(quartiles)[i], Y = fitted(my.seg), X = Xside)
        names(my.seg.pred)[2] <- Y; names(my.seg.pred)[3] <- X
        my.seg.pred$quartiles <- factor(my.seg.pred$quartiles)
        my.seg.all <- rbind(my.seg.all, my.seg.pred)
      }
      
      if(!noConvSeg){
        axis_x <- my.seg$psi[, 2]
        tempres <- data.frame("quartiles"=levels(quartiles)[i], "lm.seg.axis_x"=paste(axis_x, collapse=';')
                              , "glm_MSE"=calMSE_AIC(my.glm.pred, glm, subD, X, Y)$MSE, "glm_AIC"=calMSE_AIC(my.glm.pred, glm, subD, X, Y)$AIC
                              , "lm.seg_MSE"=calMSE_AIC(my.seg.pred, my.seg, subD, X, Y)$MSE, "lm.seg_AIC"=calMSE_AIC(my.seg.pred, my.seg, subD, X, Y)$AIC 
                              , "loess_MSE"=calMSE_AIC(my.loess.pred, my.loess, subD, X, Y)$MSE, "loess_AIC"=calMSE_AIC(my.loess.pred, my.loess, subD, X, Y)$AIC)
        tempres$quartiles <- factor(tempres$quartiles)
      }else{
        tempres <- data.frame("quartiles"=levels(quartiles)[i], "lm.seg.axis_x"=NA
                              , "glm_MSE"=calMSE_AIC(my.glm.pred, glm, subD, X, Y)$MSE, "glm_AIC"=calMSE_AIC(my.glm.pred, glm, subD, X, Y)$AIC
                              , "lm.seg_MSE"=NA, "lm.seg_AIC"=NA 
                              , "loess_MSE"=calMSE_AIC(my.loess.pred, my.loess, subD, X, Y)$MSE, "loess_AIC"=calMSE_AIC(my.loess.pred, my.loess, subD, X, Y)$AIC)
        tempres$quartiles <- factor(tempres$quartiles)
      }
      res <- rbind(res, tempres)
      if(!noConvSeg){
        label <- paste("gLm: Int.:", toString(round(glm$coefficients[[1]], digits=3)), 
                       "\nSlope:", toString(round(glm$coefficients[[2]], digits=3)), "[", toString(round(ci[1], digits=3)), ",", toString(round(ci[2], digits=3)), "]", 
                       "\nSegLm: Seg.Point:", toString(round(my.seg$psi[, 2], digits=3)), 
                       "\nSlopes:", toString(round(slope(my.seg)$Xside[,1], digits=3)), sep=" ") 
      }else{
        label <- paste("gLm\nInt.:", toString(round(glm$coefficients[[1]], digits=3)), 
                       "\nSlope:", toString(round(glm$coefficients[[2]], digits=3)), "[", toString(round(ci[1], digits=3)), ",", toString(round(ci[2], digits=3)), "]", sep=" ") 
      }
      tileLabel <- c(tileLabel, label)
    }
  }
  
  # after get label, change quartiles with texts
  numInd <- c()
  for(lev in levels(quartiles)){
    numInd <- c(numInd, length(unique(Dcopy[Dcopy$quartiles==lev, "subj"])))
  }
  newlev <- paste(LabQuartile, levels(quartiles), numInd, sep = " ")
  
  levels(Dcopy$quartiles) <- newlev
  if(!noConvSeg) levels(my.seg.all$quartiles) <- newlev
  
  # draw figure
  xpos <- 0; ypos <- (yRange[2] - yRange[1])*yrate + yRange[1]
  labeldata <- data.frame(x=xpos, y=ypos, lab=tileLabel, quartiles=newlev)
  
  if(freeY==1){
    if(regType == "linear") {
      if(!noConvSeg){
        ggplot(data = Dcopy, aes_string(x=X, y=Y)) +
          geom_line(data = my.seg.all, aes_string(x=X, y=Y), color="red", size=1.5) + 
          stat_smooth(method="lm", formula = y ~ x, color="black", se=FALSE, size=1.5) +
          stat_smooth(method="loess", formula = y ~ x, color='blue', size=1.5) +
          geom_text(data = labeldata, aes(x=xpos, y=ypos, label=lab), vjust=1, size=geom.text.size) +
          coord_cartesian(ylim = yRange) +
          xlab(xlab) + ylab(ylab) + ggtitle(title) + 
          facet_wrap(~quartiles, scales="free_y", nrow=1) + 
          theme_bw() + 
          theme(plot.title=element_text(size=theme.size), axis.text=element_text(size=theme.size), text=element_text(size=theme.size), strip.text.x=element_text(size=theme.size), 
                panel.grid.major = element_line(colour = "grey", linetype = "dotted", size = 1))
      }else{
        ggplot(Dcopy, aes_string(x=X, y=Y)) +
          stat_smooth(method="lm", formula = y ~ x, color="black", se=FALSE, size=1.5) +
          stat_smooth(method="loess", formula = y ~ x, color='blue', size=1.5) +
          geom_text(data = labeldata, aes(x=xpos, y=ypos, label=lab), vjust=1, size=geom.text.size) +
          coord_cartesian(ylim = yRange) +
          xlab(xlab) + ylab(ylab) + ggtitle(title) + 
          facet_wrap(~quartiles, scales="free_y", nrow=1) + 
          theme_bw() + 
          theme(plot.title=element_text(size=theme.size), axis.text=element_text(size=theme.size), text=element_text(size=theme.size), strip.text.x=element_text(size=theme.size), 
                panel.grid.major = element_line(colour = "grey", linetype = "dotted", size = 1))
      }
      
    }
    if(regType == "logistic") {
      if(!noConvSeg){
        ggplot(Dcopy, aes_string(x=X, y=Y)) +
          geom_line(data = my.seg.all, aes_string(x=X, y=Y), color="red", size=1.5) + 
          stat_smooth(method="glm", method.args=list(family="binomial"), formula = y ~ x, color="black", se=FALSE, size=1.5) + 
          stat_smooth(method="loess", formula = y ~ x, color='blue', size=1.5) +
          geom_text(data = labeldata, aes(x=xpos, y=ypos, label=lab), vjust=1, size=geom.text.size) +
          coord_cartesian(ylim = yRange) +
          xlab(xlab) + ylab(ylab) + ggtitle(title) + 
          facet_wrap(~quartiles, scales="free_y", nrow=1) + 
          theme_bw() + 
          theme(plot.title=element_text(size=theme.size), axis.text=element_text(size=theme.size), text=element_text(size=theme.size), strip.text.x=element_text(size=theme.size), 
                panel.grid.major = element_line(colour = "grey", linetype = "dotted", size = 1))
      }else{
        ggplot(Dcopy, aes_string(x=X, y=Y)) +
          stat_smooth(method="glm", method.args=list(family="binomial"), formula = y ~ x, color="black", se=FALSE, size=1.5) +
          stat_smooth(method="loess", formula = y ~ x, color='blue', size=1.5) +
          geom_text(data = labeldata, aes(x=xpos, y=ypos, label=lab), vjust=1, size=geom.text.size) +
          coord_cartesian(ylim = yRange) +
          xlab(xlab) + ylab(ylab) + ggtitle(title) + 
          facet_wrap(~quartiles, scales="free_y", nrow=1) + 
          theme_bw() + 
          theme(plot.title=element_text(size=theme.size), axis.text=element_text(size=theme.size), text=element_text(size=theme.size), strip.text.x=element_text(size=theme.size), 
                panel.grid.major = element_line(colour = "grey", linetype = "dotted", size = 1))
      }
    }
  }else{
    if(regType == "linear") {
      if(!noConvSeg){
        ggplot(Dcopy, aes_string(x=X, y=Y)) +
          geom_line(data = my.seg.all, aes_string(x=X, y=Y), color="red", size=1.5) + 
          stat_smooth(method="lm", formula = y ~ x, color="black", se=FALSE, size=1.5) + 
          stat_smooth(method="loess", formula = y ~ x, color='blue', size=1.5) +
          geom_text(data = labeldata, aes(x=xpos, y=ypos, label=lab), vjust=1, size=geom.text.size) +
          coord_cartesian(ylim = yRange) +
          xlab(xlab) + ylab(ylab) + ggtitle(title) + 
          facet_wrap(~quartiles, scales="fixed", nrow=1) + 
          theme_bw() + 
          theme(plot.title=element_text(size=theme.size), axis.text=element_text(size=theme.size), text=element_text(size=theme.size), strip.text.x=element_text(size=theme.size), 
                panel.grid.major = element_line(colour = "grey", linetype = "dotted", size = 1))
      }else{
        ggplot(Dcopy, aes_string(x=X, y=Y)) +
          geom_line(data = my.seg.all, aes_string(x=X, y=Y), color="red", size=1.5) + 
          stat_smooth(method="lm", formula = y ~ x, color="red", se=FALSE, size=1.5) + 
          stat_smooth(method="loess", formula = y ~ x, color='blue', size=1.5) +
          geom_text(data = labeldata, aes(x=xpos, y=ypos, label=lab), vjust=1, size=geom.text.size) +
          coord_cartesian(ylim = yRange) +
          xlab(xlab) + ylab(ylab) + ggtitle(title) + 
          facet_wrap(~quartiles, scales="fixed", nrow=1) + 
          theme_bw() + 
          theme(plot.title=element_text(size=theme.size), axis.text=element_text(size=theme.size), text=element_text(size=theme.size), strip.text.x=element_text(size=theme.size), 
                panel.grid.major = element_line(colour = "grey", linetype = "dotted", size = 1))
      }
    }
    if(regType == "logistic") {
      if(!noConvSeg){
        ggplot(Dcopy, aes_string(x=X, y=Y)) +
          geom_line(data = my.seg.all, aes_string(x=X, y=Y), color="red", size=1.5) + 
          stat_smooth(method="glm", method.args=list(family="binomial"), formula = y ~ x, color="black", se=FALSE, size=1.5) + 
          stat_smooth(method="loess", formula = y ~ x, color='blue', size=1.5) + 
          geom_text(data = labeldata, aes(x=xpos, y=ypos, label=lab), vjust=1, size=geom.text.size) +
          coord_cartesian(ylim = yRange) +
          xlab(xlab) + ylab(ylab) + ggtitle(title) + 
          facet_wrap(~quartiles, scales="fixed", nrow=1) + 
          theme_bw() + 
          theme(plot.title=element_text(size=theme.size), axis.text=element_text(size=theme.size), text=element_text(size=theme.size), strip.text.x=element_text(size=theme.size), 
                panel.grid.major = element_line(colour = "grey", linetype = "dotted", size = 1))
      }else{
        ggplot(Dcopy, aes_string(x=X, y=Y)) +
          stat_smooth(method="glm", method.args=list(family="binomial"), formula = y ~ x, color="black", se=FALSE, size=1.5) + 
          stat_smooth(method="loess", formula = y ~ x, color='blue', size=1.5) + 
          geom_text(data = labeldata, aes(x=xpos, y=ypos, label=lab), vjust=1, size=geom.text.size) +
          coord_cartesian(ylim = yRange) +
          xlab(xlab) + ylab(ylab) + ggtitle(title) + 
          facet_wrap(~quartiles, scales="fixed", nrow=1) + 
          theme_bw() + 
          theme(plot.title=element_text(size=theme.size), axis.text=element_text(size=theme.size), text=element_text(size=theme.size), strip.text.x=element_text(size=theme.size), 
                panel.grid.major = element_line(colour = "grey", linetype = "dotted", size = 1))
      }
    }
  }
  savefig(figName, 300, width, height, "in", "both")
  write.csv(res, paste(figName, ".csv", sep=""), row.names = FALSE)
}
