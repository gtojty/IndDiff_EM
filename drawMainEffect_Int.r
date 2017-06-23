require(ggplot2)
require(plyr)
require(reshape2)
require(sjPlot)
require(Hmisc)
require(GGally)
require(stats)
require(xlsx)
require(lme4)
require(coefplot)
require(broom)
require(gam)
require(MuMIn)

# draw main effect
Draw_MainEffect <- function(regType, D, X, Y, yRange, yrate, xName, yName, 
                            figName, figWidth, figHeight){
  data <- D
  # get linear regression model
  if(regType == "linear"){
    lm <- lm(data[,Y] ~ data[,X], data = D)
    coef <- summary(lm)$coefficients[2,1] 
    err <- summary(lm)$coefficients[2,2]
    ci <- coef + c(-1,1)*err*qt(0.975, 42)
  } 
  if(regType == "logistic"){
    lm <- glm(data[,Y] ~ data[,X], family=binomial(link='logit'), data = D)
    coef <- summary(lm)$coefficients[2,1] 
    err <- summary(lm)$coefficients[2,2]
    ci <- coef + c(-1,1)*err*qt(0.975, 42)
  }
  lab = paste("Int.:", toString(round(lm$coefficients[[1]], digits=3)), 
              "\nSlope:", toString(round(lm$coefficients[[2]], digits=3)), 
              "\nCI [", toString(round(ci[1], digits=3)), ",", toString(round(ci[2], digits=3)), "]", sep=" ") 
  # draw figure
  xpos <- 0; ypos <- (yRange[2] - yRange[1])*yrate + yRange[1]
  geom.text.size = 8; theme.size = (14/5)*geom.text.size
  if(regType == "linear") {
    ggplot(D, aes_string(x=X, y=Y)) +
      #geom_smooth(se=FALSE) +
      stat_smooth(method="lm", color="red", size=1.5) +
      coord_cartesian(ylim=yRange) +
      xlab(xName) + ylab(yName) +  
      geom_text(x=xpos, y=ypos, label=lab, vjust=1, size=geom.text.size) +
      theme_bw() + 
      theme(plot.title=element_text(size=theme.size), axis.text=element_text(size=theme.size), text=element_text(size=theme.size), 
            panel.grid.major = element_line(colour = "grey", linetype = "dotted", size = 1))
  }
  if(regType == "logistic"){
    ggplot(D, aes_string(x=X, y=Y)) +
      #geom_smooth(se=FALSE) +
      stat_smooth(method="glm", method.args=list(family="binomial"), color="red", size=1.5) +
      coord_cartesian(ylim=yRange) +
      xlab(xName) + ylab(yName) +  
      geom_text(x=xpos, y=ypos, label=lab, vjust=1, size=geom.text.size) +
      theme_bw() + 
      theme(plot.title=element_text(size=theme.size), axis.text=element_text(size=theme.size), text=element_text(size=theme.size), 
            panel.grid.major = element_line(colour = "grey", linetype = "dotted", size = 1))
  }
  ggsave(figName, dpi = 300, width = figWidth, height = figHeight, units = "in")   
}

# draw interactions
Draw_Int <- function(regType, number_interval, cut_number_type, D, X, Y, Z, 
                     numQuartile, LabQuartile, yRange, yrate, freeY, xName, yName, title, 
                     figName, figWidth, figHeight){
  Dcopy <- D
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
  tileLabel <- c()
  for(i in 1:numQuartile){
    data <- Dcopy[Dcopy$quartiles == levels(quartiles)[i], ]
    if(regType == "linear"){
      lm <- lm(data[,Y] ~ data[,X], data=data)
      coef <- summary(lm)$coefficients[2,1] 
      err <- summary(lm)$coefficients[2,2]
      ci <- coef + c(-1,1)*err*qt(0.975, 42)
    }
    if(regType == "logistic"){
      lm <- glm(data[,Y] ~ data[,X], family=binomial(link='logit'), data=data)
      coef <- summary(lm)$coefficients[2,1] 
      err <- summary(lm)$coefficients[2,2]
      ci <- coef + c(-1,1)*err*qt(0.975, 42)
    }
    label <- paste("Int.:", toString(round(lm$coefficients[[1]], digits=3)), 
                   "\nSlope:", toString(round(lm$coefficients[[2]], digits=3)), 
                   "\nCI [", toString(round(ci[1], digits=3)), ",", toString(round(ci[2], digits=3)), "]", sep=" ") 
    tileLabel <- c(tileLabel, label)
  }
  # after get label, change quartiles with texts
  newlev <- paste(LabQuartile, levels(quartiles), sep = " ")
  levels(quartiles) <- newlev
  Dcopy$quartiles <- quartiles
  # draw figure
  xpos <- 0; ypos <- (yRange[2] - yRange[1])*yrate + yRange[1]
  geom.text.size = 10; theme.size = (14/5)*geom.text.size
  if(freeY==1){
    if(regType == "linear") {
      ggplot(Dcopy, aes_string(x=X, y=Y)) +
        stat_smooth(method="lm", color="red", size=1.5) +
        coord_cartesian(ylim = yRange) +
        xlab(xName) + ylab(yName) + ggtitle(title) + 
        facet_wrap(~quartiles, scale="free_y", nrow=1) + 
        geom_text(aes(x=xpos, y=ypos, label=lab), data=data.frame(x=xpos, y=ypos, lab=tileLabel, quartiles=levels(quartiles)), vjust=1, size=geom.text.size) +
        theme_bw() + 
        theme(plot.title=element_text(size=theme.size), axis.text=element_text(size=theme.size), text=element_text(size=theme.size), strip.text.x=element_text(size=theme.size), 
              panel.grid.major = element_line(colour = "grey", linetype = "dotted", size = 1))
    }
    if(regType == "logistic") {
      ggplot(Dcopy, aes_string(x=X, y=Y)) +
        stat_smooth(method="glm", method.args=list(family="binomial"), color="red", size=1.5) + 
        coord_cartesian(ylim = yRange) +
        xlab(xName) + ylab(yName) + ggtitle(title) + 
        facet_wrap(~quartiles, scale="free_y", nrow=1) + 
        geom_text(aes(x=xpos, y=ypos, label=lab), data=data.frame(x=xpos, y=ypos, lab=tileLabel, quartiles=levels(quartiles)), vjust=1, size=geom.text.size) +
        theme_bw() + 
        theme(plot.title=element_text(size=theme.size), axis.text=element_text(size=theme.size), text=element_text(size=theme.size), strip.text.x=element_text(size=theme.size), 
              panel.grid.major = element_line(colour = "grey", linetype = "dotted", size = 1))
    }
  }else{
    if(regType == "linear") {
      ggplot(Dcopy, aes_string(x=X, y=Y)) +
        stat_smooth(method="lm", color="red", size=1.5) + 
        coord_cartesian(ylim = yRange) +
        xlab(xName) + ylab(yName) + ggtitle(title) + 
        facet_wrap(~quartiles, scales="fixed", nrow=1) + 
        geom_text(aes(x=xpos, y=ypos, label=lab), data=data.frame(x=xpos, y=ypos, lab=tileLabel, quartiles=levels(quartiles)), vjust=1, size=geom.text.size) +
        theme_bw() + 
        theme(plot.title=element_text(size=theme.size), axis.text=element_text(size=theme.size), text=element_text(size=theme.size), strip.text.x=element_text(size=theme.size), 
              panel.grid.major = element_line(colour = "grey", linetype = "dotted", size = 1))
    }
    if(regType == "logistic") {
      ggplot(Dcopy, aes_string(x=X, y=Y)) +
        stat_smooth(method="glm", method.args=list(family="binomial"), color="red", size=1.5) + 
        coord_cartesian(ylim = yRange) +
        xlab(xName) + ylab(yName) + ggtitle(title) + 
        facet_wrap(~quartiles, scales="fixed", nrow=1) + 
        geom_text(aes(x=xpos, y=ypos, label=lab), data=data.frame(x=xpos, y=ypos, lab=tileLabel, quartiles=levels(quartiles)), vjust=1, size=geom.text.size) +
        theme_bw() + 
        theme(plot.title=element_text(size=theme.size), axis.text=element_text(size=theme.size), text=element_text(size=theme.size), strip.text.x=element_text(size=theme.size), 
              panel.grid.major = element_line(colour = "grey", linetype = "dotted", size = 1))
    }
  }
  ggsave(figName, dpi = 300, width = figWidth, height = figHeight, units = "in")  
}
