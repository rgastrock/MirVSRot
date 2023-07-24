source('ana/shared.R')
source('ana/learningRates.R')
source('ana/exponentialandstepModel.R')

#Order effects: ROT----

getROTOrderEffects <- function(group, maxppid, location,condition){
  
  #each column will be one participant and their 90 trials
  dat <- getROTGroupLearningCurves(group=group, maxppid=maxppid,location=location)
  dat <- dat[,-1] #remove trial column to index properly
  
  #even ID gets ROT first, then MIR; odd ID gets MIR first, then ROT
  #but participant count starts at 0, whereas dat already starts at one
  #so at this point, odd numbers should have ROT first, while even have ROT second
  pp_ROTfirst <- dat[,c(seq(1,ncol(dat),2))]
  pp_ROTsecond <- dat[,c(seq(2,ncol(dat),2))]
  #then we can add trial column back to each of the separated data
  trial <- seq(1,90,1)
  pp_ROTfirst <- cbind(trial, pp_ROTfirst)
  pp_ROTsecond <- cbind(trial, pp_ROTsecond)
  
  #return whichever data is neede
  if (condition == 1){
    return(pp_ROTfirst)
  } else if (condition == 2){
    return(pp_ROTsecond)
  }
  
}

getROTOrderEffectsConfidenceInterval <- function(group, maxppid, location, condition, type){
  
  if (condition == 1){
    data <- getROTOrderEffects(group = group, maxppid = maxppid, location = location, condition = condition)
    #data <- data[,-6] #remove faulty particiapnt (pp004) so the 6th column REMOVE ONCE RESOLVED
    data <- as.data.frame(data)
    trialno <- data$trial
    data1 <- as.matrix(data[,2:dim(data)[2]])
    
    confidence <- data.frame()
    
    
    for (trial in trialno){
      cireaches <- data1[which(data$trial == trial), ]
      
      if (type == "t"){
        cireaches <- cireaches[!is.na(cireaches)]
        citrial <- t.interval(data = cireaches, variance = var(cireaches), conf.level = 0.95)
      } else if(type == "b"){
        citrial <- getBSConfidenceInterval(data = cireaches, resamples = 1000)
      }
      
      if (prod(dim(confidence)) == 0){
        confidence <- citrial
      } else {
        confidence <- rbind(confidence, citrial)
      }
      if (group == 'noninstructed'){
        write.csv(confidence, file='data/pilot/ROT_noninstructed_CI_ordereffects_1.csv', row.names = F) 
      } else if (group == 'instructed'){
        write.csv(confidence, file='data/pilot/ROT_instructed_CI_ordereffects_1.csv', row.names = F)
      }
      
    }
  }else if (condition == 2){
    data <- getROTOrderEffects(group = group, maxppid = maxppid, location = location, condition = condition)
    #data <- data[,-6] #remove faulty particiapnt (pp004) so the 6th column REMOVE ONCE RESOLVED
    data <- as.data.frame(data)
    trialno <- data$trial
    data1 <- as.matrix(data[,2:dim(data)[2]])
    
    confidence <- data.frame()
    
    
    for (trial in trialno){
      cireaches <- data1[which(data$trial == trial), ]
      
      if (type == "t"){
        cireaches <- cireaches[!is.na(cireaches)]
        citrial <- t.interval(data = cireaches, variance = var(cireaches), conf.level = 0.95)
      } else if(type == "b"){
        citrial <- getBSConfidenceInterval(data = cireaches, resamples = 1000)
      }
      
      if (prod(dim(confidence)) == 0){
        confidence <- citrial
      } else {
        confidence <- rbind(confidence, citrial)
      }
      if (group == 'noninstructed'){
        write.csv(confidence, file='data/pilot/ROT_noninstructed_CI_ordereffects_2.csv', row.names = F) 
      } else if (group == 'instructed'){
        write.csv(confidence, file='data/pilot/ROT_instructed_CI_ordereffects_2.csv', row.names = F)
      }
      
    }
  }
  
}

#for simplicity, I will make 2 functions that will generate order effects plots for non instructed and instructed groups separately

plotNIROTOrderEffects <- function(group = 'noninstructed', conditions = c(1,2), target = 'inline') {
  
  
  #but we can save plot as svg file
  if (target=='svg') {
    svglite(file='doc/fig/pilot/Fig1_ROT_NI_ordereffects.svg', width=12, height=7, pointsize=14, system_fonts=list(sans="Arial"))
  }
  
  # create plot
  meanGroupReaches <- list() #empty list so that it plots the means last
  
  #NA to create empty plot
  # could maybe use plot.new() ?
  plot(NA, NA, xlim = c(0,91), ylim = c(-200,200), 
       xlab = "Trial", ylab = "Amount of Compensation (%)", frame.plot = FALSE, #frame.plot takes away borders
       main = "Perturbation order: Non-instructed, Rotation", xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
  abline(h = c(-100,0, 100), col = 8, lty = 2) #creates horizontal dashed lines through y =  0 and 30
  axis(1, at = c(1, 30, 60, 90)) #tick marks for x axis
  axis(2, at = c(-200, -100, 0, 100, 200)) #tick marks for y axis
  
  for(condition in conditions){
    #read in files created by getGroupConfidenceInterval in filehandling.R
    groupconfidence <- read.csv(file=sprintf('data/pilot/ROT_%s_CI_ordereffects_%d.csv', group, condition))
    
    colourscheme <- getCtypeColourScheme(conditions = condition)
    #take only first, last and middle columns of file
    lower <- groupconfidence[,1]
    upper <- groupconfidence[,3]
    mid <- groupconfidence[,2]
    
    col <- colourscheme[[condition]][['T']] #use colour scheme according to group
    
    #upper and lower bounds create a polygon
    #polygon creates it from low left to low right, then up right to up left -> use rev
    #x is just trial nnumber, y depends on values of bounds
    polygon(x = c(c(1:90), rev(c(1:90))), y = c(lower, rev(upper)), border=NA, col=col)
    
    meanGroupReaches[[condition]] <- mid #use mean to fill in empty list for each group
  }
  
  
  for (condition in conditions) {
    # plot mean reaches for each group
    col <- colourscheme[[condition]][['S']]
    lines(meanGroupReaches[[condition]],col=col,lty=1)
  }
  
  #add legend
  legend(60,-100,legend=c('ROT_First','ROT_Second'),
         col=c(colourscheme[[1]][['S']],colourscheme[[2]][['S']]),
         lty=1,bty='n',cex=1,lwd=2)
  
  #close everything if you saved plot as svg
  if (target=='svg') {
    dev.off()
  }
  
}

plotIROTOrderEffects <- function(group = 'instructed', conditions = c(1,2), target = 'inline') {
  
  
  #but we can save plot as svg file
  if (target=='svg') {
    svglite(file='doc/fig/pilot/Fig2_ROT_I_ordereffects.svg', width=12, height=7, pointsize=14, system_fonts=list(sans="Arial"))
  }
  
  # create plot
  meanGroupReaches <- list() #empty list so that it plots the means last
  
  #NA to create empty plot
  # could maybe use plot.new() ?
  plot(NA, NA, xlim = c(0,91), ylim = c(-200,200), 
       xlab = "Trial", ylab = "Amount of Compensation (%)", frame.plot = FALSE, #frame.plot takes away borders
       main = "Order Effects: ROT", xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
  abline(h = c(-100,0, 100), col = 8, lty = 2) #creates horizontal dashed lines through y =  0 and 30
  axis(1, at = c(1, 30, 60, 90)) #tick marks for x axis
  axis(2, at = c(-200, -100, 0, 100, 200)) #tick marks for y axis
  
  for(condition in conditions){
    #read in files created by getGroupConfidenceInterval in filehandling.R
    groupconfidence <- read.csv(file=sprintf('data/pilot/ROT_%s_CI_ordereffects_%d.csv', group, condition))
    
    colourscheme <- getCtypeColourScheme(conditions = condition)
    #take only first, last and middle columns of file
    lower <- groupconfidence[,1]
    upper <- groupconfidence[,3]
    mid <- groupconfidence[,2]
    
    col <- colourscheme[[condition]][['T']] #use colour scheme according to group
    
    #upper and lower bounds create a polygon
    #polygon creates it from low left to low right, then up right to up left -> use rev
    #x is just trial nnumber, y depends on values of bounds
    polygon(x = c(c(1:90), rev(c(1:90))), y = c(lower, rev(upper)), border=NA, col=col)
    
    meanGroupReaches[[condition]] <- mid #use mean to fill in empty list for each group
  }
  
  
  for (condition in conditions) {
    # plot mean reaches for each group
    col <- colourscheme[[condition]][['S']]
    lines(meanGroupReaches[[condition]],col=col,lty=1)
  }
  
  #add legend
  legend(60,-100,legend=c('ROT_First','ROT_Second'),
         col=c(colourscheme[[1]][['S']],colourscheme[[2]][['S']]),
         lty=1,bty='n',cex=1,lwd=2)
  
  #close everything if you saved plot as svg
  if (target=='svg') {
    dev.off()
  }
  
}

plotIndividualROTOrderEffects <- function(groups = c('noninstructed', 'instructed'), location = 'maxvel', conditions = c(1,2), target = 'inline') {
  
  for(group in groups){
    #but we can save plot as svg file
    if (target=='pdf' & group == 'noninstructed') {
      pdf(file='doc/fig/pilot/Fig1C_ROT_NI_individualLC_ordereffects.pdf', width=12, height=7, pointsize=14)
    } else if (target=='pdf' & group == 'instructed'){
      pdf(file='doc/fig/pilot/Fig2C_ROT_I_individualLC_ordereffects.pdf', width=12, height=7, pointsize=14)
    }
    
    if(group == 'noninstructed'){
      maxppid <- 15
    } else if (group == 'instructed'){
      maxppid <- 31
    }
    
    #par(mfrow = c(4,4))
    
    for(condition in conditions){
      
      data <- getROTOrderEffects(group = group, maxppid = maxppid, location = location, condition = condition)
      subdat <- data[,2:ncol(data)]
      
      for (icol in c(1:ncol(subdat))){
        plot(NA, NA, xlim = c(0,91), ylim = c(-200,200), 
             xlab = "Trial", ylab = "Amount of Compensation (%)", frame.plot = FALSE, #frame.plot takes away borders
             main = sprintf("ROT: %s, Order: %d, p%03d", group, condition, icol), xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
        abline(h = c(-100,0, 100), col = 8, lty = 2) #creates horizontal dashed lines through y =  0 and 30
        axis(1, at = c(1, 30, 60, 90)) #tick marks for x axis
        axis(2, at = c(-200, -100, 0, 100, 200)) #tick marks for y axis
        
        points(subdat[,icol])
      }
    }
    #close everything if you saved plot as svg
    if (target=='pdf') {
      dev.off()
    }
  }
}

plotSmoothedIndividualROTOrderEffects <- function(groups = c('noninstructed', 'instructed'), location = 'maxvel', conditions = c(1,2), target = 'inline') {
  
  for(group in groups){
    #but we can save plot as svg file
    if (target=='pdf' & group == 'noninstructed') {
      pdf(file='doc/fig/pilot/Fig1E_ROT_NI_individualLC_ordereffects.pdf', width=12, height=7, pointsize=14)
    } else if (target=='pdf' & group == 'instructed'){
      pdf(file='doc/fig/pilot/Fig2E_ROT_I_individualLC_ordereffects.pdf', width=12, height=7, pointsize=14)
    }
    
    if(group == 'noninstructed'){
      maxppid <- 15
    } else if (group == 'instructed'){
      maxppid <- 31
    }
    
    #par(mfrow = c(4,4))
    
    for(condition in conditions){
      
      data <- getROTOrderEffects(group = group, maxppid = maxppid, location = location, condition = condition)
      subdat <- data[,2:ncol(data)]
      
      for (icol in c(1:ncol(subdat))){
        plot(NA, NA, xlim = c(0,91), ylim = c(-200,200), 
             xlab = "Trial", ylab = "Amount of Compensation (%)", frame.plot = FALSE, #frame.plot takes away borders
             main = sprintf("ROT: %s, Order: %d, p%03d", group, condition, icol), xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
        abline(h = c(-100,0, 100), col = 8, lty = 2) #creates horizontal dashed lines through y =  0 and 30
        axis(1, at = c(1, 30, 60, 90)) #tick marks for x axis
        axis(2, at = c(-200, -100, 0, 100, 200)) #tick marks for y axis
        
        y <- subdat[,icol]
        x <- seq(1, length(y), 1)
        smoothdat <- ksmooth(x,y, bandwidth=5)
        lines(smoothdat)
        points(subdat[,icol])
      }
    }
    #close everything if you saved plot as svg
    if (target=='pdf') {
      dev.off()
    }
  }
}

#Order effects: MIR----

getMIROrderEffects <- function(group, maxppid, location,condition){
  
  #each column will be one participant and their 90 trials
  dat <- getMIRGroupLearningCurves(group=group, maxppid=maxppid,location=location)
  dat <- dat[,-1] #remove trial column to index properly
  
  #even ID gets ROT first, then MIR; odd ID gets MIR first, then ROT
  #but participant count starts at 0, whereas dat already starts at one
  #so at this point, odd numbers should have ROT first, while even have ROT second
  pp_MIRfirst <- dat[,c(seq(2,ncol(dat),2))]
  pp_MIRsecond <- dat[,c(seq(1,ncol(dat),2))]
  #then we can add trial column back to each of the separated data
  trial <- seq(1,90,1)
  pp_MIRfirst <- cbind(trial, pp_MIRfirst)
  pp_MIRsecond <- cbind(trial, pp_MIRsecond)
  
  #return whichever data is needed
  if (condition == 1){
    return(pp_MIRfirst)
  } else if (condition == 2){
    return(pp_MIRsecond)
  }
  
}

getMIROrderEffectsConfidenceInterval <- function(group, maxppid, location, condition, type){
  
  if (condition == 1){
    data <- getMIROrderEffects(group = group, maxppid = maxppid, location = location, condition = condition)
    #data <- data[,-6] #remove faulty particiapnt (pp004) so the 6th column REMOVE ONCE RESOLVED
    data <- as.data.frame(data)
    trialno <- data$trial
    data1 <- as.matrix(data[,2:dim(data)[2]])
    
    confidence <- data.frame()
    
    
    for (trial in trialno){
      cireaches <- data1[which(data$trial == trial), ]
      
      if (type == "t"){
        cireaches <- cireaches[!is.na(cireaches)]
        citrial <- t.interval(data = cireaches, variance = var(cireaches), conf.level = 0.95)
      } else if(type == "b"){
        citrial <- getBSConfidenceInterval(data = cireaches, resamples = 1000)
      }
      
      if (prod(dim(confidence)) == 0){
        confidence <- citrial
      } else {
        confidence <- rbind(confidence, citrial)
      }
      if (group == 'noninstructed'){
        write.csv(confidence, file='data/pilot/MIR_noninstructed_CI_ordereffects_1.csv', row.names = F) 
      } else if (group == 'instructed'){
        write.csv(confidence, file='data/pilot/MIR_instructed_CI_ordereffects_1.csv', row.names = F)
      }
      
    }
  }else if (condition == 2){
    data <- getMIROrderEffects(group = group, maxppid = maxppid, location = location, condition = condition)
    #data <- data[,-6] #remove faulty particiapnt (pp004) so the 6th column REMOVE ONCE RESOLVED
    data <- as.data.frame(data)
    trialno <- data$trial
    data1 <- as.matrix(data[,2:dim(data)[2]])
    
    confidence <- data.frame()
    
    
    for (trial in trialno){
      cireaches <- data1[which(data$trial == trial), ]
      
      if (type == "t"){
        cireaches <- cireaches[!is.na(cireaches)]
        citrial <- t.interval(data = cireaches, variance = var(cireaches), conf.level = 0.95)
      } else if(type == "b"){
        citrial <- getBSConfidenceInterval(data = cireaches, resamples = 1000)
      }
      
      if (prod(dim(confidence)) == 0){
        confidence <- citrial
      } else {
        confidence <- rbind(confidence, citrial)
      }
      if (group == 'noninstructed'){
        write.csv(confidence, file='data/pilot/MIR_noninstructed_CI_ordereffects_2.csv', row.names = F) 
      } else if (group == 'instructed'){
        write.csv(confidence, file='data/pilot/MIR_instructed_CI_ordereffects_2.csv', row.names = F)
      }
      
    }
  }
  
}

#for simplicity, I will make 2 functions that will generate order effects plots for non instructed and instructed groups separately
plotNIMIROrderEffects <- function(group = 'noninstructed', conditions = c(1,2), target = 'inline') {
  
  
  #but we can save plot as svg file
  if (target=='svg') {
    svglite(file='doc/fig/pilot/Fig3_MIR_NI_ordereffects.svg', width=12, height=7, pointsize=14, system_fonts=list(sans="Arial"))
  }
  
  # create plot
  meanGroupReaches <- list() #empty list so that it plots the means last
  
  #NA to create empty plot
  # could maybe use plot.new() ?
  plot(NA, NA, xlim = c(0,91), ylim = c(-200,200), 
       xlab = "Trial", ylab = "Amount of Compensation (%)", frame.plot = FALSE, #frame.plot takes away borders
       main = "Perturbation order: Non-instructed, Mirror reversal", xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
  abline(h = c(-100,0, 100), col = 8, lty = 2) #creates horizontal dashed lines through y =  0 and 30
  axis(1, at = c(1, 30, 60, 90)) #tick marks for x axis
  axis(2, at = c(-200, -100, 0, 100, 200)) #tick marks for y axis
  
  for(condition in conditions){
    #read in files created by getGroupConfidenceInterval in filehandling.R
    groupconfidence <- read.csv(file=sprintf('data/pilot/MIR_%s_CI_ordereffects_%d.csv', group, condition))
    
    colourscheme <- getCtypeColourScheme(conditions = condition)
    #take only first, last and middle columns of file
    lower <- groupconfidence[,1]
    upper <- groupconfidence[,3]
    mid <- groupconfidence[,2]
    
    col <- colourscheme[[condition]][['T']] #use colour scheme according to group
    
    #upper and lower bounds create a polygon
    #polygon creates it from low left to low right, then up right to up left -> use rev
    #x is just trial nnumber, y depends on values of bounds
    polygon(x = c(c(1:90), rev(c(1:90))), y = c(lower, rev(upper)), border=NA, col=col)
    
    meanGroupReaches[[condition]] <- mid #use mean to fill in empty list for each group
  }
  
  
  for (condition in conditions) {
    # plot mean reaches for each group
    col <- colourscheme[[condition]][['S']]
    lines(meanGroupReaches[[condition]],col=col,lty=1)
  }
  
  #add legend
  legend(60,-100,legend=c('MIR_First','MIR_Second'),
         col=c(colourscheme[[1]][['S']],colourscheme[[2]][['S']]),
         lty=1,bty='n',cex=1,lwd=2)
  
  #close everything if you saved plot as svg
  if (target=='svg') {
    dev.off()
  }
  
}

plotIMIROrderEffects <- function(group = 'instructed', conditions = c(1,2), target = 'inline') {
  
  
  #but we can save plot as svg file
  if (target=='svg') {
    svglite(file='doc/fig/pilot/Fig4_MIR_I_ordereffects.svg', width=12, height=7, pointsize=14, system_fonts=list(sans="Arial"))
  }
  
  # create plot
  meanGroupReaches <- list() #empty list so that it plots the means last
  
  #NA to create empty plot
  # could maybe use plot.new() ?
  plot(NA, NA, xlim = c(0,91), ylim = c(-200,200), 
       xlab = "Trial", ylab = "Amount of Compensation (%)", frame.plot = FALSE, #frame.plot takes away borders
       main = "Order Effects: MIR", xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
  abline(h = c(-100,0, 100), col = 8, lty = 2) #creates horizontal dashed lines through y =  0 and 30
  axis(1, at = c(1, 30, 60, 90)) #tick marks for x axis
  axis(2, at = c(-200, -100, 0, 100, 200)) #tick marks for y axis
  
  for(condition in conditions){
    #read in files created by getGroupConfidenceInterval in filehandling.R
    groupconfidence <- read.csv(file=sprintf('data/pilot/MIR_%s_CI_ordereffects_%d.csv', group, condition))
    
    colourscheme <- getCtypeColourScheme(conditions = condition)
    #take only first, last and middle columns of file
    lower <- groupconfidence[,1]
    upper <- groupconfidence[,3]
    mid <- groupconfidence[,2]
    
    col <- colourscheme[[condition]][['T']] #use colour scheme according to group
    
    #upper and lower bounds create a polygon
    #polygon creates it from low left to low right, then up right to up left -> use rev
    #x is just trial nnumber, y depends on values of bounds
    polygon(x = c(c(1:90), rev(c(1:90))), y = c(lower, rev(upper)), border=NA, col=col)
    
    meanGroupReaches[[condition]] <- mid #use mean to fill in empty list for each group
  }
  
  
  for (condition in conditions) {
    # plot mean reaches for each group
    col <- colourscheme[[condition]][['S']]
    lines(meanGroupReaches[[condition]],col=col,lty=1)
  }
  
  #add legend
  legend(60,-100,legend=c('MIR_First','MIR_Second'),
         col=c(colourscheme[[1]][['S']],colourscheme[[2]][['S']]),
         lty=1,bty='n',cex=1,lwd=2)
  
  #close everything if you saved plot as svg
  if (target=='svg') {
    dev.off()
  }
  
}

plotIndividualMIROrderEffects <- function(groups = c('noninstructed', 'instructed'), location = 'maxvel', conditions = c(1,2), target = 'inline') {
  
  for(group in groups){
    #but we can save plot as svg file
    if (target=='pdf' & group == 'noninstructed') {
      pdf(file='doc/fig/pilot/Fig3C_MIR_NI_individualLC_ordereffects.pdf', width=12, height=7, pointsize=14)
    } else if (target=='pdf' & group == 'instructed'){
      pdf(file='doc/fig/pilot/Fig4C_MIR_I_individualLC_ordereffects.pdf', width=12, height=7, pointsize=14)
    }
    
    if(group == 'noninstructed'){
      maxppid <- 15
    } else if (group == 'instructed'){
      maxppid <- 31
    }
    
    #par(mfrow = c(4,4))
    
    for(condition in conditions){
      
      data <- getMIROrderEffects(group = group, maxppid = maxppid, location = location, condition = condition)
      subdat <- data[,2:ncol(data)]
      
      for (icol in c(1:ncol(subdat))){
        plot(NA, NA, xlim = c(0,91), ylim = c(-200,200), 
             xlab = "Trial", ylab = "Amount of Compensation (%)", frame.plot = FALSE, #frame.plot takes away borders
             main = sprintf("MIR: %s, Order: %d, p%03d", group, condition, icol), xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
        abline(h = c(-100,0, 100), col = 8, lty = 2) #creates horizontal dashed lines through y =  0 and 30
        axis(1, at = c(1, 30, 60, 90)) #tick marks for x axis
        axis(2, at = c(-200, -100, 0, 100, 200)) #tick marks for y axis
        
        points(subdat[,icol])
        lines(subdat[,icol])
      }
    }
    #close everything if you saved plot as svg
    if (target=='pdf') {
      dev.off()
    }
  }
}

plotSmoothedIndividualMIROrderEffects <- function(groups = c('noninstructed', 'instructed'), location = 'maxvel', conditions = c(1,2), target = 'inline') {
  
  for(group in groups){
    #but we can save plot as svg file
    if (target=='pdf' & group == 'noninstructed') {
      pdf(file='doc/fig/pilot/Fig3E_MIR_NI_individualLC_ordereffects.pdf', width=12, height=7, pointsize=14)
    } else if (target=='pdf' & group == 'instructed'){
      pdf(file='doc/fig/pilot/Fig4E_MIR_I_individualLC_ordereffects.pdf', width=12, height=7, pointsize=14)
    }
    
    if(group == 'noninstructed'){
      maxppid <- 15
    } else if (group == 'instructed'){
      maxppid <- 31
    }
    
    #par(mfrow = c(4,4))
    
    for(condition in conditions){
      
      data <- getMIROrderEffects(group = group, maxppid = maxppid, location = location, condition = condition)
      subdat <- data[,2:ncol(data)]
      
      for (icol in c(1:ncol(subdat))){
        plot(NA, NA, xlim = c(0,91), ylim = c(-200,200), 
             xlab = "Trial", ylab = "Amount of Compensation (%)", frame.plot = FALSE, #frame.plot takes away borders
             main = sprintf("MIR: %s, Order: %d, p%03d", group, condition, icol), xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
        abline(h = c(-100,0, 100), col = 8, lty = 2) #creates horizontal dashed lines through y =  0 and 30
        axis(1, at = c(1, 30, 60, 90)) #tick marks for x axis
        axis(2, at = c(-200, -100, 0, 100, 200)) #tick marks for y axis
        
        y <- subdat[,icol]
        x <- seq(1, length(y), 1)
        smoothdat <- ksmooth(x,y, bandwidth=5)
        lines(smoothdat)
        points(subdat[,icol])
        
      }
    }
    #close everything if you saved plot as svg
    if (target=='pdf') {
      dev.off()
    }
  }
}

#Order effects: Exponential Model----
#need group data of % compensation, bootstrap to generate upper, mid, lower CIs
getROTOrderEffectsPars <- function(groups = c('noninstructed', 'instructed'), location = 'maxvel', conditions = c(1,2), bootstraps = 1000){
  for(group in groups){
    if(group == 'noninstructed'){
      maxppid <- 15
    } else if (group == 'instructed'){
      maxppid <- 31
    }
    for(condition in conditions){
      data <- getROTOrderEffects(group = group, maxppid = maxppid, location = location, condition = condition)
      subdat <- data[,2:ncol(data)]
      lambda <- c()
      N0 <- c()
      for(bs in c(1:bootstraps)){
        cat(sprintf('group: %s, condition: %s, iteration: %s \n', group, condition, bs))
        bs_mat <- subdat[,sample(ncol(subdat),ncol(subdat), replace = TRUE)]
        bs_dat <- rowMeans(bs_mat, na.rm = TRUE)
        
        par <- exponentialFit(signal = bs_dat)
        lambda <- c(lambda, par['lambda'])
        N0 <- c(N0, par['N0'])
      }
      
      write.csv(data.frame(lambda, N0), file=sprintf('data/pilot/ROT_%s_modpar_ordereffects_%s.csv',group,condition), quote=F, row.names=F)
    }
  }
  
}

plotROTOrderEffectsModel <- function(groups = c('noninstructed', 'instructed'), conditions = c(1,2), location = 'maxvel', target='inline'){
  for(group in groups){
    
    #but we can save plot as svg file
    if (target=='svg' & group == 'noninstructed') {
      svglite(file='doc/fig/pilot/Fig1A_ROT_NI_ordereffects_model.svg', width=12, height=7, pointsize=14, system_fonts=list(sans="Arial"))
    } else if (target=='svg' & group == 'instructed'){
      svglite(file='doc/fig/pilot/Fig2A_ROT_I_ordereffects_model.svg', width=12, height=7, pointsize=14, system_fonts=list(sans="Arial"))
    }
    
    par(mfrow = c(1,2))
    
    for(condition in conditions){
      
      plot(NA, NA, xlim = c(0,91), ylim = c(-200,200), 
           xlab = "Trial", ylab = "Amount of Compensation (%)", frame.plot = FALSE, #frame.plot takes away borders
           main = sprintf("ROT: %s, order: %s", group, condition), xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
      abline(h = c(-100,0, 100), col = 8, lty = 2) #creates horizontal dashed lines through y =  0 and 30
      axis(1, at = c(0, 30, 60, 89)) #tick marks for x axis
      axis(2, at = c(-200, -100, 0, 100, 200)) #tick marks for y axis
      
      if(group == 'noninstructed'){
        maxppid <- 15
      } else if (group == 'instructed'){
        maxppid <- 31
      }
      
      #show the percent compensation from data
      groupconfidence <- read.csv(file=sprintf('data/pilot/ROT_%s_CI_ordereffects_%d.csv', group, condition))
      mid <- groupconfidence[,2]
      x <- c(0:89)
      col <- '#A9A9A9ff'
      lines(x, mid, lty=1, col=col)
      
      #get model parameters from data - no bootstrapping
      dat <- getROTOrderEffects(group = group, maxppid = maxppid, location = location, condition = condition)
      subdat <- dat[,2:ncol(dat)]
      bs_dat <- rowMeans(subdat, na.rm = TRUE)
      par <- exponentialFit(signal = bs_dat)
      
      #get CIs for rate of change, asymptote will just be 50%, then solid line is based from pars of data (no bootstrapping)
      #bootstrapped pars are used for lower and upper bounds
      data <- read.csv(sprintf('data/pilot/ROT_%s_modpar_ordereffects_%s.csv', group, condition))
      
      qs_lambda <- quantile(data$lambda, probs = c(0.025, 0.500, 0.975))
      qs_N0 <- quantile(data$N0, probs = c(0.025, 0.500, 0.975))
      
      lwr <- setNames(c(qs_lambda[['2.5%']], qs_N0[['50%']]), c('lambda', 'N0'))
      mid <- setNames(c(par[['lambda']], qs_N0[['50%']]), c('lambda', 'N0'))
      upr <- setNames(c(qs_lambda[['97.5%']], qs_N0[['50%']]), c('lambda', 'N0'))
      
      xcoords <- c(0:89)
      dfit <- exponentialModel(par=lwr, timepoints=xcoords)
      y_lwr <- dfit$output
      dfit <- exponentialModel(par=mid, timepoints=xcoords)
      y_mid <- dfit$output
      dfit <- exponentialModel(par=upr, timepoints=xcoords)
      y_upr <- dfit$output
      
      colourscheme <- getCtypeColourScheme(conditions = condition)
      col <- colourscheme[[condition]][['T']] #use colour scheme according to group
      #upper and lower bounds create a polygon
      #polygon creates it from low left to low right, then up right to up left -> use rev
      #x is just trial nnumber, y depends on values of bounds
      polygon(x = c(xcoords, rev(xcoords)), y = c(y_lwr, rev(y_upr)), border=NA, col=col)
      #add CIs for asymptote
      abline(h = c(qs_N0[['2.5%']], qs_N0[['97.5%']]), col = col, lty = 2, lwd=2)
      col <- colourscheme[[condition]][['S']]
      lines(xcoords, y_mid,col=col,lty=1,lwd=2)
      
      #add legend
      legend(0,-100,legend=c('reaches','model (rate of change)','learning asymptote 95% CI'),
             col=c('#A9A9A9ff',colourscheme[[condition]][['S']],colourscheme[[condition]][['T']]),
             lty=c(1,1,2),bty='n',cex=.65,lwd=2)
      
    }

    #close everything if you saved plot as svg
    if (target=='svg') {
      dev.off()
    }
  }
}

getMIROrderEffectsPars <- function(groups = c('noninstructed', 'instructed'), location = 'maxvel', conditions = c(1,2), bootstraps = 1000){
  for(group in groups){
    if(group == 'noninstructed'){
      maxppid <- 15
    } else if (group == 'instructed'){
      maxppid <- 31
    }
    for(condition in conditions){
      data <- getMIROrderEffects(group = group, maxppid = maxppid, location = location, condition = condition)
      subdat <- data[,2:ncol(data)]
      lambda <- c()
      N0 <- c()
      for(bs in c(1:bootstraps)){
        cat(sprintf('group: %s, condition: %s, iteration: %s \n', group, condition, bs))
        bs_mat <- subdat[,sample(ncol(subdat),ncol(subdat), replace = TRUE)]
        bs_dat <- rowMeans(bs_mat, na.rm = TRUE)
        
        par <- exponentialFit(signal = bs_dat)
        lambda <- c(lambda, par['lambda'])
        N0 <- c(N0, par['N0'])
      }
      
      write.csv(data.frame(lambda, N0), file=sprintf('data/pilot/MIR_%s_modpar_ordereffects_%s.csv',group,condition), quote=F, row.names=F)
    }
  }
  
}

plotMIROrderEffectsModel <- function(groups = c('noninstructed', 'instructed'), conditions = c(1,2), location = 'maxvel', target='inline'){
  for(group in groups){
    
    #but we can save plot as svg file
    if (target=='svg' & group == 'noninstructed') {
      svglite(file='doc/fig/pilot/Fig3A_MIR_NI_ordereffects_model.svg', width=12, height=7, pointsize=14, system_fonts=list(sans="Arial"))
    } else if (target=='svg' & group == 'instructed'){
      svglite(file='doc/fig/pilot/Fig4A_MIR_I_ordereffects_model.svg', width=12, height=7, pointsize=14, system_fonts=list(sans="Arial"))
    }
    
    par(mfrow = c(1,2))
    
    for(condition in conditions){
      
      plot(NA, NA, xlim = c(0,91), ylim = c(-200,200), 
           xlab = "Trial", ylab = "Amount of Compensation (%)", frame.plot = FALSE, #frame.plot takes away borders
           main = sprintf("MIR: %s, order: %s", group, condition), xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
      abline(h = c(-100,0, 100), col = 8, lty = 2) #creates horizontal dashed lines through y =  0 and 30
      axis(1, at = c(0, 30, 60, 89)) #tick marks for x axis
      axis(2, at = c(-200, -100, 0, 100, 200)) #tick marks for y axis
      
      if(group == 'noninstructed'){
        maxppid <- 15
      } else if (group == 'instructed'){
        maxppid <- 31
      }
      
      #show the percent compensation from data
      groupconfidence <- read.csv(file=sprintf('data/pilot/MIR_%s_CI_ordereffects_%d.csv', group, condition))
      x <- c(0:89)
      mid <- groupconfidence[,2]
      col <- '#A9A9A9ff'
      lines(x, mid, lty=1, col=col)
        
      #get model parameters from data - no bootstrapping
      dat <- getMIROrderEffects(group = group, maxppid = maxppid, location = location, condition = condition)
      subdat <- dat[,2:ncol(dat)]
      bs_dat <- rowMeans(subdat, na.rm = TRUE)
      par <- exponentialFit(signal = bs_dat)
        
      #get CIs for rate of change, asymptote will just be 50%, then solid line is based from pars of data (no bootstrapping)
      #bootstrapped pars are used for lower and upper bounds
      data <- read.csv(sprintf('data/pilot/MIR_%s_modpar_ordereffects_%s.csv', group, condition))
        
      qs_lambda <- quantile(data$lambda, probs = c(0.025, 0.500, 0.975))
      qs_N0 <- quantile(data$N0, probs = c(0.025, 0.500, 0.975))
        
      lwr <- setNames(c(qs_lambda[['2.5%']], qs_N0[['50%']]), c('lambda', 'N0'))
      mid <- setNames(c(par[['lambda']], qs_N0[['50%']]), c('lambda', 'N0'))
      upr <- setNames(c(qs_lambda[['97.5%']], qs_N0[['50%']]), c('lambda', 'N0'))
        
      xcoords <- c(0:89)
      dfit <- exponentialModel(par=lwr, timepoints=xcoords)
      y_lwr <- dfit$output
      dfit <- exponentialModel(par=mid, timepoints=xcoords)
      y_mid <- dfit$output
      dfit <- exponentialModel(par=upr, timepoints=xcoords)
      y_upr <- dfit$output
        
      colourscheme <- getCtypeColourScheme(conditions = condition)
      col <- colourscheme[[condition]][['T']] #use colour scheme according to group
      #upper and lower bounds create a polygon
      #polygon creates it from low left to low right, then up right to up left -> use rev
      #x is just trial nnumber, y depends on values of bounds
      polygon(x = c(xcoords, rev(xcoords)), y = c(y_lwr, rev(y_upr)), border=NA, col=col)
      #add CIs for asymptote
      abline(h = c(qs_N0[['2.5%']], qs_N0[['97.5%']]), col = col, lty = 2, lwd=2)
      col <- colourscheme[[condition]][['S']]
      lines(xcoords, y_mid,col=col,lty=1,lwd=2)
        
      #add legend
      legend(0,-100,legend=c('reaches','model (rate of change)','learning asymptote 95% CI'),
              col=c('#A9A9A9ff',colourscheme[[condition]][['S']],colourscheme[[condition]][['T']]),
              lty=c(1,1,2),bty='n',cex=.65,lwd=2)
        
    }
    
    #close everything if you saved plot as svg
    if (target=='svg') {
      dev.off()
    }
  }
}

#Order effects: Step Function Model----
#need group data of % compensation, bootstrap to generate upper, mid, lower CIs
getROTOrderEffectsStepPars <- function(groups = c('noninstructed', 'instructed'), location = 'maxvel', conditions = c(1,2), bootstraps = 1000){
  for(group in groups){
    if(group == 'noninstructed'){
      maxppid <- 15
    } else if (group == 'instructed'){
      maxppid <- 31
    }
    for(condition in conditions){
      data <- getROTOrderEffects(group = group, maxppid = maxppid, location = location, condition = condition)
      subdat <- data[,2:ncol(data)]
      step <- c()
      asymptote <- c()
      for(bs in c(1:bootstraps)){
        cat(sprintf('group: %s, condition: %s, iteration: %s \n', group, condition, bs))
        bs_mat <- subdat[,sample(ncol(subdat),ncol(subdat), replace = TRUE)]
        bs_dat <- rowMeans(bs_mat, na.rm = TRUE)
        
        par <- stepFunctionFit(signal = bs_dat)
        step <- c(step, par['step'])
        asymptote <- c(asymptote, par['asymptote'])
      }
      
      write.csv(data.frame(step, asymptote), file=sprintf('data/pilot/ROT_%s_stepmodpar_ordereffects_%s.csv',group,condition), quote=F, row.names=F)
    }
  }
  
}

plotROTOrderEffectsStepModel <- function(groups = c('noninstructed', 'instructed'), conditions = c(1,2), location = 'maxvel', target='inline'){
  for(group in groups){
    
    #but we can save plot as svg file
    if (target=='svg' & group == 'noninstructed') {
      svglite(file='doc/fig/pilot/Fig1B_ROT_NI_ordereffects_stepmodel.svg', width=12, height=7, pointsize=14, system_fonts=list(sans="Arial"))
    } else if (target=='svg' & group == 'instructed'){
      svglite(file='doc/fig/pilot/Fig2B_ROT_I_ordereffects_stepmodel.svg', width=12, height=7, pointsize=14, system_fonts=list(sans="Arial"))
    }
    
    par(mfrow = c(1,2))
    
    for(condition in conditions){
      
      plot(NA, NA, xlim = c(0,91), ylim = c(-200,200), 
           xlab = "Trial", ylab = "Amount of Compensation (%)", frame.plot = FALSE, #frame.plot takes away borders
           main = sprintf("ROT: %s, order: %s", group, condition), xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
      abline(h = c(-100,0, 100), col = 8, lty = 2) #creates horizontal dashed lines through y =  0 and 30
      axis(1, at = c(0, 30, 60, 89)) #tick marks for x axis
      axis(2, at = c(-200, -100, 0, 100, 200)) #tick marks for y axis
      
      if(group == 'noninstructed'){
        maxppid <- 15
      } else if (group == 'instructed'){
        maxppid <- 31
      }
      
      #show the percent compensation from data
      groupconfidence <- read.csv(file=sprintf('data/pilot/ROT_%s_CI_ordereffects_%d.csv', group, condition))
      mid <- groupconfidence[,2]
      x <- c(0:89)
      col <- '#A9A9A9ff'
      lines(x, mid, lty=1, col=col)
        
      #get model parameters from data - no bootstrapping
      dat <- getROTOrderEffects(group = group, maxppid = maxppid, location = location, condition = condition)
      subdat <- dat[,2:ncol(dat)]
      bs_dat <- rowMeans(subdat, na.rm = TRUE)
      par <- stepFunctionFit(signal = bs_dat)
        
      #get CIs for rate of change, asymptote will just be 50%, then solid line is based from pars of data (no bootstrapping)
      #bootstrapped pars are used for lower and upper bounds
      data <- read.csv(sprintf('data/pilot/ROT_%s_stepmodpar_ordereffects_%s.csv', group, condition))
        
      qs_step <- quantile(data$step, probs = c(0.025, 0.500, 0.975))
      qs_asymptote <- quantile(data$asymptote, probs = c(0.025, 0.500, 0.975))
        
      lwr <- setNames(c(qs_step[['2.5%']], qs_asymptote[['50%']]), c('step', 'asymptote'))
      mid <- setNames(c(par[['step']], qs_asymptote[['50%']]), c('step', 'asymptote'))
      upr <- setNames(c(qs_step[['97.5%']], qs_asymptote[['50%']]), c('step', 'asymptote'))
        
      xcoords <- c(0:89)
      dfit <- stepFunctionModel(par=lwr, timepoints=xcoords)
      y_lwr <- dfit$output
      dfit <- stepFunctionModel(par=mid, timepoints=xcoords)
      y_mid <- dfit$output
      dfit <- stepFunctionModel(par=upr, timepoints=xcoords)
      y_upr <- dfit$output
        
      colourscheme <- getCtypeColourScheme(conditions = condition)
      col <- colourscheme[[condition]][['T']] #use colour scheme according to group
      #upper and lower bounds create a polygon
      #polygon creates it from low left to low right, then up right to up left -> use rev
      #x is just trial nnumber, y depends on values of bounds
      polygon(x = c(xcoords, rev(xcoords)), y = c(y_lwr, rev(y_upr)), border=NA, col=col)
      #add CIs for asymptote
      abline(h = c(qs_asymptote[['2.5%']], qs_asymptote[['97.5%']]), col = col, lty = 2, lwd=2)
      col <- colourscheme[[condition]][['S']]
      lines(xcoords, y_mid,col=col,lty=1,lwd=2)
        
      #add legend
      legend(20,-100,legend=c('reaches','model (rate of change)','learning asymptote 95% CI'),
              col=c('#A9A9A9ff',colourscheme[[condition]][['S']],colourscheme[[condition]][['T']]),
              lty=c(1,1,2),bty='n',cex=1,lwd=2)
        
    }
    
    #close everything if you saved plot as svg
    if (target=='svg') {
      dev.off()
    }
  }
}

getMIROrderEffectsStepPars <- function(groups = c('noninstructed', 'instructed'), location = 'maxvel', conditions = c(1,2), bootstraps = 1000){
  for(group in groups){
    if(group == 'noninstructed'){
      maxppid <- 15
    } else if (group == 'instructed'){
      maxppid <- 31
    }
    for(condition in conditions){
      data <- getMIROrderEffects(group = group, maxppid = maxppid, location = location, condition = condition)
      subdat <- data[,2:ncol(data)]
      step <- c()
      asymptote <- c()
      for(bs in c(1:bootstraps)){
        cat(sprintf('group: %s, condition: %s, iteration: %s \n', group, condition, bs))
        bs_mat <- subdat[,sample(ncol(subdat),ncol(subdat), replace = TRUE)]
        bs_dat <- rowMeans(bs_mat, na.rm = TRUE)
        
        par <- stepFunctionFit(signal = bs_dat)
        step <- c(step, par['step'])
        asymptote <- c(asymptote, par['asymptote'])
      }
      
      write.csv(data.frame(step, asymptote), file=sprintf('data/pilot/MIR_%s_stepmodpar_ordereffects_%s.csv',group,condition), quote=F, row.names=F)
    }
  }
  
}

plotMIROrderEffectsStepModel <- function(groups = c('noninstructed', 'instructed'), conditions = c(1,2), location = 'maxvel', target='inline'){
  for(group in groups){
    
    #but we can save plot as svg file
    if (target=='svg' & group == 'noninstructed') {
      svglite(file='doc/fig/pilot/Fig3B_MIR_NI_ordereffects_stepmodel.svg', width=12, height=7, pointsize=14, system_fonts=list(sans="Arial"))
    } else if (target=='svg' & group == 'instructed'){
      svglite(file='doc/fig/pilot/Fig4B_MIR_I_ordereffects_stepmodel.svg', width=12, height=7, pointsize=14, system_fonts=list(sans="Arial"))
    }
    
    par(mfrow = c(1,2))
    
    for(condition in conditions){
      
      plot(NA, NA, xlim = c(0,91), ylim = c(-200,200), 
           xlab = "Trial", ylab = "Amount of Compensation (%)", frame.plot = FALSE, #frame.plot takes away borders
           main = sprintf("MIR: %s, order: %s", group, condition), xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
      abline(h = c(-100,0, 100), col = 8, lty = 2) #creates horizontal dashed lines through y =  0 and 30
      axis(1, at = c(0, 30, 60, 89)) #tick marks for x axis
      axis(2, at = c(-200, -100, 0, 100, 200)) #tick marks for y axis
      
      if(group == 'noninstructed'){
        maxppid <- 15
      } else if (group == 'instructed'){
        maxppid <- 31
      }
      
      #show the percent compensation from data
      groupconfidence <- read.csv(file=sprintf('data/pilot/MIR_%s_CI_ordereffects_%d.csv', group, condition))
      x <- c(0:89)
      mid <- groupconfidence[,2]
      col <- '#A9A9A9ff'
      lines(x, mid, lty=1, col=col)
        
      #get model parameters from data - no bootstrapping
      dat <- getMIROrderEffects(group = group, maxppid = maxppid, location = location, condition = condition)
      subdat <- dat[,2:ncol(dat)]
      bs_dat <- rowMeans(subdat, na.rm = TRUE)
      par <- stepFunctionFit(signal = bs_dat)
        
      #get CIs for rate of change, asymptote will just be 50%, then solid line is based from pars of data (no bootstrapping)
      #bootstrapped pars are used for lower and upper bounds
      data <- read.csv(sprintf('data/pilot/MIR_%s_stepmodpar_ordereffects_%s.csv', group, condition))
        
      qs_step <- quantile(data$step, probs = c(0.025, 0.500, 0.975))
      qs_asymptote <- quantile(data$asymptote, probs = c(0.025, 0.500, 0.975))
        
      lwr <- setNames(c(qs_step[['2.5%']], qs_asymptote[['50%']]), c('step', 'asymptote'))
      mid <- setNames(c(par[['step']], qs_asymptote[['50%']]), c('step', 'asymptote'))
      upr <- setNames(c(qs_step[['97.5%']], qs_asymptote[['50%']]), c('step', 'asymptote'))
        
      xcoords <- c(0:89)
      dfit <- stepFunctionModel(par=lwr, timepoints=xcoords)
      y_lwr <- dfit$output
      dfit <- stepFunctionModel(par=mid, timepoints=xcoords)
      y_mid <- dfit$output
      dfit <- stepFunctionModel(par=upr, timepoints=xcoords)
      y_upr <- dfit$output
        
      colourscheme <- getCtypeColourScheme(conditions = condition)
      col <- colourscheme[[condition]][['T']] #use colour scheme according to group
      #upper and lower bounds create a polygon
      #polygon creates it from low left to low right, then up right to up left -> use rev
      #x is just trial nnumber, y depends on values of bounds
      polygon(x = c(xcoords, rev(xcoords)), y = c(y_lwr, rev(y_upr)), border=NA, col=col)
      #add CIs for asymptote
      abline(h = c(qs_asymptote[['2.5%']], qs_asymptote[['97.5%']]), col = col, lty = 2, lwd=2)
      col <- colourscheme[[condition]][['S']]
      lines(xcoords, y_mid,col=col,lty=1,lwd=2)
        
      #add legend
      legend(0,-100,legend=c('reaches','model (rate of change)','learning asymptote 95% CI'),
              col=c('#A9A9A9ff',colourscheme[[condition]][['S']],colourscheme[[condition]][['T']]),
              lty=c(1,1,2),bty='n',cex=1,lwd=2)
        
    }
    
    #close everything if you saved plot as svg
    if (target=='svg') {
      dev.off()
    }
  }
}

#Order effects: Logistic Function Model----
#need group data of % compensation, bootstrap to generate upper, mid, lower CIs
getROTOrderEffectsLogPars <- function(groups = c('noninstructed', 'instructed'), location = 'maxvel', conditions = c(1,2), bootstraps = 1000){
  for(group in groups){
    if(group == 'noninstructed'){
      maxppid <- 15
    } else if (group == 'instructed'){
      maxppid <- 31
    }
    for(condition in conditions){
      data <- getROTOrderEffects(group = group, maxppid = maxppid, location = location, condition = condition)
      subdat <- data[,2:ncol(data)]
      x0 <- c()
      k <- c()
      L <- c()
      for(bs in c(1:bootstraps)){
        cat(sprintf('group: %s, condition: %s, iteration: %s \n', group, condition, bs))
        bs_mat <- subdat[,sample(ncol(subdat),ncol(subdat), replace = TRUE)]
        y <- rowMeans(bs_mat, na.rm = TRUE)
        x <- seq(1, length(y), 1)
        bs_dat <- data.frame(x,y)
        
        par <- logisticFunctionFit(data = bs_dat)
        x0 <- c(x0, par['x0'])
        k <- c(k, par['k'])
        L <- c(L, par['L'])
      }
      
      write.csv(data.frame(x0, k, L), file=sprintf('data/pilot/ROT_%s_logmodpar_ordereffects_%s.csv',group,condition), quote=F, row.names=F)
    }
  }
  
}

plotROTOrderEffectsLogModel <- function(groups = c('noninstructed', 'instructed'), conditions = c(1,2), location = 'maxvel', target='inline'){
  for(group in groups){
    
    #but we can save plot as svg file
    if (target=='svg' & group == 'noninstructed') {
      svglite(file='doc/fig/pilot/Fig1D_ROT_NI_ordereffects_logmodel.svg', width=12, height=7, pointsize=14, system_fonts=list(sans="Arial"))
    } else if (target=='svg' & group == 'instructed'){
      svglite(file='doc/fig/pilot/Fig2D_ROT_I_ordereffects_logmodel.svg', width=12, height=7, pointsize=14, system_fonts=list(sans="Arial"))
    }
    
    par(mfrow = c(1,2))
    
    for(condition in conditions){
      
      plot(NA, NA, xlim = c(0,91), ylim = c(-200,200), 
           xlab = "Trial", ylab = "Amount of Compensation (%)", frame.plot = FALSE, #frame.plot takes away borders
           main = sprintf("ROT: %s, order: %s", group, condition), xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
      abline(h = c(-100,0, 100), col = 8, lty = 2) #creates horizontal dashed lines through y =  0 and 30
      axis(1, at = c(0, 30, 60, 89)) #tick marks for x axis
      axis(2, at = c(-200, -100, 0, 100, 200)) #tick marks for y axis
      
      if(group == 'noninstructed'){
        maxppid <- 15
      } else if (group == 'instructed'){
        maxppid <- 31
      }
      
      #show the percent compensation from data
      groupconfidence <- read.csv(file=sprintf('data/pilot/ROT_%s_CI_ordereffects_%d.csv', group, condition))
      mid <- groupconfidence[,2]
      x <- c(0:89)
      col <- '#A9A9A9ff'
      lines(x, mid, lty=1, col=col)
        
      #get model parameters from data - no bootstrapping
      dat <- getROTOrderEffects(group = group, maxppid = maxppid, location = location, condition = condition)
      subdat <- dat[,2:ncol(dat)]
      y <- rowMeans(subdat, na.rm = TRUE)
      x <- seq(1, length(y), 1)
      bs_dat <- data.frame(x,y)
      par <- logisticFunctionFit(data = bs_dat)
        
      #get CIs for rate of change, asymptote will just be 50%, then solid line is based from pars of data (no bootstrapping)
      #bootstrapped pars are used for lower and upper bounds
      data <- read.csv(sprintf('data/pilot/ROT_%s_logmodpar_ordereffects_%s.csv', group, condition))
        
      qs_x0 <- quantile(data$x0, probs = c(0.025, 0.500, 0.975))
      qs_k <- quantile(data$k, probs = c(0.025, 0.500, 0.975))
      qs_L <- quantile(data$L, probs = c(0.025, 0.500, 0.975))
      
      lwr <- setNames(c(qs_x0[['2.5%']], qs_k[['2.5%']], qs_L[['50%']]), c('x0', 'k', 'L'))
      mid <- setNames(c(par[['x0']], par[['k']], qs_L[['50%']]), c('x0', 'k', 'L'))
      upr <- setNames(c(qs_x0[['97.5%']], qs_k[['97.5%']], qs_L[['50%']]), c('x0', 'k', 'L'))
        
      xcoords <- c(0:89)
      dfit <- logisticFunction(par=lwr, x=xcoords)
      y_lwr <- dfit
      dfit <- logisticFunction(par=mid, x=xcoords)
      y_mid <- dfit
      dfit <- logisticFunction(par=upr, x=xcoords)
      y_upr <- dfit
        
      colourscheme <- getCtypeColourScheme(conditions = condition)
      col <- colourscheme[[condition]][['T']] #use colour scheme according to group
      #upper and lower bounds create a polygon
      #polygon creates it from low left to low right, then up right to up left -> use rev
      #x is just trial nnumber, y depends on values of bounds
      polygon(x = c(xcoords, rev(xcoords)), y = c(y_lwr, rev(y_upr)), border=NA, col=col)
      #add CIs for asymptote
      abline(h = c(qs_L[['2.5%']], qs_L[['97.5%']]), col = col, lty = 2, lwd=2)
      col <- colourscheme[[condition]][['S']]
      lines(xcoords, y_mid,col=col,lty=1,lwd=2)
        
      #add legend
      legend(20,-100,legend=c('reaches','model (rate of change)','learning asymptote 95% CI'),
              col=c('#A9A9A9ff',colourscheme[[condition]][['S']],colourscheme[[condition]][['T']]),
              lty=c(1,1,2),bty='n',cex=1,lwd=2)
        
    }
    
    #close everything if you saved plot as svg
    if (target=='svg') {
      dev.off()
    }
  }
}

getMIROrderEffectsLogPars <- function(groups = c('noninstructed', 'instructed'), location = 'maxvel', conditions = c(1,2), bootstraps = 1000){
  for(group in groups){
    if(group == 'noninstructed'){
      maxppid <- 15
    } else if (group == 'instructed'){
      maxppid <- 31
    }
    for(condition in conditions){
      data <- getMIROrderEffects(group = group, maxppid = maxppid, location = location, condition = condition)
      subdat <- data[,2:ncol(data)]
      x0 <- c()
      k <- c()
      L <- c()
      for(bs in c(1:bootstraps)){
        cat(sprintf('group: %s, condition: %s, iteration: %s \n', group, condition, bs))
        bs_mat <- subdat[,sample(ncol(subdat),ncol(subdat), replace = TRUE)]
        y <- rowMeans(bs_mat, na.rm = TRUE)
        x <- seq(1, length(y), 1)
        bs_dat <- data.frame(x,y)
        
        par <- logisticFunctionFit(data = bs_dat)
        x0 <- c(x0, par['x0'])
        k <- c(k, par['k'])
        L <- c(L, par['L'])
      }
      
      write.csv(data.frame(x0, k, L), file=sprintf('data/pilot/MIR_%s_logmodpar_ordereffects_%s.csv',group,condition), quote=F, row.names=F)
    }
  }
  
}

plotMIROrderEffectsLogModel <- function(groups = c('noninstructed', 'instructed'), conditions = c(1,2), location = 'maxvel', target='inline'){
  for(group in groups){
    
    #but we can save plot as svg file
    if (target=='svg' & group == 'noninstructed') {
      svglite(file='doc/fig/pilot/Fig3D_ROT_NI_ordereffects_logmodel.svg', width=12, height=7, pointsize=14, system_fonts=list(sans="Arial"))
    } else if (target=='svg' & group == 'instructed'){
      svglite(file='doc/fig/pilot/Fig4D_ROT_I_ordereffects_logmodel.svg', width=12, height=7, pointsize=14, system_fonts=list(sans="Arial"))
    }
    
    par(mfrow = c(1,2))
    
    for(condition in conditions){
      
      plot(NA, NA, xlim = c(0,91), ylim = c(-200,200), 
           xlab = "Trial", ylab = "Amount of Compensation (%)", frame.plot = FALSE, #frame.plot takes away borders
           main = sprintf("MIR: %s, order: %s", group, condition), xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
      abline(h = c(-100,0, 100), col = 8, lty = 2) #creates horizontal dashed lines through y =  0 and 30
      axis(1, at = c(0, 30, 60, 89)) #tick marks for x axis
      axis(2, at = c(-200, -100, 0, 100, 200)) #tick marks for y axis
      
      if(group == 'noninstructed'){
        maxppid <- 15
      } else if (group == 'instructed'){
        maxppid <- 31
      }
      
      #show the percent compensation from data
      groupconfidence <- read.csv(file=sprintf('data/pilot/MIR_%s_CI_ordereffects_%d.csv', group, condition))
      mid <- groupconfidence[,2]
      x <- c(0:89)
      col <- '#A9A9A9ff'
      lines(x, mid, lty=1, col=col)
        
      #get model parameters from data - no bootstrapping
      dat <- getMIROrderEffects(group = group, maxppid = maxppid, location = location, condition = condition)
      subdat <- dat[,2:ncol(dat)]
      y <- rowMeans(subdat, na.rm = TRUE)
      x <- seq(1, length(y), 1)
      bs_dat <- data.frame(x,y)
      par <- logisticFunctionFit(data = bs_dat)
        
      #get CIs for rate of change, asymptote will just be 50%, then solid line is based from pars of data (no bootstrapping)
      #bootstrapped pars are used for lower and upper bounds
      data <- read.csv(sprintf('data/pilot/MIR_%s_logmodpar_ordereffects_%s.csv', group, condition))
        
      qs_x0 <- quantile(data$x0, probs = c(0.025, 0.500, 0.975))
      qs_k <- quantile(data$k, probs = c(0.025, 0.500, 0.975))
      qs_L <- quantile(data$L, probs = c(0.025, 0.500, 0.975))
        
      lwr <- setNames(c(qs_x0[['2.5%']], qs_k[['2.5%']], qs_L[['50%']]), c('x0', 'k', 'L'))
      mid <- setNames(c(par[['x0']], par[['k']], qs_L[['50%']]), c('x0', 'k', 'L'))
      upr <- setNames(c(qs_x0[['97.5%']], qs_k[['97.5%']], qs_L[['50%']]), c('x0', 'k', 'L'))
        
      xcoords <- c(0:89)
      dfit <- logisticFunction(par=lwr, x=xcoords)
      y_lwr <- dfit
      dfit <- logisticFunction(par=mid, x=xcoords)
      y_mid <- dfit
      dfit <- logisticFunction(par=upr, x=xcoords)
      y_upr <- dfit
        
      colourscheme <- getCtypeColourScheme(conditions = condition)
      col <- colourscheme[[condition]][['T']] #use colour scheme according to group
      #upper and lower bounds create a polygon
      #polygon creates it from low left to low right, then up right to up left -> use rev
      #x is just trial nnumber, y depends on values of bounds
      polygon(x = c(xcoords, rev(xcoords)), y = c(y_lwr, rev(y_upr)), border=NA, col=col)
      #add CIs for asymptote
      abline(h = c(qs_L[['2.5%']], qs_L[['97.5%']]), col = col, lty = 2, lwd=2)
      col <- colourscheme[[condition]][['S']]
      lines(xcoords, y_mid,col=col,lty=1,lwd=2)
        
      #add legend
      legend(20,-100,legend=c('reaches','model (rate of change)','learning asymptote 95% CI'),
              col=c('#A9A9A9ff',colourscheme[[condition]][['S']],colourscheme[[condition]][['T']]),
              lty=c(1,1,2),bty='n',cex=1,lwd=2)
        
    }
    
    #close everything if you saved plot as svg
    if (target=='svg') {
      dev.off()
    }
  }
}

#Order effects: Model comparisons----
#fit exponential and step models to individual data (do non-instructed for now)
# get all MSE's (two MSE per participant - from two models)
#calculate 2 AICs
#use AICs for a relative log-likelihood for each model within each participant
getOrderEffectsMSE <- function(perturb = c('ROT', 'MIR'), group = 'noninstructed', maxppid = 15, location = 'maxvel', conditions = c(1,2)){
  for(ptype in perturb){
    step <- c()
    asymptote <- c()
    mse_step <- c()
    lambda <- c()
    N0 <- c()
    mse_expl <- c()
    x0 <- c()
    k <- c()
    L <- c()
    mse_log <- c()
    order <- c()
    for(condition in conditions){
      if(ptype == 'ROT'){
        data <- getROTOrderEffects(group = group, maxppid = maxppid, location = location, condition = condition)
      } else if (ptype == 'MIR'){
        data <- getMIROrderEffects(group = group, maxppid = maxppid, location = location, condition = condition)
      }
      
      subdat <- data[,2:ncol(data)]
      for(icol in c(1:ncol(subdat))){
        ppdat <- subdat[,icol]
        par_step <- stepFunctionFit(signal = ppdat)
        pp_mse_step <- stepFunctionMSE(par=par_step, signal=ppdat)
        
        par_expl <- exponentialFit(signal = ppdat)
        pp_mse_expl<- exponentialMSE(par=par_expl, signal=ppdat)
        
        y <- ppdat
        x <- seq(1, length(y), 1)
        bs_dat <- data.frame(x,y)
        par_log <- logisticFunctionFit(data = bs_dat)
        pp_mse_log<- logisticFunctionMSE(par=par_log, data=bs_dat)
        
        cond <- condition
        
        step <- c(step, par_step['step'])
        asymptote <- c(asymptote, par_step['asymptote'])
        mse_step <- c(mse_step, pp_mse_step)
        lambda <- c(lambda, par_expl['lambda'])
        N0 <- c(N0, par_expl['N0'])
        mse_expl <- c(mse_expl, pp_mse_expl)
        x0 <- c(x0, par_log['x0'])
        k <- c(k, par_log['k'])
        L <- c(L, par_log['L'])
        mse_log <- c(mse_log, pp_mse_log)
        order <- c(order, cond)
      }
    }
    ndat <- data.frame(step, asymptote, mse_step, lambda, N0, mse_expl, x0, k, L, mse_log, order)
    write.csv(ndat, file=sprintf('data/pilot/%s_%s_MSE_ordereffects.csv',ptype, group), quote=F, row.names=F)
  }
}

getParticipantOrderEffectsLikelihoods <- function(perturb = c('ROT', 'MIR'), klevel = c(2,2,3), N = 2, conditions = c(1,2)){
  participant <- c()
  loglikelihood_step <- c()
  loglikelihood_expl <- c()
  loglikelihood_log <- c()
  for(ptype in perturb){
    data <- read.csv(sprintf('data/pilot/%s_noninstructed_MSE_ordereffects.csv', ptype))
    for(condition in conditions){
      ndat <- data[which(data$order == condition),]
      for(irow in c(1:nrow(ndat))){
        subdat <- ndat[irow,]
        MSE_step <- setNames(subdat$mse_step, 'mse_step')
        MSE_expl <- setNames(subdat$mse_expl, 'mse_expl')
        MSE_log <- setNames(subdat$mse_log, 'mse_log')
        
        MSE <- c(MSE_step, MSE_expl, MSE_log)
        #k <- rep(klevel, length(MSE))
        k <- klevel
        AICs <- AICc(MSE, k, N)
        loglikelihoods <- relativeLikelihood(AICs)
        
        pp <- sprintf('p%03d_%s_%d', irow, ptype, condition)
        
        participant <- c(participant, pp)
        loglikelihood_step <- c(loglikelihood_step, loglikelihoods['mse_step'])
        loglikelihood_expl <- c(loglikelihood_expl, loglikelihoods['mse_expl'])
        loglikelihood_log <- c(loglikelihood_log, loglikelihoods['mse_log'])
      }
    }
  }
  alldat <- data.frame(participant, loglikelihood_step, loglikelihood_expl, loglikelihood_log)
  write.csv(alldat,'data/pilot/participant_model_likelihoods.csv', row.names=FALSE)
}

#do model comparisons on group level (sums of MSEs)
getGroupOrderEffectsLikelihoods <- function(perturb = c('ROT', 'MIR'), klevel = c(2,2,3), N = 2, conditions = c(1,2)){
  group <- c()
  loglikelihood_step <- c()
  loglikelihood_expl <- c()
  loglikelihood_log <- c()
  for(ptype in perturb){
    data <- read.csv(sprintf('data/pilot/%s_noninstructed_MSE_ordereffects.csv', ptype))
    for(cond in conditions){
      ndat <- data[which(data$order == cond),]
      MSE_step <- setNames(sum(ndat$mse_step), 'mse_step')
      MSE_expl <- setNames(sum(ndat$mse_expl), 'mse_expl')
      MSE_log <- setNames(sum(ndat$mse_log), 'mse_log')
      
      MSE <- c(MSE_step, MSE_expl, MSE_log)
      #k <- rep(klevel, length(MSE))
      k <- klevel
      AICs <- AICc(MSE, k, N)
      loglikelihoods <- relativeLikelihood(AICs)
      
      grpinfo <- sprintf('%s_%d', ptype, cond)
      
      group <- c(group, grpinfo)
      loglikelihood_step <- c(loglikelihood_step, loglikelihoods['mse_step'])
      loglikelihood_expl <- c(loglikelihood_expl, loglikelihoods['mse_expl'])
      loglikelihood_log <- c(loglikelihood_log, loglikelihoods['mse_log'])
    }
  }
  alldat <- data.frame(group, loglikelihood_step, loglikelihood_expl, loglikelihood_log)
  write.csv(alldat,'data/pilot/group_model_likelihoods.csv', row.names=FALSE)
}

getPerturbOrderEffectsLikelihoods <- function(perturb = c('ROT', 'MIR'), klevel = c(2,2,3), N = 2){
  group <- c()
  loglikelihood_step <- c()
  loglikelihood_expl <- c()
  loglikelihood_log <- c()
  for(ptype in perturb){
    data <- read.csv(sprintf('data/pilot/%s_noninstructed_MSE_ordereffects.csv', ptype))
    
    MSE_step <- setNames(sum(data$mse_step), 'mse_step')
    MSE_expl <- setNames(sum(data$mse_expl), 'mse_expl')
    MSE_log <- setNames(sum(data$mse_log), 'mse_log')
    
    MSE <- c(MSE_step, MSE_expl, MSE_log)
    #k <- rep(klevel, length(MSE))
    k <- klevel
    AICs <- AICc(MSE, k, N)
    loglikelihoods <- relativeLikelihood(AICs)
    
    grpinfo <- sprintf('%s', ptype)
    
    group <- c(group, grpinfo)
    loglikelihood_step <- c(loglikelihood_step, loglikelihoods['mse_step'])
    loglikelihood_expl <- c(loglikelihood_expl, loglikelihoods['mse_expl'])
    loglikelihood_log <- c(loglikelihood_log, loglikelihoods['mse_log'])
    
  }
  alldat <- data.frame(group, loglikelihood_step, loglikelihood_expl, loglikelihood_log)
  write.csv(alldat,'data/pilot/all_model_likelihoods.csv', row.names=FALSE)
}

#Order Effects: Stats----


#Exponential decay model will be fit for each participant, generating lambda and asymptote parameters for each.
#We can then compare either lambdas or asymptotes for different conditions (e.g. order effects) using a t-test (frequentist and Bayesian)
getLambdaOrderEffectsTTest <- function(perturbation = c('ROT', 'MIR')){
  for(ptype in perturbation){
    data <- read.csv(sprintf('data/pilot/%s_noninstructed_MSE_ordereffects.csv', ptype))
    
    subdat1 <- data[which(data$order == 1),]
    subdat1 <- subdat1$lambda
    
    subdat2 <- data[which(data$order == 2),]
    subdat2 <- subdat2$lambda
    
    cat(sprintf('Frequentist t-test (perturbation: %s, condition: first vs. second): \n', ptype))
    print(t.test(subdat1, subdat2))
    cat('Effect Size - Cohen d:\n')
    print(cohensD(subdat1, subdat2, method = 'unequal'))
    cat(sprintf('Bayesian t-test (perturbation: %s, condition: first vs. second): \n', ptype))
    print(ttestBF(subdat1, subdat2))
  }
}

getAsymptoteOrderEffectsTTest <- function(perturbation = c('ROT', 'MIR')){
  for(ptype in perturbation){
    data <- read.csv(sprintf('data/pilot/%s_noninstructed_MSE_ordereffects.csv', ptype))
    
    subdat1 <- data[which(data$order == 1),]
    subdat1 <- subdat1$N0
    
    subdat2 <- data[which(data$order == 2),]
    subdat2 <- subdat2$N0
    
    cat(sprintf('Frequentist t-test (perturbation: %s, condition: first vs. second): \n', ptype))
    print(t.test(subdat1, subdat2))
    cat('Effect Size - Cohen d:\n')
    print(cohensD(subdat1, subdat2, method = 'unequal'))
    cat(sprintf('Bayesian t-test (perturbation: %s, condition: first vs. second): \n', ptype))
    print(ttestBF(subdat1, subdat2))
  }
}


# Target Location Effects: ROT----
getROTParticipantTargetLoc <- function(group, id, location) {
  
  #take learnive curve for both aligned and perturbed (rot, mir, rand) sessions
  #rotation should show percentage of compensation (not angular deviation of hand)
  #because this makes it comparable to mirror reversal where angular deviation will differ depending on location of target relative to mirror
  #measure where hand should be minus where it is: if this is spot on then percentage is 0%
  
  alignedTraining <- getParticipantTaskData(group, id, taskno = 1, task = 'aligned') #these values will change if need nocursor or localization
  
  if (id%%2 == 1){
    #mirror then rotation if odd id
    rotatedTraining <- getParticipantTaskData(group, id, taskno = 11, task = 'rotation')
  } else if (id%%2 == 0){
    #if pp id is even
    #rotation first then mirror
    rotatedTraining <- getParticipantTaskData(group, id, taskno = 5, task = 'rotation')
  }
  
  biases <- getAlignedTrainingBiases(alignedTraining, location = location) #use function to get biases
  #AT<- getReachAngles(alignedTraining, starttrial = 1, endtrial = 45) #aligned is first 45 trials
  RT<- getReachAngles(rotatedTraining, starttrial=0, endtrial=89, location = location) #rotated is 90 trials; appended to end of aligned
  
  for (biasno in c(1: dim(biases)[1])){ #from 1 to however many biases there are in data
    
    target<- biases[biasno, 'targetangle'] #get corresponding target angle
    bias<- biases[biasno, 'reachdev'] #get corresponding reachdev or bias
    
    #subtract bias from reach deviation for rotated session only
    RT$reachdev[which(RT$targetangle == target)] <- RT$reachdev[which(RT$targetangle == target)] - bias
  }
  
  #then for this study we want a measure of percentage of compensation, not angular hand deviation
  #perturbation is constant here (always 30deg), so the (reachdev/30)*100
  #note that rotation direction is counterbalanced (CCW and CW)
  # alltargetsbef <- c(67.5, 75, 82.5,
  #                    157.5, 165, 172.5,
  #                    247.5, 255, 262.5,
  #                    337.5, 345, 352.5) #should compensate for 30 degrees
  # alltargetsaft <- c(7.5, 15, 22.5,
  #                    97.5, 105, 112.5,
  #                    187.5, 195, 202.5,
  #                    277.5, 285, 292.5) #compensate 30 degrees
  
  angles <- unique(RT$targetangle)
  #RT['compensate'] <- NA
  
  for (target in angles){

      RT$reachdev[which(RT$targetangle == target)] <- ((RT$reachdev[which(RT$targetangle == target)])/30)*100
      #RT$compensate[which(RT$targetangle == target)] <- 30

      #multiply by negative 1 bec targets after axis will have negative values

      #RT$compensate[which(RT$targetangle == target)] <- 30

  }
  
  # for (target in angles){
  #   if (target %in% alltargetsbef){
  #     RT$reachdev[which(RT$targetangle == target)] <- ((RT$reachdev[which(RT$targetangle == target)])/30)*100
  #     #RT$compensate[which(RT$targetangle == target)] <- 30
  #   } else if (target %in% alltargetsaft){
  #     #multiply by negative 1 bec targets after axis will have negative values
  #     RT$reachdev[which(RT$targetangle == target)] <- (((RT$reachdev[which(RT$targetangle == target)])*-1)/30)*100
  #     #RT$compensate[which(RT$targetangle == target)] <- 30
  #   }
  # }
  
  #RT$reachdev <- ((RT$reachdev * -1)/30)*100
  
  #use below for absolute errors:
  #so we subtract rotation size (30deg) from all reach deviations
  #RT$reachdev <- (RT$reachdev * -1) - 30 #if we want negative values
  #RT$reachdev <- RT$reachdev - 30 #if we want positive values
  return(RT)
}

getROTGroupTargetLoc <- function(group, maxppid, location) {
  #participants <- getGroupParticipants(group) #the function that gives all participant ID's for a specified group
  
  #a consequence of adding the groups late led me to fix it in the manner below
  if (group == 'noninstructed'){
    participants <- seq(0,maxppid,1)
  } else if (group == 'instructed'){
    participants <- seq(16,maxppid,1)
  }
  
  
  dataoutput<- data.frame() #create place holder
  #go through each participant in this group
  for (participant in participants) {
    ppangles <- getROTParticipantTargetLoc(group = group, id=participant, location = location) #for every participant, get learning curve data
    
    reaches <- ppangles$reachdev #get reach deviations column from learning curve data
    trial <- c(1:length(reaches)) #sets up trial column
    dat <- cbind(trial, reaches)
    #rdat <- dat$reaches
    
    if (prod(dim(dataoutput)) == 0){
      dataoutput <- dat
    } else {
      dataoutput <- cbind(dataoutput, reaches)
    }
    
  }
  return(dataoutput)
}

getROTTargetEffects <- function(group, maxppid, location,condition){
  
  #each column will be one participant and their 90 trials
  dat <- getROTGroupTargetLoc(group=group, maxppid=maxppid,location=location)
  dat <- dat[,-1] #remove trial column to index properly
  
  #0,1 get rot after axis, 2,3 get rot before axis and so on
  #but participant count starts at 0, whereas dat already starts at one
  #so condition 1 will be those falling after axis, condition 2 will be before
  pp_ROTaft<- dat[,c(1,2,5,6,9,10,13,14)] #targets fall after axis
  pp_ROTbef <- dat[,c(3,4,7,8,11,12,15,16)] #targets fall before axis
  #then we can add trial column back to each of the separated data
  trial <- seq(1,90,1)
  pp_ROTaft <- cbind(trial, pp_ROTaft)
  pp_ROTbef <- cbind(trial, pp_ROTbef)
  
  #return whichever data is needed
  if (condition == 1){
    return(pp_ROTaft)
  } else if (condition == 2){
    return(pp_ROTbef)
  }
  
}

getROTTargetEffectsConfidenceInterval <- function(group, maxppid, location, condition, type){
  
  if (condition == 1){
    data <- getROTTargetEffects(group = group, maxppid = maxppid, location = location, condition = condition)
    #data <- data[,-6] #remove faulty particiapnt (pp004) so the 6th column REMOVE ONCE RESOLVED
    data <- as.data.frame(data)
    trialno <- data$trial
    data1 <- as.matrix(data[,2:dim(data)[2]])
    
    confidence <- data.frame()
    
    
    for (trial in trialno){
      cireaches <- data1[which(data$trial == trial), ]
      
      if (type == "t"){
        cireaches <- cireaches[!is.na(cireaches)]
        citrial <- t.interval(data = cireaches, variance = var(cireaches), conf.level = 0.95)
      } else if(type == "b"){
        citrial <- getBSConfidenceInterval(data = cireaches, resamples = 1000)
      }
      
      if (prod(dim(confidence)) == 0){
        confidence <- citrial
      } else {
        confidence <- rbind(confidence, citrial)
      }
      if (group == 'noninstructed'){
        write.csv(confidence, file='data/pilot/ROT_noninstructed_CI_targeteffects_1.csv', row.names = F) 
      } else if (group == 'instructed'){
        write.csv(confidence, file='data/pilot/ROT_instructed_CI_targeteffects_1.csv', row.names = F)
      }
      
    }
  }else if (condition == 2){
    data <- getROTTargetEffects(group = group, maxppid = maxppid, location = location, condition = condition)
    #data <- data[,-6] #remove faulty particiapnt (pp004) so the 6th column REMOVE ONCE RESOLVED
    data <- as.data.frame(data)
    trialno <- data$trial
    data1 <- as.matrix(data[,2:dim(data)[2]])
    
    confidence <- data.frame()
    
    
    for (trial in trialno){
      cireaches <- data1[which(data$trial == trial), ]
      
      if (type == "t"){
        cireaches <- cireaches[!is.na(cireaches)]
        citrial <- t.interval(data = cireaches, variance = var(cireaches), conf.level = 0.95)
      } else if(type == "b"){
        citrial <- getBSConfidenceInterval(data = cireaches, resamples = 1000)
      }
      
      if (prod(dim(confidence)) == 0){
        confidence <- citrial
      } else {
        confidence <- rbind(confidence, citrial)
      }
      if (group == 'noninstructed'){
        write.csv(confidence, file='data/pilot/ROT_noninstructed_CI_targeteffects_2.csv', row.names = F) 
      } else if (group == 'instructed'){
        write.csv(confidence, file='data/pilot/ROT_instructed_CI_targeteffects_2.csv', row.names = F)
      }
      
    }
  }
  
}

plotNIROTTargetEffects <- function(group = 'noninstructed', conditions = c(1,2), target = 'inline') {
  
  
  #but we can save plot as svg file
  if (target=='svg') {
    svglite(file='doc/fig/pilot/Fig5_ROT_NI_targeteffects.svg', width=12, height=7, pointsize=14, system_fonts=list(sans="Arial"))
  }
  
  # create plot
  meanGroupReaches <- list() #empty list so that it plots the means last
  
  #NA to create empty plot
  # could maybe use plot.new() ?
  plot(NA, NA, xlim = c(0,91), ylim = c(-200,200), 
       xlab = "Trial", ylab = "Amount of Compensation (%)", frame.plot = FALSE, #frame.plot takes away borders
       main = "Target Effects: ROT", xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
  abline(h = c(-100,0, 100), col = 8, lty = 2) #creates horizontal dashed lines through y =  0 and 30
  axis(1, at = c(1, 30, 60, 90)) #tick marks for x axis
  axis(2, at = c(-200, -100, 0, 100, 200)) #tick marks for y axis
  
  for(condition in conditions){
    #read in files created by getGroupConfidenceInterval in filehandling.R
    groupconfidence <- read.csv(file=sprintf('data/pilot/ROT_%s_CI_targeteffects_%d.csv', group, condition))
    
    colourscheme <- getCtypeColourScheme(conditions = condition)
    #take only first, last and middle columns of file
    lower <- groupconfidence[,1]
    upper <- groupconfidence[,3]
    mid <- groupconfidence[,2]
    
    col <- colourscheme[[condition]][['T']] #use colour scheme according to group
    
    #upper and lower bounds create a polygon
    #polygon creates it from low left to low right, then up right to up left -> use rev
    #x is just trial nnumber, y depends on values of bounds
    polygon(x = c(c(1:90), rev(c(1:90))), y = c(lower, rev(upper)), border=NA, col=col)
    
    meanGroupReaches[[condition]] <- mid #use mean to fill in empty list for each group
  }
  
  
  for (condition in conditions) {
    # plot mean reaches for each group
    col <- colourscheme[[condition]][['S']]
    lines(meanGroupReaches[[condition]],col=col,lty=1)
  }
  
  #add legend
  legend(60,-100,legend=c('After Axis','Before Axis'),
         col=c(colourscheme[[1]][['S']],colourscheme[[2]][['S']]),
         lty=1,bty='n',cex=1,lwd=2)
  
  #close everything if you saved plot as svg
  if (target=='svg') {
    dev.off()
  }
  
}

plotIROTTargetEffects <- function(group = 'instructed', conditions = c(1,2), target = 'inline') {
  
  
  #but we can save plot as svg file
  if (target=='svg') {
    svglite(file='doc/fig/pilot/Fig6_ROT_I_targeteffects.svg', width=12, height=7, pointsize=14, system_fonts=list(sans="Arial"))
  }
  
  # create plot
  meanGroupReaches <- list() #empty list so that it plots the means last
  
  #NA to create empty plot
  # could maybe use plot.new() ?
  plot(NA, NA, xlim = c(0,91), ylim = c(-200,200), 
       xlab = "Trial", ylab = "Amount of Compensation (%)", frame.plot = FALSE, #frame.plot takes away borders
       main = "Target Effects: ROT", xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
  abline(h = c(-100,0, 100), col = 8, lty = 2) #creates horizontal dashed lines through y =  0 and 30
  axis(1, at = c(1, 30, 60, 90)) #tick marks for x axis
  axis(2, at = c(-200, -100, 0, 100, 200)) #tick marks for y axis
  
  for(condition in conditions){
    #read in files created by getGroupConfidenceInterval in filehandling.R
    groupconfidence <- read.csv(file=sprintf('data/pilot/ROT_%s_CI_targeteffects_%d.csv', group, condition))
    
    colourscheme <- getCtypeColourScheme(conditions = condition)
    #take only first, last and middle columns of file
    lower <- groupconfidence[,1]
    upper <- groupconfidence[,3]
    mid <- groupconfidence[,2]
    
    col <- colourscheme[[condition]][['T']] #use colour scheme according to group
    
    #upper and lower bounds create a polygon
    #polygon creates it from low left to low right, then up right to up left -> use rev
    #x is just trial nnumber, y depends on values of bounds
    polygon(x = c(c(1:90), rev(c(1:90))), y = c(lower, rev(upper)), border=NA, col=col)
    
    meanGroupReaches[[condition]] <- mid #use mean to fill in empty list for each group
  }
  
  
  for (condition in conditions) {
    # plot mean reaches for each group
    col <- colourscheme[[condition]][['S']]
    lines(meanGroupReaches[[condition]],col=col,lty=1)
  }
  
  #add legend
  legend(60,-100,legend=c('After Axis','Before Axis'),
         col=c(colourscheme[[1]][['S']],colourscheme[[2]][['S']]),
         lty=1,bty='n',cex=1,lwd=2)
  
  #close everything if you saved plot as svg
  if (target=='svg') {
    dev.off()
  }
  
}

#Target Location Effects: MIR----
getMIRParticipantTargetLoc <- function(group, id, location){
  #same as rotation, we look into percentage of compensation, but note that magnitude to compensate differs per target
  alignedTraining <- getParticipantTaskData(group, id, taskno = 1, task = 'aligned') #these values will change if need nocursor or localization
  
  if (id%%2 == 1){
    #mirror then rotation if odd id
    rotatedTraining <- getParticipantTaskData(group, id, taskno = 5, task = 'mirror')
  } else if (id%%2 == 0){
    #if pp id is even
    #rotation first then mirror
    rotatedTraining <- getParticipantTaskData(group, id, taskno = 11, task = 'mirror')
  }
  
  biases <- getAlignedTrainingBiases(alignedTraining, location = location) #use function to get biases
  #AT<- getReachAngles(alignedTraining, starttrial = 1, endtrial = 45) #aligned is first 45 trials
  RT<- getReachAngles(rotatedTraining, starttrial=0, endtrial=89, location = location) #rotated is 90 trials; appended to end of aligned
  
  for (biasno in c(1: dim(biases)[1])){ #from 1 to however many biases there are in data
    
    target<- biases[biasno, 'targetangle'] #get corresponding target angle
    bias<- biases[biasno, 'reachdev'] #get corresponding reachdev or bias
    
    #subtract bias from reach deviation for rotated session only
    RT$reachdev[which(RT$targetangle == target)] <- RT$reachdev[which(RT$targetangle == target)] - bias
    
  }
  #after baseline correction, we need to assign specific targets to corresponding magnitudes to compensate
  #we have 24 possible targets, but they differ depending on which side of mirror axis they are (before or after mirror)
  #this will affect calculations later on (due to negative values)
  #so we separate them by amount of compensation, and whether they are before or after mirror axis
  alltargets15bef <- c(82.5, 172.5, 262.5, 352.5) #should compensate for 15 degrees
  alltargets15aft <- c(7.5, 97.5, 187.5, 277.5)
  alltargets30bef <- c(75, 165, 255, 345) #30 degrees
  alltargets30aft <- c(15, 105, 195, 285)
  alltargets45bef <- c(67.5, 157.5, 247.5, 337.5) #45 degrees
  alltargets45aft <- c(22.5, 112.5, 202.5, 292.5)
  
  angles <- unique(RT$targetangle)
  RT['compensate'] <- NA
  
  #we want percentage of compensation
  #we multily by -1 so that getting positive values mean that the hand went to the correct direction
  #above 100 values would mean an overcompensation, 0 is going directly to target, negative values are undercompensation
  
  #since we are testing for target effects, we can disregard multiplying by minus 1
  for (target in angles){
    if (target %in% alltargets15bef){
      RT$reachdev[which(RT$targetangle == target)] <- ((RT$reachdev[which(RT$targetangle == target)])/15)*100
      RT$compensate[which(RT$targetangle == target)] <- 15
    } else if (target %in% alltargets15aft){
      RT$reachdev[which(RT$targetangle == target)] <- ((RT$reachdev[which(RT$targetangle == target)])/15)*100
      RT$compensate[which(RT$targetangle == target)] <- 15
    } else if (target %in% alltargets30bef){
      RT$reachdev[which(RT$targetangle == target)] <- ((RT$reachdev[which(RT$targetangle == target)])/30)*100
      RT$compensate[which(RT$targetangle == target)] <- 30
    } else if (target %in% alltargets30aft){
      RT$reachdev[which(RT$targetangle == target)] <- ((RT$reachdev[which(RT$targetangle == target)])/30)*100
      RT$compensate[which(RT$targetangle == target)] <- 30
    } else if (target %in% alltargets45bef){
      RT$reachdev[which(RT$targetangle == target)] <- ((RT$reachdev[which(RT$targetangle == target)])/45)*100
      RT$compensate[which(RT$targetangle == target)] <- 45
    } else if (target %in% alltargets45aft){
      RT$reachdev[which(RT$targetangle == target)] <- ((RT$reachdev[which(RT$targetangle == target)])/45)*100
      RT$compensate[which(RT$targetangle == target)] <- 45
    }
  }
  #write.csv(RT, file='data/PPLCmir.csv', row.names = F)
  return(RT)  
}

getMIRGroupTargetLoc<- function(group, maxppid, location) { # add angle?
  #participants <- getGroupParticipants(group) #the function that gives all participant ID's for a specified group
  
  #a consequence of adding the groups late led me to fix it in the manner below
  if (group == 'noninstructed'){
    participants <- seq(0,maxppid,1)
  } else if (group == 'instructed'){
    participants <- seq(16,maxppid,1)
  }
  
  dataoutput<- data.frame() #create place holder
  #go through each participant in this group
  for (participant in participants) {
    ppangles <- getMIRParticipantTargetLoc(group = group, id=participant, location = location) #for every participant, get learning curve data
    
    reaches <- ppangles$reachdev #get reach deviations column from learning curve data
    trial <- c(1:length(reaches)) #sets up trial column
    dat <- cbind(trial, reaches)
    #rdat <- dat$reaches
    
    if (prod(dim(dataoutput)) == 0){
      dataoutput <- dat
    } else {
      dataoutput <- cbind(dataoutput, reaches)
    }
    
  }
  return(dataoutput)
}

getMIRTargetEffects <- function(group, maxppid, location,condition){
  
  #each column will be one participant and their 90 trials
  dat <- getMIRGroupTargetLoc(group=group, maxppid=maxppid,location=location)
  dat <- dat[,-1] #remove trial column to index properly
  
  #0,1 get mir before axis, 2,3 get mir after axis and so on
  #but participant count starts at 0, whereas dat already starts at one
  #so condition 1 will be those falling before axis, condition 2 will be after
  pp_MIRaft <- dat[,c(3,4,7,8,11,12,15,16)] #targets fall before axis
  pp_MIRbef<- dat[,c(1,2,5,6,9,10,13,14)] #targets fall after axis
  #then we can add trial column back to each of the separated data
  trial <- seq(1,90,1)
  pp_MIRaft <- cbind(trial, pp_MIRaft)
  pp_MIRbef <- cbind(trial, pp_MIRbef)
  
  #return whichever data is needed
  if (condition == 1){
    return(pp_MIRaft)
  } else if (condition == 2){
    return(pp_MIRbef)
  }
  
}

getMIRTargetEffectsConfidenceInterval <- function(group, maxppid, location, condition, type){
  
  if (condition == 1){
    data <- getMIRTargetEffects(group = group, maxppid = maxppid, location = location, condition = condition)
    #data <- data[,-6] #remove faulty particiapnt (pp004) so the 6th column REMOVE ONCE RESOLVED
    data <- as.data.frame(data)
    trialno <- data$trial
    data1 <- as.matrix(data[,2:dim(data)[2]])
    
    confidence <- data.frame()
    
    
    for (trial in trialno){
      cireaches <- data1[which(data$trial == trial), ]
      
      if (type == "t"){
        cireaches <- cireaches[!is.na(cireaches)]
        citrial <- t.interval(data = cireaches, variance = var(cireaches), conf.level = 0.95)
      } else if(type == "b"){
        citrial <- getBSConfidenceInterval(data = cireaches, resamples = 1000)
      }
      
      if (prod(dim(confidence)) == 0){
        confidence <- citrial
      } else {
        confidence <- rbind(confidence, citrial)
      }
      if (group == 'noninstructed'){
        write.csv(confidence, file='data/pilot/MIR_noninstructed_CI_targeteffects_1.csv', row.names = F) 
      } else if (group == 'instructed'){
        write.csv(confidence, file='data/pilot/MIR_instructed_CI_targeteffects_1.csv', row.names = F)
      }
      
    }
  }else if (condition == 2){
    data <- getMIRTargetEffects(group = group, maxppid = maxppid, location = location, condition = condition)
    #data <- data[,-6] #remove faulty particiapnt (pp004) so the 6th column REMOVE ONCE RESOLVED
    data <- as.data.frame(data)
    trialno <- data$trial
    data1 <- as.matrix(data[,2:dim(data)[2]])
    
    confidence <- data.frame()
    
    
    for (trial in trialno){
      cireaches <- data1[which(data$trial == trial), ]
      
      if (type == "t"){
        cireaches <- cireaches[!is.na(cireaches)]
        citrial <- t.interval(data = cireaches, variance = var(cireaches), conf.level = 0.95)
      } else if(type == "b"){
        citrial <- getBSConfidenceInterval(data = cireaches, resamples = 1000)
      }
      
      if (prod(dim(confidence)) == 0){
        confidence <- citrial
      } else {
        confidence <- rbind(confidence, citrial)
      }
      if (group == 'noninstructed'){
        write.csv(confidence, file='data/pilot/MIR_noninstructed_CI_targeteffects_2.csv', row.names = F) 
      } else if (group == 'instructed'){
        write.csv(confidence, file='data/pilot/MIR_instructed_CI_targeteffects_2.csv', row.names = F)
      }
      
    }
  }
  
}

plotNIMIRTargetEffects <- function(group = 'noninstructed', conditions = c(1,2), target = 'inline') {
  
  
  #but we can save plot as svg file
  if (target=='svg') {
    svglite(file='doc/fig/pilot/Fig7_MIR_NI_targeteffects.svg', width=12, height=7, pointsize=14, system_fonts=list(sans="Arial"))
  }
  
  # create plot
  meanGroupReaches <- list() #empty list so that it plots the means last
  
  #NA to create empty plot
  # could maybe use plot.new() ?
  plot(NA, NA, xlim = c(0,91), ylim = c(-200,200), 
       xlab = "Trial", ylab = "Amount of Compensation (%)", frame.plot = FALSE, #frame.plot takes away borders
       main = "Target Effects: MIR", xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
  abline(h = c(-100,0, 100), col = 8, lty = 2) #creates horizontal dashed lines through y =  0 and 30
  axis(1, at = c(1, 30, 60, 90)) #tick marks for x axis
  axis(2, at = c(-200, -100, 0, 100, 200)) #tick marks for y axis
  
  for(condition in conditions){
    #read in files created by getGroupConfidenceInterval in filehandling.R
    groupconfidence <- read.csv(file=sprintf('data/pilot/MIR_%s_CI_targeteffects_%d.csv', group, condition))
    
    colourscheme <- getCtypeColourScheme(conditions = condition)
    #take only first, last and middle columns of file
    lower <- groupconfidence[,1]
    upper <- groupconfidence[,3]
    mid <- groupconfidence[,2]
    
    col <- colourscheme[[condition]][['T']] #use colour scheme according to group
    
    #upper and lower bounds create a polygon
    #polygon creates it from low left to low right, then up right to up left -> use rev
    #x is just trial nnumber, y depends on values of bounds
    polygon(x = c(c(1:90), rev(c(1:90))), y = c(lower, rev(upper)), border=NA, col=col)
    
    meanGroupReaches[[condition]] <- mid #use mean to fill in empty list for each group
  }
  
  
  for (condition in conditions) {
    # plot mean reaches for each group
    col <- colourscheme[[condition]][['S']]
    lines(meanGroupReaches[[condition]],col=col,lty=1)
  }
  
  #add legend
  legend(60,-100,legend=c('After Axis','Before Axis'),
         col=c(colourscheme[[1]][['S']],colourscheme[[2]][['S']]),
         lty=1,bty='n',cex=1,lwd=2)
  
  #close everything if you saved plot as svg
  if (target=='svg') {
    dev.off()
  }
  
}

plotIMIRTargetEffects <- function(group = 'instructed', conditions = c(1,2), target = 'inline') {
  
  
  #but we can save plot as svg file
  if (target=='svg') {
    svglite(file='doc/fig/pilot/Fig8_MIR_I_targeteffects.svg', width=12, height=7, pointsize=14, system_fonts=list(sans="Arial"))
  }
  
  # create plot
  meanGroupReaches <- list() #empty list so that it plots the means last
  
  #NA to create empty plot
  # could maybe use plot.new() ?
  plot(NA, NA, xlim = c(0,91), ylim = c(-200,200), 
       xlab = "Trial", ylab = "Amount of Compensation (%)", frame.plot = FALSE, #frame.plot takes away borders
       main = "Target Effects: MIR", xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
  abline(h = c(-100,0, 100), col = 8, lty = 2) #creates horizontal dashed lines through y =  0 and 30
  axis(1, at = c(1, 30, 60, 90)) #tick marks for x axis
  axis(2, at = c(-200, -100, 0, 100, 200)) #tick marks for y axis
  
  for(condition in conditions){
    #read in files created by getGroupConfidenceInterval in filehandling.R
    groupconfidence <- read.csv(file=sprintf('data/pilot/MIR_%s_CI_targeteffects_%d.csv', group, condition))
    
    colourscheme <- getCtypeColourScheme(conditions = condition)
    #take only first, last and middle columns of file
    lower <- groupconfidence[,1]
    upper <- groupconfidence[,3]
    mid <- groupconfidence[,2]
    
    col <- colourscheme[[condition]][['T']] #use colour scheme according to group
    
    #upper and lower bounds create a polygon
    #polygon creates it from low left to low right, then up right to up left -> use rev
    #x is just trial nnumber, y depends on values of bounds
    polygon(x = c(c(1:90), rev(c(1:90))), y = c(lower, rev(upper)), border=NA, col=col)
    
    meanGroupReaches[[condition]] <- mid #use mean to fill in empty list for each group
  }
  
  
  for (condition in conditions) {
    # plot mean reaches for each group
    col <- colourscheme[[condition]][['S']]
    lines(meanGroupReaches[[condition]],col=col,lty=1)
  }
  
  #add legend
  legend(60,-100,legend=c('After Axis','Before Axis'),
         col=c(colourscheme[[1]][['S']],colourscheme[[2]][['S']]),
         lty=1,bty='n',cex=1,lwd=2)
  
  #close everything if you saved plot as svg
  if (target=='svg') {
    dev.off()
  }
  
}

#Target Effect Stats----
getTargetEffectsMSE <- function(perturb = c('ROT', 'MIR'), group = 'noninstructed', maxppid = 15, location = 'maxvel', conditions = c(1,2)){
  for(ptype in perturb){
    step <- c()
    asymptote <- c()
    mse_step <- c()
    lambda <- c()
    N0 <- c()
    mse_expl <- c()
    x0 <- c()
    k <- c()
    L <- c()
    mse_log <- c()
    order <- c()
    for(condition in conditions){
      if(ptype == 'ROT'){
        data <- getROTTargetEffects(group = group, maxppid = maxppid, location = location, condition = condition)
      } else if (ptype == 'MIR'){
        data <- getMIRTargetEffects(group = group, maxppid = maxppid, location = location, condition = condition)
      }
      
      subdat <- data[,2:ncol(data)]
      if(condition == 1){
        subdat <- subdat*-1 #signflip to make curves go in the same direction, but only if targets are after axis
      }
      for(icol in c(1:ncol(subdat))){
        ppdat <- subdat[,icol]
        par_step <- stepFunctionFit(signal = ppdat)
        pp_mse_step <- stepFunctionMSE(par=par_step, signal=ppdat)
        
        par_expl <- exponentialFit(signal = ppdat)
        pp_mse_expl<- exponentialMSE(par=par_expl, signal=ppdat)
        
        y <- ppdat
        x <- seq(1, length(y), 1)
        bs_dat <- data.frame(x,y)
        par_log <- logisticFunctionFit(data = bs_dat)
        pp_mse_log<- logisticFunctionMSE(par=par_log, data=bs_dat)
        
        cond <- condition
        
        step <- c(step, par_step['step'])
        asymptote <- c(asymptote, par_step['asymptote'])
        mse_step <- c(mse_step, pp_mse_step)
        lambda <- c(lambda, par_expl['lambda'])
        N0 <- c(N0, par_expl['N0'])
        mse_expl <- c(mse_expl, pp_mse_expl)
        x0 <- c(x0, par_log['x0'])
        k <- c(k, par_log['k'])
        L <- c(L, par_log['L'])
        mse_log <- c(mse_log, pp_mse_log)
        order <- c(order, cond)
      }
    }
    ndat <- data.frame(step, asymptote, mse_step, lambda, N0, mse_expl, x0, k, L, mse_log, order)
    write.csv(ndat, file=sprintf('data/pilot/%s_%s_MSE_targeteffects.csv',ptype, group), quote=F, row.names=F)
  }
}

getGroupTargetEffectsLikelihoods <- function(perturb = c('ROT', 'MIR'), klevel = c(2,2,3), N = 2, conditions = c(1,2)){
  group <- c()
  loglikelihood_step <- c()
  loglikelihood_expl <- c()
  loglikelihood_log <- c()
  for(ptype in perturb){
    data <- read.csv(sprintf('data/pilot/%s_noninstructed_MSE_targeteffects.csv', ptype))
    for(cond in conditions){
      ndat <- data[which(data$order == cond),]
      MSE_step <- setNames(sum(ndat$mse_step), 'mse_step')
      MSE_expl <- setNames(sum(ndat$mse_expl), 'mse_expl')
      MSE_log <- setNames(sum(ndat$mse_log), 'mse_log')
      
      MSE <- c(MSE_step, MSE_expl, MSE_log)
      #k <- rep(klevel, length(MSE))
      k <- klevel
      AICs <- AICc(MSE, k, N)
      loglikelihoods <- relativeLikelihood(AICs)
      
      grpinfo <- sprintf('%s_%d', ptype, cond)
      
      group <- c(group, grpinfo)
      loglikelihood_step <- c(loglikelihood_step, loglikelihoods['mse_step'])
      loglikelihood_expl <- c(loglikelihood_expl, loglikelihoods['mse_expl'])
      loglikelihood_log <- c(loglikelihood_log, loglikelihoods['mse_log'])
    }
  }
  alldat <- data.frame(group, loglikelihood_step, loglikelihood_expl, loglikelihood_log)
  return(alldat)
}

getLambdaTargetEffectsTTest <- function(perturbation = c('ROT', 'MIR')){
  for(ptype in perturbation){
    data <- read.csv(sprintf('data/pilot/%s_noninstructed_MSE_targeteffects.csv', ptype))
    
    subdat1 <- data[which(data$order == 1),]
    subdat1 <- subdat1$lambda
    
    subdat2 <- data[which(data$order == 2),]
    subdat2 <- subdat2$lambda
    
    cat(sprintf('Frequentist t-test (perturbation: %s, condition: after vs. before): \n', ptype))
    print(t.test(subdat1, subdat2))
    cat('Effect Size - Cohen d:\n')
    print(cohensD(subdat1, subdat2, method = 'unequal'))
    cat(sprintf('Bayesian t-test (perturbation: %s, condition: after vs. before): \n', ptype))
    print(ttestBF(subdat1, subdat2))
  }
}

getAsymptoteTargetEffectsTTest <- function(perturbation = c('ROT', 'MIR')){
  for(ptype in perturbation){
    data <- read.csv(sprintf('data/pilot/%s_noninstructed_MSE_targeteffects.csv', ptype))
    
    subdat1 <- data[which(data$order == 1),]
    subdat1 <- subdat1$N0
    
    subdat2 <- data[which(data$order == 2),]
    subdat2 <- subdat2$N0
    
    cat(sprintf('Frequentist t-test (perturbation: %s, condition: after vs. before): \n', ptype))
    print(t.test(subdat1, subdat2))
    cat('Effect Size - Cohen d:\n')
    print(cohensD(subdat1, subdat2, method = 'unequal'))
    cat(sprintf('Bayesian t-test (perturbation: %s, condition: after vs. before): \n', ptype))
    print(ttestBF(subdat1, subdat2))
  }
}

#Axis Effects: ROT----

getROTAxisEffects <- function(group, maxppid, location,condition){
  
  #each column will be one participant and their 90 trials
  dat <- getROTGroupLearningCurves(group=group, maxppid=maxppid,location=location)
  dat <- dat[,-1] #remove trial column to index properly
  
  #0,1,2,3 gets ROT on HOR axis, 4,5,6,7 gets ROT on VER axis
  #but participant count starts at 0, whereas dat already starts at one
  #first condition would be horizontal, second would be vertical
  pp_ROThor <- dat[,c(1,2,3,4,9,10,11,12)]
  pp_ROTver <- dat[,c(5,6,7,8,13,14,15,16)]
  #then we can add trial column back to each of the separated data
  trial <- seq(1,90,1)
  pp_ROThor <- cbind(trial, pp_ROThor)
  pp_ROTver <- cbind(trial, pp_ROTver)
  
  #return whichever data is needed
  if (condition == 1){
    return(pp_ROThor)
  } else if (condition == 2){
    return(pp_ROTver)
  }
  
}

getROTAxisEffectsConfidenceInterval <- function(group, maxppid, location, condition, type){
  
  if (condition == 1){
    data <- getROTAxisEffects(group = group, maxppid = maxppid, location = location, condition = condition)
    #data <- data[,-6] #remove faulty particiapnt (pp004) so the 6th column REMOVE ONCE RESOLVED
    data <- as.data.frame(data)
    trialno <- data$trial
    data1 <- as.matrix(data[,2:dim(data)[2]])
    
    confidence <- data.frame()
    
    
    for (trial in trialno){
      cireaches <- data1[which(data$trial == trial), ]
      
      if (type == "t"){
        cireaches <- cireaches[!is.na(cireaches)]
        citrial <- t.interval(data = cireaches, variance = var(cireaches), conf.level = 0.95)
      } else if(type == "b"){
        citrial <- getBSConfidenceInterval(data = cireaches, resamples = 1000)
      }
      
      if (prod(dim(confidence)) == 0){
        confidence <- citrial
      } else {
        confidence <- rbind(confidence, citrial)
      }
      if (group == 'noninstructed'){
        write.csv(confidence, file='data/pilot/ROT_noninstructed_CI_axiseffects_1.csv', row.names = F) 
      } else if (group == 'instructed'){
        write.csv(confidence, file='data/pilot/ROT_instructed_CI_axiseffects_1.csv', row.names = F)
      }
      
    }
  }else if (condition == 2){
    data <- getROTAxisEffects(group = group, maxppid = maxppid, location = location, condition = condition)
    #data <- data[,-6] #remove faulty particiapnt (pp004) so the 6th column REMOVE ONCE RESOLVED
    data <- as.data.frame(data)
    trialno <- data$trial
    data1 <- as.matrix(data[,2:dim(data)[2]])
    
    confidence <- data.frame()
    
    
    for (trial in trialno){
      cireaches <- data1[which(data$trial == trial), ]
      
      if (type == "t"){
        cireaches <- cireaches[!is.na(cireaches)]
        citrial <- t.interval(data = cireaches, variance = var(cireaches), conf.level = 0.95)
      } else if(type == "b"){
        citrial <- getBSConfidenceInterval(data = cireaches, resamples = 1000)
      }
      
      if (prod(dim(confidence)) == 0){
        confidence <- citrial
      } else {
        confidence <- rbind(confidence, citrial)
      }
      if (group == 'noninstructed'){
        write.csv(confidence, file='data/pilot/ROT_noninstructed_CI_axiseffects_2.csv', row.names = F) 
      } else if (group == 'instructed'){
        write.csv(confidence, file='data/pilot/ROT_instructed_CI_axiseffects_2.csv', row.names = F)
      }
      
    }
  }
  
}

#for simplicity, I will make 2 functions that will generate order effects plots for non instructed and instructed groups separately
plotNIROTAxisEffects <- function(group = 'noninstructed', conditions = c(1,2), target = 'inline') {
  
  
  #but we can save plot as svg file
  if (target=='svg') {
    svglite(file='doc/fig/pilot/Fig9_ROT_NI_axiseffects.svg', width=12, height=7, pointsize=14, system_fonts=list(sans="Arial"))
  }
  
  # create plot
  meanGroupReaches <- list() #empty list so that it plots the means last
  
  #NA to create empty plot
  # could maybe use plot.new() ?
  plot(NA, NA, xlim = c(0,91), ylim = c(-200,200), 
       xlab = "Trial", ylab = "Amount of Compensation (%)", frame.plot = FALSE, #frame.plot takes away borders
       main = "Axis Effects: ROT", xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
  abline(h = c(-100,0, 100), col = 8, lty = 2) #creates horizontal dashed lines through y =  0 and 30
  axis(1, at = c(1, 30, 60, 90)) #tick marks for x axis
  axis(2, at = c(-200, -100, 0, 100, 200)) #tick marks for y axis
  
  for(condition in conditions){
    #read in files created by getGroupConfidenceInterval in filehandling.R
    groupconfidence <- read.csv(file=sprintf('data/pilot/ROT_%s_CI_axiseffects_%d.csv', group, condition))
    
    colourscheme <- getCtypeColourScheme(conditions = condition)
    #take only first, last and middle columns of file
    lower <- groupconfidence[,1]
    upper <- groupconfidence[,3]
    mid <- groupconfidence[,2]
    
    col <- colourscheme[[condition]][['T']] #use colour scheme according to group
    
    #upper and lower bounds create a polygon
    #polygon creates it from low left to low right, then up right to up left -> use rev
    #x is just trial nnumber, y depends on values of bounds
    polygon(x = c(c(1:90), rev(c(1:90))), y = c(lower, rev(upper)), border=NA, col=col)
    
    meanGroupReaches[[condition]] <- mid #use mean to fill in empty list for each group
  }
  
  
  for (condition in conditions) {
    # plot mean reaches for each group
    col <- colourscheme[[condition]][['S']]
    lines(meanGroupReaches[[condition]],col=col,lty=1)
  }
  
  #add legend
  legend(60,-100,legend=c('Hor_axis','Ver_axis'),
         col=c(colourscheme[[1]][['S']],colourscheme[[2]][['S']]),
         lty=1,bty='n',cex=1,lwd=2)
  
  #close everything if you saved plot as svg
  if (target=='svg') {
    dev.off()
  }
  
}

plotIROTAxisEffects <- function(group = 'instructed', conditions = c(1,2), target = 'inline') {
  
  
  #but we can save plot as svg file
  if (target=='svg') {
    svglite(file='doc/fig/pilot/Fig10_ROT_I_axiseffects.svg', width=12, height=7, pointsize=14, system_fonts=list(sans="Arial"))
  }
  
  # create plot
  meanGroupReaches <- list() #empty list so that it plots the means last
  
  #NA to create empty plot
  # could maybe use plot.new() ?
  plot(NA, NA, xlim = c(0,91), ylim = c(-200,200), 
       xlab = "Trial", ylab = "Amount of Compensation (%)", frame.plot = FALSE, #frame.plot takes away borders
       main = "Axis Effects: ROT", xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
  abline(h = c(-100,0, 100), col = 8, lty = 2) #creates horizontal dashed lines through y =  0 and 30
  axis(1, at = c(1, 30, 60, 90)) #tick marks for x axis
  axis(2, at = c(-200, -100, 0, 100, 200)) #tick marks for y axis
  
  for(condition in conditions){
    #read in files created by getGroupConfidenceInterval in filehandling.R
    groupconfidence <- read.csv(file=sprintf('data/pilot/ROT_%s_CI_axiseffects_%d.csv', group, condition))
    
    colourscheme <- getCtypeColourScheme(conditions = condition)
    #take only first, last and middle columns of file
    lower <- groupconfidence[,1]
    upper <- groupconfidence[,3]
    mid <- groupconfidence[,2]
    
    col <- colourscheme[[condition]][['T']] #use colour scheme according to group
    
    #upper and lower bounds create a polygon
    #polygon creates it from low left to low right, then up right to up left -> use rev
    #x is just trial nnumber, y depends on values of bounds
    polygon(x = c(c(1:90), rev(c(1:90))), y = c(lower, rev(upper)), border=NA, col=col)
    
    meanGroupReaches[[condition]] <- mid #use mean to fill in empty list for each group
  }
  
  
  for (condition in conditions) {
    # plot mean reaches for each group
    col <- colourscheme[[condition]][['S']]
    lines(meanGroupReaches[[condition]],col=col,lty=1)
  }
  
  #add legend
  legend(60,-100,legend=c('Hor_axis','Ver_axis'),
         col=c(colourscheme[[1]][['S']],colourscheme[[2]][['S']]),
         lty=1,bty='n',cex=1,lwd=2)
  
  #close everything if you saved plot as svg
  if (target=='svg') {
    dev.off()
  }
  
}

#Axis effects: MIR----

getMIRAxisEffects <- function(group, maxppid, location,condition){
  
  #each column will be one participant and their 90 trials
  dat <- getMIRGroupLearningCurves(group=group, maxppid=maxppid,location=location)
  dat <- dat[,-1] #remove trial column to index properly
  
  #0,1,2,3 gets MIR on VER axis, 4,5,6,7 gets MIR on HOR axis
  #but participant count starts at 0, whereas dat already starts at one
  #first condition would be vertical, second would be horizontal
  pp_MIRhor <- dat[,c(5,6,7,8,13,14,15,16)]
  pp_MIRver <- dat[,c(1,2,3,4,9,10,11,12)]
  #then we can add trial column back to each of the separated data
  trial <- seq(1,90,1)
  pp_MIRhor <- cbind(trial, pp_MIRhor)
  pp_MIRver <- cbind(trial, pp_MIRver)
  
  #return whichever data is needed
  if (condition == 1){
    return(pp_MIRhor)
  } else if (condition == 2){
    return(pp_MIRver)
  }
  
}

getMIRAxisEffectsConfidenceInterval <- function(group, maxppid, location, condition, type){
  
  if (condition == 1){
    data <- getMIRAxisEffects(group = group, maxppid = maxppid, location = location, condition = condition)
    #data <- data[,-6] #remove faulty particiapnt (pp004) so the 6th column REMOVE ONCE RESOLVED
    data <- as.data.frame(data)
    trialno <- data$trial
    data1 <- as.matrix(data[,2:dim(data)[2]])
    
    confidence <- data.frame()
    
    
    for (trial in trialno){
      cireaches <- data1[which(data$trial == trial), ]
      
      if (type == "t"){
        cireaches <- cireaches[!is.na(cireaches)]
        citrial <- t.interval(data = cireaches, variance = var(cireaches), conf.level = 0.95)
      } else if(type == "b"){
        citrial <- getBSConfidenceInterval(data = cireaches, resamples = 1000)
      }
      
      if (prod(dim(confidence)) == 0){
        confidence <- citrial
      } else {
        confidence <- rbind(confidence, citrial)
      }
      if (group == 'noninstructed'){
        write.csv(confidence, file='data/pilot/MIR_noninstructed_CI_axiseffects_1.csv', row.names = F) 
      } else if (group == 'instructed'){
        write.csv(confidence, file='data/pilot/MIR_instructed_CI_axiseffects_1.csv', row.names = F)
      }
      
    }
  }else if (condition == 2){
    data <- getMIRAxisEffects(group = group, maxppid = maxppid, location = location, condition = condition)
    #data <- data[,-6] #remove faulty particiapnt (pp004) so the 6th column REMOVE ONCE RESOLVED
    data <- as.data.frame(data)
    trialno <- data$trial
    data1 <- as.matrix(data[,2:dim(data)[2]])
    
    confidence <- data.frame()
    
    
    for (trial in trialno){
      cireaches <- data1[which(data$trial == trial), ]
      
      if (type == "t"){
        cireaches <- cireaches[!is.na(cireaches)]
        citrial <- t.interval(data = cireaches, variance = var(cireaches), conf.level = 0.95)
      } else if(type == "b"){
        citrial <- getBSConfidenceInterval(data = cireaches, resamples = 1000)
      }
      
      if (prod(dim(confidence)) == 0){
        confidence <- citrial
      } else {
        confidence <- rbind(confidence, citrial)
      }
      if (group == 'noninstructed'){
        write.csv(confidence, file='data/pilot/MIR_noninstructed_CI_axiseffects_2.csv', row.names = F) 
      } else if (group == 'instructed'){
        write.csv(confidence, file='data/pilot/MIR_instructed_CI_axiseffects_2.csv', row.names = F)
      }
      
    }
  }
  
}

#for simplicity, I will make 2 functions that will generate order effects plots for non instructed and instructed groups separately
plotNIMIRAxisEffects <- function(group = 'noninstructed', conditions = c(1,2), target = 'inline') {
  
  
  #but we can save plot as svg file
  if (target=='svg') {
    svglite(file='doc/fig/pilot/Fig11_MIR_NI_axiseffects.svg', width=12, height=7, pointsize=14, system_fonts=list(sans="Arial"))
  }
  
  # create plot
  meanGroupReaches <- list() #empty list so that it plots the means last
  
  #NA to create empty plot
  # could maybe use plot.new() ?
  plot(NA, NA, xlim = c(0,91), ylim = c(-200,200), 
       xlab = "Trial", ylab = "Amount of Compensation (%)", frame.plot = FALSE, #frame.plot takes away borders
       main = "Axis Effects: MIR", xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
  abline(h = c(-100,0, 100), col = 8, lty = 2) #creates horizontal dashed lines through y =  0 and 30
  axis(1, at = c(1, 30, 60, 90)) #tick marks for x axis
  axis(2, at = c(-200, -100, 0, 100, 200)) #tick marks for y axis
  
  for(condition in conditions){
    #read in files created by getGroupConfidenceInterval in filehandling.R
    groupconfidence <- read.csv(file=sprintf('data/pilot/MIR_%s_CI_axiseffects_%d.csv', group, condition))
    
    colourscheme <- getCtypeColourScheme(conditions = condition)
    #take only first, last and middle columns of file
    lower <- groupconfidence[,1]
    upper <- groupconfidence[,3]
    mid <- groupconfidence[,2]
    
    col <- colourscheme[[condition]][['T']] #use colour scheme according to group
    
    #upper and lower bounds create a polygon
    #polygon creates it from low left to low right, then up right to up left -> use rev
    #x is just trial nnumber, y depends on values of bounds
    polygon(x = c(c(1:90), rev(c(1:90))), y = c(lower, rev(upper)), border=NA, col=col)
    
    meanGroupReaches[[condition]] <- mid #use mean to fill in empty list for each group
  }
  
  
  for (condition in conditions) {
    # plot mean reaches for each group
    col <- colourscheme[[condition]][['S']]
    lines(meanGroupReaches[[condition]],col=col,lty=1)
  }
  
  #add legend
  legend(60,-100,legend=c('Hor_axis','Ver_axis'),
         col=c(colourscheme[[1]][['S']],colourscheme[[2]][['S']]),
         lty=1,bty='n',cex=1,lwd=2)
  
  #close everything if you saved plot as svg
  if (target=='svg') {
    dev.off()
  }
  
}

plotIMIRAxisEffects <- function(group = 'instructed', conditions = c(1,2), target = 'inline') {
  
  
  #but we can save plot as svg file
  if (target=='svg') {
    svglite(file='doc/fig/pilot/Fig12_MIR_I_axiseffects.svg', width=12, height=7, pointsize=14, system_fonts=list(sans="Arial"))
  }
  
  # create plot
  meanGroupReaches <- list() #empty list so that it plots the means last
  
  #NA to create empty plot
  # could maybe use plot.new() ?
  plot(NA, NA, xlim = c(0,91), ylim = c(-200,200), 
       xlab = "Trial", ylab = "Amount of Compensation (%)", frame.plot = FALSE, #frame.plot takes away borders
       main = "Axis Effects: MIR", xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
  abline(h = c(-100,0, 100), col = 8, lty = 2) #creates horizontal dashed lines through y =  0 and 30
  axis(1, at = c(1, 30, 60, 90)) #tick marks for x axis
  axis(2, at = c(-200, -100, 0, 100, 200)) #tick marks for y axis
  
  for(condition in conditions){
    #read in files created by getGroupConfidenceInterval in filehandling.R
    groupconfidence <- read.csv(file=sprintf('data/pilot/MIR_%s_CI_axiseffects_%d.csv', group, condition))
    
    colourscheme <- getCtypeColourScheme(conditions = condition)
    #take only first, last and middle columns of file
    lower <- groupconfidence[,1]
    upper <- groupconfidence[,3]
    mid <- groupconfidence[,2]
    
    col <- colourscheme[[condition]][['T']] #use colour scheme according to group
    
    #upper and lower bounds create a polygon
    #polygon creates it from low left to low right, then up right to up left -> use rev
    #x is just trial nnumber, y depends on values of bounds
    polygon(x = c(c(1:90), rev(c(1:90))), y = c(lower, rev(upper)), border=NA, col=col)
    
    meanGroupReaches[[condition]] <- mid #use mean to fill in empty list for each group
  }
  
  
  for (condition in conditions) {
    # plot mean reaches for each group
    col <- colourscheme[[condition]][['S']]
    lines(meanGroupReaches[[condition]],col=col,lty=1)
  }
  
  #add legend
  legend(60,-100,legend=c('Hor_axis','Ver_axis'),
         col=c(colourscheme[[1]][['S']],colourscheme[[2]][['S']]),
         lty=1,bty='n',cex=1,lwd=2)
  
  #close everything if you saved plot as svg
  if (target=='svg') {
    dev.off()
  }
  
}

#Axis Effects: Stats----

getAxisEffectsMSE <- function(perturb = c('ROT', 'MIR'), group = 'noninstructed', maxppid = 15, location = 'maxvel', conditions = c(1,2)){
  for(ptype in perturb){
    step <- c()
    asymptote <- c()
    mse_step <- c()
    lambda <- c()
    N0 <- c()
    mse_expl <- c()
    x0 <- c()
    k <- c()
    L <- c()
    mse_log <- c()
    order <- c()
    for(condition in conditions){
      if(ptype == 'ROT'){
        data <- getROTAxisEffects(group = group, maxppid = maxppid, location = location, condition = condition)
      } else if (ptype == 'MIR'){
        data <- getMIRAxisEffects(group = group, maxppid = maxppid, location = location, condition = condition)
      }
      
      subdat <- data[,2:ncol(data)]
      for(icol in c(1:ncol(subdat))){
        ppdat <- subdat[,icol]
        par_step <- stepFunctionFit(signal = ppdat)
        pp_mse_step <- stepFunctionMSE(par=par_step, signal=ppdat)
        
        par_expl <- exponentialFit(signal = ppdat)
        pp_mse_expl<- exponentialMSE(par=par_expl, signal=ppdat)
        
        y <- ppdat
        x <- seq(1, length(y), 1)
        bs_dat <- data.frame(x,y)
        par_log <- logisticFunctionFit(data = bs_dat)
        pp_mse_log<- logisticFunctionMSE(par=par_log, data=bs_dat)
        
        cond <- condition
        
        step <- c(step, par_step['step'])
        asymptote <- c(asymptote, par_step['asymptote'])
        mse_step <- c(mse_step, pp_mse_step)
        lambda <- c(lambda, par_expl['lambda'])
        N0 <- c(N0, par_expl['N0'])
        mse_expl <- c(mse_expl, pp_mse_expl)
        x0 <- c(x0, par_log['x0'])
        k <- c(k, par_log['k'])
        L <- c(L, par_log['L'])
        mse_log <- c(mse_log, pp_mse_log)
        order <- c(order, cond)
      }
    }
    ndat <- data.frame(step, asymptote, mse_step, lambda, N0, mse_expl, x0, k, L, mse_log, order)
    write.csv(ndat, file=sprintf('data/pilot/%s_%s_MSE_axiseffects.csv',ptype, group), quote=F, row.names=F)
  }
}

getGroupAxisEffectsLikelihoods <- function(perturb = c('ROT', 'MIR'), klevel = c(2,2,3), N = 2, conditions = c(1,2)){
  group <- c()
  loglikelihood_step <- c()
  loglikelihood_expl <- c()
  loglikelihood_log <- c()
  for(ptype in perturb){
    data <- read.csv(sprintf('data/pilot/%s_noninstructed_MSE_axiseffects.csv', ptype))
    for(cond in conditions){
      ndat <- data[which(data$order == cond),]
      MSE_step <- setNames(sum(ndat$mse_step), 'mse_step')
      MSE_expl <- setNames(sum(ndat$mse_expl), 'mse_expl')
      MSE_log <- setNames(sum(ndat$mse_log), 'mse_log')
      
      MSE <- c(MSE_step, MSE_expl, MSE_log)
      #k <- rep(klevel, length(MSE))
      k <- klevel
      AICs <- AICc(MSE, k, N)
      loglikelihoods <- relativeLikelihood(AICs)
      
      grpinfo <- sprintf('%s_%d', ptype, cond)
      
      group <- c(group, grpinfo)
      loglikelihood_step <- c(loglikelihood_step, loglikelihoods['mse_step'])
      loglikelihood_expl <- c(loglikelihood_expl, loglikelihoods['mse_expl'])
      loglikelihood_log <- c(loglikelihood_log, loglikelihoods['mse_log'])
    }
  }
  alldat <- data.frame(group, loglikelihood_step, loglikelihood_expl, loglikelihood_log)
  return(alldat)
}

getLambdaAxisEffectsTTest <- function(perturbation = c('ROT', 'MIR')){
  for(ptype in perturbation){
    data <- read.csv(sprintf('data/pilot/%s_noninstructed_MSE_axiseffects.csv', ptype))
    
    subdat1 <- data[which(data$order == 1),]
    subdat1 <- subdat1$lambda
    
    subdat2 <- data[which(data$order == 2),]
    subdat2 <- subdat2$lambda
    
    cat(sprintf('Frequentist t-test (perturbation: %s, condition: horizontal vs. vertical): \n', ptype))
    print(t.test(subdat1, subdat2))
    cat('Effect Size - Cohen d:\n')
    print(cohensD(subdat1, subdat2, method = 'unequal'))
    cat(sprintf('Bayesian t-test (perturbation: %s, condition: horizontal vs. vertical): \n', ptype))
    print(ttestBF(subdat1, subdat2))
  }
}

getAsymptoteAxisEffectsTTest <- function(perturbation = c('ROT', 'MIR')){
  for(ptype in perturbation){
    data <- read.csv(sprintf('data/pilot/%s_noninstructed_MSE_axiseffects.csv', ptype))
    
    subdat1 <- data[which(data$order == 1),]
    subdat1 <- subdat1$N0
    
    subdat2 <- data[which(data$order == 2),]
    subdat2 <- subdat2$N0
    
    cat(sprintf('Frequentist t-test (perturbation: %s, condition: horizontal vs. vertical): \n', ptype))
    print(t.test(subdat1, subdat2))
    cat('Effect Size - Cohen d:\n')
    print(cohensD(subdat1, subdat2, method = 'unequal'))
    cat(sprintf('Bayesian t-test (perturbation: %s, condition: horizontal vs. vertical): \n', ptype))
    print(ttestBF(subdat1, subdat2))
  }
}

