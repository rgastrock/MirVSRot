source('ana/shared.R')
source('ana/learningRates.R')

#Path Length
#Every sample has (x,y) coordinate.
# First sample will have distance from origin as sqrt(x^2 + y^2) - called absolute vector
# Path length is total distance (given x and y trajectories) traveled between movement onset and offset.
# One may think that simply calculating absolute vector from endpoint will measure this, but trajectories
# may curve or go in different directions. So we need to account for every sample.But we simply can't
# add absolute vectors across samples (this will include lengths accounted for by previous sample). So,
# every new sample's absolute vector will be calculated using the previous sample as its origin. Then all
# these values are added to come up with a total path length.
#Repeat this process for all trials within one participant. Then show mean measures across participants for every trial.
# Currently, measures are in cm. But can normalize this (divide path length by distance between target and home) to make it comparable
# to other experiments that have different measures.

getParticipantPathLength <- function(group, id, taskno, task){
  dat <- getParticipantTaskData(group=group, id=id, taskno=taskno, task=task)
  #get only selected trials, and the samples selected for this trial
  subdat <- dat[which(dat$trialselected_bool == 1),]
  subdat <- subdat[which(subdat$sampleselected_bool == 1),]
  trials <- unique(dat$trial) #need to be based off of dat, because some trials may not be selected - assign NA instead
  
  alldat <- data.frame()
  for (trialno in trials){
    trialdat <- subdat[which(subdat$trial == trialno),]
    trialdat <- trialdat[trialdat$step == 4,] 
    ndat <- data.frame()
    for (idx in 1:nrow(trialdat)){
      if (idx == 1){
        sampx <- trialdat$mousex_cm[idx]
        sampy <- trialdat$mousey_cm[idx]
        absvec <- sqrt(((sampx)^2)+((sampy)^2))
      } else {
        sampx <- trialdat$mousex_cm[idx] - trialdat$mousex_cm[idx-1]
        sampy <- trialdat$mousey_cm[idx] - trialdat$mousey_cm[idx-1]
        absvec <- sqrt(((sampx)^2)+((sampy)^2))
      }
      
      
      if (prod(dim(ndat)) == 0){
        ndat <- absvec
      } else {
        ndat <- rbind(ndat, absvec)
      }
      
    }
    pathlength <- sum(ndat[1:length(ndat)])
    #print(pathlength)
    #print(trialno)
    #dat <- cbind(trialno, pathlength)
    
    if (prod(dim(alldat)) == 0){
      alldat <- pathlength
    } else {
      alldat <- rbind(alldat, pathlength)
    }
  }
  
  return(alldat)
  
}
#Aligned Reaches-----
getAlignedGroupPathLength <- function(group, maxppid){
  
  if (group == 'noninstructed'){
    participants <- seq(0,maxppid,1)
  } else if (group == 'instructed'){
    participants <- seq(16,maxppid,1)
  }
  
  dataoutput<- data.frame() #create place holder
  #go through each participant in this group
  for (participant in participants) {
    pppath <- getParticipantPathLength(group=group, id=participant, taskno = 1, task = 'aligned')
    trial <- c(1:length(pppath))
    dat <- cbind(trial, pppath)
    #print(participant)
    if (prod(dim(dataoutput)) == 0){
      dataoutput <- dat
    } else {
      dataoutput <- cbind(dataoutput, pppath)
    }
  }
  return(dataoutput)
}

getAlignedPathLengthCI <- function(group, maxppid, type = 't'){
  #for (group in groups){
  # get the confidence intervals for each trial of each group
  data <- getAlignedGroupPathLength(group = group, maxppid = maxppid)
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
      write.csv(confidence, file='data/pilot/processed/ALIGNED_noninstructed_CI_pathlength.csv', row.names = F) 
    } else if (group == 'instructed'){
      write.csv(confidence, file='data/pilot/processed/ALIGNED_instructed_CI_pathlength.csv', row.names = F)
    }
    
  }
}

getPLBlockedAlignedConfInt <- function(group, maxppid, type = 't'){
  
  data <- getAlignedGroupPathLength(group = group, maxppid = maxppid)
  subdat <- data[,2:ncol(data)]
  
  #we want to get the mean for every 12 trials (they go through each of 12 possible targets in aligned)
  n <- 12;
  ndat <- aggregate(subdat, list(rep(1:(nrow(data)%/%n+1),each=n,len=nrow(data))), mean, na.rm = T)[-1];
  blockno <- c(1:nrow(ndat))
  ndat <- as.data.frame(cbind(blockno,ndat))
  
  data1 <- as.matrix(ndat[,2:dim(ndat)[2]])
  confidence <- data.frame()
  
  
  for (block in blockno){
    cireaches <- data1[which(ndat$blockno == block), ]
    
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
      write.csv(confidence, file='data/pilot/processed/ALIGNED_noninstructed_blocked_CI_PL.csv', row.names = F) 
    } else if (group == 'instructed'){
      write.csv(confidence, file='data/pilot/processed/ALIGNED_instructed_blocked_CI_PL.csv', row.names = F)
    } 
    
  }
  
}

plotAlignedPathLength <- function(groups=c('noninstructed', 'instructed'),target='inline') {
  
  for (group in groups){
    #but we can save plot as svg file
    if (target=='svg') {
      if(group == 'noninstructed'){
        svglite(file='doc/fig/pilot/Fig45_NI_ALIGNED_pathlength.svg', width=8, height=7, pointsize=16, system_fonts=list(sans="Arial"))
      } else if (group == 'instructed'){
        svglite(file='doc/fig/pilot/Fig46_I_ALIGNED_pathlength.svg', width=8, height=7, pointsize=16, system_fonts=list(sans="Arial"))
      }
      
    }
    
    # create plot
    meanGroupReaches <- list() #empty list so that it plots the means last
    
    #NA to create empty plot
    # could maybe use plot.new() ?
    plot(NA, NA, xlim = c(0,49), ylim = c(10,20), 
         xlab = "Trial", ylab = "Path Length (cm)", frame.plot = FALSE, #frame.plot takes away borders
         main = 'Path Length: Aligned', xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
    #abline(h = c(-100,0, 100), col = 8, lty = 2) #creates horizontal dashed lines through y =  0 and 30
    axis(1, at = c(1, 12, 24, 36, 48)) #tick marks for x axis
    axis(2, at = c(10, 12, 14, 16, 18)) #tick marks for y axis
    
    
    #read in files created by getGroupConfidenceInterval in filehandling.R
    groupconfidence <- read.csv(file=sprintf('data/pilot/processed/ALIGNED_%s_CI_pathlength.csv', group))
    
    colourscheme <- getColourScheme(groups = group)
    #take only first, last and middle columns of file
    lower <- groupconfidence[,1]
    upper <- groupconfidence[,3]
    mid <- groupconfidence[,2]
    
    col <- colourscheme[[group]][['T']] #use colour scheme according to group
    
    #upper and lower bounds create a polygon
    #polygon creates it from low left to low right, then up right to up left -> use rev
    #x is just trial nnumber, y depends on values of bounds
    polygon(x = c(c(1:48), rev(c(1:48))), y = c(lower, rev(upper)), border=NA, col=col)
    
    meanGroupReaches[[group]] <- mid #use mean to fill in empty list for each group
    
    
    
    
    # plot mean reaches for each group
    col <- colourscheme[[group]][['S']]
    lines(meanGroupReaches[[group]],col=col,lty=1)
    
    
    #add legend
    # # legend(70,-100,legend=c('Aligned'),
    #        col=c(colourscheme[['noninstructed']][['S']],colourscheme[['instructed']][['S']]),
    #        lty=1,bty='n',cex=1,lwd=2)
    
    #close everything if you saved plot as svg
    if (target=='svg') {
      dev.off()
    }
  }
}
#Rotated reaches-----
getROTGroupPathLength <- function(group, maxppid){
  
  if (group == 'noninstructed'){
    participants <- seq(0,maxppid,1)
  } else if (group == 'instructed'){
    participants <- seq(16,maxppid,1)
  }
  
  dataoutput<- data.frame() #create place holder
  #go through each participant in this group
  for (participant in participants) {
    
    if (participant%%2 == 1){
      #mirror then rotation if odd id
      pppath <- getParticipantPathLength(group, id=participant, taskno = 11, task = 'rotation')
    } else if (participant%%2 == 0){
      #if pp id is even
      #rotation first then mirror
      pppath <- getParticipantPathLength(group, id=participant, taskno = 5, task = 'rotation')
    }
    
    trial <- c(1:length(pppath))
    dat <- cbind(trial, pppath)
    #print(participant)
    if (prod(dim(dataoutput)) == 0){
      dataoutput <- dat
    } else {
      dataoutput <- cbind(dataoutput, pppath)
    }
  }
  return(dataoutput)
}

getROTPathLengthCI <- function(group, maxppid, type = 't'){
  #for (group in groups){
  # get the confidence intervals for each trial of each group
  data <- getROTGroupPathLength(group = group, maxppid = maxppid)
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
      write.csv(confidence, file='data/pilot/processed/ROT_noninstructed_CI_pathlength.csv', row.names = F) 
    } else if (group == 'instructed'){
      write.csv(confidence, file='data/pilot/processed/ROT_instructed_CI_pathlength.csv', row.names = F)
    }
    
  }
}

getPLBlockedROTConfInt <- function(group, maxppid, type = 't'){
  
  data <- getROTGroupPathLength(group = group, maxppid = maxppid)
  subdat <- data[,2:ncol(data)]
  
  #we want to get the mean for every 6 trials (they go through each of 6 possible targets)
  n <- 6;
  ndat <- aggregate(subdat, list(rep(1:(nrow(data)%/%n+1),each=n,len=nrow(data))), mean, na.rm = T)[-1];
  blockno <- c(1:nrow(ndat))
  ndat <- as.data.frame(cbind(blockno,ndat))
  
  data1 <- as.matrix(ndat[,2:dim(ndat)[2]])
  confidence <- data.frame()
  
  
  for (block in blockno){
    cireaches <- data1[which(ndat$blockno == block), ]
    
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
      write.csv(confidence, file='data/pilot/processed/ROT_noninstructed_blocked_CI_PL.csv', row.names = F) 
    } else if (group == 'instructed'){
      write.csv(confidence, file='data/pilot/processed/ROT_instructed_blocked_CI_PL.csv', row.names = F)
    } 
    
  }
  
}

#Mirror reaches----
getMIRGroupPathLength <- function(group, maxppid){
  
  if (group == 'noninstructed'){
    participants <- seq(0,maxppid,1)
  } else if (group == 'instructed'){
    participants <- seq(16,maxppid,1)
  }
  
  dataoutput<- data.frame() #create place holder
  #go through each participant in this group
  for (participant in participants) {
    
    if (participant%%2 == 1){
      #mirror then rotation if odd id
      pppath <- getParticipantPathLength(group, id=participant, taskno = 5, task = 'mirror')
    } else if (participant%%2 == 0){
      #if pp id is even
      #rotation first then mirror
      pppath <- getParticipantPathLength(group, id=participant, taskno = 11, task = 'mirror')
    }
    
    trial <- c(1:length(pppath))
    dat <- cbind(trial, pppath)
    #print(participant)
    if (prod(dim(dataoutput)) == 0){
      dataoutput <- dat
    } else {
      dataoutput <- cbind(dataoutput, pppath)
    }
  }
  return(dataoutput)
}

getMIRPathLengthCI <- function(group, maxppid, type = 't'){
  #for (group in groups){
  # get the confidence intervals for each trial of each group
  data <- getMIRGroupPathLength(group = group, maxppid = maxppid)
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
      write.csv(confidence, file='data/pilot/processed/MIR_noninstructed_CI_pathlength.csv', row.names = F) 
    } else if (group == 'instructed'){
      write.csv(confidence, file='data/pilot/processed/MIR_instructed_CI_pathlength.csv', row.names = F)
    }
    
  }
}

getPLBlockedMIRConfInt <- function(group, maxppid, type = 't'){
  
  data <- getMIRGroupPathLength(group = group, maxppid = maxppid)
  subdat <- data[,2:ncol(data)]
  
  #we want to get the mean for every 6 trials (they go through each of 6 possible targets)
  n <- 6;
  ndat <- aggregate(subdat, list(rep(1:(nrow(data)%/%n+1),each=n,len=nrow(data))), mean, na.rm = T)[-1];
  blockno <- c(1:nrow(ndat))
  ndat <- as.data.frame(cbind(blockno,ndat))
  
  data1 <- as.matrix(ndat[,2:dim(ndat)[2]])
  confidence <- data.frame()
  
  
  for (block in blockno){
    cireaches <- data1[which(ndat$blockno == block), ]
    
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
      write.csv(confidence, file='data/pilot/processed/MIR_noninstructed_blocked_CI_PL.csv', row.names = F) 
    } else if (group == 'instructed'){
      write.csv(confidence, file='data/pilot/processed/MIR_instructed_blocked_CI_PL.csv', row.names = F)
    } 
    
  }
  
}


#ROTWASHOUT----
getROTWASHGroupPLTrials <- function(group = 'noninstructed', maxppid = 15) {
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
    if (participant%%2 == 1){
      #mirror then rotation if odd id
      ppRT <- getParticipantPathLength(group=group, id=participant, taskno = 13, task = 'washout1')
    } else if (participant%%2 == 0){
      #if pp id is even
      #rotation first then mirror
      ppRT <- getParticipantPathLength(group=group, id=participant, taskno = 7, task = 'washout0')
    }
    
    reaction <- as.numeric(ppRT)#get RT column from RT data
    trial <- c(1:length(reaction)) #sets up trial column
    dat <- cbind(trial, reaction)
    #rdat <- dat$reaches
    
    if (prod(dim(dataoutput)) == 0){
      dataoutput <- dat
    } else {
      dataoutput <- cbind(dataoutput, reaction)
    }
    
  }
  
  return(dataoutput)
  
}

getPLBlockedRotwashConfInt <- function(group, maxppid, type = 't'){
  
  data <- getROTWASHGroupPLTrials(group = group, maxppid = maxppid)
  subdat <- data[,2:ncol(data)]
  
  #we want to get the mean for every 6 trials (they go through each of 6 possible targets)
  n <- 6;
  ndat <- aggregate(subdat, list(rep(1:(nrow(data)%/%n+1),each=n,len=nrow(data))), mean, na.rm = T)[-1];
  blockno <- c(1:nrow(ndat))
  ndat <- as.data.frame(cbind(blockno,ndat))
  
  data1 <- as.matrix(ndat[,2:dim(ndat)[2]])
  confidence <- data.frame()
  
  
  for (block in blockno){
    cireaches <- data1[which(ndat$blockno == block), ]
    
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
      write.csv(confidence, file='data/pilot/processed/ROTWASH_noninstructed_blocked_CI_PL.csv', row.names = F) 
    } else if (group == 'instructed'){
      write.csv(confidence, file='data/pilot/processed/ROTWASH_instructed_blocked_CI_PL.csv', row.names = F)
    } 
    
  }
  
}

#MIRWASHOUT----
getMIRWASHGroupPLTrials <- function(group = 'noninstructed', maxppid = 15) {
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
    if (participant%%2 == 1){
      #mirror then rotation if odd id
      ppRT <- getParticipantPathLength(group=group, id=participant, taskno = 7, task = 'washout0')
    } else if (participant%%2 == 0){
      #if pp id is even
      #rotation first then mirror
      ppRT <- getParticipantPathLength(group=group, id=participant, taskno = 13, task = 'washout1')
    }
    
    reaction <- as.numeric(ppRT)#get RT column from RT data
    trial <- c(1:length(reaction)) #sets up trial column
    dat <- cbind(trial, reaction)
    #rdat <- dat$reaches
    
    if (prod(dim(dataoutput)) == 0){
      dataoutput <- dat
    } else {
      dataoutput <- cbind(dataoutput, reaction)
    }
    
  }
  
  return(dataoutput)
  
}

getPLBlockedMirwashConfInt <- function(group, maxppid, type = 't'){
  
  data <- getROTWASHGroupPLTrials(group = group, maxppid = maxppid)
  subdat <- data[,2:ncol(data)]
  
  #we want to get the mean for every 6 trials (they go through each of 6 possible targets)
  n <- 6;
  ndat <- aggregate(subdat, list(rep(1:(nrow(data)%/%n+1),each=n,len=nrow(data))), mean, na.rm = T)[-1];
  blockno <- c(1:nrow(ndat))
  ndat <- as.data.frame(cbind(blockno,ndat))
  
  data1 <- as.matrix(ndat[,2:dim(ndat)[2]])
  confidence <- data.frame()
  
  
  for (block in blockno){
    cireaches <- data1[which(ndat$blockno == block), ]
    
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
      write.csv(confidence, file='data/pilot/processed/MIRWASH_noninstructed_blocked_CI_PL.csv', row.names = F) 
    } else if (group == 'instructed'){
      write.csv(confidence, file='data/pilot/processed/MIRWASH_instructed_blocked_CI_PL.csv', row.names = F)
    } 
    
  }
  
}

#plots for ROT and MIR----
plotPTypePathLength <- function(perturb = c('ROT', 'MIR'), group, target='inline') {
  
  
  #but we can save plot as svg file
  if (target=='svg') {
    if(group == 'noninstructed'){
      svglite(file='doc/fig/pilot/Fig47_NI_pathlength.svg', width=11.5, height=7, pointsize=16, system_fonts=list(sans="Arial"))
    } else if (group == 'instructed'){
      svglite(file='doc/fig/pilot/Fig48_I_pathlength.svg', width=11.5, height=7, pointsize=16, system_fonts=list(sans="Arial"))
    }
    
  }
  
  # create plot
  meanGroupReaches <- list() #empty list so that it plots the means last
  
  #NA to create empty plot
  # could maybe use plot.new() ?
  plot(NA, NA, xlim = c(0,91), ylim = c(10,20), 
       xlab = "Trial", ylab = "Path Length (cm)", frame.plot = FALSE, #frame.plot takes away borders
       main = "Path Length: Rot and Mir", xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
  #abline(h = c(-100,0, 100), col = '#000000', lty = 2) #creates horizontal dashed lines through y =  0 and 30
  axis(1, at = c(1, 30, 60, 90)) #tick marks for x axis
  axis(2, at = c(10, 12, 14, 16, 18)) #tick marks for y axis
  
  for(ptype in perturb){
    #read in files created by getGroupConfidenceInterval in filehandling.R
    groupconfidence <- read.csv(file=sprintf('data/pilot/processed/%s_%s_CI_pathlength.csv', ptype, group))
    
    colourscheme <- getPtypeColourScheme(ptype)
    #take only first, last and middle columns of file
    lower <- groupconfidence[,1]
    upper <- groupconfidence[,3]
    mid <- groupconfidence[,2]
    
    col <- colourscheme[[ptype]][['T']] #use colour scheme according to group
    
    #upper and lower bounds create a polygon
    #polygon creates it from low left to low right, then up right to up left -> use rev
    #x is just trial nnumber, y depends on values of bounds
    polygon(x = c(c(1:90), rev(c(1:90))), y = c(lower, rev(upper)), border=NA, col=col)
    
    meanGroupReaches[[ptype]] <- mid #use mean to fill in empty list for each group
  }
  
  
  for (ptype in perturb) {
    # plot mean reaches for each group
    col <- colourscheme[[ptype]][['S']]
    lines(meanGroupReaches[[ptype]],col=col,lty=1)
  }
  
  #add legend
  legend(50,18,legend=c('Visuomotor rotation','Mirror reversal'),
         col=c(colourscheme[['ROT']][['S']],colourscheme[['MIR']][['S']]),
         lty=1,bty='n',cex=1,lwd=2)
  
  #close everything if you saved plot as svg
  if (target=='svg') {
    dev.off()
  }
  
}

plotNIBlockedPL <- function(target='inline'){
  
  #but we can save plot as svg file
  if (target=='svg') {
    svglite(file='doc/fig/pilot/Fig49_NI_BlockedPL.svg', width=11.5, height=8.5, pointsize=16, system_fonts=list(sans="Arial"))
  }
  
  #read in the csv files and plot them in one big plot
  dat1 <- read.csv(file='data/pilot/processed/ALIGNED_noninstructed_blocked_CI_PL.csv')
  dat2 <- read.csv(file='data/pilot/processed/ROT_noninstructed_blocked_CI_PL.csv')
  dat3 <- read.csv(file='data/pilot/processed/ROTWASH_noninstructed_blocked_CI_PL.csv')
  dat4 <- read.csv(file='data/pilot/processed/MIR_noninstructed_blocked_CI_PL.csv')
  dat5 <- read.csv(file='data/pilot/processed/MIRWASH_noninstructed_blocked_CI_PL.csv')
  
  dat <- rbind(dat1, dat2, dat3, dat4, dat5)
  
  #separate each task, then plot as usual according to blocks
  
  X1 <- seq(1, 4,1)
  X3 <- seq(5,19,1)
  X5 <- seq(20,27,1)
  X7 <- seq(28,42,1)
  X9 <- seq(43,50,1)
  
  Y <- as.numeric(dat$V2)
  YLow <- as.numeric(dat$V1)
  YUp <- as.numeric(dat$V3)
  
  plot(c(1:length(Y)), Y, type = 'n', axes = FALSE,
       xlab = 'Blocks', ylab = 'Path length (cm)', main = '',
       xlim = c(0,51), ylim = c(10,11.5))
  
  #labs <- c('1:AL','9:ROT','24:WASH','32:MIR','47:WASH','54')
  #axis(side=1, at=c(1,9,24,32,47,54), labels=labs)
  axis(side=1, at=c(1,5,20,28,43,50))
  #mtext('Trial & Task', side = 1, outer = TRUE, line=-1, cex = 1)
  axis(side=2, at=c(10, 10.25, 10.5, 10.75, 11, 11.25, 11.5),las=2)
  
  #abline(h = c(400,700), col = 'black', lty = 2)
  abline(v = c(4.5,19.5,27.5,42.5), col = 8, lty = 2)
  #abline(h = c(9), col = 8, lty = 2)
  
  polygon(x = c(X1, rev(X1)), y = c(YLow[1:4], rev(YUp[1:4])), border=NA, col=alpha("#b4b4b4",.5))
  polygon(x = c(X3, rev(X3)), y = c(YLow[5:19], rev(YUp[5:19])), border=NA, col="#e516362f")
  polygon(x = c(X5, rev(X5)), y = c(YLow[20:27], rev(YUp[20:27])), border=NA, col="#e516362f")
  polygon(x = c(X7, rev(X7)), y = c(YLow[28:42], rev(YUp[28:42])), border=NA, col="#005de42f")
  polygon(x = c(X9, rev(X9)), y = c(YLow[43:50], rev(YUp[43:50])), border=NA, col="#005de42f")
  
  lines(X1,Y[1:4], col = alpha("#000000", 1))#aligned
  lines(X3, Y[5:19], col = alpha("#e51636ff", 1))#rotation
  lines(X5, Y[20:27], col = alpha("#e51636ff", 1))#rotwashout
  lines(X7, Y[28:42], col = alpha("#005de4ff", 1))#mirror
  lines(X9, Y[43:50], col = alpha("#005de4ff", 1))#mirwashout
  
  
  
  #add legend
  legend(5,11.5,legend=c('Aligned','Rotation and washout','Mirror Reversal and washout'),
         col=c("#000000", "#e51636ff", "#005de4ff"),
         lty=1,bty='n',cex=0.8,lwd=2)
  
  #close everything if you saved plot as svg
  if (target=='svg') {
    dev.off()
  }
  
}

#Path Length Stats----
getPathLongFormat <- function(groups = c('noninstructed','instructed'), location = 'maxvel'){
  
  for (group in groups){
    if(group == 'noninstructed'){
      maxppid = 15
      #Aligned data
      ALdat <- getAlignedGroupPathLength(group=group,maxppid=maxppid)
      ppcols <- c('p0','p1', 'p2', 'p3', 'p4', 'p5', 'p6', 'p7', 'p8', 'p9', 'p10', 'p11', 'p12', 'p13', 'p14', 'p15')
      colnames(ALdat) <- c('trial', 'p0','p1', 'p2', 'p3', 'p4', 'p5', 'p6', 'p7', 'p8', 'p9', 'p10', 'p11', 'p12', 'p13', 'p14', 'p15')
      
      ALdat <- as.data.frame(ALdat)
      perturb <- rep('AL', nrow(ALdat))
      ALdat <- cbind(ALdat, perturb)
      #gather(data, the pp cols changed to rows, reachdev values to rows, specify how many ppcols to change)
      longALdata <- gather(ALdat, participant, compensation, ppcols[1:length(ppcols)], factor_key=TRUE)
      write.csv(longALdata, file=sprintf('data/pilot/processed/ALIGNED_%s_PL_long.csv', group), row.names = F)
      
      
      #Rotation data
      ROTdat <- getROTGroupPathLength(group=group,maxppid=maxppid)
      ppcols <- c('p0','p1', 'p2', 'p3', 'p4', 'p5', 'p6', 'p7', 'p8', 'p9', 'p10', 'p11', 'p12', 'p13', 'p14', 'p15')
      colnames(ROTdat) <- c('trial', 'p0','p1', 'p2', 'p3', 'p4', 'p5', 'p6', 'p7', 'p8', 'p9', 'p10', 'p11', 'p12', 'p13', 'p14', 'p15')
      
      ROTdat <- as.data.frame(ROTdat)
      perturb <- rep('ROT', nrow(ROTdat))
      ROTdat <- cbind(ROTdat, perturb)
      #gather(data, the pp cols changed to rows, reachdev values to rows, specify how many ppcols to change)
      longROTdata <- gather(ROTdat, participant, compensation, ppcols[1:length(ppcols)], factor_key=TRUE)
      write.csv(longROTdata, file=sprintf('data/pilot/processed/ROT_%s_PL_long.csv', group), row.names = F)
      
      #RotWASH data
      ROTWASHdat <- getROTWASHGroupPLTrials(group=group,maxppid=maxppid)
      ppcols <- c('p0','p1', 'p2', 'p3', 'p4', 'p5', 'p6', 'p7', 'p8', 'p9', 'p10', 'p11', 'p12', 'p13', 'p14', 'p15')
      colnames(ROTWASHdat) <- c('trial', 'p0','p1', 'p2', 'p3', 'p4', 'p5', 'p6', 'p7', 'p8', 'p9', 'p10', 'p11', 'p12', 'p13', 'p14', 'p15')
      
      ROTWASHdat <- as.data.frame(ROTWASHdat)
      perturb <- rep('ROTWASH', nrow(ROTWASHdat))
      ROTWASHdat <- cbind(ROTWASHdat, perturb)
      #gather(data, the pp cols changed to rows, reachdev values to rows, specify how many ppcols to change)
      longROTWASHdata <- gather(ROTWASHdat, participant, compensation, ppcols[1:length(ppcols)], factor_key=TRUE)
      write.csv(longROTWASHdata, file=sprintf('data/pilot/processed/ROTWASH_%s_PL_long.csv', group), row.names = F)
      
      #Mirror data
      MIRdat <- getMIRGroupPathLength(group=group,maxppid=maxppid)
      ppcols <- c('p0','p1', 'p2', 'p3', 'p4', 'p5', 'p6', 'p7', 'p8', 'p9', 'p10', 'p11', 'p12', 'p13', 'p14', 'p15')
      colnames(MIRdat) <- c('trial', 'p0','p1', 'p2', 'p3', 'p4', 'p5', 'p6', 'p7', 'p8', 'p9', 'p10', 'p11', 'p12', 'p13', 'p14', 'p15')
      
      MIRdat <- as.data.frame(MIRdat)
      perturb <- rep('MIR', nrow(MIRdat))
      MIRdat <- cbind(MIRdat, perturb)
      #gather(data, the pp cols changed to rows, reachdev values to rows, specify how many ppcols to change)
      longMIRdata <- gather(MIRdat, participant, compensation, ppcols[1:length(ppcols)], factor_key=TRUE)
      write.csv(longMIRdata, file=sprintf('data/pilot/processed/MIR_%s_PL_long.csv', group), row.names = F)
      
      #MirWASH data
      MIRWASHdat <- getMIRWASHGroupPLTrials(group=group,maxppid=maxppid)
      ppcols <- c('p0','p1', 'p2', 'p3', 'p4', 'p5', 'p6', 'p7', 'p8', 'p9', 'p10', 'p11', 'p12', 'p13', 'p14', 'p15')
      colnames(MIRWASHdat) <- c('trial', 'p0','p1', 'p2', 'p3', 'p4', 'p5', 'p6', 'p7', 'p8', 'p9', 'p10', 'p11', 'p12', 'p13', 'p14', 'p15')
      
      MIRWASHdat <- as.data.frame(MIRWASHdat)
      perturb <- rep('MIRWASH', nrow(MIRWASHdat))
      MIRWASHdat <- cbind(MIRWASHdat, perturb)
      #gather(data, the pp cols changed to rows, reachdev values to rows, specify how many ppcols to change)
      longMIRWASHdata <- gather(MIRWASHdat, participant, compensation, ppcols[1:length(ppcols)], factor_key=TRUE)
      write.csv(longMIRWASHdata, file=sprintf('data/pilot/processed/MIRWASH_%s_PL_long.csv', group), row.names = F)
      
    } else if (group == 'instructed'){
      maxppid = 31
      #Aligned data
      ALdat <- getAlignedGroupPathLength(group=group,maxppid=maxppid)
      ppcols <- c('p16','p17', 'p18', 'p19', 'p20', 'p21', 'p22', 'p23', 'p24', 'p25', 'p26', 'p27', 'p28', 'p29', 'p30', 'p31')
      colnames(ALdat) <- c('trial', 'p16','p17', 'p18', 'p19', 'p20', 'p21', 'p22', 'p23', 'p24', 'p25', 'p26', 'p27', 'p28', 'p29', 'p30', 'p31')
      
      ALdat <- as.data.frame(ALdat)
      perturb <- rep('AL', nrow(ALdat))
      ALdat <- cbind(ALdat, perturb)
      #gather(data, the pp cols changed to rows, reachdev values to rows, specify how many ppcols to change)
      longALdata <- gather(ALdat, participant, compensation, ppcols[1:length(ppcols)], factor_key=TRUE)
      write.csv(longALdata, file=sprintf('data/pilot/processed/ALIGNED_%s_PL_long.csv', group), row.names = F)
      
      
      #Rotation data
      ROTdat <- getROTGroupPathLength(group=group,maxppid=maxppid)
      ppcols <- c('p16','p17', 'p18', 'p19', 'p20', 'p21', 'p22', 'p23', 'p24', 'p25', 'p26', 'p27', 'p28', 'p29', 'p30', 'p31')
      colnames(ROTdat) <- c('trial', 'p16','p17', 'p18', 'p19', 'p20', 'p21', 'p22', 'p23', 'p24', 'p25', 'p26', 'p27', 'p28', 'p29', 'p30', 'p31')
      
      ROTdat <- as.data.frame(ROTdat)
      perturb <- rep('ROT', nrow(ROTdat))
      ROTdat <- cbind(ROTdat, perturb)
      #gather(data, the pp cols changed to rows, reachdev values to rows, specify how many ppcols to change)
      longROTdata <- gather(ROTdat, participant, compensation, ppcols[1:length(ppcols)], factor_key=TRUE)
      write.csv(longROTdata, file=sprintf('data/pilot/processed/ROT_%s_PL_long.csv', group), row.names = F)
      
      #RotWASH data
      ROTWASHdat <- getROTWASHGroupPLTrials(group=group,maxppid=maxppid)
      ppcols <- c('p16','p17', 'p18', 'p19', 'p20', 'p21', 'p22', 'p23', 'p24', 'p25', 'p26', 'p27', 'p28', 'p29', 'p30', 'p31')
      colnames(ROTWASHdat) <- c('trial', 'p16','p17', 'p18', 'p19', 'p20', 'p21', 'p22', 'p23', 'p24', 'p25', 'p26', 'p27', 'p28', 'p29', 'p30', 'p31')
      
      ROTWASHdat <- as.data.frame(ROTWASHdat)
      perturb <- rep('ROTWASH', nrow(ROTWASHdat))
      ROTWASHdat <- cbind(ROTWASHdat, perturb)
      #gather(data, the pp cols changed to rows, reachdev values to rows, specify how many ppcols to change)
      longROTWASHdata <- gather(ROTWASHdat, participant, compensation, ppcols[1:length(ppcols)], factor_key=TRUE)
      write.csv(longROTWASHdata, file=sprintf('data/pilot/processed/ROTWASH_%s_PL_long.csv', group), row.names = F)
      
      #Mirror data
      MIRdat <- getMIRGroupPathLength(group=group,maxppid=maxppid)
      ppcols <- c('p16','p17', 'p18', 'p19', 'p20', 'p21', 'p22', 'p23', 'p24', 'p25', 'p26', 'p27', 'p28', 'p29', 'p30', 'p31')
      colnames(MIRdat) <- c('trial', 'p16','p17', 'p18', 'p19', 'p20', 'p21', 'p22', 'p23', 'p24', 'p25', 'p26', 'p27', 'p28', 'p29', 'p30', 'p31')
      
      MIRdat <- as.data.frame(MIRdat)
      perturb <- rep('MIR', nrow(MIRdat))
      MIRdat <- cbind(MIRdat, perturb)
      #gather(data, the pp cols changed to rows, reachdev values to rows, specify how many ppcols to change)
      longMIRdata <- gather(MIRdat, participant, compensation, ppcols[1:length(ppcols)], factor_key=TRUE)
      write.csv(longMIRdata, file=sprintf('data/pilot/processed/MIR_%s_PL_long.csv', group), row.names = F)
      
      #MirWASH data
      MIRWASHdat <- getMIRWASHGroupPLTrials(group=group,maxppid=maxppid)
      ppcols <- c('p16','p17', 'p18', 'p19', 'p20', 'p21', 'p22', 'p23', 'p24', 'p25', 'p26', 'p27', 'p28', 'p29', 'p30', 'p31')
      colnames(MIRWASHdat) <- c('trial', 'p16','p17', 'p18', 'p19', 'p20', 'p21', 'p22', 'p23', 'p24', 'p25', 'p26', 'p27', 'p28', 'p29', 'p30', 'p31')
      
      MIRWASHdat <- as.data.frame(MIRWASHdat)
      perturb <- rep('MIRWASH', nrow(MIRWASHdat))
      MIRWASHdat <- cbind(MIRWASHdat, perturb)
      #gather(data, the pp cols changed to rows, reachdev values to rows, specify how many ppcols to change)
      longMIRWASHdata <- gather(MIRWASHdat, participant, compensation, ppcols[1:length(ppcols)], factor_key=TRUE)
      write.csv(longMIRWASHdata, file=sprintf('data/pilot/processed/MIRWASH_%s_PL_long.csv', group), row.names = F)
      
    }
  }
}

#First, we grab data from aligned, since it is the baseline for all
getPLBlockedAlignedData <- function(group, blockdefs){
  LCaov <- data.frame()
  curves <- read.csv(sprintf('data/pilot/processed/ALIGNED_%s_PL_long.csv',group), stringsAsFactors=FALSE)  
  participants <- unique(curves$participant)
  #R <- dim(curves)[1] # not needed, checks if rows=90 (correct trial numbers)
  #curves <- curves[,-1] #take away trial column
  N <- length(participants) #gets the number of participants
  perturb = 'AL'
  #blocked <- array(NA, dim=c(N,length(blockdefs))) #empty array where every participant will get 3 corresponding columns
  #row.names(blocked) <- participants
  #colnames(blocked) <- names(blockdefs)
  
  perturbtype <- c()
  participant <- c()
  block <- c()
  compensation <- c()
  
  for (pp.idx in c(1:length(participants))) {
    
    pp <- participants[pp.idx] #loop through each participant
    
    for (blockno in c(1:length(blockdefs))) { #loop through each block (first, second, third)
      
      blockdef <- blockdefs[[blockno]] #creates a list which specifies start trial of every block, and how many trials in total for this block
      blockstart <- blockdef[1] #either trial 1, 4, or 76
      blockend <- blockstart + blockdef[2] - 1 #either trial 3, 6, or 90
      #samples <- curves[blockstart:blockend,pp] #gets corresponding reach angle per participant
      # moved to long format files:
      samples <- c()
      for (trial in c(blockstart:blockend)) {
        # print(which(curves$participant == pp))
        # print(which(curves$participant == pp & curves$trial == trial))
        samples <- c(samples, curves$compensation[which(curves$participant == pp & curves$trial == trial)]) #get reachdev for current pp and trial
        
      }
      #print(mean(samples, na.rm=TRUE))
      #blocked[pp.idx,block] <- mean(samples, na.rm=TRUE) #compute the mean for it and put it in array
      perturbtype <- c(perturbtype, perturb)
      participant <- c(participant, pp) #the participant
      block <- c(block, names(blockdefs)[blockno]) #the name of the block number (first, second or third)
      compensation <- c(compensation, mean(samples, na.rm=T)) #mean compensation of trials for that block
    }
    
  }
  
  GroupLCBlocked <- data.frame(perturbtype,participant,block,compensation)
  
  
  if (prod(dim(LCaov)) == 0){
    LCaov <- GroupLCBlocked
  } else {
    LCaov <- rbind(LCaov, GroupLCBlocked)
  }
  
  #need to make some columns as factors for ANOVA
  LCaov$perturbtype <- as.factor(LCaov$perturbtype)
  LCaov$block <- as.factor(LCaov$block)
  LCaov$block <- factor(LCaov$block, levels = c('first','second','last')) #so that it does not order it alphabetically
  return(LCaov)
}

#Then grab data for washout and perturb types

getPLBlockedPerturbData <- function(perturbations = c('ROT','MIR'), group, blockdefs) {
  #function reads in aftereffects_long.csv file then creates a df with cols participant, block, reachdev
  LCaov <- data.frame()
  #to include instructed group, just create another for loop here
  for (perturb in perturbations){  
    curves <- read.csv(sprintf('data/pilot/processed/%s_%s_PL_long.csv',perturb,group), stringsAsFactors=FALSE)  
    participants <- unique(curves$participant)
    #R <- dim(curves)[1] # not needed, checks if rows=90 (correct trial numbers)
    #curves <- curves[,-1] #take away trial column
    N <- length(participants) #gets the number of participants
    
    #blocked <- array(NA, dim=c(N,length(blockdefs))) #empty array where every participant will get 3 corresponding columns
    #row.names(blocked) <- participants
    #colnames(blocked) <- names(blockdefs)
    
    perturbtype <- c()
    participant <- c()
    block <- c()
    compensation <- c()
    
    for (pp.idx in c(1:length(participants))) {
      
      pp <- participants[pp.idx] #loop through each participant
      
      for (blockno in c(1:length(blockdefs))) { #loop through each block (first, second, third)
        
        blockdef <- blockdefs[[blockno]] #creates a list which specifies start trial of every block, and how many trials in total for this block
        blockstart <- blockdef[1] #either trial 1, 4, or 76
        blockend <- blockstart + blockdef[2] - 1 #either trial 3, 6, or 90
        #samples <- curves[blockstart:blockend,pp] #gets corresponding reach angle per participant
        # moved to long format files:
        samples <- c()
        for (trial in c(blockstart:blockend)) {
          # print(which(curves$participant == pp))
          # print(which(curves$participant == pp & curves$trial == trial))
          samples <- c(samples, curves$compensation[which(curves$participant == pp & curves$trial == trial)]) #get reachdev for current pp and trial
          
        }
        #print(mean(samples, na.rm=TRUE))
        #blocked[pp.idx,block] <- mean(samples, na.rm=TRUE) #compute the mean for it and put it in array
        perturbtype <- c(perturbtype, perturb)
        participant <- c(participant, pp) #the participant
        block <- c(block, names(blockdefs)[blockno]) #the name of the block number (first, second or third)
        compensation <- c(compensation, mean(samples, na.rm=T)) #mean compensation of trials for that block
      }
      
    }
    
    GroupLCBlocked <- data.frame(perturbtype,participant,block,compensation)
    
    
    if (prod(dim(LCaov)) == 0){
      LCaov <- GroupLCBlocked
    } else {
      LCaov <- rbind(LCaov, GroupLCBlocked)
    }
  }
  #need to make some columns as factors for ANOVA
  LCaov$perturbtype <- as.factor(LCaov$perturbtype)
  LCaov$block <- as.factor(LCaov$block)
  LCaov$block <- factor(LCaov$block, levels = c('first','second','last')) #so that it does not order it alphabetically
  return(LCaov)
  
}

getPLBlockedWashoutData <- function(perturbations = c('ROTWASH','MIRWASH'), group, blockdefs) {
  #function reads in aftereffects_long.csv file then creates a df with cols participant, block, reachdev
  LCaov <- data.frame()
  #to include instructed group, just create another for loop here
  for (perturb in perturbations){  
    curves <- read.csv(sprintf('data/pilot/processed/%s_%s_PL_long.csv',perturb, group), stringsAsFactors=FALSE)  
    participants <- unique(curves$participant)
    #R <- dim(curves)[1] # not needed, checks if rows=90 (correct trial numbers)
    #curves <- curves[,-1] #take away trial column
    N <- length(participants) #gets the number of participants
    
    #blocked <- array(NA, dim=c(N,length(blockdefs))) #empty array where every participant will get 3 corresponding columns
    #row.names(blocked) <- participants
    #colnames(blocked) <- names(blockdefs)
    
    perturbtype <- c()
    participant <- c()
    block <- c()
    compensation <- c()
    
    for (pp.idx in c(1:length(participants))) {
      
      pp <- participants[pp.idx] #loop through each participant
      
      for (blockno in c(1:length(blockdefs))) { #loop through each block (first, second, third)
        
        blockdef <- blockdefs[[blockno]] #creates a list which specifies start trial of every block, and how many trials in total for this block
        blockstart <- blockdef[1] #either trial 1, 4, or 76
        blockend <- blockstart + blockdef[2] - 1 #either trial 3, 6, or 90
        #samples <- curves[blockstart:blockend,pp] #gets corresponding reach angle per participant
        # moved to long format files:
        samples <- c()
        for (trial in c(blockstart:blockend)) {
          # print(which(curves$participant == pp))
          # print(which(curves$participant == pp & curves$trial == trial))
          samples <- c(samples, curves$compensation[which(curves$participant == pp & curves$trial == trial)]) #get reachdev for current pp and trial
          
        }
        #print(mean(samples, na.rm=TRUE))
        #blocked[pp.idx,block] <- mean(samples, na.rm=TRUE) #compute the mean for it and put it in array
        perturbtype <- c(perturbtype, perturb)
        participant <- c(participant, pp) #the participant
        block <- c(block, names(blockdefs)[blockno]) #the name of the block number (first, second or third)
        compensation <- c(compensation, mean(samples, na.rm=T)) #mean compensation of trials for that block
      }
      
    }
    
    GroupLCBlocked <- data.frame(perturbtype,participant,block,compensation)
    
    
    if (prod(dim(LCaov)) == 0){
      LCaov <- GroupLCBlocked
    } else {
      LCaov <- rbind(LCaov, GroupLCBlocked)
    }
  }
  #need to make some columns as factors for ANOVA
  LCaov$perturbtype <- as.factor(LCaov$perturbtype)
  LCaov$block <- as.factor(LCaov$block)
  LCaov$block <- factor(LCaov$block, levels = c('first','second','last')) #so that it does not order it alphabetically
  return(LCaov)
  
}

PLt.test <- function(group) {
  
  blockdefs <- list('first'=c(1,12),'second'=c(13,12),'last'=c(37,12))
  LC4test1 <- getPLBlockedAlignedData(group=group,blockdefs=blockdefs)
  
  blockdefs <- list('first'=c(1,6),'second'=c(7,6),'last'=c(85,6)) #6 trials per block
  LC4test2 <- getPLBlockedPerturbData(group=group,blockdefs=blockdefs)
  
  blockdefs <- list('first'=c(1,6),'second'=c(7,6),'last'=c(43,6)) #6 trials per block
  LC4test3 <- getPLBlockedWashoutData(group=group,blockdefs=blockdefs)
  
  LC4test <- rbind(LC4test1, LC4test2, LC4test3)
  LC4test$participant <- as.factor(LC4test$participant)
  
  ALdat <- LC4test[which(LC4test$block == 'last' & LC4test$perturbtype == 'AL'),]
  ROTdat <- LC4test[which(LC4test$block == 'first' & LC4test$perturbtype == 'ROT'),]
  MIRdat <-LC4test[which(LC4test$block == 'first' & LC4test$perturbtype == 'MIR'),]
  ROTWASHdat <-LC4test[which(LC4test$block == 'first' & LC4test$perturbtype == 'ROTWASH'),]
  MIRWASHdat <-LC4test[which(LC4test$block == 'first' & LC4test$perturbtype == 'MIRWASH'),]
  
  cat('Aligned (last block) compared to Rotation (first block):\n')
  print(t.test(ALdat$compensation, ROTdat$compensation, paired = TRUE))
  cat('Effect Size - Cohen d:\n')
  print(cohensD(ALdat$compensation, ROTdat$compensation, method = 'paired'))
  cat('Bayesian t-test Aligned (last block) compared to Rotation (first block):\n')
  print(ttestBF(ALdat$compensation, ROTdat$compensation, paired = TRUE))
  
  cat('Aligned (last block) compared to Mirror (first block):\n')
  print(t.test(ALdat$compensation, MIRdat$compensation, paired = TRUE))
  cat('Effect Size - Cohen d:\n')
  print(cohensD(ALdat$compensation, MIRdat$compensation, method = 'paired'))
  cat('Bayesian t-test Aligned (last block) compared to Mirror (first block):\n')
  print(ttestBF(ALdat$compensation, MIRdat$compensation, paired = TRUE))
  
  cat('Aligned (last block) compared to Rotation Washout (first block):\n')
  print(t.test(ALdat$compensation, ROTWASHdat$compensation, paired = TRUE))
  cat('Effect Size - Cohen d:\n')
  print(cohensD(ALdat$compensation, ROTWASHdat$compensation, method = 'paired'))
  cat('Bayesian t-test Aligned (last block) compared to Rotation Washout (first block):\n')
  print(ttestBF(ALdat$compensation, ROTWASHdat$compensation, paired = TRUE))
  
  cat('Aligned (last block) compared to Mirror Washout (first block):\n')
  print(t.test(ALdat$compensation, MIRWASHdat$compensation, paired = TRUE))
  cat('Effect Size - Cohen d:\n')
  print(cohensD(ALdat$compensation, MIRWASHdat$compensation, method = 'paired'))
  cat('Bayesian t-test Aligned (last block) compared to Mirror Washout (first block):\n')
  print(ttestBF(ALdat$compensation, MIRWASHdat$compensation, paired = TRUE))
  
  #I add a comparison of last block of ROT and MIR compared to aligned, and ROTWASH and MIRWASH last blocks.
  ROTdatlast <- LC4test[which(LC4test$block == 'last' & LC4test$perturbtype == 'ROT'),]
  MIRdatlast <-LC4test[which(LC4test$block == 'last' & LC4test$perturbtype == 'MIR'),]
  
  cat('Aligned (last block) compared to Rotation (last block):\n')
  print(t.test(ALdat$compensation, ROTdatlast$compensation, paired = TRUE))
  cat('Effect Size - Cohen d:\n')
  print(cohensD(ALdat$compensation, ROTdatlast$compensation, method = 'paired'))
  cat('Bayesian t-test Aligned (last block) compared to Rotation (last block):\n')
  print(ttestBF(ALdat$compensation, ROTdatlast$compensation, paired = TRUE))
  
  cat('Aligned (last block) compared to Mirror (last block):\n')
  print(t.test(ALdat$compensation, MIRdatlast$compensation, paired = TRUE))
  cat('Effect Size - Cohen d:\n')
  print(cohensD(ALdat$compensation, MIRdatlast$compensation, method = 'paired'))
  cat('Bayesian t-test Aligned (last block) compared to Mirror (last block):\n')
  print(ttestBF(ALdat$compensation, MIRdatlast$compensation, paired = TRUE))
  
  #I add a comparison of last block of MIR WASHOUT compared to aligned
  ROTWASHdatlast <- LC4test[which(LC4test$block == 'last' & LC4test$perturbtype == 'ROTWASH'),]
  MIRWASHdatlast <-LC4test[which(LC4test$block == 'last' & LC4test$perturbtype == 'MIRWASH'),]
  
  cat('Aligned (last block) compared to Rotation Washout (last block):\n')
  print(t.test(ALdat$compensation, ROTWASHdatlast$compensation, paired = TRUE))
  cat('Effect Size - Cohen d:\n')
  print(cohensD(ALdat$compensation, ROTWASHdatlast$compensation, method = 'paired'))
  cat('Bayesian t-test Aligned (last block) compared to Rotation Washout (last block):\n')
  print(ttestBF(ALdat$compensation, ROTWASHdatlast$compensation, paired = TRUE))
  
  cat('Aligned (last block) compared to Mirror Washout (last block):\n')
  print(t.test(ALdat$compensation, MIRWASHdatlast$compensation, paired = TRUE))
  cat('Effect Size - Cohen d:\n')
  print(cohensD(ALdat$compensation, MIRWASHdatlast$compensation, method = 'paired'))
  cat('Bayesian t-test Aligned (last block) compared to Mirror Washout (last block):\n')
  print(ttestBF(ALdat$compensation, MIRWASHdatlast$compensation, paired = TRUE))
  
  #add comparison of first block of MIR WASHOUT to last block of MIR
  cat('Rotation (last block) compared to Rotation Washout (first block):\n')
  print(t.test(ROTdatlast$compensation, ROTWASHdat$compensation, paired = TRUE))
  cat('Effect Size - Cohen d:\n')
  print(cohensD(ROTdatlast$compensation, ROTWASHdat$compensation, method = 'paired'))
  cat('Bayesian t-test Rotation (last block) compared to Rotation Washout (first block):\n')
  print(ttestBF(ROTdatlast$compensation, ROTWASHdat$compensation, paired = TRUE))
  
  cat('Mirror (last block) compared to Mirror Washout (first block):\n')
  print(t.test(MIRdatlast$compensation, MIRWASHdat$compensation, paired = TRUE))
  cat('Effect Size - Cohen d:\n')
  print(cohensD(MIRdatlast$compensation, MIRWASHdat$compensation, method = 'paired'))
  cat('Bayesian t-test Mirror (last block) compared to Mirror Washout (first block):\n')
  print(ttestBF(MIRdatlast$compensation, MIRWASHdat$compensation, paired = TRUE))
  
  #add comparison of first MIR WASHOUT block to last MIR WASHOUT block
  cat('Mirror Washout (first block) compared to Mirror Washout (last block):\n')
  print(t.test(MIRWASHdat$compensation, MIRWASHdatlast$compensation, paired = TRUE))
  cat('Effect Size - Cohen d:\n')
  print(cohensD(MIRWASHdat$compensation, MIRWASHdatlast$compensation, method = 'paired'))
  cat('Bayesian t-test Mirror Washout (first block) compared to Mirror Washout (last block):\n')
  print(ttestBF(MIRWASHdat$compensation, MIRWASHdatlast$compensation, paired = TRUE))
}

pathlengthPerturbANOVA <- function(group) {
  
  #styles <- getStyle()
  blockdefs <- list('first'=c(1,6),'second'=c(7,6),'last'=c(85,6)) #6 trials per block
  
  LC4aov <- getPLBlockedPerturbData(group=group,blockdefs=blockdefs)
  
  #looking into interaction below:
  #interaction.plot(LC4aov$diffgroup, LC4aov$block, LC4aov$reachdeviation)
  
  #learning curve ANOVA's
  # for ez, case ID should be a factor:
  LC4aov$participant <- as.factor(LC4aov$participant)
  firstAOV <- ezANOVA(data=LC4aov, wid=participant, dv=compensation, within=c(perturbtype, block),type=3, return_aov = TRUE) #which type of SS is appropriate?
  print(firstAOV[1:3]) #so that it doesn't print the aov object as well
}

#significant interaction for instructed group
PLPerturbComparisonMeans <- function(group){
  
  #can plot interaction just to eyeball it:
  #plot(interactionMeans(lm(compensation ~ block * perturbtype, data=LC4aov), factors=c('perturbtype', 'block'), atx='block'))
  
  blockdefs <- list('first'=c(1,6),'second'=c(7,6),'last'=c(85,6))
  
  LC4aov <- getPLBlockedPerturbData(group=group, blockdefs=blockdefs)

  secondAOV <- aov_ez("participant","compensation",LC4aov,within=c("perturbtype","block"))
  
  #nice(secondAOV, correction = 'none') #correction set to none since first AOV reveals no violation of sphericity
  #summary(secondAOV) #shows all results
  #run code above for figuring out df
  #output is the same
  #follow-ups using emmeans
  
  cellmeans <- emmeans(secondAOV,specs=c('perturbtype','block'))
  #cellmeans <- lsmeans(secondAOV$aov,specs=c('perturbtype','block'))
  print(cellmeans)
}

PLPerturbComparisonsAllBlocks <- function(group, method='bonferroni'){
  #styles <- getStyle()
  blockdefs <- list('first'=c(1,6),'second'=c(7,6),'last'=c(85,6))
  
  LC4aov <- getPLBlockedPerturbData(group=group,blockdefs=blockdefs)

  secondAOV <- aov_ez("participant","compensation",LC4aov,within=c("perturbtype","block"))
  #based on cellmeans, confidence intervals and plots give us an idea of what contrasts we want to compare
  
  MIR_firstvsMIR_second  <- c(1,0,-1,0,0,0)
  MIR_firstvsMIR_last    <- c(1,0,0,0,-1,0)
  ROT_firstvsROT_second  <- c(0,1,0,-1,0,0)
  ROT_firstvsROT_last    <- c(0,1,0,0,0,-1)
  ROT_firstvsMIR_first   <- c(1,-1,0,0,0,0)
  ROT_secondvsMIR_second <- c(0,0,1,-1,0,0)
  ROT_lastvsMIR_last     <- c(0,0,0,0,1,-1)
  
  contrastList <- list('Block1: MIR vs. Block2: MIR'=MIR_firstvsMIR_second, 'Block1: MIR vs. Block3: MIR'=MIR_firstvsMIR_last,
                       'Block1: ROT vs. Block2: ROT'=ROT_firstvsROT_second, 'Block1: ROT vs. Block3: ROT'=ROT_firstvsROT_last,
                       'Block1: ROT vs. MIR'=ROT_firstvsMIR_first, 'Block2: ROT vs. MIR'=ROT_secondvsMIR_second, 'Block3: ROT vs. MIR'=ROT_lastvsMIR_last)
  
  comparisons<- contrast(emmeans(secondAOV,specs=c('perturbtype','block')), contrastList, adjust=method)
  
  print(comparisons)
}

#instructed group main effect of block is driven by difference in ROT blocks 1 and 2

#effect size
PLPerturbComparisonsAllBlocksEffSize <- function(group, method = 'bonferroni'){
  comparisons <- PLPerturbComparisonsAllBlocks(group=group,method=method)
  #we can use eta-squared as effect size
  #% of variance in DV(percentcomp) accounted for 
  #by the difference between target1 and target2
  comparisonsdf <- as.data.frame(comparisons)
  etasq <- ((comparisonsdf$t.ratio)^2)/(((comparisonsdf$t.ratio)^2)+(comparisonsdf$df))
  comparisons1 <- cbind(comparisonsdf,etasq)
  
  effectsize <- data.frame(comparisons1$contrast, comparisons1$etasq)
  colnames(effectsize) <- c('contrast', 'etasquared')
  #print(comparisons)
  print(effectsize)
}

#no significant effects in follow up tests

#BayesAnova
PLBayesANOVA <- function(group) {
  
  #styles <- getStyle()
  blockdefs <- list('first'=c(1,6),'second'=c(7,6),'last'=c(85,6)) #6 trials per block
  
  LC4aov <- getPLBlockedPerturbData(group=group,blockdefs=blockdefs)
  
  #Bayes ANOVA - can use long format
  #will compare models to null (intercept) or no effect - this will be 1
  #higher than 1 will be evidence for alternative hypothesis, lower will be evidence for null hypothesis
  #compare models either if only main effects, interaction of effects
  #use lmBF function for specific models
  LC4aov$participant <- as.factor(LC4aov$participant)
  bfLC<- anovaBF(compensation ~ perturbtype*block + participant, data = LC4aov, whichRandom = 'participant') #include data from participants, but note that this is a random factor
  #compare interaction contribution, over the contribution of both main effects
  #bfinteraction <- bfLC[4]/bfLC[3]
  bfinteraction <- bayesfactor_inclusion(bfLC)
  print(bfLC)
  print(bfinteraction)
  
  #bfLC will give main effects of perturbtype, can test which perturbation has larger compensation with:
  #LC4aov %>% group_by(perturbtype) %>% summarise(mean(compensation))
  #can also test main effects of block:
  #LC4aov %>% group_by(block) %>% summarise(mean(compensation))
  #but since interaction has large bf (in frequentist, interaction was significant), we can just compare [3] and [4]
}
