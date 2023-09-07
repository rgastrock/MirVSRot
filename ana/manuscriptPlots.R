source('ana/shared.R')
source('ana/learningRates.R')
source('ana/rae.R')
source('ana/RTandMT.R')
source('ana/pathlength.R')
source('ana/controlmir.R')
source('ana/controlmirgen.R')

plotTabletLearningRAE <- function(target='inline'){

  #but we can save plot as svg file
  if (target=='svg') {
    svglite(file='doc/fig_manuscripts/Fig2_Tablet_LCandRAE.svg', width=8, height=12, pointsize=16, system_fonts=list(sans="Arial"))
  }
  
  #par(mfrow=c(1,2), mar=c(4,4,2,0.1))
  par(mar=c(4,4,2,0.1))
  
  #layout(matrix(c(1,2,3), nrow=1, ncol=3, byrow = TRUE), widths=c(2,2,2), heights=c(1,1))
  layout(matrix(c(1,1,2,3,3,4), 2, 3, byrow = TRUE), widths=c(2,2,2), heights=c(1,1))
  
  # # # # # # # # # #
  # panel A: Learning Curves across blocks
  plotCollapsedBlockedIndLC()
  mtext('a', side=3, outer=FALSE, line=-1, adj=0, padj=1, font=2)
  
  # # # # # # # # # #
  # panel B: RAE across blocks
  plotCollapsedBlockedIndRAE()
  mtext('b', side=3, outer=FALSE, line=-1, adj=0, padj=1, font=2)
  
  # # # # # # # # # #
  # panel C: Learning Curves across blocks - instructed
  plotCollapsedBlockedIndLC(group='instructed', maxppid=31)
  mtext('c', side=3, outer=FALSE, line=-1, adj=0, padj=1, font=2)
  
  # # # # # # # # # #
  # panel D: RAE across blocks - instructed
  plotCollapsedBlockedIndRAE(group='instructed', maxppid=31)
  mtext('d', side=3, outer=FALSE, line=-1, adj=0, padj=1, font=2)
  


  #close everything if you saved plot as svg
  if (target=='svg') {
    dev.off()
  }

}

plotTabletMovementMeasures <- function(target='inline'){
  
  #but we can save plot as svg file
  if (target=='svg') {
    svglite(file='doc/fig_manuscripts/Fig3_Tablet_RTMTPL.svg', width=11.5, height=5, pointsize=16, system_fonts=list(sans="Arial"))
  }
  
  #par(mfrow=c(1,2), mar=c(4,4,2,0.1))
  par(mar=c(4,4,2,0.1))
  
  #layout(matrix(c(1,2,3), nrow=1, ncol=3, byrow = TRUE), widths=c(2,2,2), heights=c(1,1))
  layout(matrix(c(1,2,3), 1, 3, byrow = TRUE), widths=c(2,2,2), heights=c(1))
  
  # # # # # # # # # #
  # panel A: Learning Curves across blocks
  plotNIBlockedRT()
  mtext('a', side=3, outer=FALSE, line=-1, adj=0, padj=1, font=2)
  
  # # # # # # # # # #
  # panel B: RAE across blocks
  plotNIBlockedMT()
  mtext('b', side=3, outer=FALSE, line=-1, adj=0, padj=1, font=2)
  
  # # # # # # # # # #
  # panel C: Learning Curves across blocks - instructed
  plotNIBlockedPL()
  mtext('c', side=3, outer=FALSE, line=-1, adj=0, padj=1, font=2)
  
  
  #close everything if you saved plot as svg
  if (target=='svg') {
    dev.off()
  }
  
}

plotBlockedSession1 <- function(target='inline', groups = c('far', 'mid', 'near')) {
  
  if (target == 'svg') {
    svglite(file='doc/fig_manuscripts/Fig4_Session1LC.svg', width=10, height=10, pointsize=18, system_fonts=list(sans='Arial'))
  }
  
  #par(mfrow=c(1,2), mar=c(4,4,2,0.1))
  par(mar=c(4,4,2,0.1))
  
  
  
  #layout(matrix(c(1,2,3), nrow=1, ncol=3, byrow = TRUE), widths=c(2,2,2), heights=c(1,1))
  layout(matrix(c(1,1,1,2,3,4), 2, 3, byrow = TRUE), widths=c(2,2,2), heights=c(1,1))
  
  
  # # # # # # # # # #
  # panel A: Learning Curves for all groups across all trials
  plotAllTasksCtrl()
  #mtext('A', side=3, outer=TRUE, at=c(0,1), line=-1, adj=0, padj=1)
  mtext('a', side=3, outer=FALSE, line=-1, adj=0, padj=1, font=2)
  
  
  # # # # # # # # # #
  # panel B: First trial set - use percentage of compensation
  plot(c(0,4),c(0,0),col=rgb(0.5,0.5,0.5),type='l',lty=2,xlim=c(0.5,3.5),ylim=c(-10, 200),xlab='Mirror trials 1 - 3',ylab='Amount of compensation (%)',xaxt='n',yaxt='n',bty='n',main='',font.main=1, cex.lab=1.10)
  
  mtext('b', side=3, outer=FALSE, line=-1, adj=0, padj=1, font=2)
  abline(h = c(0, 100), col = 8, lty = 2)
  
  blockdefs <- list(c(1,3))
  #blockdefs <- list('first'=c(1,3),'second'=c(4,3),'last'=c(76,15))
  groupno <- 0
  
  for (group in groups) {
    
    groupno <- groupno + 1 #counter for group, so that we can refer to it in x coordinates
    blocked <- getBlockedMirCtrlPercentage(group, blockdefs)
    colourscheme <- getCtrlColourScheme(group=group)
    #get bootstrapped 2.5, 50, 97.5% CIs of percentages
    meandist <- getConfidenceInterval(data=blocked, method='b')
    #meandist <- getAngularReachDevsCI(data = blocked, group = group)
    
    col <- colourscheme[[group]][['S']]
    lines(x=rep(groupno,2),y=c(meandist[[1]], meandist[[3]]),col=col) #lower and upper CI
    points(x=groupno,y=meandist[[2]],pch=16,cex=1.5,col=col) #50% plotted as a point
    
  }
  
  
  axis(side=1, at=c(1,2,3),labels=c('far','mid','near'),cex.axis=1.13)
  axis(side=2, at=c(0,50, 100, 150),labels=c('0','50','100','150'),cex.axis=1.13, las=2)
  
  
  # # # # # # # # # #
  # panel C: Second trial set
  plot(c(0,4),c(0,0),col=rgb(0.5,0.5,0.5),type='l',lty=2,xlim=c(0.5,3.5),ylim=c(-10, 200),xlab='Mirror Trials 4 - 6',ylab='',xaxt='n',yaxt='n',bty='n',main='',font.main=1, cex.lab=1.10)
  
  mtext('c', side=3, outer=FALSE, line=-1, adj=0, padj=1, font=2)
  abline(h = c(0, 100), col = 8, lty = 2)
  
  blockdefs <- list(c(4,3))
  #blockdefs <- list('first'=c(1,3),'second'=c(4,3),'last'=c(76,15))
  groupno <- 0
  
  for (group in groups) {
    
    groupno <- groupno + 1 #counter for group, so that we can refer to it in x coordinates
    blocked <- getBlockedMirCtrlPercentage(group, blockdefs)
    colourscheme <- getCtrlColourScheme(group=group)
    #get 2.5, 50, 97.5% CIs
    meandist <- getConfidenceInterval(data=blocked, method='b')
    
    col <- colourscheme[[group]][['S']]
    lines(x=rep(groupno,2),y=c(meandist[[1]], meandist[[3]]),col=col) #lower and upper CI
    points(x=groupno,y=meandist[[2]],pch=16,cex=1.5,col=col) #50% plotted as a point
    
  }
  
  
  axis(side=1, at=c(1,2,3),labels=c('far','mid','near'),cex.axis=1.13)
  axis(side=2, at=c(0,50, 100, 150),labels=c('0','50','100','150'),cex.axis=1.13, las=2)
  
  
  # # # # # # # # # #
  # panel D: Last trial set
  plot(c(0,4),c(0,0),col=rgb(0.5,0.5,0.5),type='l',lty=2,xlim=c(0.5,3.5),ylim=c(-10, 200),xlab='Mirror Trials 76 - 90',ylab='',xaxt='n',yaxt='n',bty='n',main='',font.main=1, cex.lab=1.10)
  
  mtext('d', side=3, outer=FALSE, line=-1, adj=0, padj=1, font=2)
  abline(h = c(0, 100), col = 8, lty = 2)
  
  blockdefs <- list(c(76,15))
  #blockdefs <- list('first'=c(1,3),'second'=c(4,3),'last'=c(76,15))
  groupno <- 0
  
  for (group in groups) {
    
    groupno <- groupno + 1 #counter for group, so that we can refer to it in x coordinates
    blocked <- getBlockedMirCtrlPercentage(group, blockdefs)
    colourscheme <- getCtrlColourScheme(group=group)
    #get 2.5, 50, 97.5% CIs
    meandist <- getConfidenceInterval(data=blocked, method='b')
    
    col <- colourscheme[[group]][['S']]
    lines(x=rep(groupno,2),y=c(meandist[[1]], meandist[[3]]),col=col) #lower and upper CI
    points(x=groupno,y=meandist[[2]],pch=16,cex=1.5,col=col) #50% plotted as a point
    
  }
  
  
  axis(side=1, at=c(1,2,3),labels=c('far','mid','near'),cex.axis=1.13)
  axis(side=2, at=c(0,50, 100, 150),labels=c('0','50','100','150'),cex.axis=1.13, las=2)
  
  
  if (target == 'svg') {
    dev.off()
  }
  
}

plotSession1MovementMeasures <- function(target='inline'){
  
  #but we can save plot as svg file
  if (target=='svg') {
    svglite(file='doc/fig_manuscripts/Fig5_Session1_CTPL.svg', width=12, height=6, pointsize=16, system_fonts=list(sans="Arial"))
  }
  
  #par(mfrow=c(1,2), mar=c(4,4,2,0.1))
  par(mar=c(4,4,2,0.1))
  
  #layout(matrix(c(1,2,3), nrow=1, ncol=3, byrow = TRUE), widths=c(2,2,2), heights=c(1,1))
  layout(matrix(c(1,2), 1, 2, byrow = TRUE), widths=c(2,2), heights=c(1))
  
  # # # # # # # # # #
  # panel A: Learning Curves across blocks
  plotCtrlMT()
  mtext('a', side=3, outer=FALSE, line=-1, adj=0, padj=1, font=2)
  
  # # # # # # # # # #
  # panel B: RAE across blocks
  plotCtrlPL()
  mtext('b', side=3, outer=FALSE, line=-1, adj=0, padj=1, font=2)
  

  #close everything if you saved plot as svg
  if (target=='svg') {
    dev.off()
  }
  
}

plotBlockedSession2<- function(groups = c('far', 'mid', 'near'), target='inline') {
  
  
  if (target=='svg') {
    svglite(file='doc/fig_manuscripts/Fig6_Session2LC.svg', width=10, height=14, pointsize=18, system_fonts=list(sans="Arial"))
  }
  
  
  
  par(mar=c(4,4,2,0.1)) #4,4,2,.1
  
  
  
  layout(matrix(c(1,2), nrow=2, ncol=1, byrow = TRUE), widths=c(2), heights=c(1,1))
  #layout(matrix(c(1,1,1,2,3,4), 2, 3, byrow = TRUE), widths=c(2,2,2), heights=c(1,1))
  
  
  # # # # # # # # # #
  # panel A: Learning Curves for all groups across all trials
  plotLearningCtrlGen()
  #mtext('A', side=3, outer=TRUE, at=c(0,1), line=-1, adj=0, padj=1)
  mtext('a', side=3, outer=FALSE, line=-1, adj=0, padj=1, font=2)
  
  
  # # # # # # # # # #
  # panel B: First trial set - use percentage of compensation
  plotBlockedLearningPercentages()
  
  mtext('b', side=3, outer=FALSE, line=-1, adj=0, padj=1, font=2)
  
  
  
  
  if (target == 'svg') {
    dev.off()
  }
}

plotSession2MovementMeasures <- function(target='inline'){
  
  #but we can save plot as svg file
  if (target=='svg') {
    svglite(file='doc/fig_manuscripts/Fig7_Session2_CTPL.svg', width=10, height=14, pointsize=16, system_fonts=list(sans="Arial"))
  }
  
  #par(mfrow=c(1,2), mar=c(4,4,2,0.1))
  par(mar=c(4,4,2,0.1))
  
  #layout(matrix(c(1,2,3), nrow=1, ncol=3, byrow = TRUE), widths=c(2,2,2), heights=c(1,1))
  layout(matrix(c(1,2), 2, 1, byrow = TRUE), widths=c(2), heights=c(1,1))
  
  # # # # # # # # # #
  # panel A: Learning Curves across blocks
  plotCtrlGenMT()
  mtext('a', side=3, outer=FALSE, line=-1, adj=0, padj=1, font=2)
  
  # # # # # # # # # #
  # panel B: RAE across blocks
  plotCtrlGenPL()
  mtext('b', side=3, outer=FALSE, line=-1, adj=0, padj=1, font=2)
  
  
  #close everything if you saved plot as svg
  if (target=='svg') {
    dev.off()
  }
  
}


plotOnlineDependentMeasures <- function(target='inline', groups = c('far', 'mid', 'near')) {
  
  if (target == 'svg') {
    svglite(file='doc/fig_manuscripts/Fig8_OnlineDV.svg', width=10, height=12, pointsize=18, system_fonts=list(sans='Arial'))
  }
  
  #par(mfrow=c(1,2), mar=c(4,4,2,0.1))
  par(mar=c(4,4,2,0.1))
  
  
  
  #layout(matrix(c(1,2,3), nrow=1, ncol=3, byrow = TRUE), widths=c(2,2,2), heights=c(1,1))
  #layout(matrix(c(1,1,1,2,3,4), 2, 3, byrow = TRUE), widths=c(2,2,2), heights=c(1,1))
  layout(matrix(c(1,2,3,4,5,6), 3, 2, byrow = TRUE), widths=c(2,2), heights=c(1,1,1))
  
  
  # # # # # # # # # #
  # panel A: Learning Curves session 1
  plotAllTasksCtrl()
  mtext('a', side=3, outer=FALSE, line=-1, adj=0, padj=1, font=2)
  
  # # # # # # # # # #
  # panel B: Learning Curves session 2
  plotLearningCtrlGen()
  mtext('b', side=3, outer=FALSE, line=-1, adj=0, padj=1, font=2)
  
  # # # # # # # # # #
  # panel C: Completion time session 1
  plotCtrlMT()
  mtext('c', side=3, outer=FALSE, line=-1, adj=0, padj=1, font=2)
  
  # # # # # # # # # #
  # panel D: Completion time session 2
  plotCtrlGenMT()
  mtext('d', side=3, outer=FALSE, line=-1, adj=0, padj=1, font=2)
  
  # # # # # # # # # #
  # panel E: Path length session 1
  plotCtrlPL()
  mtext('e', side=3, outer=FALSE, line=-1, adj=0, padj=1, font=2)
  
  # # # # # # # # # #
  # panel F: Path length session 2
  plotCtrlGenPL()
  mtext('f', side=3, outer=FALSE, line=-1, adj=0, padj=1, font=2)
  
  
  
  if (target == 'svg') {
    dev.off()
  }
  
}

plotBlockedDependentMeasures <- function(groups = c('far', 'mid', 'near'), trialtypes = c('mir', 'washout'), quadrants = c('1', '4', '2', '1A', '1L', '1W'), target='inline'){
  
  if (target=='svg') {
    svglite(file='doc/fig_manuscripts/Fig9_OnlinePercentages.svg', width=7, height=11, pointsize=14, system_fonts=list(sans='Arial'))
  }
  
  #par(mfrow=c(1,2), mar=c(4,4,2,0.1))
  par(mar=c(4,4,2,0.1))
  
  
  
  #layout(matrix(c(1,2,3), nrow=1, ncol=3, byrow = TRUE), widths=c(2,2,2), heights=c(1,1))
  #layout(matrix(c(1,1,1,2,3,4), 2, 3, byrow = TRUE), widths=c(2,2,2), heights=c(1,1))
  layout(matrix(c(1,2,3), 3, 1, byrow = TRUE), widths=c(2), heights=c(1,1,1))
  
  #Percentages
  plot(NA, NA, xlim = c(0,168), ylim = c(-200,300), 
       xlab = "Block", ylab = "Amount of compensation (%)", frame.plot = FALSE, #frame.plot takes away borders
       main = "", xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
  
  lim <- par('usr')
  rect(22, lim[3]-1, 42, lim[4]+1, border = "#ededed", col = "#ededed") #xleft, ybottom, x right, ytop; light grey hex code
  rect(127, lim[3]-1, 168, lim[4]+1, border = "#ededed", col = "#ededed") #xleft, ybottom, x right, ytop; light grey hex code
  abline(h = c(0, 100), v = c(21, 42, 63, 84, 105, 126, 147), col = 8, lty = 2) #creates horizontal dashed lines through y =  0 and 30
  #abline(h = c(0, 100), v = c(21), col = 8, lty = 2) #creates horizontal dashed lines through y =  0 and 30
  
  axis(1, at = c(4, 11, 18, 25, 32, 39, 46, 53, 60, 67, 74, 81, 88, 95, 102, 109, 116, 123, 130, 137, 144, 151, 158, 165),
       labels = c('1', '2', '26-30', '1', '2', '7', '1', '2', '7', '1', '2', '7', '1', '2', '7', '1', '2', '7', '1', '2', '7', '1', '2', '7')) #tick marks for x axis
  axis(2, at = c(-200,-150,-100, -50, 0, 50, 100, 150, 200), las = 2) #tick marks for y axis
  axis.break(1, 41, style='gap', breakcol='white')
  
  
  for(type in trialtypes){
    if(type == 'mir'){
      blockdefs <- list('first'=c(1,3),'second'=c(4,3),'last'=c(76,15))
      blocked <- getMirrorBlockedLearningAOV(blockdefs=blockdefs) 
      for(group in groups){
        colourscheme <- getCtrlColourScheme(group=group)
        subblocked <- blocked[which(blocked$target == group),]
        blocks <- unique(subblocked$block)
        blockedconfidence <- data.frame()
        
        for(blockname in blocks){
          subdat <- subblocked[which(subblocked$block == blockname),]
          meandist <- getConfidenceInterval(data=subdat$percentcomp, method='b')
          
          if (prod(dim(blockedconfidence)) == 0){
            blockedconfidence <- meandist
          } else {
            blockedconfidence <- rbind(blockedconfidence, meandist)
          }
        }
        
        lower <- blockedconfidence[,1]
        upper <- blockedconfidence[,3]
        mid <- blockedconfidence[,2]
        
        if(group == 'near'){
          col <- colourscheme[[group]][['T']]
          col <- alpha(col, .15)
        } else {
          col <- colourscheme[[group]][['T']]
        }
        
        
        X <- c(4, 11, 18) #hard coded x axis values on plot
        polygon(x = c(X, rev(X)), y = c(lower, rev(upper)), border=NA, col=col)
        
        if(group == 'near'){
          col <- colourscheme[[group]][['S']]
          col <- alpha(col, .15)
        } else {
          col <- colourscheme[[group]][['S']]
        }
        lines(x=X,y=mid,col=col, lwd=2) #lower and upper CI
      }
    } else if(type == 'washout'){
      blockdefs <- list('first'=c(1,3),'second'=c(4,3),'last'=c(19,3))
      blocked <- getRAEBlockedPercentagesAOV(blockdefs=blockdefs) 
      for(group in groups){
        colourscheme <- getCtrlColourScheme(group=group)
        subblocked <- blocked[which(blocked$target == group),]
        blocks <- unique(subblocked$block)
        blockedconfidence <- data.frame()
        
        for(blockname in blocks){
          subdat <- subblocked[which(subblocked$block == blockname),]
          meandist <- getConfidenceInterval(data=subdat$percentcomp, method='b')
          
          if (prod(dim(blockedconfidence)) == 0){
            blockedconfidence <- meandist
          } else {
            blockedconfidence <- rbind(blockedconfidence, meandist)
          }
        }
        
        lower <- blockedconfidence[,1]
        upper <- blockedconfidence[,3]
        mid <- blockedconfidence[,2]
        
        if(group == 'near'){
          col <- colourscheme[[group]][['T']]
          col <- alpha(col, .15)
        } else {
          col <- colourscheme[[group]][['T']]
        }
        
        X <- c(25, 32, 39)
        polygon(x = c(X, rev(X)), y = c(lower, rev(upper)), border=NA, col=col)
        
        if(group == 'near'){
          col <- colourscheme[[group]][['S']]
          col <- alpha(col, .15)
        } else {
          col <- colourscheme[[group]][['S']]
        }
        lines(x=X,y=mid,col=col, lwd=2) #lower and upper CI
      }
    }
  }
  
  for(quadrant in quadrants){
    if(quadrant == '1'){
      blockdefs <- list('first'=c(1,3),'second'=c(4,3),'last'=c(19,3))
      X <- c(46, 53, 60)
    } else if(quadrant == '4'){
      blockdefs <- list('first'=c(22,3),'second'=c(25,3),'last'=c(40,3))
      X <- c(67, 74, 81)
    } else if(quadrant == '2'){
      blockdefs <- list('first'=c(43,3),'second'=c(46,3),'last'=c(61,3))
      X <- c(88, 95, 102)
    } else if(quadrant == '1A'){
      blockdefs <- list('first'=c(64,3),'second'=c(67,3),'last'=c(82,3))
      X <- c(109, 116, 123)
    } else if(quadrant == '1L'){
      blockdefs <- list('first'=c(85,3),'second'=c(88,3),'last'=c(103,3))
      X <- c(130, 137, 144)
    } else if(quadrant == '1W'){
      blockdefs <- list('first'=c(106,3),'second'=c(109,3),'last'=c(124,3))
      X <- c(151, 158, 165)
    }
    blocked <- getBlockedLearningAOV(blockdefs=blockdefs, quadrant=quadrant) 
    for(group in groups){
      colourscheme <- getCtrlColourScheme(group=group)
      subblocked <- blocked[which(blocked$target == group),]
      blocks <- unique(subblocked$block)
      blockedconfidence <- data.frame()
      
      for(blockname in blocks){
        subdat <- subblocked[which(subblocked$block == blockname),]
        meandist <- getConfidenceInterval(data=subdat$percentcomp, method='b')
        
        if (prod(dim(blockedconfidence)) == 0){
          blockedconfidence <- meandist
        } else {
          blockedconfidence <- rbind(blockedconfidence, meandist)
        }
      }
      
      lower <- blockedconfidence[,1]
      upper <- blockedconfidence[,3]
      mid <- blockedconfidence[,2]
      
      if(group == 'near'){
        col <- colourscheme[[group]][['T']]
        col <- alpha(col, .15)
      } else {
        col <- colourscheme[[group]][['T']]
      }
      
      
      polygon(x = c(X, rev(X)), y = c(lower, rev(upper)), border=NA, col=col)
      
      if(group == 'near'){
        col <- colourscheme[[group]][['S']]
        col <- alpha(col, .15)
      } else {
        col <- colourscheme[[group]][['S']]
      }
      lines(x=X,y=mid,col=col, lwd=2) #lower and upper CI
    }
  }
  
  #Completion time
  plot(NA, NA, xlim = c(0,168), ylim = c(0,12), 
       xlab = "Block", ylab = "Completion time (s)", frame.plot = FALSE, #frame.plot takes away borders
       main = "", xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
  
  lim <- par('usr')
  rect(22, lim[3]-1, 42, lim[4]+1, border = "#ededed", col = "#ededed") #xleft, ybottom, x right, ytop; light grey hex code
  rect(127, lim[3]-1, 168, lim[4]+1, border = "#ededed", col = "#ededed") #xleft, ybottom, x right, ytop; light grey hex code
  abline(h = c(0, 100), v = c(21, 42, 63, 84, 105, 126, 147), col = 8, lty = 2) #creates horizontal dashed lines through y =  0 and 30
  #abline(h = c(0, 100), v = c(21), col = 8, lty = 2) #creates horizontal dashed lines through y =  0 and 30
  
  axis(1, at = c(4, 11, 18, 25, 32, 39, 46, 53, 60, 67, 74, 81, 88, 95, 102, 109, 116, 123, 130, 137, 144, 151, 158, 165),
       labels = c('1', '2', '26-30', '1', '2', '7', '1', '2', '7', '1', '2', '7', '1', '2', '7', '1', '2', '7', '1', '2', '7', '1', '2', '7')) #tick marks for x axis
  axis(2, at = c(0, 2, 4, 6, 8, 10, 12), las = 2) #tick marks for y axis
  axis.break(1, 41, style='gap', breakcol='white')
  
  
  for(type in trialtypes){
    if(type == 'mir'){
      blockdefs <- list('first'=c(67,3),'second'=c(70,3),'last'=c(142,15))
      blocked <- getAlignedBlockedMTAOV(blockdefs=blockdefs, hand='trained') 
      for(group in groups){
        
        colourscheme <- getCtrlColourScheme(group=group)
        subblocked <- blocked[which(blocked$target == group),]
        blocks <- unique(subblocked$block)
        blockedconfidence <- data.frame()
        
        for(blockname in blocks){
          subdat <- subblocked[which(subblocked$block == blockname),]
          meandist <- getConfidenceInterval(data=subdat$movementtime, method='b')
          
          if (prod(dim(blockedconfidence)) == 0){
            blockedconfidence <- meandist
          } else {
            blockedconfidence <- rbind(blockedconfidence, meandist)
          }
        }
        
        lower <- blockedconfidence[,1]
        upper <- blockedconfidence[,3]
        mid <- blockedconfidence[,2]
        
        col <- colourscheme[[group]][['T']]
        
        X <- c(4, 11, 18) #hard coded x axis values on plot
        polygon(x = c(X, rev(X)), y = c(lower, rev(upper)), border=NA, col=col)
        
        col <- colourscheme[[group]][['S']]
        lines(x=X,y=mid,col=col, lwd=2) #lower and upper CI
      }
    } else if(type == 'washout'){
      blockdefs <- list('first'=c(157,3),'second'=c(160,3),'last'=c(175,3))
      blocked <- getAlignedBlockedMTAOV(blockdefs=blockdefs, hand='trained')
      
      for(group in groups){
        
        colourscheme <- getCtrlColourScheme(group=group)
        subblocked <- blocked[which(blocked$target == group),]
        blocks <- unique(subblocked$block)
        blockedconfidence <- data.frame()
        
        for(blockname in blocks){
          subdat <- subblocked[which(subblocked$block == blockname),]
          meandist <- getConfidenceInterval(data=subdat$movementtime, method='b')
          
          if (prod(dim(blockedconfidence)) == 0){
            blockedconfidence <- meandist
          } else {
            blockedconfidence <- rbind(blockedconfidence, meandist)
          }
        }
        
        lower <- blockedconfidence[,1]
        upper <- blockedconfidence[,3]
        mid <- blockedconfidence[,2]
        
        col <- colourscheme[[group]][['T']]
        
        X <- c(25, 32, 39) #hard coded x axis values on plot
        polygon(x = c(X, rev(X)), y = c(lower, rev(upper)), border=NA, col=col)
        
        col <- colourscheme[[group]][['S']]
        lines(x=X,y=mid,col=col, lwd=2) #lower and upper CI
      }
    }
  }
  
  
  for(quadrant in quadrants){
    if(quadrant == '1'){
      blockdefs <- list('first'=c(1,3),'second'=c(4,3),'last'=c(19,3))
      X <- c(46, 53, 60)
    } else if(quadrant == '4'){
      blockdefs <- list('first'=c(22,3),'second'=c(25,3),'last'=c(40,3))
      X <- c(67, 74, 81)
    } else if(quadrant == '2'){
      blockdefs <- list('first'=c(43,3),'second'=c(46,3),'last'=c(61,3))
      X <- c(88, 95, 102)
    } else if(quadrant == '1A'){
      blockdefs <- list('first'=c(64,3),'second'=c(67,3),'last'=c(82,3))
      X <- c(109, 116, 123)
    } else if(quadrant == '1L'){
      blockdefs <- list('first'=c(85,3),'second'=c(88,3),'last'=c(103,3))
      X <- c(130, 137, 144)
    } else if(quadrant == '1W'){
      blockdefs <- list('first'=c(106,3),'second'=c(109,3),'last'=c(124,3))
      X <- c(151, 158, 165)
    }
    
    
    blocked <- getBlockedMTAOV(blockdefs=blockdefs, quadrant=quadrant) 
    for(group in groups){
      
      colourscheme <- getCtrlColourScheme(group=group)
      subblocked <- blocked[which(blocked$target == group),]
      blocks <- unique(subblocked$block)
      blockedconfidence <- data.frame()
      
      for(blockname in blocks){
        subdat <- subblocked[which(subblocked$block == blockname),]
        meandist <- getConfidenceInterval(data=subdat$movementtime, method='b')
        
        if (prod(dim(blockedconfidence)) == 0){
          blockedconfidence <- meandist
        } else {
          blockedconfidence <- rbind(blockedconfidence, meandist)
        }
      }
      
      lower <- blockedconfidence[,1]
      upper <- blockedconfidence[,3]
      mid <- blockedconfidence[,2]
      
      
      col <- colourscheme[[group]][['T']]
      
      
      
      polygon(x = c(X, rev(X)), y = c(lower, rev(upper)), border=NA, col=col)
      
      
      col <- colourscheme[[group]][['S']]
      
      lines(x=X,y=mid,col=col, lwd=2) #lower and upper CI
    }
  }
  
  
  #Path length
  plot(NA, NA, xlim = c(0,168), ylim = c(0.4,3.2), 
       xlab = "Block", ylab = "Path length (monitor scale)", frame.plot = FALSE, #frame.plot takes away borders
       main = "", xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
  
  lim <- par('usr')
  rect(22, lim[3]-1, 42, lim[4]+1, border = "#ededed", col = "#ededed")
  rect(127, lim[3]-1, 168, lim[4]+1, border = "#ededed", col = "#ededed") #xleft, ybottom, x right, ytop; light grey hex code
  abline(h = c(0.4), v = c(21, 42, 63, 84, 105, 126, 147), col = 8, lty = 2) #creates horizontal dashed lines through y =  0 and 30
  #abline(h = c(0, 100), v = c(21), col = 8, lty = 2) #creates horizontal dashed lines through y =  0 and 30
  
  axis(1, at = c(4, 11, 18, 25, 32, 39, 46, 53, 60, 67, 74, 81, 88, 95, 102, 109, 116, 123, 130, 137, 144, 151, 158, 165),
       labels = c('1', '2', '26-30', '1', '2', '7', '1', '2', '7', '1', '2', '7', '1', '2', '7', '1', '2', '7', '1', '2', '7', '1', '2', '7')) #tick marks for x axis
  axis(2, at = c(.4, .8, 1.2, 1.6, 2, 2.4, 2.8, 3.2), las = 2) #tick marks for y axis
  axis.break(1, 41, style='gap', breakcol='white')
  
  
  for(type in trialtypes){
    if(type == 'mir'){
      blockdefs <- list('first'=c(67,3),'second'=c(70,3),'last'=c(142,15))
      blocked <- getAlignedBlockedPLAOV(blockdefs=blockdefs, hand='trained') 
      for(group in groups){
        
        colourscheme <- getCtrlColourScheme(group=group)
        subblocked <- blocked[which(blocked$target == group),]
        blocks <- unique(subblocked$block)
        blockedconfidence <- data.frame()
        
        for(blockname in blocks){
          subdat <- subblocked[which(subblocked$block == blockname),]
          meandist <- getConfidenceInterval(data=subdat$pathlength, method='b')
          
          if (prod(dim(blockedconfidence)) == 0){
            blockedconfidence <- meandist
          } else {
            blockedconfidence <- rbind(blockedconfidence, meandist)
          }
        }
        
        lower <- blockedconfidence[,1]
        upper <- blockedconfidence[,3]
        mid <- blockedconfidence[,2]
        
        col <- colourscheme[[group]][['T']]
        
        X <- c(4, 11, 18) #hard coded x axis values on plot
        polygon(x = c(X, rev(X)), y = c(lower, rev(upper)), border=NA, col=col)
        
        col <- colourscheme[[group]][['S']]
        lines(x=X,y=mid,col=col, lwd=2) #lower and upper CI
      }
    } else if(type == 'washout'){
      blockdefs <- list('first'=c(157,3),'second'=c(160,3),'last'=c(175,3))
      blocked <- getAlignedBlockedPLAOV(blockdefs=blockdefs, hand='trained')
      
      for(group in groups){
        
        colourscheme <- getCtrlColourScheme(group=group)
        subblocked <- blocked[which(blocked$target == group),]
        blocks <- unique(subblocked$block)
        blockedconfidence <- data.frame()
        
        for(blockname in blocks){
          subdat <- subblocked[which(subblocked$block == blockname),]
          meandist <- getConfidenceInterval(data=subdat$pathlength, method='b')
          
          if (prod(dim(blockedconfidence)) == 0){
            blockedconfidence <- meandist
          } else {
            blockedconfidence <- rbind(blockedconfidence, meandist)
          }
        }
        
        lower <- blockedconfidence[,1]
        upper <- blockedconfidence[,3]
        mid <- blockedconfidence[,2]
        
        col <- colourscheme[[group]][['T']]
        
        X <- c(25, 32, 39) #hard coded x axis values on plot
        polygon(x = c(X, rev(X)), y = c(lower, rev(upper)), border=NA, col=col)
        
        col <- colourscheme[[group]][['S']]
        lines(x=X,y=mid,col=col, lwd=2) #lower and upper CI
      }
    }
  }
  
  
  for(quadrant in quadrants){
    if(quadrant == '1'){
      blockdefs <- list('first'=c(1,3),'second'=c(4,3),'last'=c(19,3))
      X <- c(46, 53, 60)
    } else if(quadrant == '4'){
      blockdefs <- list('first'=c(22,3),'second'=c(25,3),'last'=c(40,3))
      X <- c(67, 74, 81)
    } else if(quadrant == '2'){
      blockdefs <- list('first'=c(43,3),'second'=c(46,3),'last'=c(61,3))
      X <- c(88, 95, 102)
    } else if(quadrant == '1A'){
      blockdefs <- list('first'=c(64,3),'second'=c(67,3),'last'=c(82,3))
      X <- c(109, 116, 123)
    } else if(quadrant == '1L'){
      blockdefs <- list('first'=c(85,3),'second'=c(88,3),'last'=c(103,3))
      X <- c(130, 137, 144)
    } else if(quadrant == '1W'){
      blockdefs <- list('first'=c(106,3),'second'=c(109,3),'last'=c(124,3))
      X <- c(151, 158, 165)
    }
    
    
    blocked <- getBlockedPLAOV(blockdefs=blockdefs, quadrant=quadrant) 
    for(group in groups){
      
      colourscheme <- getCtrlColourScheme(group=group)
      subblocked <- blocked[which(blocked$target == group),]
      blocks <- unique(subblocked$block)
      blockedconfidence <- data.frame()
      
      for(blockname in blocks){
        subdat <- subblocked[which(subblocked$block == blockname),]
        meandist <- getConfidenceInterval(data=subdat$pathlength, method='b')
        
        if (prod(dim(blockedconfidence)) == 0){
          blockedconfidence <- meandist
        } else {
          blockedconfidence <- rbind(blockedconfidence, meandist)
        }
      }
      
      lower <- blockedconfidence[,1]
      upper <- blockedconfidence[,3]
      mid <- blockedconfidence[,2]
      
      
      col <- colourscheme[[group]][['T']]
      
      
      
      polygon(x = c(X, rev(X)), y = c(lower, rev(upper)), border=NA, col=col)
      
      
      col <- colourscheme[[group]][['S']]
      
      lines(x=X,y=mid,col=col, lwd=2) #lower and upper CI
    }
  }
  
  
  #close everything if you saved plot as svg
  if (target=='svg') {
    dev.off()
  }
  
}

# plotBlockedDependentMeasures <- function(groups = c('far', 'mid', 'near'), trialtypes = c('mir', 'washout'), quadrants = c('1', '4', '2', '1A', '1L', '1W'), target='inline'){
#   
#   if (target=='svg') {
#     svglite(file='doc/fig_manuscripts/Fig9_OnlinePercentages.svg', width=9, height=11, pointsize=14, system_fonts=list(sans='Arial'))
#   }
#   
#   #par(mfrow=c(1,2), mar=c(4,4,2,0.1))
#   par(mar=c(4,4,2,0.1))
#   
#   
#   
#   #layout(matrix(c(1,2,3), nrow=1, ncol=3, byrow = TRUE), widths=c(2,2,2), heights=c(1,1))
#   #layout(matrix(c(1,1,1,2,3,4), 2, 3, byrow = TRUE), widths=c(2,2,2), heights=c(1,1))
#   layout(matrix(c(1,2,3), 3, 1, byrow = TRUE), widths=c(2), heights=c(1,1,1))
#   
#   #Percentages
#   plot(NA, NA, xlim = c(0,168), ylim = c(-200,300), 
#        xlab = "Block", ylab = "Amount of compensation (%)", frame.plot = FALSE, #frame.plot takes away borders
#        main = "", xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
#   
#   lim <- par('usr')
#   rect(127, lim[3]-1, 168, lim[4]+1, border = "#ededed", col = "#ededed") #xleft, ybottom, x right, ytop; light grey hex code
#   abline(h = c(0, 100), v = c(21, 42, 63, 84, 105, 126, 147), col = 8, lty = 2) #creates horizontal dashed lines through y =  0 and 30
#   #abline(h = c(0, 100), v = c(21), col = 8, lty = 2) #creates horizontal dashed lines through y =  0 and 30
#   
#   axis(1, at = c(4, 11, 18, 25, 32, 39, 46, 53, 60, 67, 74, 81, 88, 95, 102, 109, 116, 123, 130, 137, 144, 151, 158, 165),
#        labels = c('1', '2', '26-30', '1', '2', '7', '1', '2', '7', '1', '2', '7', '1', '2', '7', '1', '2', '7', '1', '2', '7', '1', '2', '7')) #tick marks for x axis
#   axis(2, at = c(-200,-150,-100, -50, 0, 50, 100, 150, 200), las = 2) #tick marks for y axis
#   axis.break(1, 41, style='gap', breakcol='white')
#   
#   
#   for(type in trialtypes){
#     if(type == 'mir'){
#       blockdefs <- list('first'=c(1,3),'second'=c(4,3),'last'=c(76,15))
#       blocked <- getMirrorBlockedLearningAOV(blockdefs=blockdefs) 
#       
#       groupno <- 0
#       
#       for(group in groups){
#         
#         groupno <- groupno + 1
#         colourscheme <- getCtrlColourScheme(group=group)
#         if(group == 'near'){
#           col <- colourscheme[[group]][['S']]
#           col <- alpha(col, .8)
#         } else {
#           col <- colourscheme[[group]][['S']]
#         }
#         
#         
#         subblocked <- blocked[which(blocked$target == group),]
#         blocks <- unique(subblocked$block)
#         for(blockname in blocks){
#           subdat <- subblocked[which(subblocked$block == blockname),]
#           meandist <- getConfidenceInterval(data=subdat$percentcomp, method='b')
#           
#           trialstart <- blockdefs[blockname][[1]][1] - 1
#           if(blockname == 'first'){
#             trialstart <- trialstart + 2
#           } else if (blockname == 'second'){
#             trialstart <- trialstart + 6
#           } else if (blockname == 'last'){
#             trialstart <-18-2 #hardcoded to make trial number match sets with shorter number of trials
#           }
#           
#           X <- trialstart + groupno
#           
#           lines(x=rep(X,2),y=c(meandist[[1]], meandist[[3]]),col=col) #lower and upper CI
#           points(x=X,y=meandist[[2]],pch=16,cex=1.5,col=col) #50% plotted as a point
#         }
#       }
#       
#     } else if(type == 'washout'){
#       blockdefs <- list('first'=c(1,3),'second'=c(4,3),'last'=c(19,3))
#       blocked <- getRAEBlockedPercentagesAOV(blockdefs=blockdefs) 
#       groupno <- 0
#       
#       for(group in groups){
#         
#         groupno <- groupno + 1
#         colourscheme <- getCtrlColourScheme(group=group)
#         if(group == 'near'){
#           col <- colourscheme[[group]][['S']]
#           col <- alpha(col, .8)
#         } else {
#           col <- colourscheme[[group]][['S']]
#         }
#         
#         
#         subblocked <- blocked[which(blocked$target == group),]
#         blocks <- unique(subblocked$block)
#         for(blockname in blocks){
#           subdat <- subblocked[which(subblocked$block == blockname),]
#           meandist <- getConfidenceInterval(data=subdat$percentcomp, method='b')
#           
#           trialstart <- blockdefs[blockname][[1]][1] - 1
#           if(blockname == 'first'){
#             trialstart <- trialstart + 2
#           } else if (blockname == 'second'){
#             trialstart <- trialstart + 6
#           } else if (blockname == 'last'){
#             trialstart <-18-2 #hardcoded to make trial number match sets with shorter number of trials
#           }
#           
#           X <- trialstart + groupno + 21
#           
#           lines(x=rep(X,2),y=c(meandist[[1]], meandist[[3]]),col=col) #lower and upper CI
#           points(x=X,y=meandist[[2]],pch=16,cex=1.5,col=col) #50% plotted as a point
#         }
#       }
#     }
#   }
#   
#   for(quadrant in quadrants){
#     if(quadrant == '1'){
#       blockdefs <- list('first'=c(1,3),'second'=c(4,3),'last'=c(19,3))
#     } else if(quadrant == '4'){
#       blockdefs <- list('first'=c(22,3),'second'=c(25,3),'last'=c(40,3))
#     } else if(quadrant == '2'){
#       blockdefs <- list('first'=c(43,3),'second'=c(46,3),'last'=c(61,3))
#     } else if(quadrant == '1A'){
#       blockdefs <- list('first'=c(64,3),'second'=c(67,3),'last'=c(82,3))
#     } else if(quadrant == '1L'){
#       blockdefs <- list('first'=c(85,3),'second'=c(88,3),'last'=c(103,3))
#     } else if(quadrant == '1W'){
#       blockdefs <- list('first'=c(106,3),'second'=c(109,3),'last'=c(124,3))
#     }
#     
#     
#     groupno <- 0
#     blocked <- getBlockedLearningAOV(blockdefs=blockdefs, quadrant=quadrant) 
#     for(group in groups){
#       
#       groupno <- groupno + 1
#       colourscheme <- getCtrlColourScheme(group=group)
#       if(group == 'near'){
#         col <- colourscheme[[group]][['S']]
#         col <- alpha(col, .8)
#       } else {
#         col <- colourscheme[[group]][['S']]
#       }
#       
#       subblocked <- blocked[which(blocked$target == group),]
#       blocks <- unique(subblocked$block)
#       for(blockname in blocks){
#         subdat <- subblocked[which(subblocked$block == blockname),]
#         meandist <- getConfidenceInterval(data=subdat$percentcomp, method='b')
#         
#         trialstart <- blockdefs[blockname][[1]][1] - 1
#         if(blockname == 'first'){
#           trialstart <- trialstart + 2
#         } else if (blockname == 'second'){
#           trialstart <- trialstart + 6
#         } else if (blockname == 'last'){
#           trialstart <- trialstart - 2
#         }
#         
#         X <- trialstart + groupno + 42
#         
#         lines(x=rep(X,2),y=c(meandist[[1]], meandist[[3]]),col=col) #lower and upper CI
#         points(x=X,y=meandist[[2]],pch=16,cex=1.5,col=col) #50% plotted as a point
#       }
#     }
#   }
#   
#   
#   #Completion time
#   plot(NA, NA, xlim = c(0,168), ylim = c(0,12), 
#        xlab = "Block", ylab = "Completion time (s)", frame.plot = FALSE, #frame.plot takes away borders
#        main = "", xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
#   
#   lim <- par('usr')
#   rect(127, lim[3]-1, 168, lim[4]+1, border = "#ededed", col = "#ededed") #xleft, ybottom, x right, ytop; light grey hex code
#   abline(h = c(0, 100), v = c(21, 42, 63, 84, 105, 126, 147), col = 8, lty = 2) #creates horizontal dashed lines through y =  0 and 30
#   #abline(h = c(0, 100), v = c(21), col = 8, lty = 2) #creates horizontal dashed lines through y =  0 and 30
#   
#   axis(1, at = c(4, 11, 18, 25, 32, 39, 46, 53, 60, 67, 74, 81, 88, 95, 102, 109, 116, 123, 130, 137, 144, 151, 158, 165),
#        labels = c('1', '2', '26-30', '1', '2', '7', '1', '2', '7', '1', '2', '7', '1', '2', '7', '1', '2', '7', '1', '2', '7', '1', '2', '7')) #tick marks for x axis
#   axis(2, at = c(0, 2, 4, 6, 8, 10, 12), las = 2) #tick marks for y axis
#   axis.break(1, 41, style='gap', breakcol='white')
#   
#   
#   for(type in trialtypes){
#     if(type == 'mir'){
#       blockdefs <- list('first'=c(67,3),'second'=c(70,3),'last'=c(142,15))
#       blocked <- getAlignedBlockedMTAOV(blockdefs=blockdefs, hand='trained') 
#       
#       groupno <- 0
#       
#       for(group in groups){
#         
#         groupno <- groupno + 1
#         colourscheme <- getCtrlColourScheme(group=group)
#         if(group == 'near'){
#           col <- colourscheme[[group]][['S']]
#           col <- alpha(col, .8)
#         } else {
#           col <- colourscheme[[group]][['S']]
#         }
#         
#         
#         subblocked <- blocked[which(blocked$target == group),]
#         blocks <- unique(subblocked$block)
#         for(blockname in blocks){
#           subdat <- subblocked[which(subblocked$block == blockname),]
#           meandist <- getConfidenceInterval(data=subdat$movementtime, method='b')
#           
#           trialstart <- blockdefs[blockname][[1]][1] - 1
#           if(blockname == 'first'){
#             trialstart <- 0 + 2
#           } else if (blockname == 'second'){
#             trialstart <- 3 + 6
#           } else if (blockname == 'last'){
#             trialstart <-18-2 #hardcoded to make trial number match sets with shorter number of trials
#           }
#           
#           X <- trialstart + groupno
#           
#           lines(x=rep(X,2),y=c(meandist[[1]], meandist[[3]]),col=col) #lower and upper CI
#           points(x=X,y=meandist[[2]],pch=16,cex=1.5,col=col) #50% plotted as a point
#         }
#       }
#       
#     } else if(type == 'washout'){
#       blockdefs <- list('first'=c(157,3),'second'=c(160,3),'last'=c(175,3))
#       blocked <- getAlignedBlockedMTAOV(blockdefs=blockdefs, hand='trained')
#       groupno <- 0
#       
#       for(group in groups){
#         
#         groupno <- groupno + 1
#         colourscheme <- getCtrlColourScheme(group=group)
#         if(group == 'near'){
#           col <- colourscheme[[group]][['S']]
#           col <- alpha(col, .8)
#         } else {
#           col <- colourscheme[[group]][['S']]
#         }
#         
#         
#         subblocked <- blocked[which(blocked$target == group),]
#         blocks <- unique(subblocked$block)
#         for(blockname in blocks){
#           subdat <- subblocked[which(subblocked$block == blockname),]
#           meandist <- getConfidenceInterval(data=subdat$movementtime, method='b')
#           
#           trialstart <- blockdefs[blockname][[1]][1] - 1
#           if(blockname == 'first'){
#             trialstart <- 0 + 2
#           } else if (blockname == 'second'){
#             trialstart <- 3 + 6
#           } else if (blockname == 'last'){
#             trialstart <-18-2 #hardcoded to make trial number match sets with shorter number of trials
#           }
#           
#           X <- trialstart + groupno + 21
#           
#           lines(x=rep(X,2),y=c(meandist[[1]], meandist[[3]]),col=col) #lower and upper CI
#           points(x=X,y=meandist[[2]],pch=16,cex=1.5,col=col) #50% plotted as a point
#         }
#       }
#     }
#   }
#   
#   for(quadrant in quadrants){
#     if(quadrant == '1'){
#       blockdefs <- list('first'=c(1,3),'second'=c(4,3),'last'=c(19,3))
#     } else if(quadrant == '4'){
#       blockdefs <- list('first'=c(22,3),'second'=c(25,3),'last'=c(40,3))
#     } else if(quadrant == '2'){
#       blockdefs <- list('first'=c(43,3),'second'=c(46,3),'last'=c(61,3))
#     } else if(quadrant == '1A'){
#       blockdefs <- list('first'=c(64,3),'second'=c(67,3),'last'=c(82,3))
#     } else if(quadrant == '1L'){
#       blockdefs <- list('first'=c(85,3),'second'=c(88,3),'last'=c(103,3))
#     } else if(quadrant == '1W'){
#       blockdefs <- list('first'=c(106,3),'second'=c(109,3),'last'=c(124,3))
#     }
#     
#     
#     groupno <- 0
#     blocked <- getBlockedMTAOV(blockdefs=blockdefs, quadrant=quadrant) 
#     for(group in groups){
#       
#       groupno <- groupno + 1
#       colourscheme <- getCtrlColourScheme(group=group)
#       if(group == 'near'){
#         col <- colourscheme[[group]][['S']]
#         col <- alpha(col, .8)
#       } else {
#         col <- colourscheme[[group]][['S']]
#       }
#       
#       subblocked <- blocked[which(blocked$target == group),]
#       blocks <- unique(subblocked$block)
#       for(blockname in blocks){
#         subdat <- subblocked[which(subblocked$block == blockname),]
#         meandist <- getConfidenceInterval(data=subdat$movementtime, method='b')
#         
#         trialstart <- blockdefs[blockname][[1]][1] - 1
#         if(blockname == 'first'){
#           trialstart <- trialstart + 2
#         } else if (blockname == 'second'){
#           trialstart <- trialstart + 6
#         } else if (blockname == 'last'){
#           trialstart <- trialstart - 2
#         }
#         
#         X <- trialstart + groupno + 42
#         
#         lines(x=rep(X,2),y=c(meandist[[1]], meandist[[3]]),col=col) #lower and upper CI
#         points(x=X,y=meandist[[2]],pch=16,cex=1.5,col=col) #50% plotted as a point
#       }
#     }
#   }
#   
#   #Path length
#   plot(NA, NA, xlim = c(0,168), ylim = c(0.4,3.2), 
#        xlab = "Block", ylab = "path length (monitor scale)", frame.plot = FALSE, #frame.plot takes away borders
#        main = "", xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
#   
#   lim <- par('usr')
#   rect(127, lim[3]-1, 168, lim[4]+1, border = "#ededed", col = "#ededed") #xleft, ybottom, x right, ytop; light grey hex code
#   abline(h = c(0.4), v = c(21, 42, 63, 84, 105, 126, 147), col = 8, lty = 2) #creates horizontal dashed lines through y =  0 and 30
#   #abline(h = c(0, 100), v = c(21), col = 8, lty = 2) #creates horizontal dashed lines through y =  0 and 30
#   
#   axis(1, at = c(4, 11, 18, 25, 32, 39, 46, 53, 60, 67, 74, 81, 88, 95, 102, 109, 116, 123, 130, 137, 144, 151, 158, 165),
#        labels = c('1', '2', '26-30', '1', '2', '7', '1', '2', '7', '1', '2', '7', '1', '2', '7', '1', '2', '7', '1', '2', '7', '1', '2', '7')) #tick marks for x axis
#   axis(2, at = c(.4, .8, 1.2, 1.6, 2, 2.4, 2.8, 3.2), las = 2) #tick marks for y axis
#   axis.break(1, 41, style='gap', breakcol='white')
#   
#   
#   for(type in trialtypes){
#     if(type == 'mir'){
#       blockdefs <- list('first'=c(67,3),'second'=c(70,3),'last'=c(142,15))
#       blocked <- getAlignedBlockedPLAOV(blockdefs=blockdefs, hand='trained') 
#       
#       groupno <- 0
#       
#       for(group in groups){
#         
#         groupno <- groupno + 1
#         colourscheme <- getCtrlColourScheme(group=group)
#         if(group == 'near'){
#           col <- colourscheme[[group]][['S']]
#           col <- alpha(col, .8)
#         } else {
#           col <- colourscheme[[group]][['S']]
#         }
#         
#         
#         subblocked <- blocked[which(blocked$target == group),]
#         blocks <- unique(subblocked$block)
#         for(blockname in blocks){
#           subdat <- subblocked[which(subblocked$block == blockname),]
#           meandist <- getConfidenceInterval(data=subdat$pathlength, method='b')
#           
#           trialstart <- blockdefs[blockname][[1]][1] - 1
#           if(blockname == 'first'){
#             trialstart <- 0 + 2
#           } else if (blockname == 'second'){
#             trialstart <- 3 + 6
#           } else if (blockname == 'last'){
#             trialstart <-18-2 #hardcoded to make trial number match sets with shorter number of trials
#           }
#           
#           X <- trialstart + groupno
#           
#           lines(x=rep(X,2),y=c(meandist[[1]], meandist[[3]]),col=col) #lower and upper CI
#           points(x=X,y=meandist[[2]],pch=16,cex=1.5,col=col) #50% plotted as a point
#         }
#       }
#       
#     } else if(type == 'washout'){
#       blockdefs <- list('first'=c(157,3),'second'=c(160,3),'last'=c(175,3))
#       blocked <- getAlignedBlockedPLAOV(blockdefs=blockdefs, hand='trained')
#       groupno <- 0
#       
#       for(group in groups){
#         
#         groupno <- groupno + 1
#         colourscheme <- getCtrlColourScheme(group=group)
#         if(group == 'near'){
#           col <- colourscheme[[group]][['S']]
#           col <- alpha(col, .8)
#         } else {
#           col <- colourscheme[[group]][['S']]
#         }
#         
#         
#         subblocked <- blocked[which(blocked$target == group),]
#         blocks <- unique(subblocked$block)
#         for(blockname in blocks){
#           subdat <- subblocked[which(subblocked$block == blockname),]
#           meandist <- getConfidenceInterval(data=subdat$pathlength, method='b')
#           
#           trialstart <- blockdefs[blockname][[1]][1] - 1
#           if(blockname == 'first'){
#             trialstart <- 0 + 2
#           } else if (blockname == 'second'){
#             trialstart <- 3 + 6
#           } else if (blockname == 'last'){
#             trialstart <-18-2 #hardcoded to make trial number match sets with shorter number of trials
#           }
#           
#           X <- trialstart + groupno + 21
#           
#           lines(x=rep(X,2),y=c(meandist[[1]], meandist[[3]]),col=col) #lower and upper CI
#           points(x=X,y=meandist[[2]],pch=16,cex=1.5,col=col) #50% plotted as a point
#         }
#       }
#     }
#   }
#   
#   for(quadrant in quadrants){
#     if(quadrant == '1'){
#       blockdefs <- list('first'=c(1,3),'second'=c(4,3),'last'=c(19,3))
#     } else if(quadrant == '4'){
#       blockdefs <- list('first'=c(22,3),'second'=c(25,3),'last'=c(40,3))
#     } else if(quadrant == '2'){
#       blockdefs <- list('first'=c(43,3),'second'=c(46,3),'last'=c(61,3))
#     } else if(quadrant == '1A'){
#       blockdefs <- list('first'=c(64,3),'second'=c(67,3),'last'=c(82,3))
#     } else if(quadrant == '1L'){
#       blockdefs <- list('first'=c(85,3),'second'=c(88,3),'last'=c(103,3))
#     } else if(quadrant == '1W'){
#       blockdefs <- list('first'=c(106,3),'second'=c(109,3),'last'=c(124,3))
#     }
#     
#     
#     groupno <- 0
#     blocked <- getBlockedPLAOV(blockdefs=blockdefs, quadrant=quadrant) 
#     for(group in groups){
#       
#       groupno <- groupno + 1
#       colourscheme <- getCtrlColourScheme(group=group)
#       if(group == 'near'){
#         col <- colourscheme[[group]][['S']]
#         col <- alpha(col, .8)
#       } else {
#         col <- colourscheme[[group]][['S']]
#       }
#       
#       subblocked <- blocked[which(blocked$target == group),]
#       blocks <- unique(subblocked$block)
#       for(blockname in blocks){
#         subdat <- subblocked[which(subblocked$block == blockname),]
#         meandist <- getConfidenceInterval(data=subdat$pathlength, method='b')
#         
#         trialstart <- blockdefs[blockname][[1]][1] - 1
#         if(blockname == 'first'){
#           trialstart <- trialstart + 2
#         } else if (blockname == 'second'){
#           trialstart <- trialstart + 6
#         } else if (blockname == 'last'){
#           trialstart <- trialstart - 2
#         }
#         
#         X <- trialstart + groupno + 42
#         
#         lines(x=rep(X,2),y=c(meandist[[1]], meandist[[3]]),col=col) #lower and upper CI
#         points(x=X,y=meandist[[2]],pch=16,cex=1.5,col=col) #50% plotted as a point
#       }
#     }
#   }
#   
#   
#   #close everything if you saved plot as svg
#   if (target=='svg') {
#     dev.off()
#   }
#   
# }

# plotBlockedOnlinePercentages <- function(groups = c('far', 'mid', 'near'), trialtypes = c('mir', 'washout'), quadrants = c('1', '4', '2', '1A', '1L', '1W'), target='inline'){
#   
#   if (target=='svg') {
#     svglite(file='doc/fig_manuscripts/Fig9_OnlinePercentages.svg', width=14, height=10, pointsize=14, system_fonts=list(sans='Arial'))
#   }
#   
#   # create plot
#   #meanGroupReaches <- list() #empty list so that it plots the means last
#   
#   #NA to create empty plot
#   # could maybe use plot.new() ?
#   plot(NA, NA, xlim = c(0,168), ylim = c(-200,300), 
#        xlab = "Block", ylab = "Amount of compensation (%)", frame.plot = FALSE, #frame.plot takes away borders
#        main = "", xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
#   
#   lim <- par('usr')
#   rect(127, lim[3]-1, 168, lim[4]+1, border = "#ededed", col = "#ededed") #xleft, ybottom, x right, ytop; light grey hex code
#   abline(h = c(0, 100), v = c(21, 42, 63, 84, 105, 126, 147), col = 8, lty = 2) #creates horizontal dashed lines through y =  0 and 30
#   #abline(h = c(0, 100), v = c(21), col = 8, lty = 2) #creates horizontal dashed lines through y =  0 and 30
#   
#   axis(1, at = c(4, 11, 18, 25, 32, 39, 46, 53, 60, 67, 74, 81, 88, 95, 102, 109, 116, 123, 130, 137, 144, 151, 158, 165),
#        labels = c('1', '2', '26-30', '1', '2', '7', '1', '2', '7', '1', '2', '7', '1', '2', '7', '1', '2', '7', '1', '2', '7', '1', '2', '7')) #tick marks for x axis
#   axis(2, at = c(-200,-150,-100, -50, 0, 50, 100, 150, 200), las = 2) #tick marks for y axis
#   axis.break(1, 41, style='gap', breakcol='white')
#   
#   
#   for(type in trialtypes){
#     if(type == 'mir'){
#       blockdefs <- list('first'=c(1,3),'second'=c(4,3),'last'=c(76,15))
#       blocked <- getMirrorBlockedLearningAOV(blockdefs=blockdefs) 
#       
#       groupno <- 0
#       
#       for(group in groups){
#         
#         groupno <- groupno + 1
#         colourscheme <- getCtrlColourScheme(group=group)
#         if(group == 'near'){
#           col <- colourscheme[[group]][['T']]
#         } else {
#           col <- colourscheme[[group]][['S']]
#         }
#         
#         
#         subblocked <- blocked[which(blocked$target == group),]
#         blocks <- unique(subblocked$block)
#         for(blockname in blocks){
#           subdat <- subblocked[which(subblocked$block == blockname),]
#           meandist <- getConfidenceInterval(data=subdat$percentcomp, method='b')
#           
#           trialstart <- blockdefs[blockname][[1]][1] - 1
#           if(blockname == 'first'){
#             trialstart <- trialstart + 2
#           } else if (blockname == 'second'){
#             trialstart <- trialstart + 6
#           } else if (blockname == 'last'){
#             trialstart <-18-2 #hardcoded to make trial number match sets with shorter number of trials
#           }
#           
#           X <- trialstart + groupno
#           
#           lines(x=rep(X,2),y=c(meandist[[1]], meandist[[3]]),col=col) #lower and upper CI
#           points(x=X,y=meandist[[2]],pch=16,cex=1.5,col=col) #50% plotted as a point
#         }
#       }
#       
#     } else if(type == 'washout'){
#       blockdefs <- list('first'=c(1,3),'second'=c(4,3),'last'=c(19,3))
#       blocked <- getRAEBlockedPercentagesAOV(blockdefs=blockdefs) 
#       groupno <- 0
#       
#       for(group in groups){
#         
#         groupno <- groupno + 1
#         colourscheme <- getCtrlColourScheme(group=group)
#         if(group == 'near'){
#           col <- colourscheme[[group]][['T']]
#         } else {
#           col <- colourscheme[[group]][['S']]
#         }
#         
#         
#         subblocked <- blocked[which(blocked$target == group),]
#         blocks <- unique(subblocked$block)
#         for(blockname in blocks){
#           subdat <- subblocked[which(subblocked$block == blockname),]
#           meandist <- getConfidenceInterval(data=subdat$percentcomp, method='b')
#           
#           trialstart <- blockdefs[blockname][[1]][1] - 1
#           if(blockname == 'first'){
#             trialstart <- trialstart + 2
#           } else if (blockname == 'second'){
#             trialstart <- trialstart + 6
#           } else if (blockname == 'last'){
#             trialstart <-18-2 #hardcoded to make trial number match sets with shorter number of trials
#           }
#           
#           X <- trialstart + groupno + 21
#           
#           lines(x=rep(X,2),y=c(meandist[[1]], meandist[[3]]),col=col) #lower and upper CI
#           points(x=X,y=meandist[[2]],pch=16,cex=1.5,col=col) #50% plotted as a point
#         }
#       }
#     }
#   }
#   
#   for(quadrant in quadrants){
#     if(quadrant == '1'){
#       blockdefs <- list('first'=c(1,3),'second'=c(4,3),'last'=c(19,3))
#     } else if(quadrant == '4'){
#       blockdefs <- list('first'=c(22,3),'second'=c(25,3),'last'=c(40,3))
#     } else if(quadrant == '2'){
#       blockdefs <- list('first'=c(43,3),'second'=c(46,3),'last'=c(61,3))
#     } else if(quadrant == '1A'){
#       blockdefs <- list('first'=c(64,3),'second'=c(67,3),'last'=c(82,3))
#     } else if(quadrant == '1L'){
#       blockdefs <- list('first'=c(85,3),'second'=c(88,3),'last'=c(103,3))
#     } else if(quadrant == '1W'){
#       blockdefs <- list('first'=c(106,3),'second'=c(109,3),'last'=c(124,3))
#     }
#     
#     
#     groupno <- 0
#     blocked <- getBlockedLearningAOV(blockdefs=blockdefs, quadrant=quadrant) 
#     for(group in groups){
#       
#       groupno <- groupno + 1
#       colourscheme <- getCtrlColourScheme(group=group)
#       if(group == 'near'){
#         col <- colourscheme[[group]][['T']]
#       } else {
#         col <- colourscheme[[group]][['S']]
#       }
#       
#       subblocked <- blocked[which(blocked$target == group),]
#       blocks <- unique(subblocked$block)
#       for(blockname in blocks){
#         subdat <- subblocked[which(subblocked$block == blockname),]
#         meandist <- getConfidenceInterval(data=subdat$percentcomp, method='b')
#         
#         trialstart <- blockdefs[blockname][[1]][1] - 1
#         if(blockname == 'first'){
#           trialstart <- trialstart + 2
#         } else if (blockname == 'second'){
#           trialstart <- trialstart + 6
#         } else if (blockname == 'last'){
#           trialstart <- trialstart - 2
#         }
#         
#         X <- trialstart + groupno + 42
#         
#         lines(x=rep(X,2),y=c(meandist[[1]], meandist[[3]]),col=col) #lower and upper CI
#         points(x=X,y=meandist[[2]],pch=16,cex=1.5,col=col) #50% plotted as a point
#       }
#     }
#   }
#   
#   #close everything if you saved plot as svg
#   if (target=='svg') {
#     dev.off()
#   }
#   
# }