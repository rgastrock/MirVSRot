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