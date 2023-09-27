source('ana/shared.R')
source('ana/learningRates.R')
source('ana/exponentialandstepModel.R')
source('ana/rae.R')

plotIndividualRates <- function(group='noninstructed', location = 'maxvel'){
  pdf(file='doc/fig/pilot/CheckData1_MIR_LearningandWashout.pdf', width=12, height=7, pointsize=14)
  maxppid <- 15
  for(id in c(0:maxppid)){
    data <- getMIRParticipantLearningCurve(group=group, id = id, location = location)
    LClasttrial <- data[nrow(data),]
    
    data1 <- getMIRParticipantAftereffects(group=group, id = id, location = location)
    RAEfirsttrial <- data1[1,]
    par_expl <- exponentialFit(signal = data1$reachdev, mode='washout')
    asymptote <- par_expl[['N0']]
    
    par(mfrow = c(1,2))
    
    #plot learning and washout curves
    plot(NA, NA, xlim = c(0,139), ylim = c(-200,200), 
         xlab = "Trial", ylab = "Amount of Compensation (%)", frame.plot = FALSE, #frame.plot takes away borders
         main = sprintf("MIR: pp%03d, Learning and Washout", id), xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
    abline(h = c(-100,0, 100), col = 8, lty = 2) #creates horizontal dashed lines through y =  0 and 30
    axis(1, at = c(0, 89), labels = c('0', '89')) #tick marks for x axis
    axis(1, at = c(90, 138), labels = c('', '47')) #tick marks for x axis
    axis(2, at = c(-200, -100, 0, 100, 200)) #tick marks for y axis
    
    x <- c(0:89)
    col <- '#ff8200ff'
      lines(x, data$reachdev, lty=1, col=col, lwd=2)
      
      x <- c(90:137)
      col <- '#c400c4ff'
        lines(x, data1$reachdev, lty=1, col=col, lwd=2)
        
        #plot the circle
        if(id == 0 | id == 1 | id == 8 | id == 9){
          target <- as.circular(c(67.5, 75, 82.5, 
                                  247.5, 255, 262.5), 
                                type='angles', units='degrees', template = 'none', modulo = 'asis', zero = 0, rotation = 'counter')
          
          perfreach <- as.circular(c(97.5, 105, 112.5, 
                                     277.5, 285, 292.5), 
                                   type='angles', units='degrees', template = 'none', modulo = 'asis', zero = 0, rotation = 'counter')
        } else if (id == 2 | id == 3 | id == 10 | id == 11){
          target <- as.circular(c(97.5, 105, 112.5, 
                                  277.5, 285, 292.5), 
                                type='angles', units='degrees', template = 'none', modulo = 'asis', zero = 0, rotation = 'counter')
          perfreach <- as.circular(c(67.5, 75, 82.5, 
                                     247.5, 255, 262.5), 
                                   type='angles', units='degrees', template = 'none', modulo = 'asis', zero = 0, rotation = 'counter')
        } else if (id == 4 | id == 5 | id == 12 | id == 13){
          target <- as.circular(c(157.5, 165, 172.5, 
                                  337.5, 345, 352.5), 
                                type='angles', units='degrees', template = 'none', modulo = 'asis', zero = 0, rotation = 'counter')
          
          perfreach <- as.circular(c(7.5, 15, 22.5, 
                                     187.5, 195, 202.5), 
                                   type='angles', units='degrees', template = 'none', modulo = 'asis', zero = 0, rotation = 'counter')
          
        } else if (id == 6 | id == 7 | id == 14 | id == 15){
          target <- as.circular(c(7.5, 15, 22.5, 
                                  187.5, 195, 202.5), 
                                type='angles', units='degrees', template = 'none', modulo = 'asis', zero = 0, rotation = 'counter')
          perfreach <- as.circular(c(157.5, 165, 172.5, 
                                     337.5, 345, 352.5), 
                                   type='angles', units='degrees', template = 'none', modulo = 'asis', zero = 0, rotation = 'counter')
        }
        
        alltargetsbef <- c(67.5, 75, 82.5,
                           157.5, 165, 172.5,
                           247.5, 255, 262.5,
                           337.5, 345, 352.5) #should compensate for 30 degrees
        alltargetsaft <- c(7.5, 15, 22.5,
                           97.5, 105, 112.5,
                           187.5, 195, 202.5,
                           277.5, 285, 292.5) #compensate 30 degrees
        
        plot(target, shrink = 1.5, tcl.text=0.25)
        arrows.circular(perfreach, length = 0, angle = 0, col = '#A9A9A9ff')
        LCangle <- LClasttrial$targetangle
        if(LCangle %in% alltargetsbef){
          LCcomp <- LCangle + LClasttrial$compensate
          LCcompunder <- LCangle - LClasttrial$compensate
          LCreachdev <- ((LClasttrial$reachdev/100)*LClasttrial$compensate) + LCangle
        } else if (LCangle %in% alltargetsaft){
          LCcomp <- LCangle - LClasttrial$compensate
          LCcompunder <- LCangle + LClasttrial$compensate
          LCreachdev <- (((LClasttrial$reachdev/100)*LClasttrial$compensate)*-1) + LCangle
        }
        LCangle <- as.circular(LCangle, type='angles', units='degrees', template = 'none', modulo = 'asis', zero = 0, rotation = 'counter')
        LCcomp <- as.circular(LCcomp, type='angles', units='degrees', template = 'none', modulo = 'asis', zero = 0, rotation = 'counter')
        LCcompunder <- as.circular(LCcompunder, type='angles', units='degrees', template = 'none', modulo = 'asis', zero = 0, rotation = 'counter')
        LCreachdev <- as.circular(LCreachdev, type='angles', units='degrees', template = 'none', modulo = 'asis', zero = 0, rotation = 'counter')
        
        points.circular(LCangle, col='#FF0000')
        arrows.circular(LCcomp, length = 0, angle = 0, col='#FF0000', lwd=2, lty=1)
        arrows.circular(LCcompunder, length = 0, angle = 0, col='#FF0000', lwd=2, lty=2)
        arrows.circular(LCreachdev, length = 0, angle = 0, col='#ff8200ff', lwd=2, lty=5)
        
        RAEangle <- RAEfirsttrial$targetangle
        if(RAEangle %in% alltargetsbef){
          RAEcomp <- RAEangle + RAEfirsttrial$compensate
          RAEcompunder <- RAEangle - RAEfirsttrial$compensate
          RAEreachdev <- ((asymptote/100)*RAEfirsttrial$compensate) + RAEangle
        } else if (RAEangle %in% alltargetsaft){
          RAEcomp <- RAEangle - RAEfirsttrial$compensate
          RAEcompunder <- RAEangle + RAEfirsttrial$compensate
          RAEreachdev <- (((asymptote/100)*RAEfirsttrial$compensate)*-1) + RAEangle
        }
        RAEangle <- as.circular(RAEangle, type='angles', units='degrees', template = 'none', modulo = 'asis', zero = 0, rotation = 'counter')
        RAEcomp <- as.circular(RAEcomp, type='angles', units='degrees', template = 'none', modulo = 'asis', zero = 0, rotation = 'counter')
        RAEcompunder <- as.circular(RAEcompunder, type='angles', units='degrees', template = 'none', modulo = 'asis', zero = 0, rotation = 'counter')
        RAEreachdev <- as.circular(RAEreachdev, type='angles', units='degrees', template = 'none', modulo = 'asis', zero = 0, rotation = 'counter')
        
        points.circular(RAEangle, col='#005de4ff')
        #arrows.circular(RAEangle, length = 0, angle = 0, col='#005de4ff', lwd=2, lty=2)
        arrows.circular(RAEcomp, length = 0, angle = 0, col='#005de4ff', lwd=2, lty=1)
        arrows.circular(RAEcompunder, length = 0, angle = 0, col='#005de4ff', lwd=2, lty=2)
        arrows.circular(RAEreachdev, length = 0, angle = 0, col='#c400c4ff', lwd=2, lty=5)
        
        legend(-1,-1,legend=c('Learning: 100% compensation','Learning: -100% compensation', 'Learning: Last trial reach deviation', 'RAE: 100% compensation during learning','RAE: -100% compensation during learning', 'RAE: asymptote parameter as reach deviation'),
               col=c('#FF0000','#FF0000', '#ff8200ff', '#005de4ff', '#005de4ff', '#c400c4ff'),
               lty=c(1,2,3,1,2,3),lwd = c(2,2,2,2,2,2), bty='n',cex=0.65)
        
  }
  dev.off()
}


plotIndividualMeasures <- function(group='noninstructed', location = 'maxvel'){
  pdf(file='doc/fig/pilot/CheckData2_MIR_LearningMeasures.pdf', width=12, height=7, pointsize=14)
  maxppid <- 15
  for(id in c(0:maxppid)){
    data <- getMIRParticipantLearningCurve(group=group, id = id, location = location)
    
    #plot learning compensation
    plot(NA, NA, xlim = c(0,91), ylim = c(-250,250), 
         xlab = "Trial", ylab = "Amount of Compensation (%)", frame.plot = FALSE, #frame.plot takes away borders
         main = sprintf("MIR: pp%03d, Compensation", id), xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
    abline(h = c(-100,0, 100), col = 8, lty = 2) #creates horizontal dashed lines through y =  0 and 30
    axis(1, at = c(1, 45, 90), labels = c('1', '45', '90')) #tick marks for x axis
    #axis(1, at = c(90, 138), labels = c('', '47')) #tick marks for x axis
    axis(2, at = c(-200, -100, 0, 100, 200)) #tick marks for y axis
    
    for(triali in 1:nrow(data)){
      subdat <- data[triali,]
      if(subdat$compensate == 15){
        col <- '#ff8200ff'
        points(triali, subdat$reachdev, pch=19, col=col)
      } else if (subdat$compensate == 30){
        col <- '#e51636ff'
        points(triali, subdat$reachdev, pch=19, col=col)
      } else if (subdat$compensate == 45){
        col <- '#c400c4ff'
        points(triali, subdat$reachdev, pch=19, col=col)
      }
    }
    lines(c(1:90), data$reachdev, lty=1, col='grey', lwd=1)
    
    
    legend(80,250,legend=c('target 7.5° away','target 14.5° away', 'target 22.5.5° away'),
           col=c('#ff8200ff','#FF0000', '#c400c4ff'),
           pch=19, bty='n',cex=0.85)
    
  }
  dev.off()
}
