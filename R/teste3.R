plotObsPrevMensal= function(obs,prev,xlim,ylim,tit=''){
  
  par(default_plot_parameters)
  
  l.y=min(c(obs[,2],prev), na.rm=T)
  u.y=max(c(obs[,2],prev), na.rm=T)
  
  prev.xts <- prev
  obs.xts <- obs[,2]
  
  obs$date <- as.Date(paste0(substr(obs$am,1,4),'-',substr(obs$am,5,6),'-01'))
  
  names(obs.xts) <- obs$date
  names(prev.xts) <- obs$date
  
  obs.xts <- as.xts(as.data.frame(obs.xts))
  prev.xts <- as.xts(as.data.frame(prev.xts))
  
  primeira.abline <- nrow(obs.xts) - 12
  segunda.abline <- nrow(obs.xts) - 1
  
  if(is.null(ylim))
  {
    ylim <- c(l.y,u.y)
  }
  
  
  par(col='dodgerblue4')
  plot.xts(obs.xts, type='b', pch=21, cex=0.8,bg='cornflowerblue',ylab='Quantidade Mensal',main=tit,
           ylim=ylim,xlim=xlim, minor.ticks = TRUE, major.ticks="months")
  #graphics::box(col='black')
  
  abline(v=as.POSIXct.Date(as.Date(index(obs.xts)[primeira.abline])), col='dark green')
  abline(v=as.POSIXct.Date(as.Date(index(obs.xts)[segunda.abline])), col='dark green')
  
  par(col='black')
  lines(prev.xts, t='b',pch=18,col='brown3',cex=0.9, lty=5)
  # par(mar=c(0,0,0,0))
  # plot.new()
  # legend('center',c("Observado","Previsto"), lty = c(1,5),
  #        col=c('cornflowerblue','brown3'),ncol=2,bty ="n", cex=1.1,lwd=1.9)
  # 
  return(recordPlot())
}
