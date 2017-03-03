plotObsPrevDiario <- function(obs, pred, lb=0, ub=0, begin, end, tit, lastInSampledate, dat_fer=NULL, col_fer="gray", dat_prom=NULL, col_prom="lightcoral"){
  
  l.t=as.Date(begin)#(min(names(obs)))
  u.t=as.Date(end)#(max(names(obs)))
  
  per.t=seq(l.t, u.t, length.out = (u.t-l.t+1) ) #datas dentro do intervalo selecionado
  
  #min.date=min(l.t,as.Date(names(obs)))
  #max.date=max(u.t,as.Date(names(obs)))
  #per.t=seq(min.date, max.date, length.out = (max.date - min.date +1) )
  
  ind.per.t=which(as.Date(names(obs)) >= l.t & as.Date(names(obs)) <= u.t) #indice da sÃ©rie
  
  l.y=min(c(obs[ind.per.t], pred[ind.per.t], lb[ind.per.t], ub[ind.per.t]), na.rm = T)*0.95
  u.y=max(c(obs[ind.per.t], pred[ind.per.t], lb[ind.per.t], ub[ind.per.t]), na.rm = T)*1.05
  
  w.day.min=as.POSIXlt(begin)$wday #Ã­ndice do dia da semana
  l.t2=as.Date(begin)+7-as.numeric(w.day.min) #primeiro domingo apÃ³s data inicial
  tlab=seq(as.Date(l.t2), as.Date(u.t), by="week") #datas dos domingos da sÃ©rie
  
  plot(per.t, obs[ind.per.t], t="l", ylim=c(l.y,u.y), xaxt="n", xlab="", ylab="",
       main=paste(tit,sep=""), col="dodgerblue4")
  lines(per.t, pred[ind.per.t], col="brown3", lty=5) #col="dodgerblue4"
  grid(nx=NA,ny=NULL)
  
  tam.serie=length(pred[ind.per.t])
  ub=c(rep(NA,tam.serie-length(ub)),ub)
  lb=c(rep(NA,tam.serie-length(lb)),lb)
  
  lines(per.t, lb, col="grey", lty=4) #col="dodgerblue4"
  lines(per.t, ub, col="grey", lty=4) #col="dodgerblue4"
  
  axis(1, at=tlab, labels=FALSE)
  text(x=tlab, y=par()$usr[3]-0.06*(par()$usr[4]-par()$usr[3]),
       labels=tlab, srt=90, adj=1, xpd=TRUE, cex=0.7)
  # legend((u.t-(u.t-l.t-0.7*as.numeric(u.t-l.t))),ceiling(0.34*(u.y-l.y)+l.y), 
  #        c("SÃ©rie observada", "PrevisÃ£o Reg Simples"), col = c("black", "red"),
  #        text.col = "black", lty = c(1, 1), pch = c( '_', '_'), bty = "n", cex=0.8)
  
  abline(v=as.Date(lastInSampledate), col=3)#fim amostra de ajuste do modelo
  
  if (is.null(dat_prom)==F) abline(v=as.Date(dat_prom), col=col_prom) #fim amostra de ajuste do modelo
  
  if (is.null(dat_fer)==F) abline(v=as.Date(dat_fer), col=col_fer) #fim amostra de ajuste do modelo

}
