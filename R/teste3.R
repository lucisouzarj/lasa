plotObsPrevMensal= function(obs, pred){
  
  l.y=min(c(obs[,2],pred), na.rm=T)
  u.y=max(c(obs[,2],pred), na.rm=T)
  
  plot(obs[,2], t="b", pch=21, bg="cornflowerblue", col='dodgerblue4', xaxt='n', xlab="", cex=0.8, 
       ylab="quantidade", main="Quantidade prevista x observada", ylim=c(l.y,u.y))
  lines(pred, t="b", pch=18, col="brown3", cex=0.9, lty=5)
  abline(v=(dim(obs)[1]-12), col=3)
  abline(v=(dim(obs)[1]-1), col="brown")
  axt=round(seq(1,nrow(obs),length.out=20),0)
  axis(1, at=axt, labels=obs[axt,1], las=2 , cex.axis = 0.7)
  
}
