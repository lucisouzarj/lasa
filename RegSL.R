library(glmnet)

RegSL<-function(y,x,lagy=0,lagx=0,ini=0,out.of.sample=0)
  {
  
  Ntimes=nrow(x)
  if (ini==0) {ini=1+lagy} else {ini=31}
  lim=Ntimes-out.of.sample
  indIN=ini:lim #indice amostra in
  if (lim < Ntimes) {indOUT=(lim+1):Ntimes}#indice amostra out #lim=Ntimes se não há amostra ofs
  
  nExpl=ncol(x)
  
  lags.y=c()
    if (lagy!=0){
      if (lagy==1) {
        lags1.y=c( matrix(rep(NA, lagy^2 ), nrow=lagy ), embed( y, (lagy + 1) )[, 2: (lagy+1) ] )
        lags.y=lags1.y
      }else { 
        lags.y=rbind( matrix(rep(NA, lagy^2 ), nrow=lagy ), embed( y, (lagy + 1) )[, 2: (lagy+1) ] )
        colnames(lags.y)=paste0(rep("lag",lagy),1:lagy,rep(".y",lagy))
      }
    }
  
  lags.x=c()
    if (lagx!=0){
      lags.x=rbind( matrix(rep(NA,nExpl*lagx^2), nrow=lagx ), embed( x, (lagx + 1) )[, (nExpl+1) : (nExpl*(lagx+1)) ] )
      colnames(lags.x)=paste0(rep("lag",lagx*nExpl),rep(1:lagx,each=nExpl),rep(".",lagx*nExpl),rep(colnames(x),lagx))
    }
  
  # head(lags.x)
  XX=cbind(x,lags.y,lags.x) #Matriz de vars explicativas completa
  nExpl=ncol(XX) #número definitivo de variáveis explicativas testadas no modelo
  #head(XX)
  #colnames(XX)

  # Regressão:  

      yIN=y[indIN]
      yOUT=y[indOUT] 
      
      xIN=XX[indIN,] 
      xOUT=XX[indOUT,] 
      
      #Testa dados faltantes:
      teste.miss.y=sum(is.na(yIN))
      if (teste.miss.y>0) {stop("Valor faltante na variável dependente")}
      teste.miss.x=sum(is.na(xIN))
      if (teste.miss.x>0) {stop("Valor faltante em alguma(s) variável(eis) explicativa(s)")}
      teste.dim.x=ncol(xIN)
      if (teste.dim.x==1) {stop("Matrix x com apenas 1 coluna. A função RegSL ajusta modelos multivariados.")}

      
      #Regressão Lasso:
      #cross-validation lasso:
      cv=cv.glmnet(xIN,yIN,type.measure='mse',nfolds=10,alpha=1)
      model=glmnet(xIN,yIN,type.gaussian="covariance",lambda=cv$lambda.min) #family=c("gaussian")
      #coeficientes lasso:
      betas_las=as.vector(predict(model, type="coefficients"))
      
      #Regressão simples somente com variáveis do lasso:
      nvars=length(which(betas_las!=0))
      if (nvars>1) {
        selec=which(betas_las!=0)[-1]-1 #menos a posição do intercepto em betas_las
        xLas=xIN[,selec]
        reg = lm(yIN~xLas) #modelo de regressão simples 
      } else{
        reg = lm(y~1) #modelo de regressão simples
      }
      
      #Zera betas da regressão com NA: multicolinearidade ou variância nula:
      betas_reg=rep(0,nExpl+1)
      aux_betas=reg$coefficients #coeficientes das vars selecionadas no lasso na reg simples 
      aux_betas[which(is.na(aux_betas))]=0 #Colinearidade ou variância nula
      betas_reg[c(1,(selec+1))]=aux_betas #armazena coeficientes nas posições das vars selecionadas pelo lasso
 
      #Cálculo de previsões em ambos modelos:
      
      #matriz design para aplicação do modelo com previsão in sample usando valores observados de y + previsão 1 passo a frente.
      if (lagy==0) {
        auxx=cbind(c(rep(NA,ini-1),rep(1,Ntimes-(ini-1))), XX) #LAG MAXIMO=30
        prevY_reg=rep(NA,Ntimes)
        prevY_reg=auxx %*% betas_reg#previsões in sample + 1 out of sample #prevlogsq
      } else {
      auxx=cbind(c(rep(NA,ini-1),rep(1,Ntimes-(ini-1))), XX) #LAG MAXIMO=30
      prevY_reg=rep(NA,Ntimes)
      prevY_reg[1:indOUT[1]]=auxx[1:indOUT[1],] %*% betas_reg#previsões in sample + 1 out of sample #prevlogsq
      
      #j=indOUT[2]
      
      for (j in indOUT[2:length(indOUT)])
        { 
          aux_y=c(y[1:max(indIN)], prevY_reg[indOUT[1]:(j-1)])# série observada + previsão 1 passo a frente
          
          lags.prevY_reg=c()
          if (lagy!=0)
            {
            if (lagy==1) {
              lag1.y=c( matrix(rep(NA, lagy^2 ), nrow=lagy ), embed(aux_y, lagy) [, 1:lagy ] )
              auxx[j,"lags.y"]=lag1.y[j]
              } else {
                lags.prevY_reg=rbind( matrix(rep(NA, lagy^2 ), nrow=lagy ), embed(aux_y, lagy) [, 1:lagy ] )
                colnames(lags.prevY_reg)=paste0(rep("lag",lagy),1:lagy,rep(".y",lagy))
                auxx[j,colnames(lags.y)]=lags.prevY_reg[j,]
                }
            prevY_reg[j]=auxx[j,] %*% betas_reg #vetor com previsões in sample + 1 out of sample
          } #if (lagy!=0)
      } #for (j in indOUT[2:length(indOUT)])
      
      }
      
      #Regressão usando Lasso:
      #matriz design para aplicação do modelo com previsão in sample usando valores observados de y + previsão 1 passo a frente.
      if (lagy==0) {
        auxx=cbind(c(rep(NA,ini-1),rep(1,Ntimes-(ini-1))),XX) #LAG MAXIMO=30
        prevY_las=rep(NA,Ntimes)
        prevY_las=auxx%*% betas_las#previsões in sample + 1 out of sample #prevlogsq
      } else {
      auxx=cbind(c(rep(NA,ini-1),rep(1,Ntimes-(ini-1))), XX[,]) #LAG MAXIMO=30
      prevY_las=rep(NA,Ntimes)
      prevY_las[1:indOUT[1]]=auxx[1:indOUT[1],] %*% betas_las#previsões in sample + 1 out of sample #prevlogsq_las
      
      for (j in indOUT[2:length(indOUT)])
      { 
        aux_y=c(y[1:max(indIN)], prevY_las[indOUT[1]:(j-1)])# série observada + previsão 1 passo a frente
        lags.prevY_las=c()
        if (lagy!=0)
          {
          if (lagy==1) {
            lag1.y=c( matrix(rep(NA, lagy^2 ), nrow=lagy ), embed(aux_y, lagy) [, 1:lagy ] )
            auxx[j,"lags.y"]=lag1.y[j]
          } else {
            lags.prevY_reg=rbind( matrix(rep(NA, lagy^2 ), nrow=lagy ), embed(aux_y, lagy) [, 1:lagy ] )
            colnames(lags.prevY_las)=paste0(rep("lag",lagy),1:lagy,rep(".y",lagy))
            auxx[j,colnames(lags.y)]=lags.prevY_las[j,]
          }
          prevY_las[j]=auxx[j,] %*% betas_las #vetor com previsões in sample + 1 out of sample
        } 
      }
      }
      
      #Previsões in e out para saída:
      prevY_reg_in=prevY_reg[indIN]
      prevY_las_in=prevY_las[indIN]
      prevY_reg_out=prevY_reg[indOUT]
      prevY_las_out=prevY_las[indOUT]
      
      #Resíduos:
      erro_reg_in=yIN-prevY_reg_in
      erro_las_in=yIN-prevY_las_in
      erro_reg_out=yOUT-prevY_reg_out
      erro_las_out=yOUT-prevY_las_out

      #Atributos da regressão:
      R2_reg=summary(reg)$adj.r.squared 
      R2_adj_reg=summary(reg)$adj.r.squared
      sse_las = sum(erro_las_in^2); sst = sum(((y[indIN] - mean(y[indIN]))^2))
      R2_las= 1 - (sse_las/sst)
      R2_adj_las= 1 - ( (1 - R2_las) * (length(indIN) - 1))/(length(indIN) - nvars-1 - 1)
      
      #p-valores da regressão simples:
      selec_ncolin=c(1,selec+1)*((is.na(reg$coefficients)==F)*1) #possição dos betas não NA
      pvalues  =rep(NA,nExpl+1)
      pvalues[selec_ncolin[selec_ncolin!=0]]=summary(reg)$coefficients[,4] #armazena p-valores dos betas ne NA
      
      #atribuição de nomes aos betas:
      vars.names=c("intercept",colnames(xIN))
      betas=cbind(vars.names,round(betas_reg,4),round(betas_las,4),round(pvalues,4))
      colnames(betas)=c("var","beta.reg","beta.las","p.value.reg")
      
      #Fim regressão.
  
      return(list(yIN = yIN, yOUT = yOUT, 
              xIN=xIN, xOUT=xOUT,
              prevY_reg_in=prevY_reg_in,  prevY_las_in=prevY_las_in,
              prevY_reg_out=prevY_reg_out, prevY_las_out=prevY_las_out,
              erro_reg_in=erro_reg_in, erro_las_in=erro_las_in,
              erro_reg_out=erro_reg_out, erro_las_out=erro_las_out,
              betas=betas,
              R2_reg=R2_reg, R2_las=R2_las,
              R2_adj_reg=R2_adj_reg, R2_adj_las=R2_adj_las))
}
