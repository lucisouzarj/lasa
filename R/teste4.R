Rw=function(model.data, LastRWDay, endDataDate, lagy, lagx, model.start, group.stores.numb, beta.exists, beta.price,
                               beta.pvalue, pctPriceVar, horizon){
  
  while(names(dev.cur())!='null device')
  {
    dev.off()
  }
  
    data0=Sys.time()
    
    #file.remove("output.previsao.xlsx")
    
    #model.data com horizonte usuÃ¡rio:
    model.data.mensal <- subset(model.data, as.Date(rownames(model.data)) <= as.Date(timeLastDayInMonth(endDataDate)))
    model.data <- subset(model.data, as.Date(rownames(model.data)) <= as.Date(horizon))
    
    dias=rownames(model.data)
    
    if (beta.exists==1) {
      y.mod= model.data[,"y"] - group.stores.numb*beta.price*model.data[,"p"]
    }else{
      y.mod= model.data[,"y"]
    }
    
    if (beta.exists==1) {
      x.mod= subset( model.data, select = -c( y, p) ) #model.data[,-c("y","p")]
    }else{
      x.mod= subset( model.data, select = -c( y) ) 
    }
    
    lagy.mod= lagy
    lagx.mod= lagx
    vars.x.lag.mod=colnames(subset(x.mod, select= -c(seg, ter, qua, qui, sex, sab))) #retira dummies de dia da semana
    model.start=0 #quando informo 0 ou nÃ£o informo nada ini=lagy.mod+1

    #12 janelas rw:
    ini=as.Date( timeFirstDayInMonth(LastRWDay - years(1))) #dia que encerra a primeira janela dados.in
    fim=as.Date( timeFirstDayInMonth(ymd(ini+years(1))-20)) #dia que encerra a Ãºltima janela dados.in
    
    #grupo de SKU's
    p=forPerRollWds(ini,fim) #datas de encerramento de cada janela
    ofs=as.vector(LastRWDay-p) #perÃ­odo out-of-sample considerado no ajuste de cada janela
    
    #dados rw (somente meses completos):
    y.mod.rw= y.mod[which(dias<=LastRWDay)]
    x.mod.rw= x.mod[which(dias<=LastRWDay),]
    
    #Ajusta modelo m.AAAAMM e gera listas com erros err.AAAAMM (201511 a 201610):
    #i=1
    #set.seed(100) #necessÃ¡rio se fosse usado o cvglm para definiÃ§Ã£o do lambda do lasso
    for (i in 1:length(ofs)){
      
      m=year(p[i])*100+month(p[i]) #Ãºltimo mÃªs dos dados usados para ajuste do modelo
      
      eval(parse(text=paste("m.", m, "=forPredDailySeries(y=y.mod.rw, x=x.mod.rw, lagy=lagy.mod, lagx=lagx.mod, vars.x.lag=vars.x.lag.mod, ini=model.start, out.of.sample=", ofs[i],")", sep="") ) )
      
      #Resultados e cÃ¡lculo de medidas de erro out of sample (1 stp forward) (calcErros2):
      
      #names(m.201512)
      
        Ntimes=length(y.mod.rw)
        
        inicio=1+lagy
        lim=Ntimes-ofs[i]
        indIN=inicio:lim #indice amostra in
        if (lim < Ntimes) {indOUT=(lim+1):Ntimes} else {indOUT=NULL}#indice amostra out #lim=Ntimes se nÃ£o hÃ¡ amostra ofs
        
        if (beta.exists==1){
        
        predY_reg_out=eval(parse(text=paste("m.",m,"$predY_reg_out + group.stores.numb*beta.price*model.data[indOUT,'p']",sep="")))
        predY_reg_out[predY_reg_out<0]=0
        predY_reg_out_corr=predY_reg_out
        
        predY_las_out=eval(parse(text=paste("m.",m,"$predY_las_out + group.stores.numb*beta.price*model.data[indOUT,'p']",sep="")))
        predY_las_out[predY_las_out<0]=0
        predY_las_out_corr=predY_las_out
        
        eval(parse(text=paste("m.",m,"$yOUT= m.",m,"$yOUT + group.stores.numb*beta.price*model.data[indOUT,'p']",sep="")))
        
        } else {
          
          predY_reg_out=eval(parse(text=paste("m.",m,"$predY_reg_out",sep="")))
          predY_reg_out[predY_reg_out<0]=0
          predY_reg_out_corr=predY_reg_out
          
          predY_las_out=eval(parse(text=paste("m.",m,"$predY_las_out",sep="")))
          predY_las_out[predY_las_out<0]=0
          predY_las_out_corr=predY_las_out
          
        }
        
        eval(parse(text=paste("prev.",m,".nv=cbind(predY_reg_out, predY_reg_out_corr, predY_las_out, predY_las_out_corr)",sep="")))
        eval(parse(text=paste("obs.",m,".nv=m.",m,"$yOUT",sep="")))
      
      #forRWErrorMeasuresDailySeries:
      ini=p[i]+1
      eval(parse(text=paste("err.",m,".nv=forRWErrorMeasuresDailySeries(prev.",m,".nv,obs.",m,".nv,ini)[[c(4)]]",sep="")))#[c(4,8,12)]
      
    } #for (i in 1:length(ofs)) : um ajuste e cÃ¡lculo de previsÃµes e erros para cada janela de dados
    
    # MAPE dia dos Ãºltimos 30 dias da Ãºltima janela (NÃ£o aparece na interface):
    eval(parse(text=paste("MAPE.dia.",m,"=mean(abs(prev.",m,".nv[1:30,1]-obs.",m,".nv[1:30])/obs.",m,".nv[1:30])*100",sep="")))
    
    # modelo com melhor desempenho:
    best.pred=list()
    
    #Resultados 12 janelas:
    m=year(p)*100 + month(p)
    m=head(m,-1)
    n=paste0("as.data.frame(err.",m,".nv)[1,]")
    txt=paste(n,collapse=",")
    err.1stp=eval(parse(text=paste0("rbind(",txt,")"))) 
    colnames(err.1stp)=c("APE.reg_out","APE.reg_out_corr","APE.las_out","APE.las_out_corr", 
                         "AE.reg_out", "AE.reg_out_corr", "AE.las_out", "AE.las_out_corr", 
                         "E2.reg_out", "E2.reg_out_corr", "E2.las_out", "E2.las_out_corr", 
                         "E.reg_out", "E.reg_out_corr", "E.las_out", "E.las_out_corr", 
                         "Pred.reg_out", "Pred.reg_out_corr", "Pred.las_out", "Pred.las_out_corr", 
                         "Obs_out","qtd.dias")
    bottom.line=c(round(colMeans(err.1stp[,1:8]), 2),round(sqrt(colMeans(err.1stp[,9:12])),2), 
                  round(colMeans(err.1stp[,13:(ncol(err.1stp)-1)]), 2),"-") 
    
    err.1stp=rbind(err.1stp,bottom.line)
    rownames(err.1stp)=c(rownames(err.1stp[-nrow(err.1stp),]),"Global")
    
    prev.1stp=as.numeric(err.1stp[ ,"Pred.las_out"]) #Mudou de previsÃµes da regressÃ£o para lasso
    obs.1stp=as.numeric(err.1stp[ ,"Obs_out"])
    
    bottom.MAPE=as.numeric(bottom.line["APE.las_out"]) #Mudou de previsÃµes da regressÃ£o para lasso
    bottom.MAE=round(as.numeric(bottom.line["AE.las_out"]),0) #Mudou de previsÃµes da regressÃ£o para lasso
    
    best.pred <- colnames(err.1stp[,1:4])[as.numeric(bottom.line[1:4])==min(as.numeric(bottom.line[1:4]))] #remover?
    
    #Modelo usando todos os dados:
    
    #dados rw (somente meses completos):
    # ofs.final=as.Date(max(dias))-as.Date(endDataDate)
    ofs.final=as.Date(horizon)-as.Date(endDataDate)
    m.final=forPredDailySeries(y=y.mod, x=x.mod, lagy=lagy.mod, lagx=lagx.mod, vars.x.lag=vars.x.lag.mod, ini=model.start, out.of.sample=ofs.final)
    
    #CÃ¡lculo de erros de previsÃ£o da soma de h passos a frente:
    
    #testa se modelo final tem autoregressivo:
    ha.autoreg=0
    if (sum(sapply(m.final$betas$beta.las[2:(1+lagy)],function(x) (x!=0)*1)) > 0) ha.autoreg=1
    
    #Argumentos para cÃ¡lculo previsÃ£o:
    h=30 #as.numeric(ofs.final)
    XX=m.final$xIN
    auxx=cbind( rep( 1, nrow(XX)), XX)
    betas.dia.compl=m.final$betas$beta.las
    
    #Vetores para armazenamento de resultados:
    Erros.h=c() #erros dia
    Obss.h=c() #quantidades observadas dia
    PrevsY.h=c() #previsÃµes dia
    
    #CÃ¡lculo de erros previsÃ£o atÃ© 30 dias a frente ou horizonte de previsÃ£o informado pelo usuÃ¡rio:
    prazo= min( nrow( auxx), h,  ofs.final)
    
    #Quantos erros podem ser obtidos da sÃ©rie em questÃ£o? n
    n= floor( nrow(auxx)/ prazo)
    
    #A cada t+prazo calcula previsÃµes dia 1 a h=prazo passos a frente
    #t=1
    for (t in seq(1, nrow( auxx), prazo)[1:n]) {
      
      prevsY.dia=c()
    
      Obs.h=sum( m.final$yIN[ t:(prazo+t-1)]) #quantidade total observada nos dias dentro de h=prazo a partir do dia t
      
      if (ha.autoreg==0) { #quando nÃ£o hÃ¡ autoregressivo:
        
        prevsY.dia= rep( NA, length( m.final$yIN ))
        prevsY.dia= auxx %*% betas.dia.compl 
        prevY.h= sum( prevsY.dia[ t: (prazo+ t- 1) ] ) #soma da previsÃ£o no horizonte desejado
        
      } else { #quando hÃ¡ autoregressivo:
        
        prevY.dia= auxx[t,] %*% betas.dia.compl 
        prevsY.dia= c( prevsY.dia, prevY.dia)
        
        if (lagy==1) {
          
          for ( j in (t+1): (prazo+ t- 1) ) {
            auxx[j, 2]= prevY.dia #lag1.y= Ãºltima previsÃ£o
            prevY.dia= auxx[j,] %*% betas.dia.compl 
            prevsY.dia= c( prevsY.dia, prevY.dia)}
          
        } else { # lagy>=2
          
          #j=2
          for ( j in (t+ 1 ): (prazo+ t- 1)) {
            auxx[ j, 2: (lagy+ 1) ]= c(prevY.dia, auxx[ j- 1, 2: lagy]) #atualiza lags de y
            prevY.dia= auxx[j, ] %*% betas.dia.compl 
            prevsY.dia= c( prevsY.dia, prevY.dia)
            }
        
        } # fim if (lagy==1)
        
        prevY.h=sum(prevsY.dia) #soma da previsÃ£o no horizonte desejado qd hÃ¡ autoregressivo
      
      } #fim if (ha.autoreg==0)
      
      PrevsY.h= c(PrevsY.h, prevY.h)
      Obss.h= c(Obss.h, Obs.h)
      Erros.h= Obss.h - PrevsY.h
      
    } #fim for
    
    sigma.soma.prev=sqrt(var(Erros.h))
    range.ic95.soma.prev=1.96*sigma.soma.prev
    
    #Betas(interpretaÃ§Ã£o) + p-value: Ãºltimo modelo
    # if (beta.exists){
    # final.betas=data.frame(Variavel=c(as.vector(m.final$betas[,1])),"P99"), 
    #                        Beta=c(as.vector(m.final$betas[,2]),beta.price),
    #                        p_valores=c(as.vector(m.final$betas[,4]),beta.pvalue) )[!is.na(m.final$betas[,4]),]
    # } else {
    final.betas=data.frame(Variavel=as.vector(m.final$betas[,1]), 
                           Beta=as.vector(m.final$betas[,"beta.las"]))[which(m.final$betas[,"beta.las"]!=0),] #Modificado de regressÃ£o para lasso
    final.betas$Variavel=gsub(".y", ".q", final.betas$Variavel, fixed=T)
                           #,p_valores=as.vector(m.final$betas[,4]))[which(!is.na(m.final$betas[,4])),] #sem p-valores no lasso
    # }
    
     #PrevisÃ£o Ã s cegas dias in (Modificado de regressÃ£o para lasso):
    if (beta.exists==1) {
    prev.final.in=m.final$predY_las_in + group.stores.numb*beta.price*model.data[(lagy+1):(length(m.final$yIN)+lagy),'p']
    }else {prev.final.in=m.final$predY_las_in}
    prev.final.in[prev.final.in<0]=0
    
    #R2 Cristiano: Ãºltimo modelo - nÃ£o ajustado (Modificado de regressÃ£o para lasso):
    if (beta.exists==1) {
    R2=(cor(prev.final.in, model.data[(lagy+1):(length(m.final$yIN)+lagy),"y"]))^2
    } else {
    R2=m.final$R2_las
    }
    
    #erros in.sample(Modificado de regressÃ£o para lasso):
    if (beta.exists==1) {
      final.errors=model.data[(lagy+1):(length(m.final$yIN)+lagy),"y"]-prev.final.in
    } else {
      final.errors=m.final$erro_las_in
    }
    
    #Prev dia Ã s cegas (Modificado de regressÃ£o para lasso):
    if (beta.exists==1) {
    prev.final.out= m.final$predY_las_out + group.stores.numb*beta.price*model.data[(length(m.final$yIN)+lagy+1):nrow(model.data),'p']
    } else {prev.final.out=m.final$predY_las_out}
    prev.final.out[prev.final.out<0]=0 #previsÃµes negativas consideradas 0
    
    #PrevisÃ£o mensal "Ã s cegas"= soma parte observada + prevista diÃ¡ria:
    # pred.blind.last.month= sum(m.final$yIN[(length(m.final$yIN)-(endDataDate-LastRWDay)):length(m.final$yIN)]) + #obs
    #   sum(prev.final.out[1:(as.Date(timeLastDayInMonth(endDataDate))-as.Date(endDataDate))]) #previsto
    
    #PrevisÃ£o mensal "Ã s cegas"= soma da previsÃ£o diÃ¡ria ( Modificado de regressÃ£o para lasso):
    ofs.final.mensal=as.Date(timeLastDayInMonth(endDataDate))-as.Date(LastRWDay)
    if (beta.exists==1) {
      y.mod= model.data.mensal[,"y"] - group.stores.numb*beta.price*model.data.mensal[,"p"]
    }else{
      y.mod= model.data.mensal[,"y"]
    }
    if (beta.exists==1) {
      x.mod= subset( model.data.mensal, select = -c( y, p) ) #model.data[,-c("y","p")]
    }else{
      x.mod= subset( model.data.mensal, select = -c( y) ) 
    }
    m.final.mensal=forPredDailySeries(y=y.mod, x=x.mod, lagy=lagy.mod, lagx=lagx.mod, vars.x.lag=vars.x.lag.mod, ini=model.start, out.of.sample=ofs.final.mensal)
    if (beta.exists==1) {
      prev.mensal.out=m.final.mensal$predY_las_out + group.stores.numb*beta.price*model.data.mensal[(length(m.final.mensal$yIN)+lagy+1):nrow(x.mod),'p']
    } else {prev.mensal.out=m.final.mensal$predY_las_out}
    prev.mensal.out[prev.mensal.out<0]=0 #previsÃµes negativas consideradas 0
    pred.blind.last.month= sum(prev.mensal.out) #previsto
    
    #IC prev mensal Ã s cegas:
    #Dispensado por enquanto...
    
    #meses rw:
    dias.rw=dias[(lagy+1):length(seq(as.Date(min(dias)),as.Date(timeFirstDayInMonth(endDataDate)),by="days"))]           
    am=(year(as.Date(dias.rw))*100+month(as.Date(dias.rw)))
    
    #grÃ¡fico mensal 12 janelas: 
    obs.dia=data.frame(am=am, c(model.data.mensal[(lagy+1):max(which(dias<=as.Date(LastRWDay))),"y"],NA)) #mudar para y.mod quando tiver previsÃ£o in sample usando dados completos
    obs.mes=aggregate(. ~ am, data = obs.dia, FUN = 'sum', na.rm = TRUE, na.action=NULL)
    obs.mes[nrow(obs.mes),2]=NA
    
    #Valores ajustados in sample do modelo - janela 1 - diÃ¡rio (Modificado de regressÃ£o para lasso):
    if (beta.exists==1) {
    prevIN.m1=eval(parse(text=paste("m.",m[1],"$predY_las_in + group.stores.numb*beta.price*model.data[(lagy+1):max(which(dias<=as.Date(p[1]))),'p']",sep="")))
    }else { prevIN.m1=eval(parse(text=paste("m.",m[1],"$predY_las_in",sep="")))}
    prevIN.m1[prevIN.m1<0]=0
    
    prevIN.m1.dia=data.frame(am=am[1:length(prevIN.m1)], prevIN.m1) #cria data frame com coluna am e valores ajustados
    prevIN.m1.mes=aggregate(. ~ am, data = prevIN.m1.dia, FUN = 'sum', na.rm = TRUE, na.action=NULL)
    prev.mes=c(prevIN.m1.mes[,-1], prev.1stp, pred.blind.last.month) #concatena valore ajustados m1, previsÃµes 1stp fwd rw, previsÃ£o Ã s cegas Ãºltimo mÃªs
    
    month.table=data.frame(am=obs.mes[,1], obs=obs.mes[,2], prev=prev.mes)
    
    forPlotMonthObsPred(obs.mes,prev.mes)
    month.graph = recordPlot()
    
    #IC prev diÃ¡ria Ã s cegas:
    ub.dia=prev.final.out + m.final$ic_range[,"0.95"]
    lb.dia=prev.final.out - m.final$ic_range[,"0.95"]
    ub.dia[ub.dia<0]=0
    lb.dia[lb.dia<0]=0
    
    #grÃ¡fico diÃ¡rio com prev Ã s cegas(com IC) atÃ© a data escolhida pelo usuÃ¡rio:
    prev.dia=c(prev.final.in, prev.final.out);  names(prev.dia)=dias[-(1:lagy)]
    obs.dia= model.data[-(1:lagy),"y"]; names(obs.dia)= dias[-(1:lagy)]
    
    daily.table=data.frame(data=as.Date(names(obs.dia)), obs=obs.dia, prev=prev.dia, lb95=c(rep(NA, length(obs.dia)-length(lb.dia)),lb.dia), 
                        ub95=c(rep(NA, length(obs.dia)-length(ub.dia)),ub.dia))
    
    
    forPlotDailyObsPred(obs=obs.dia, pred=prev.dia, lb=lb.dia, ub=ub.dia, begin=(endDataDate-200), end=max(dias),
                      tit="SÃ©rie DiÃ¡ria Observada x Prevista", lastInSampledate=endDataDate)
    daily.graph = recordPlot()
    
    barplot(c(bottom.MAPE,bottom.MAE), axes=F, main="Medidas de Erro(%)", width=0.1, space=0.99, ylim=c(0,round(max(c(bottom.MAPE,bottom.MAE))+1,0)),
            xlab="",  ylab="", border="black", col="gray", density=40 , names.arg=c("MAPE","MAE"))
            #xlab="",  ylab="", border="red", col="blue", density=10, names.arg=c("MAPE","MAE"))
    axis(2,seq(0,ceiling(max(c(bottom.MAPE,bottom.MAE))),length.out=5),seq(0,ceiling(max(c(bottom.MAPE,bottom.MAE))),length.out=5))
    g.erro = recordPlot()
    
    i=1
    #GrÃ¡fico previsÃ£o x preÃ§o, quando hÃ¡ beta pricing (Marcelo):
    pxpred.graph=NULL
    if (beta.exists==1) {
      var.preco.x.prev= data.frame(var.preco=seq(-pctPriceVar, pctPriceVar, length.out = 10), prev=NA, lb=NA, ub=NA)
      
      for (i in 1:10) {
        pct=var.preco.x.prev[i,"var.preco"]
        var.preco.x.prev[i,"prev"]=sum(prev.final.out +  group.stores.numb*beta.price*pct*model.data[(length(m.final$yIN)+lagy+1):nrow(model.data),'p'])
        var.preco.x.prev[i,"lb"]=max((var.preco.x.prev[i,"prev"]- range.ic95.soma.prev),0)
        var.preco.x.prev[i,"ub"]=var.preco.x.prev[i,"prev"]+ range.ic95.soma.prev
        
      }
      
      
      l=min(var.preco.x.prev[,"lb"])
      u=max(var.preco.x.prev[,"ub"])
      
      plot(var.preco.x.prev[,"var.preco"], var.preco.x.prev[,"prev"], t="b", ylim=c(l,u), xlab="VariaÃ§Ã£o Percentual de PreÃ§o"
           ,ylab=""
           ,main=paste("VariaÃ§Ã£o de preÃ§o x previsÃ£o total(",prazo,")"), lty=2, cex=0.8, pch=20, yaxt="n", cex.axis=0.8, cex.lab=1)
      
      
      axis(2, at=seq(l, u, length.out=6), labels=FALSE)
      axis(1, labels = FALSE)
      
      abline(h=round(seq(l/1000, u/1000, length.out=6),3)*1000, lty=3, col='grey')
      text(y = seq(l,u,length.out = 6)
           ,labels=round(seq(l/1000, u/1000, length.out=6),0), par("usr")[1], srt = 0, pos = 2, xpd = TRUE)
      lines(var.preco.x.prev[,"var.preco"], var.preco.x.prev[,"lb"], t="b", col=2, lty=2, cex=0.8, pch=20 )
      lines(var.preco.x.prev[,"var.preco"], var.preco.x.prev[,"ub"], t="b", col=2, lty=2, cex=0.8, pch=20 )
      title(ylab=expression("PrevisÃ£o ( x" ~ 10^3 ~ ")"), line=2.4, cex.lab=1)#, cex.lab=1.2, family="Calibri Light")
      
      pxpred.graph= recordPlot()
    }

    
    #Export em excel:
    write.xlsx(final.betas, file="output.previsao.xlsx", sheetName="final.betas")
    write.xlsx(month.table, file="output.previsao.xlsx", sheetName="month.table", append=TRUE)
    write.xlsx(daily.table, file="output.previsao.xlsx", sheetName="daily.table", append=TRUE)
    
    data1=Sys.time()
    cat(paste("\n InÃ­cio em ", data0, ".\n TÃ©rmino em ",Sys.time(),". Tempo total de ", round(data1-data0,3),"." ,sep="" ))
    
    return=( list(bottom.MAPE= bottom.MAPE, bottom.MAE= bottom.MAE, best.pred= best.pred, final.betas=final.betas, R2=R2,
                  month.graph=month.graph, month.table=month.table, daily.graph=daily.graph, daily.table=daily.table, pxpred.graph=pxpred.graph,
                  final.errors=final.errors) )
    
}
