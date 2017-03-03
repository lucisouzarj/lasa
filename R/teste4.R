Rw=function(dados.mod, fim.janelas, fim.dados, lagy, lagx, ini.mod, n.lojas, existe.beta, beta.preco, pvalue.preco, pct.var.preco, final.horizonte)
{
  
  #se nÃ£o existe beta, deixo a variÃ¡vel preÃ§o no ajuste?
  
  data0=Sys.time()
  
  if(file.exists("/RProjetos/Interfaces/output.previsao.xlsx")){
    file.remove('/RProjetos/Interfaces/output.previsao.xlsx')
  }

  #dados.mod com horizonte usuÃ¡rio:
  dados.mod.mensal <- subset(dados.mod, as.Date(rownames(dados.mod)) <= as.Date(timeLastDayInMonth(fim.dados)))
  dados.mod <- subset(dados.mod, as.Date(rownames(dados.mod)) <= as.Date(final.horizonte))
  
  dias=rownames(dados.mod)
  
  
  
  if (existe.beta==1) {
    y.mod= dados.mod[,"y"] - n.lojas*beta.preco*dados.mod[,"p"]
  }else{
    y.mod= dados.mod[,"y"]
  }
  
  if (existe.beta==1) {
    x.mod= subset( dados.mod, select = -c( y, p) ) #dados.mod[,-c("y","p")]
  }else{
    x.mod= subset( dados.mod, select = -c( y) ) 
  }
  
  lagy.mod= lagy
  lagx.mod= lagx
  vars.x.lag.mod=colnames(subset(x.mod, select= -c(seg, ter, qua, qui, sex, sab))) #retira dummies de dia da semana
  ini.mod=0 #quando informo 0 ou nÃ£o informo nada ini=lagy.mod+1
  
  #12 janelas rw:
  ini=as.Date( timeFirstDayInMonth(fim.janelas - years(1))) #dia que encerra a primeira janela dados.in
  fim=as.Date( timeFirstDayInMonth(ymd(ini+years(1))-20)) #dia que encerra a Ãºltima janela dados.in
  
  #grupo de SKU's
  p=perRollWds(ini,fim) #datas de encerramento de cada janela
  ofs=as.vector(fim.janelas-p) #perÃ­odo out-of-sample considerado no ajuste de cada janela
  
  #dados rw (somente meses completos):
  y.mod.rw= y.mod[which(dias<=fim.janelas)]
  x.mod.rw= x.mod[which(dias<=fim.janelas),]
  
  #Ajusta modelo m.AAAAMM e gera listas com erros err.AAAAMM (201511 a 201610):
  #i=1
  #set.seed(100) #necessÃ¡rio se fosse usado o cvglm para definiÃ§Ã£o do lambda do lasso
  for (i in 1:length(ofs)){
    
    m=year(p[i])*100+month(p[i]) #Ãºltimo mÃªs dos dados usados para ajuste do modelo
    eval(parse(text=paste("m.", m, "=RegSBICL(y=y.mod.rw, x=x.mod.rw, lagy=lagy.mod, lagx=lagx.mod, vars.x.lag=vars.x.lag.mod, ini=ini.mod, out.of.sample=", ofs[i],")", sep="") ) )
    
    #Resultados e cÃ¡lculo de medidas de erro out of sample (1 stp forward) (calcErros2):
    
    Ntimes=length(y.mod.rw)
    inicio=1+lagy
    lim=Ntimes-ofs[i]
    indIN=inicio:lim #indice amostra in
    if (lim < Ntimes) {indOUT=(lim+1):Ntimes} else {indOUT=NULL}#indice amostra out #lim=Ntimes se nÃ£o hÃ¡ amostra ofs
    
    if (existe.beta==1){
      
      prevY_reg_out=eval(parse(text=paste("m.",m,"$prevY_reg_out + n.lojas*beta.preco*dados.mod[indOUT,'p']",sep="")))
      prevY_reg_out[prevY_reg_out<0]=0
      prevY_reg_out_corr=prevY_reg_out
      
      prevY_las_out=eval(parse(text=paste("m.",m,"$prevY_las_out + n.lojas*beta.preco*dados.mod[indOUT,'p']",sep="")))
      prevY_las_out[prevY_las_out<0]=0
      prevY_las_out_corr=prevY_las_out
      
      eval(parse(text=paste("m.",m,"$yOUT= m.",m,"$yOUT + n.lojas*beta.preco*dados.mod[indOUT,'p']",sep="")))
      
    } else {
      
      prevY_reg_out=eval(parse(text=paste("m.",m,"$prevY_reg_out",sep="")))
      prevY_reg_out[prevY_reg_out<0]=0
      prevY_reg_out_corr=prevY_reg_out
      
      prevY_las_out=eval(parse(text=paste("m.",m,"$prevY_las_out",sep="")))
      prevY_las_out[prevY_las_out<0]=0
      prevY_las_out_corr=prevY_las_out
      
    }
    
    eval(parse(text=paste("prev.",m,".nv=cbind(prevY_reg_out,prevY_reg_out_corr,prevY_las_out,prevY_las_out_corr)",sep="")))
    eval(parse(text=paste("obs.",m,".nv=m.",m,"$yOUT",sep="")))
    
    #calcErrors2:
    ini=p[i]+1
    eval(parse(text=paste("err.",m,".nv=calcErrors2(prev.",m,".nv,obs.",m,".nv,ini)[[c(4)]]",sep="")))#[c(4,8,12)]
    
  } #for (i in 1:length(ofs)) : um ajuste e cÃ¡lculo de previsÃµes e erros para cada janela de dados
  
  # MAPE dia dos Ãºltimos 30 dias da Ãºltima janela:
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
                       "Prev.reg_out", "Prev.reg_out_corr", "Prev.las_out", "Prev.las_out_corr", 
                       "Obs_out","qtd.dias")
  bottom.line=c(round(colMeans(err.1stp[,1:8]), 2),round(sqrt(colMeans(err.1stp[,9:12])),2), 
                round(colMeans(err.1stp[,13:(ncol(err.1stp)-1)]), 2),"-") 
  
  err.1stp=rbind(err.1stp,bottom.line)
  rownames(err.1stp)=c(rownames(err.1stp[-nrow(err.1stp),]),"Global")
  
  prev.1stp=as.numeric(err.1stp[ ,"Prev.las_out"]) #Mudou de previsÃµes da regressÃ£o para lasso
  obs.1stp=as.numeric(err.1stp[ ,"Obs_out"])
  
  bottom.MAPE=as.numeric(bottom.line["APE.las_out"]) #Mudou de previsÃµes da regressÃ£o para lasso
  bottom.MAE=round(as.numeric(bottom.line["AE.las_out"]),0) #Mudou de previsÃµes da regressÃ£o para lasso
  
  
  best.pred <- colnames(err.1stp[,1:4])[as.numeric(bottom.line[1:4])==min(as.numeric(bottom.line[1:4]))]#remover?
  
  #Modelo usando todos os dados:
  
  #dados rw (somente meses completos):
  #ofs.final=as.Date(max(dias))-as.Date(fim.dados)
  ofs.final=as.Date(final.horizonte)-as.Date(fim.dados)
  m.final=RegSBICL(y=y.mod, x=x.mod, lagy=lagy.mod, lagx=lagx.mod, vars.x.lag=vars.x.lag.mod, ini=ini.mod, out.of.sample=ofs.final)
  #CÃ¡lculo de erros de previsÃ£o da soma de h passos a frente:
  
  ha.autoreg=0
  if (sum(sapply(m.final$betas$beta.las[2:(1+lagy)],function(x) (x!=0)*1)) > 0) ha.autoreg=1
  
  h=30 #as.numeric(ofs.final)
  XX=m.final$xIN
  auxx=cbind( rep( 1, nrow(XX)), XX)
  betas.dia.compl=m.final$betas$beta.las
  
  Erros.h=c()
  Obss.h=c()
  PrevsY.h=c()
  
  prazo= min( nrow( auxx), h,  ofs.final)
  
  n= floor( nrow(auxx)/ prazo)
  
  #t=1
  for (t in seq(1, nrow( auxx), prazo)[1:n]) {
    
    prevsY.dia=c()
    
    Obs.h=sum( m.final$yIN[ t:(prazo+t-1)])
    
    if (ha.autoreg==0) {
      
      prevsY.dia= rep( NA, length( m.final$yIN ))
      prevsY.dia= auxx %*% betas.dia.compl 
      prevY.h= sum( prevsY.dia[ t: (prazo+ t- 1) ] ) #soma da previsÃ£o no horizonte desejado
      
    } else {
      
      prevY.dia= auxx[t,] %*% betas.dia.compl 
      prevsY.dia= c( prevsY.dia, prevY.dia)
      
      if (lagy==1) {
        
        for ( j in (t+1): (prazo+ t- 1) ) {
          auxx[j, 2]= prevY.dia
          prevY.dia= auxx[j,] %*% betas.dia.compl 
          prevsY.dia= c( prevsY.dia, prevY.dia)}
        
      } else { # lagy>=2
        
        #j=2
        for ( j in (t+ 1 ): (prazo+ t- 1)) {
          auxx[ j, 2: (lagy+ 1) ]= c(prevY.dia, auxx[ j- 1, 2: lagy])
          prevY.dia= auxx[j, ] %*% betas.dia.compl 
          prevsY.dia= c( prevsY.dia, prevY.dia)
        }
        
      } # fim if (lagy==1)
      
      prevY.h=sum(prevsY.dia)
      
    } #fim if (ha.autoreg==0)
    
    PrevsY.h= c(PrevsY.h, prevY.h)
    Obss.h= c(Obss.h, Obs.h)
    Erros.h= Obss.h- PrevsY.h
    
  } #fim for
  
  sigma.soma.prev=sqrt(var(Erros.h))
  range.ic95.soma.prev=1.96*sigma.soma.prev
  
  
  
  #Betas(interpretaÃ§Ã£o) + p-value: Ãºltimo modelo
  # if (existe.beta){
  # betas.final=data.frame(Variavel=c(as.vector(m.final$betas[,1])),"P99"), 
  #                        Beta=c(as.vector(m.final$betas[,2]),beta.preco),
  #                        p_valores=c(as.vector(m.final$betas[,4]),pvalue.preco) )[!is.na(m.final$betas[,4]),]
  # } else {
  betas.final=data.frame(Variavel=as.vector(m.final$betas[,1]), 
                         Beta=as.vector(m.final$betas[,"beta.las"]))[which(m.final$betas[,"beta.las"]!=0),] #Modificado de regressÃ£o para lasso
  betas.final$Variavel=gsub(".y", ".q", betas.final$Variavel, fixed=TRUE)
  
  #) #,p_valores=as.vector(m.final$betas[,4]))[which(!is.na(m.final$betas[,4])),] #sem p-valores no lasso
  # }
  
  #PrevisÃ£o Ã s cegas dias in(Modificado de regressÃ£o para lasso):
  if (existe.beta==1) {
    prev.final.in=m.final$prevY_las_in + n.lojas*beta.preco*dados.mod[(lagy+1):(length(m.final$yIN)+lagy),'p']
  }else {prev.final.in=m.final$prevY_las_in}
  prev.final.in[prev.final.in<0]=0
  
  #R2 Cristiano: Ãºltimo modelo - nÃ£o ajustado
  if (existe.beta==1) {
    R2=cor(prev.final.in, dados.mod[(lagy+1):(length(m.final$yIN)+lagy),"y"])^2
  } else {
    R2=m.final$R2_las
  }
  
  #erros in.sample:
  if (existe.beta==1) {
    erro.final=dados.mod[(lagy+1):(length(m.final$yIN)+lagy),"y"]-prev.final.in
  } else {
    erro.final=m.final$erro_las_in
  }
  
  
  #Prev mensal Ã s cegas:
  if (existe.beta==1) {
    prev.final.out= m.final$prevY_las_out + n.lojas*beta.preco*dados.mod[(length(m.final$yIN)+lagy+1):nrow(dados.mod),'p']
  } else {prev.final.out=m.final$prevY_las_out}
  prev.final.out[prev.final.out<0]=0 #previsÃµes negativas consideradas 0
  
  #PrevisÃ£o mensal "Ã s cegas"= soma parte observada + prevista diÃ¡ria:
  # pred.blind.last.month= sum(m.final$yIN[(length(m.final$yIN)-(fim.dados-fim.janelas)):length(m.final$yIN)]) + #obs
  #   sum(prev.final.out[1:(as.Date(timeLastDayInMonth(fim.dados))-as.Date(fim.dados))]) #previsto
  
  #PrevisÃ£o mensal "Ã s cegas"= soma da previsÃ£o diÃ¡ria ( Modificado de regressÃ£o para lasso):
  ofs.final.mensal=as.Date(timeLastDayInMonth(fim.dados))-as.Date(fim.janelas)
  if (existe.beta==1) {
    y.mod= dados.mod.mensal[,"y"] - n.lojas*beta.preco*dados.mod.mensal[,"p"]
  }else{
    y.mod= dados.mod.mensal[,"y"]
  }
  if (existe.beta==1) {
    x.mod= subset( dados.mod.mensal, select = -c( y, p) ) #dados.mod[,-c("y","p")]
  }else{
    x.mod= subset( dados.mod.mensal, select = -c( y) ) 
  }
  m.final.mensal=RegSBICL(y=y.mod, x=x.mod, lagy=lagy.mod, lagx=lagx.mod, vars.x.lag=vars.x.lag.mod, ini=ini.mod, out.of.sample=ofs.final.mensal)
  if (existe.beta==1) {
    prev.mensal.out=m.final.mensal$prevY_las_out + n.lojas*beta.preco*dados.mod.mensal[(length(m.final.mensal$yIN)+lagy+1):nrow(x.mod),'p']
  } else {prev.mensal.out=m.final.mensal$prevY_las_out}
  prev.mensal.out[prev.mensal.out<0]=0 #previsÃµes negativas consideradas 0
  pred.blind.last.month= sum(prev.mensal.out) #previsto
  
  
  #IC prev mensal Ã s cegas:
  #Dispensado por enquanto...
  
  #meses rw:
  dias.rw=dias[(lagy+1):length(seq(as.Date(min(dias)),as.Date(timeFirstDayInMonth(fim.dados)),by="days"))]           
  am=(year(as.Date(dias.rw))*100+month(as.Date(dias.rw)))
  
  #grÃ¡fico mensal 12 janelas com prev Ã s cegas(com IC) atÃ© data usuÃ¡rio: 
  obs.dia=data.frame(am=am, c(dados.mod[(lagy+1):max(which(dias<=as.Date(fim.janelas))),"y"],NA)) #mudar para y.mod quando tiver previsÃ£o in sample usando dados completos
  obs.mes=aggregate(. ~ am, data = obs.dia, FUN = 'sum', na.rm = TRUE, na.action=NULL)
  obs.mes[nrow(obs.mes),2]=NA
  
  if (existe.beta==1) {
    prevIN.m1=eval(parse(text=paste("m.",m[1],"$prevY_las_in + n.lojas*beta.preco*dados.mod[(lagy+1):max(which(dias<=as.Date(p[1]))),'p']",sep="")))
  }else { prevIN.m1=eval(parse(text=paste("m.",m[1],"$prevY_las_in",sep="")))}
  
  prevIN.m1.dia=data.frame(am=am[1:length(prevIN.m1)],prevIN.m1)
  prevIN.m1.mes=aggregate(. ~ am, data = prevIN.m1.dia, FUN = 'sum', na.rm = TRUE, na.action=NULL)
  prev.mes=c(prevIN.m1.mes[,-1], prev.1stp, pred.blind.last.month)
  
  t.mensal=data.frame(am=obs.mes[,1],obs=obs.mes[,2],prev=prev.mes)
  
  dados_para_plot_mensal <- list()
  dados_para_plot_mensal$obs.mes <- obs.mes
  dados_para_plot_mensal$prev.mes <- prev.mes
  
  # plotObsPrevMensal(obs.mes,prev.mes)
  # g.mensal = recordPlot()
  
  # while(names(dev.cur())!='null device')
  # {
  #   dev.off()
  # }
  # 
  par(default_plot_parameters)
  
  #IC prev diÃ¡ria Ã s cegas:
  ub.dia=prev.final.out + m.final$ic_range[,"0.95"]
  lb.dia=prev.final.out - m.final$ic_range[,"0.95"]
  ub.dia[ub.dia<0]=0
  lb.dia[lb.dia<0]=0
  
  #grÃ¡fico diÃ¡rio com prev Ã s cegas(com IC) atÃ© a data escolhida pelo usuÃ¡rio:
  prev.dia=c(prev.final.in, prev.final.out);  names(prev.dia)=dias[-(1:lagy)]
  obs.dia= dados.mod[-(1:lagy),"y"];  names(obs.dia)=dias[-(1:lagy)]
  
  t.diario=data.frame(data=as.Date(names(obs.dia)), obs=obs.dia, prev=prev.dia, lb95=c(rep(NA, length(obs.dia)-length(lb.dia)),lb.dia), 
                      ub95=c(rep(NA, length(obs.dia)-length(ub.dia)),ub.dia))
  
  data_inicio_serie <- names(obs.dia)[1]
  dados_para_plot_diario <- list()
  dados_para_plot_diario$obs.dia <- obs.dia
  dados_para_plot_diario$prev.dia <- prev.dia
  dados_para_plot_diario$lb.dia <- lb.dia
  dados_para_plot_diario$ub.dia <- ub.dia
  dados_para_plot_diario$ini <- as.Date(ifelse((fim.dados - 2*365) < data_inicio_serie, data_inicio_serie, fim.dados - 2*365)) # Plota os ultimos 2 anos OU o historico inteiro
  dados_para_plot_diario$fim <- max(dias)
  dados_para_plot_diario$tit <- ""
  dados_para_plot_diario$dat_in <- fim.dados
  
  # plotObsPrevDiario(obs=obs.dia, prev=prev.dia, lb=lb.dia, ub=ub.dia, ini=(fim.dados-200), fim=max(dias),
  #                   tit="SÃ©rie DiÃ¡ria Observada x Prevista", dat_in=fim.dados)
  # g.diario = recordPlot()
  # 
  barplot(c(bottom.MAPE,bottom.MAE), axes=F, main="Medidas de Erro(%)", width=0.1, space=0.99, ylim=c(0,round(max(c(bottom.MAPE,bottom.MAE))+1,0)),
          xlab="",  ylab="", border="black", col="gray", names.arg=c("MAPE","MAE"))
  axis(2,seq(0,ceiling(max(c(bottom.MAPE,bottom.MAE))),length.out=5),seq(0,ceiling(max(c(bottom.MAPE,bottom.MAE))),length.out=5))
  g.erro = recordPlot()
  
  # while(names(dev.cur())!='null device')
  # {
  #   dev.off()
  # }
  
  par(default_plot_parameters)
  
  
  i=1
  #GrÃ¡fico previsÃ£o x preÃ§o, quando hÃ¡ beta pricing (Marcelo):
  g.pxprev=NULL
  if (existe.beta==1) {
    len <- 5
    var.preco.x.prev= data.frame(var.preco=seq(-pct.var.preco, pct.var.preco, length.out = len), prev=NA, lb=NA, ub=NA)
    
    for (i in 1:len) {
      pct=var.preco.x.prev[i,"var.preco"]
      var.preco.x.prev[i,"prev"]=sum(prev.final.out +  n.lojas*beta.preco*pct*dados.mod[(length(m.final$yIN)+lagy+1):nrow(dados.mod),'p'])
      var.preco.x.prev[i,"lb"]=max((var.preco.x.prev[i,"prev"]- range.ic95.soma.prev),0)
      var.preco.x.prev[i,"ub"]=var.preco.x.prev[i,"prev"]+ range.ic95.soma.prev
      
    }
    
    l=min(var.preco.x.prev[,"lb"])
    u=max(var.preco.x.prev[,"ub"])
    
    plot(var.preco.x.prev[,"var.preco"], var.preco.x.prev[,"prev"], t="b", ylim=c(l,u), xlab="VariaÃ§Ã£o Percentual de PreÃ§o"
         ,ylab="", xaxt='n'
         ,main=paste0("VariaÃ§Ã£o de preÃ§o x previsÃ£o acumulada para os prÃ³ximos ",prazo," dias"), lty=2, cex=0.8, pch=20, yaxt="n", cex.axis=0.8, cex.lab=1)
    
    
    axis(2, at=seq(l, u, length.out=6), labels=FALSE)
    axis(1, at=var.preco.x.prev$var.preco, labels = paste0(var.preco.x.prev$var.preco*100, '%'))
    if(max(var.preco.x.prev$prev > 1e4)){
      abline(h=round(seq(l/1000, u/1000, length.out=6),3)*1000, lty=3, col='grey')
      text(y = seq(l,u,length.out = 6)
           ,labels=round(seq(l/1000, u/1000, length.out=6),0), par("usr")[1], srt = 0, pos = 2, xpd = TRUE)
      lines(var.preco.x.prev[,"var.preco"], var.preco.x.prev[,"lb"], t="b", col=2, lty=2, cex=0.8, pch=20 )
      lines(var.preco.x.prev[,"var.preco"], var.preco.x.prev[,"ub"], t="b", col=2, lty=2, cex=0.8, pch=20 )
      title(ylab=expression("PrevisÃ£o ( x" ~ 10^3 ~ ")"), line=2.4, cex.lab=1)#, cex.lab=1.2, family="Calibri Light")
    } else{
      text(y = seq(l,u,length.out = 6)
           ,labels=round(seq(l, u, length.out=6),0), par("usr")[1], srt = 0, pos = 2, xpd = TRUE)
      lines(var.preco.x.prev[,"var.preco"], var.preco.x.prev[,"lb"], t="b", col=2, lty=2, cex=0.8, pch=20 )
      lines(var.preco.x.prev[,"var.preco"], var.preco.x.prev[,"ub"], t="b", col=2, lty=2, cex=0.8, pch=20 )
      title(ylab='PrevisÃ£o', line=2.4, cex.lab=1)#, cex.lab=1.2, family="Calibri Light")
    }
    g.pxprev= recordPlot()
  } else{
    g.pxprev = hist(1)
  }
  stats.matrix <- rbind.data.frame(bottom.MAPE,bottom.MAE,R2)
  colnames(stats.matrix) <- 'Estatisticas'
  rownames(stats.matrix) <- c('MAPE','MAE','R2')
  
  colnames(t.mensal) <- c('Data','Observado','Previsto')
  colnames(t.diario) <- c('Data','Observado','Previsto','Lower Bound','Upper Bound')
  
  t.diario.insample <- subset(t.diario, is.na(t.diario[['Lower Bound']]))
  colunas.retirar.insample <- which(colnames(t.diario.insample) %in%  c('Lower Bound','Upper Bound'))
  t.diario.insample <- t.diario.insample[,-colunas.retirar.insample]
  t.diario.outofsample <- subset(t.diario, !is.na(t.diario[['Lower Bound']]))
  colunas.retirar.outofsample <- which(colnames(t.diario.outofsample) == 'Observado')
  t.diario.outofsample <- t.diario.outofsample[,-colunas.retirar.outofsample]
  
  #Export em excel:
  write.xlsx(stats.matrix, file="/home/RProjetos/Interfaces/output.previsao.xlsx", sheetName="Estatisticas da previsao", col.names = TRUE)
  
  write.xlsx(t.mensal, file="/home/RProjetos/Interfaces/output.previsao.xlsx", sheetName="Previsao mensal", append=TRUE, row.names = FALSE)
  write.xlsx(t.diario.insample, file="/home/RProjetos/Interfaces/output.previsao.xlsx", sheetName="Previsao diaria in-sample", append=TRUE, row.names = FALSE)
  write.xlsx(t.diario.outofsample, file="/home/RProjetos/Interfaces/output.previsao.xlsx", sheetName="Previsao diaria out of sample", append=TRUE, row.names = FALSE)
  write.xlsx(betas.final, file="/home/RProjetos/Interfaces/output.previsao.xlsx", sheetName="Coeficientes beta", row.names = FALSE,append=TRUE)
  
  rownames(stats.matrix) <- c('MAPE(%) *','MAE *','R2 **') ## Botando os asteriscos para bater com a legenda da tabela, no Shiny
  
  data1=Sys.time()
  
  cat(paste("\n InÃ­cio em ", data0, ".\n TÃ©rmino em ",Sys.time(),". Tempo total de ", round(data1-data0,3),"." ,sep="" ))
  
  return=list(bottom.MAPE= bottom.MAPE, bottom.MAE= bottom.MAE, best.pred= best.pred, betas.final=betas.final, R2=R2, g.erro=g.erro,
              g.pxprev=g.pxprev, betas.final.latex = xtable(betas.final), stats.matrix=stats.matrix,
              stats.matrix.latex=xtable(stats.matrix), erro.final=erro.final, dados_para_plot_diario = dados_para_plot_diario,
              dados_para_plot_mensal = dados_para_plot_mensal)
  
}
