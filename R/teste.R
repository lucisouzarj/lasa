forModelData= function(StoreGroup, SKUsGroup, horizon) {
  #shinyjs::hide('div.slider.choque.previsao')
  load("/RProjetos/Dados/betaspricing.Rda")
  if(!is.na(betas.pricing[SKUsGroup,'beta'])){
    beta.exists=1
    beta.price <- betas.pricing[SKUsGroup,'beta']
    beta.pvalue <- betas.pricing[SKUsGroup,'p.value']
  }else{
    beta.exists=0
    beta.price <- NA
    beta.pvalue <- NA
  }
  
  #infos lojas:
  #load("~/lojas.Rda")
  lojas <- dataGetDetailStore()
  lojas$cidade_limpa <- stri_trans_general(lojas$CGEO_TX_CIDADE, "Latin-ASCII")
  cols.interesse.loja=c('GEO_CD_LOJA','ADMG_CD_GR','EGEO_UF_ESTADO','cidade_limpa')
  info_loja=cbind(lojas[,cols.interesse.loja],str_sub(lojas[,c("GEO_TX_CENTRO_DISTRIBUICAO")],-2,-1))
  g.pri=priRCTComposition() #grupos pricing
  info_loja_completa <- merge(info_loja, g.pri, by.x= 'GEO_CD_LOJA', by.y= 'Loja', sort = FALSE) # 1 min
  colnames(info_loja_completa)=c("Loja","G.LASA","UF","CIDADE","CD","G.PRI")
  
  #cria tabela usuario:
  
  grp=SKUsGroup[1]
  info.grp.sku=paste0(c("VDA_BRT_G","VDA_QTD_G","EST_QTD_G","EST_TRAN_G","p_99_G"),rep(grp,5))
  cols.interesse.base=c(c("Data","Loja","opened"),info.grp.sku)
  if (StoreGroup[1]=="BRASIL"){
    lojas.interesse=as.vector(t(subset(info_loja_completa, select = "Loja")))
  } else {
    lojas.interesse=as.vector(t(subset(info_loja_completa, eval(parse(text=StoreGroup[1])) == StoreGroup[2], select = "Loja")))
  }
  group.stores.numb=length(lojas.interesse)
  base.interesse <- base[which(base[,"Loja"] %in% lojas.interesse),cols.interesse.base]
  colnames(base.interesse)=c("Data","Loja","opened","v","q","s","str","p99")
  
  #Trata base de interesse (selecionada pelo usuÃ¡rio):
  serie.interesse=aggregate(. ~ Data, data = base.interesse, FUN = 'sum', na.rm = TRUE, na.action=NULL) #NA->0 Ideal? NÃ£o
  #or: serie.interesse=aggregate(base.interesse[,-c(1,2)], by=list(base.interesse$Data), FUN=sum, na.rm=TRUE) #NÃ£o nomeia a coluna de Data como Data, mas como Group1
  
  #MantÃ©m soma das colunas preenchidas sÃ³ com NA como NA:
  # base.teste=data.frame(Data=base.interesse[,"Data"],teste=(is.na(base.interesse[,'q']))*1,l=1)
  # group.stores.numbxn.na=aggregate(. ~ Data, data = base.teste, FUN = 'sum', na.rm = TRUE, na.action=NULL) #NA->0 Ideal? NÃ£o
  # teste.na=group.stores.numbxn.na[,"teste"]==group.stores.numbxn.na[,"l"]
  # serie.interesse[teste.na]=NA

  aux=rowSums(serie.interesse[,-c(1:3)])
  for(i in length(aux):1){
    if(aux[i]!=0){break}
  }
  serie.interesse=serie.interesse[1:i,]
  
  
  DDSem=auxDailySeriesDummiesWday(serie.interesse[,'Data']) #Dias da semana
  DFer=auxDailySeriesDummiesNatlHolidays(serie.interesse[,'Data']) #Feriados nacionais
  
  #Promocoes:
  load("/RProjetos/Dados/prom.atu.Rda")
  prom=prom.atu[[1]]
  MPromDate=max(prom[,'data'])
  mPromDate=min(prom[,'data'])
  MSerieDate=max(as.Date(serie.interesse[,'Data']))
  mSerieDate=min(as.Date(serie.interesse[,'Data']))
  min_data_obs=max(mPromDate,mSerieDate)
  max_data_obs=min(MPromDate,MSerieDate)
  
  if (min_data_obs==as.Date(timeFirstDayInMonth(min_data_obs)) ){
    fdcm_obs=min_data_obs
  } else {
    fdcm_obs=as.Date(timeFirstDayInMonth(min_data_obs)) #primeiro dia dos meses completos observados
  }
  if (max_data_obs==as.Date(timeLastDayInMonth(max_data_obs)) ){
    ldcm_obs=max_data_obs
  } else {
    ldcm_obs=as.Date(timeLastDayInMonth(max_data_obs-30)) #Ãºltimo dia dos meses completos observados
  }
  #PerÃ?odo base completa
  # cat(paste("\n Dados para ajuste de modelos com perÃ?odo", fdcm_obs, "a", ldcm_obs,". Ãltimo dia da tabela de
  #           \n promoÃ§Ãµes", MPromDate, "e Ãºltimo dia da base", MSerieDate,"."))
  
  #preÃ§o:
  p=serie.interesse[,"p99"]/length(unique(base.interesse$Loja))#p=serie.interesse[,'v']/serie.interesse[,'q']
  
  #Estoque loja:
  # slj=serie.interesse[,'s']/serie.interesse[,'str']
  # slj[is.na(slj)]=0
  # slj[is.nan(slj)]=0
  # slj[is.infinite(slj)]=0
  
  
  #Dados para previsÃ£o Ã s cegas:
  #linhas para previsÃ£o Ã s cegas, no mÃ?nimo, atÃ© final do mÃªs da data horizon:
  if ( (as.Date( horizon)- as.Date( ldcm_obs) ) <=( as.Date( timeLastDayInMonth( ldcm_obs+ 1) )- as.Date( ldcm_obs) )) 
    {horizon= as.Date(timeLastDayInMonth(ldcm_obs+1))}
  
  #periodo 1 mÃªs a frente:
  #blind.period=seq(as.Date(ldcm_obs+1), as.Date(timeLastDayInMonth(ldcm_obs+25)), by="days")
  blind.period=seq(as.Date(MSerieDate+1), as.Date(horizon), by="days")
  #Dados:
  #q=NA; Dias da semana; Dias de promoÃ§Ã£o; Feriados Nacionais; preÃ§o mÃ©dio; estoque merc. entregue; nÃºmero de lojas abertas.
  q.blind=rep(NA,length(blind.period))
  #Dias da semana
  DDSem.blind=auxDailySeriesDummiesWday(blind.period) 
  #promoÃ§Ãµes:merge blind.period x prom
  prom.obs=subset(prom, data %in% blind.period)
  falta=blind.period[!(blind.period %in% prom[,'data'])]
  prom.falta=data.frame(matrix(0,nrow=length(falta),ncol=ncol(prom)))
  colnames(prom.falta)=colnames(prom)
  prom.falta[,'data']=falta
  prom.blind=rbind(prom.obs, prom.falta)
  MaxObs.prom=NULL
  if (length(falta)!=0) MaxObs.prom=min(falta)-1
  #Feriados nacionais
  DFer.blind=auxDailySeriesDummiesNatlHolidays(blind.period) 
  #preÃ§o mÃ©dio:
  p.blind= rep(p[which(as.Date(serie.interesse[,'Data'])==MSerieDate)], length(blind.period))
  #estoque
  
  
  #NÃºmero de lojas abertas:
  num.lojas.blind=forFutureOpenStoresNum(blind.period, DFer, serie.interesse[,"opened"], MSerieDate,
                                DDSem, DFer.blind, DDSem.blind)
  #num.lojas.blind=rep(0,nrow(DDSem.blind))## Luciene arrumar forFutureOpenStoresNum
  
  
  #Todos os dados:
  dias=as.Date(serie.interesse[,'Data'])
  selec=dias>=fdcm_obs & dias<=MSerieDate #ldcm_obs
  dias=dias[selec]
  dados=data.frame(y=c(serie.interesse[selec,'q'], q.blind), 
                   rbind(DDSem[selec,], DDSem.blind), 
                   rbind(prom[prom[,'data'] %in% dias,-1], prom.blind[,-1]), 
                   rbind(DFer[selec,], DFer.blind), 
                   p=c(p[selec], p.blind),
                   #slj=slj[selec],
                   qlj=c(serie.interesse[selec,"opened"],num.lojas.blind))
  rownames(dados)=c(as.Date(serie.interesse[selec,"Data"]),as.Date(blind.period))
  
  
  #quebra em in e out of sample
  in.data=dados[1:which(rownames(dados)==MSerieDate),]
  out.data.aux=dados[(which(rownames(dados)==MSerieDate)+1):nrow(dados),]
  
  if(horizon>tail(rownames(out.data.aux),1)){
    datas.aux=seq(as.Date(head(rownames(out.data.aux),1)),as.Date(horizon),by="day") #datas dias out
    aux=matrix(0,length(datas.aux)-nrow(out.data.aux),ncol(out.data.aux))
    colnames(aux)=colnames(out.data.aux)
    out.data=rbind(out.data.aux,aux)
    rownames(out.data)=datas.aux
    out.data[,"p"]=round(out.data[1,"p"],2)
    out.data[,c("seg", "ter", "qua", "qui", "sex", "sab")]=auxDailySeriesDummiesWday(datas.aux)
    aux=data.frame(auxDailySeriesDummiesNatlHolidays(datas.aux))
    out.data[,colnames(aux)]=aux
    
    num.lojas.blind=forFutureOpenStoresNum(datas.aux, in.data[,colnames(aux)], serie.interesse[,"opened"], MSerieDate,
                                  DDSem, auxDailySeriesDummiesNatlHolidays(datas.aux), auxDailySeriesDummiesWday(datas.aux))
    out.data$qlj=num.lojas.blind
    
    
  }
  if(horizon==tail(rownames(out.data.aux),1)){
    out.data=out.data.aux
  }
  if(horizon<tail(rownames(out.data.aux),1)){
    out.data=out.data.aux[1:which(rownames(out.data.aux)==horizon),]
  }
  
  out.data$y <- NULL
  out.data$data <- rownames(out.data)
  out.data$dados.na.base <- ifelse(out.data$data <= MPromDate, 'Sim', 'Nao')
  out.data$data <- NULL
  out.data <- cbind.data.frame('dados.na.base' = out.data$dados.na.base, 'p' = out.data$p, 'qlj' = out.data$qlj, 
                                out.data[,!(colnames(out.data) %in% c('dados.na.base','p','qlj'))])
  

  in.data <- cbind.data.frame('y' = in.data$y, 'p' = in.data$p, 'qlj' = in.data$qlj, 
                               in.data[,!(colnames(in.data) %in% c('y', 'p','qlj'))])
  
  #trata perÃ?odo anterior ao perÃ?odo de atividade do SKU como NA
  test.y=cumsum(in.data$y)
  in.data=in.data[test.y!=0,]
  analysis.ok=1
  if (nrow(serie.interesse)/365.25<1.5) {analysis.ok=0}

  return(list(in.data=in.data, out.data=out.data, fdcm_obs=fdcm_obs, ldcm_obs=ldcm_obs, MPromDate=MPromDate, MSerieDate=MSerieDate, blind.period=blind.period,
              MaxObs.prom=MaxObs.prom, group.stores.numb=group.stores.numb, beta.exists=beta.exists, beta.price=beta.price, beta.pvalue=beta.pvalue,
              analysis.ok=analysis.ok))
}
