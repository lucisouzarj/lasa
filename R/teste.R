forModelData= function(grp.loja.usuario,grp.sku.usuario,final.horizonte)
{
  load("/RProjetos/Dados/betaspricing.Rda")
  if(!is.na(betas.pricing[grp.sku.usuario,'beta'])){
    existe.beta=1
    beta.preco <- betas.pricing[grp.sku.usuario,'beta']
    beta.pvalue <- betas.pricing[grp.sku.usuario,'p.value']
  }else{
    existe.beta=0
    beta.preco <- NA
    beta.pvalue <- NA
  }
  
  #infos lojas:
  lojas <- dataGetDetailStore()
  lojas$cidade_limpa <- stri_trans_general(lojas$CGEO_TX_CIDADE, "Latin-ASCII")
  cols.interesse.loja=c('GEO_CD_LOJA','ADMG_CD_GR','EGEO_UF_ESTADO','cidade_limpa')
  info_loja=cbind(lojas[,cols.interesse.loja],str_sub(lojas[,c("GEO_TX_CENTRO_DISTRIBUICAO")],-2,-1))
  g.pri=priRCTComposition() #grupos pricing
  info_loja_completa <- merge(info_loja, g.pri, by.x= 'GEO_CD_LOJA', by.y= 'Loja', sort = FALSE) # 1 min
  colnames(info_loja_completa)=c("Loja","G.LASA","UF","CIDADE","CD","G.PRI")
  
  #cria tabela usuario:
  
  grp=grp.sku.usuario[1]
  info.grp.sku=paste0(c("VDA_BRT_G","VDA_QTD_G","EST_QTD_G","EST_TRAN_G","p_99_G"),rep(grp,5))
  cols.interesse.base=c(c("Data","Loja","opened"),info.grp.sku)
  if (grp.loja.usuario[1]=="BRASIL"){
    lojas.interesse=as.vector(t(subset(info_loja_completa, select = "Loja")))
  } else {
    lojas.interesse=as.vector(t(subset(info_loja_completa, eval(parse(text=grp.loja.usuario[1])) == grp.loja.usuario[2], select = "Loja")))
  }
  n.lojas=length(lojas.interesse)
  base.interesse <- base[which(base[,"Loja"] %in% lojas.interesse),cols.interesse.base]
  colnames(base.interesse)=c("Data","Loja","opened","v","q","s","str","p99")
  
  #Trata base de interesse (selecionada pelo usuÃ¡rio):
  serie.interesse=aggregate(. ~ Data, data = base.interesse, FUN = 'sum', na.rm = TRUE, na.action=NULL) #NA->0 Ideal?
  libera.analise=1
  if (length(serie.interesse[(serie.interesse[,5])!=0,5])/365.25<1.5) {libera.analise=0}
  aux=rowSums(serie.interesse[,-c(1:3)])
  for(i in length(aux):1){
    if(aux[i]!=0){break}
  }
  serie.interesse=serie.interesse[1:i,]
  
  #or: serie.interesse=aggregate(base.interesse[,-c(1,2)], by=list(base.interesse$Data), FUN=sum, na.rm=TRUE) #NÃ£o nomeia a coluna de Data como Data, mas como Group1
  
  DDSem=DSem(serie.interesse[,'Data']) #Dias da semana
  DFer=DFeriados(serie.interesse[,'Data']) #Feriados nacionais
  
  #Promocoes:
  load("/RProjetos/Dados/prom.atu.Rda")
  prom=prom.atu[[1]]
  MDataProm=max(prom[,'data'])
  mDataProm=min(prom[,'data'])
  MDataSerie=max(as.Date(serie.interesse[,'Data']))
  mDataSerie=min(as.Date(serie.interesse[,'Data']))
  min_data_obs=max(mDataProm,mDataSerie)
  max_data_obs=min(MDataProm,MDataSerie)
  
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
  #PerÃ­odo base completa
  # cat(paste("\n Dados para ajuste de modelos com perÃ­odo", fdcm_obs, "a", ldcm_obs,". Ãltimo dia da tabela de
  #           \n promoÃ§Ãµes", MDataProm, "e Ãºltimo dia da base", MDataSerie,"."))
  
  #preÃ§o:
  p=serie.interesse[,"p99"]/length(unique(base.interesse$Loja))#p=serie.interesse[,'v']/serie.interesse[,'q']
  
  #Estoque loja:
  # slj=serie.interesse[,'s']/serie.interesse[,'str']
  # slj[is.na(slj)]=0
  # slj[is.nan(slj)]=0
  # slj[is.infinite(slj)]=0
  
  #Dados para previsÃ£o Ã s cegas:
  #linhas para previsÃ£o Ã s cegas, no mÃ­nimo, atÃ© final do mÃªs da data final.horizonte:
  if ( (as.Date( final.horizonte)- as.Date( ldcm_obs) ) <=( as.Date( timeLastDayInMonth( ldcm_obs+ 1) )- as.Date( ldcm_obs) )) 
  {final.horizonte= as.Date(timeLastDayInMonth(ldcm_obs+1))}
  
  #periodo 1 mÃªs a frente:
  #per.blind=seq(as.Date(ldcm_obs+1), as.Date(timeLastDayInMonth(ldcm_obs+25)), by="days")
  per.blind=seq(as.Date(MDataSerie+1), as.Date(final.horizonte), by="days")
  #Dados:
  #q=NA; Dias da semana; Dias de promoÃ§Ã£o; Feriados Nacionais; preÃ§o mÃ©dio; estoque merc. entregue; nÃºmero de lojas abertas.
  q.blind=rep(NA,length(per.blind))
  #Dias da semana
  DDSem.blind=DSem(per.blind) 
  #promoÃ§Ãµes:merge per.blind x prom
  prom.obs=subset(prom, data %in% per.blind)
  falta=per.blind[!(per.blind %in% prom[,'data'])]
  prom.falta=data.frame(matrix(0,nrow=length(falta),ncol=ncol(prom)))
  colnames(prom.falta)=colnames(prom)
  prom.falta[,'data']=falta
  prom.blind=rbind(prom.obs, prom.falta)
  MaxObs.prom=NULL
  if (length(falta)!=0) MaxObs.prom=min(falta)-1
  #Feriados nacionais
  DFer.blind=DFeriados(per.blind) 
  #preÃ§o mÃ©dio:
  p.blind= rep(p[which(as.Date(serie.interesse[,'Data'])==MDataSerie)], length(per.blind))
  #estoque
  
  
  #NÃºmero de lojas abertas:
  num.lojas.blind=MediaNlojasAb(per.blind, DFer, serie.interesse[,"opened"], MDataSerie,
                                DDSem, DFer.blind, DDSem.blind)
  
  #num.lojas.blind=rep(0,nrow(DDSem.blind))## Luciene arrumar MediaNLojasAb
  
  #Todos os dados:
  dias=as.Date(serie.interesse[,'Data'])
  selec=dias>=fdcm_obs & dias<=MDataSerie #ldcm_obs
  dias=dias[selec]
  dados=data.frame(y=c(serie.interesse[selec,'q'], q.blind), 
                   rbind(DDSem[selec,], DDSem.blind), 
                   rbind(prom[prom[,'data'] %in% dias,-1], prom.blind[,-1]), 
                   rbind(DFer[selec,], DFer.blind), 
                   p=c(p[selec], p.blind),
                   #slj=slj[selec],
                   qlj=c(serie.interesse[selec,"opened"],num.lojas.blind))
  rownames(dados)=c(as.Date(serie.interesse[selec,"Data"]),as.Date(per.blind))
  
  
  #quebrar em in e out of sample
  dados.in=dados[1:which(rownames(dados)==MDataSerie),]
  dados.out.aux=dados[(which(rownames(dados)==MDataSerie)+1):nrow(dados),]
  
  if(final.horizonte>tail(rownames(dados.out.aux),1)){
    datas.aux=seq(as.Date(head(rownames(dados.out.aux),1)),as.Date(final.horizonte),by="day")
    aux=matrix(0,length(datas.aux)-nrow(dados.out.aux),ncol(dados.out.aux))
    colnames(aux)=colnames(dados.out.aux)
    dados.out=rbind(dados.out.aux,aux)
    rownames(dados.out)=datas.aux
    dados.out[,"p"]=round(dados.out[1,"p"],2)
    dados.out[,c("seg", "ter", "qua", "qui", "sex", "sab")]=DSem(datas.aux)
    aux=data.frame(DFeriados(datas.aux))
    dados.out[,colnames(aux)]=aux
    
    num.lojas.blind=MediaNlojasAb(datas.aux, dados.in[,colnames(aux)], serie.interesse[,"opened"], MDataSerie,
                                  DDSem, DFeriados(datas.aux), DSem(datas.aux))
    dados.out$qlj=num.lojas.blind
    
  }
  if(final.horizonte==tail(rownames(dados.out.aux),1)){
    dados.out=dados.out.aux
  }
  if(final.horizonte<tail(rownames(dados.out.aux),1)){
    dados.out=dados.out.aux[1:which(rownames(dados.out.aux)==final.horizonte),]
  }
  
  
  dados.out$y <- NULL
  dados.out$data <- rownames(dados.out)
  dados.out$dados.na.base <- ifelse(dados.out$data <= MDataProm, 'Sim', 'Nao')
  dados.out$data <- NULL
  dados.out <- cbind.data.frame('dados.na.base' = dados.out$dados.na.base, 'p' = dados.out$p, 'qlj' = dados.out$qlj, 
                                dados.out[,!(colnames(dados.out) %in% c('dados.na.base','p','qlj'))])
  
  dados.in <- cbind.data.frame('y' = dados.in$y, 'p' = dados.in$p, 'qlj' = dados.in$qlj, 
                               dados.in[,!(colnames(dados.in) %in% c('y', 'p','qlj'))])
  
  #trata perÃ­odo anterior ao perÃ­odo de atividade do SKU como NA
  test.y=cumsum(dados.in$y)
  dados.in=dados.in[test.y!=0,]
  
  if(libera.analise==0)
  {
    shinyjs::show('div.txt.analise.nao.liberada')
  } else{
    shinyjs::hide('div.txt.analise.nao.liberada')
  }
  
  
  return(list(dados.in=dados.in,dados.out=dados.out,fdcm_obs=fdcm_obs,ldcm_obs=ldcm_obs,MDataProm=MDataProm,MDataSerie=MDataSerie, per.blind=per.blind,
              MaxObs.prom=MaxObs.prom,n.lojas=n.lojas, existe.beta=existe.beta, beta.preco=beta.preco,beta.pvalue=beta.pvalue,
              libera.analise=libera.analise,grp.sku.usuario=grp.sku.usuario))
}
