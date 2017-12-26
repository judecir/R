#CAGECE - GEPED 2017
#Autor: Judecir Cavalcante
#Codigo para gerar graficos dos parametros para unidades de negocio, Aquiraz, Caucaia, ETA Gaviao e ETA Oeste

#Definindo diretorio de trabalho =================
setwd("E:/CAGECE/Projetos/dados")
#=================================================

#Carregando pacote necessario ====================
#Pacote precisa estar instalado, utilizar RStudio para isso
library("gdata")
#=================================================

#Lendo tabela das unidades, Aquiraz e Caucaia ====
unidades <-  read.table("parametros-unidades.csv", header=TRUE, sep=";", quote="/",dec=",",fill=T)
#=================================================

#Retirando possiveis erros nos parametros ==========
#Parametros em questao devem ser todos maiores que zero
unidades <- unidades[unidades$VALOR_RESULTADO >= 0, ]
#=================================================

#Verificando colunas e convertendo ===============
sapply(unidades, class)
unidades$DATA_COLETA <- as.Date(unidades$DATA_COLETA, format="%d/%m/%y")
unidades$MES_COLETA <- as.factor(unidades$MES_COLETA)
unidades$ANO_COLETA <- as.factor(unidades$ANO_COLETA)
unidades$ANO_MES_COLETA <- as.factor(unidades$ANO_MES_COLETA)
unidades$UAD_SGL_UNIDADE_ADMINISTRATIVA <- trim(unidades$UAD_SGL_UNIDADE_ADMINISTRATIVA)
sapply(unidades, class)
#=================================================

#Parametros para gerar graficos ==================
nomesParametros <- c( "Cloro",
                      #"Coliformes Totais", 
                      "Condutividade", 
                      "Cor", 
                      #"Escherichia coli", 
                      "Turbidez")

unidadeParametros <- c("mg/L", 
                       #"NMP/100mL", 
                       "µS/cm", 
                       "UC", 
                       #"NMP/100mL", 
                       "NTU")

nomesUnidades <- c("UN-MTN",
                   "UN-MTS",
                   "UN-MTL",
                   "UN-MTO")
#==============================================

#Iniciando analise e geracao dos graficos =======
#Criando tabela auxiliares
  tabela <- data.frame() #c/ estatisticas gerais
  tabelaN <- data.frame()#c/ estatisticas da UNMTN
  tabelaS <- data.frame()#c/ estatisticas da UNMTS
  tabelaL <- data.frame()#c/ estatisticas da UNMTL
  tabelaO <- data.frame()#c/ estatisticas da UNMTO
  Taqu <- data.frame()#c/ estatisticas de Aquiraz
  Tcau <- data.frame()#c/ estatisticas de Caucaia
  
cat("Analise UNMT e ETAS")
for (p in 1:length(nomesParametros)){
  #Tabela auxiliar que salva analises dos parametros
  param <- unidades[unidades$PARAMETRO==nomesParametros[p],]
  
  #Tabela com estatisticas Geral
  v <- cbind(nomesParametros[p], max(param$VALOR_RESULTADO), min(param$VALOR_RESULTADO), mean(param$VALOR_RESULTADO))
  tabela <- rbind(tabela, v)
  
  #Tabela com estatisticas da UN-MTN
  aux <- unidades[unidades$UAD_SGL_UNIDADE_ADMINISTRATIVA == "UN-MTN" & unidades$PARAMETRO == nomesParametros[p],]
  v <- cbind(nomesParametros[p], max(aux$VALOR_RESULTADO), min(aux$VALOR_RESULTADO), mean(aux$VALOR_RESULTADO))
  tabelaN <- rbind(tabelaN, v)
  
  #Tabela com estatisticas da UN-MTS
  aux <- unidades[unidades$UAD_SGL_UNIDADE_ADMINISTRATIVA == "UN-MTS" & unidades$PARAMETRO == nomesParametros[p],]
  v <- cbind(nomesParametros[p], max(aux$VALOR_RESULTADO), min(aux$VALOR_RESULTADO), mean(aux$VALOR_RESULTADO))
  tabelaS <- rbind(tabelaS, v)
  
  #Tabela com estatisticas da UN-MTL
  aux <- unidades[unidades$UAD_SGL_UNIDADE_ADMINISTRATIVA == "UN-MTL" & unidades$PARAMETRO == nomesParametros[p],]
  v <- cbind(nomesParametros[p], max(aux$VALOR_RESULTADO), min(aux$VALOR_RESULTADO), mean(aux$VALOR_RESULTADO))
  tabelaL <- rbind(tabelaL, v)
  
  #Tabela com estatisticas da UN-MTO
  aux <- unidades[unidades$UAD_SGL_UNIDADE_ADMINISTRATIVA == "UN-MTO" & unidades$PARAMETRO == nomesParametros[p],]
  v <- cbind(nomesParametros[p], max(aux$VALOR_RESULTADO), min(aux$VALOR_RESULTADO), mean(aux$VALOR_RESULTADO))
  tabelaO <- rbind(tabelaO, v)
  
  #Criando grafico boxplot mensal
  setwd("E:/CAGECE/Projetos/dados/graficos/mensais")
  png(filename = paste("boxplot-mensal", nomesParametros[p],".png", sep=""),width = 1200, height = 1000)
    par(mfrow=c(2,2))
    for(u in nomesUnidades){
      paramUN <- param[param$UAD_SGL_UNIDADE_ADMINISTRATIVA==u,]
      plot(paramUN$MES_COLETA, paramUN$VALOR_RESULTADO, 
           main=paste(nomesParametros[p], u," Mensal - 2008 a 2017" ,sep=" "),
           xlab="Meses", #col=rainbow(12),
           ylab=unidadeParametros[p], outline=F)
      }
  dev.off()
  
  #Criando histograma
  setwd("E:/CAGECE/Projetos/dados/graficos/histogramas")
  png(filename = paste("histograma-", nomesParametros[p],".png", sep=""),width = 1200, height = 1000)
    par(mfrow=c(2,2))
    for(u in nomesUnidades){
      paramU <- param[param$UAD_SGL_UNIDADE_ADMINISTRATIVA==u,]
      hist(paramU$VALOR_RESULTADO, 
           main=paste(nomesParametros[p], u, "- 2008 a 2017" ,sep=" "),
           xlab= unidadeParametros[p],
           ylab="Frequência Relativa", freq=T, density=30,
           labels=T)
    }
  dev.off()  
  
  #Criando grafico boxplot anual
  setwd("E:/CAGECE/Projetos/dados/graficos/anuais")
  png(filename = paste(nomesParametros[p],".png", sep=""),width = 1200, height = 1000)
    par(mfrow=c(2,2))
    for(u in nomesUnidades){
      paramUN <- param[param$UAD_SGL_UNIDADE_ADMINISTRATIVA==u,]
      plot(paramUN$ANO_COLETA, paramUN$VALOR_RESULTADO, 
           main=paste(nomesParametros[p], u,"Anual - 2008 a 2017" ,sep=" "),
           xlab="Anos", #col=rainbow(12),
           ylab=unidadeParametros[p], outline=F)
    }
  dev.off()
  
  #AQUIRAZ E CAUCAIA
  
  aquiraz <- unidades[unidades$LOC_DSC_LOCALIDADE_IBGE=="AQUIRAZ" & unidades$PARAMETRO==nomesParametros[p],]
  caucaia <- unidades[unidades$LOC_DSC_LOCALIDADE_IBGE=="CAUCAIA" & unidades$PARAMETRO==nomesParametros[p],]
  
  #Tabelas auxiliares para criar planilha com estatisticas
    param <- unidades[unidades$LOC_DSC_LOCALIDADE_IBGE=="AQUIRAZ" & unidades$PARAMETRO==nomesParametros[p],]
    v <- cbind(nomesParametros[p], max(param$VALOR_RESULTADO), min(param$VALOR_RESULTADO), mean(param$VALOR_RESULTADO))
    Taqu <- rbind(Taqu, v)
    
    param <- unidades[unidades$LOC_DSC_LOCALIDADE_IBGE=="CAUCAIA" & unidades$PARAMETRO==nomesParametros[p],]
    v <- cbind(nomesParametros[p], max(param$VALOR_RESULTADO), min(param$VALOR_RESULTADO), mean(param$VALOR_RESULTADO))
    Tcau <- rbind(Tcau, v)
  
    
  #Criando grafico boxplot mensal  
  setwd("E:/CAGECE/Projetos/dados/graficos/mensais")
  png(filename = paste("AQUIRAZeCAUCAIAboxplot-mensal", nomesParametros[p],".png", sep=""),width = 1200, height = 1000)
  par(mfrow=c(2,1))
  plot(aquiraz$MES_COLETA, aquiraz$VALOR_RESULTADO, 
       main=paste(nomesParametros[p], "Aquiraz","Mensal - 2008 a 2017" ,sep=" "),
       xlab="Meses", #col=rainbow(12),
       ylab=unidadeParametros[p], outline=F)
  
  plot(caucaia$MES_COLETA, caucaia$VALOR_RESULTADO, 
       main=paste(nomesParametros[p], "Caucaia","Mensal - 2008 a 2017" ,sep=" "),
       xlab="Meses", #col=rainbow(12),
       ylab=unidadeParametros[p], outline=F)
  dev.off()  
  
  #Criando grafico histograma
  setwd("E:/CAGECE/Projetos/dados/graficos/histogramas")
  png(filename = paste("AQUIRAZeCAUCAIAhistograma-", nomesParametros[p],".png", sep=""),width = 1200, height = 1000)
  par(mfrow=c(2,1))
  hist(aquiraz$VALOR_RESULTADO, 
       main=paste(nomesParametros[p], "Aquiraz - 2008 a 2017" ,sep=" "),
       xlab= unidadeParametros[p],
       ylab="Frequência Relativa", freq=T, density=30,
       labels=T)
  hist(caucaia$VALOR_RESULTADO, 
       main=paste(nomesParametros[p], "Caucaia - 2008 a 2017" ,sep=" "),
       xlab= unidadeParametros[p],
       ylab="Frequência Relativa", freq=T, density=30,
       labels=T)
  dev.off()  
  
  #Criando grafico boxplot anual
  setwd("E:/CAGECE/Projetos/dados/graficos/anuais")
  png(filename = paste("AQUIRAZeCAUCAIAboxplot-anual", nomesParametros[p],".png", sep=""),width = 1200, height = 1000)
  par(mfrow=c(2,1))
  plot(aquiraz$ANO_COLETA, aquiraz$VALOR_RESULTADO, 
       main=paste(nomesParametros[p], "Aquiraz","Anual - 2008 a 2017" ,sep=" "),
       xlab="Anos", #col=rainbow(12),
       ylab=unidadeParametros[p], outline=F)
  
  plot(caucaia$ANO_COLETA, caucaia$VALOR_RESULTADO, 
       main=paste(nomesParametros[p], "Caucaia","Anual - 2008 a 2017" ,sep=" "),
       xlab="Anos", #col=rainbow(12),
       ylab=unidadeParametros[p], outline=F)
  dev.off()  
  
  cat(100*p/length(nomesParametros), "% \n")
}

#Salvando tabelas 
  setwd("E:/CAGECE/Projetos/dados")
  write.table(file="geral",tabela, sep=";", dec=",")
  write.table(file="norte",tabelaN, sep=";", dec=",")
  write.table(file="sul",tabelaS, sep=";", dec=",")
  write.table(file="leste",tabelaL, sep=";", dec=",")
  write.table(file="oeste",tabelaO, sep=";", dec=",")
  write.table(file="estatisticas-aquiraz",Taqu, sep=";", dec=",")
  write.table(file="estatisticas-caucaia",Tcau, sep=";", dec=",")

#=================================================

  
  

#Analise ETA Gaviao e ETA Oeste ==================  
setwd("E:/CAGECE/Projetos/dados")

eta_gaviao <-  read.table("eta-gaviao.csv", header=TRUE, sep=";",dec=",")
eta_oeste <-  read.table("eta-oeste.csv", header=TRUE, sep=";",dec=",")

sapply(eta_gaviao, class)
eta_gaviao$DATA.DE.COLETA <- as.Date(eta_gaviao$DATA.DE.COLETA, format="%d/%m/%y")
eta_gaviao$MES <- as.factor(eta_gaviao$MES)
eta_gaviao$ANO <- as.factor(eta_gaviao$ANO)
sapply(eta_gaviao, class)

sapply(eta_oeste, class)
eta_oeste$DATA.DE.COLETA <- as.Date(eta_oeste$DATA.DE.COLETA, format="%d/%m/%y")
eta_oeste$MES <- as.factor(eta_oeste$MES)
eta_oeste$ANO <- as.factor(eta_oeste$ANO)
sapply(eta_oeste, class)

paramG <- levels(eta_gaviao$PARAMETRO)
paramO <- levels(eta_oeste$PARAMETRO)

nParamG <- c("mg de CaCO3 / L", #Alcalinidade
             "mg de CaCO3 / L", #Alcalinidade 
             "mg de CaCO3 / L", #Alcalinidade 
             "mg/L", #Aluminio
             "mg/L", #Aluminio
             "mg/L", #Amonia
             "mg/L", #Amonia
             "mg/L", #Amonia
             "mg/L", #Boro
             "mg/L", #Calcio
             "mg/L", #Cloreto
             "mg/L", #Cloro
             "mg/L", #Cloro
             "µS/cm", #Condutividade
             "UC", #Cor
             "mg de CaCO3 / L", #Dureza
             "mg/L", #Ferro
             "mg/L", #Ferro
             "mg/L", #Fluoreto
             "mg/L", #Magnesio
             "mg/L", #Manganes
             "mg/L", #Nitrato
             "Valor", #pH
             "mg/L", #Sodio
             "mg/L", #Solidos Dissolvidos
             "mg/L", #Sulfato
             "NTU", #Turbidez
             "mg/L" #Zinco
             )

nParamO <- c("mg de CaCO3 / L", #Alcalinidade
             "mg de CaCO3 / L", #Alcalinidade 
             "mg de CaCO3 / L", #Alcalinidade 
             "mg/L", #Aluminio
             "mg/L", #Amonia
             "mg/L", #Amonia
             "mg/L", #Boro
             "mg/L", #Calcio
             "mg/L", #Cloreto
             "mg/L", #Cloro
             "mg/L", #Cloro
             "µS/cm", #Condutividade
             "UC", #Cor
             "mg de CaCO3 / L", #Dureza
             "mg/L", #Ferro
             "mg/L", #Ferro
             "mg/L", #Fluoreto
             "mg/L", #Magnesio
             "mg/L", #Manganes
             "mg/L", #Nitrato
             "Valor", #pH
             "mg/L", #Sodio
             "mg/L", #Solidos Dissolvidos
             "mg/L", #Sulfato
             "NTU", #Turbidez
             "mg/L" #Zinco
             )

tabelaGa <- data.frame()
tabelaOt <- data.frame()

for(p in 1:length(paramG)){
  param <- eta_gaviao[eta_gaviao$PARAMETRO==paramG[p],]
  
  v <- cbind(paramG[p], max(param$VALOR.RESULTADO), min(param$VALOR.RESULTADO), mean(param$VALOR.RESULTADO))
  tabelaGa <- rbind(tabelaGa, v)
  
  setwd("E:/CAGECE/Projetos/dados/graficos/mensais")
  png(filename = paste("ETAGAVIAOboxplot-mensal", paramG[p],".png", sep=""),width = 1200, height = 1000)
    plot(param$MES, param$VALOR.RESULTADO, 
         main=paste(paramG[p], "Eta Gavião","Mensal - 2008 a 2017" ,sep=" "),
         xlab="Meses", #col=rainbow(12),
         ylab= nParamG[p], outline=F)
  dev.off()
  
  setwd("E:/CAGECE/Projetos/dados/graficos/anuais")
  png(filename = paste("ETAGAVIAOboxplot-anual", paramG[p],".png", sep=""),width = 1200, height = 1000)
  plot(param$ANO, param$VALOR.RESULTADO, 
       main=paste(paramG[p], "Eta Gavião","Anual - 2008 a 2017" ,sep=" "),
       xlab="Anos", #col=rainbow(12),
       ylab= nParamG[p], outline=F)
  dev.off()
  
  setwd("E:/CAGECE/Projetos/dados/graficos/histogramas")
  png(filename = paste("ETAGAVIAOhistograma-", paramG[p],".png", sep=""),width = 1200, height = 1000)
  hist(param$VALOR.RESULTADO, 
       main=paste(paramG[p], "Eta Gavião - 2008 a 2017" ,sep=" "),
       xlab= nParamG[p],
       ylab="Frequência Relativa", freq=T, density=30,
       labels=T)
  dev.off()
  
  cat(100*p/length(paramG), "% \n")
}

for(p in 1:length(paramO)){
  
  param <- eta_oeste[eta_oeste$PARAMETRO==paramO[p],]
  v <- cbind(paste(paramO[p]," (", nParamO[p] ,")", sep=""), max(param$VALOR.RESULTADO), min(param$VALOR.RESULTADO), mean(param$VALOR.RESULTADO))
  tabelaOt <- rbind(tabelaOt, v)
  
  setwd("E:/CAGECE/Projetos/dados/graficos/mensais")
  png(filename = paste("ETAOESTEboxplot-mensal", paramO[p],".png", sep=""),width = 1200, height = 1000)
  plot(param$MES, param$VALOR.RESULTADO, 
       main=paste(paramO[p], "Eta Oeste","Mensal - 2013 a 2017" ,sep=" "),
       xlab="Meses", #col=rainbow(12),
       ylab= nParamO[p], outline=F)
  dev.off()
  
  setwd("E:/CAGECE/Projetos/dados/graficos/anuais")
  png(filename = paste("ETAOESTEboxplot-anual", paramO[p],".png", sep=""),width = 1200, height = 1000)
  plot(param$ANO, param$VALOR.RESULTADO, 
       main=paste(paramO[p], "Eta Oeste","Anual - 2013 a 2017" ,sep=" "),
       xlab="Anos", #col=rainbow(12),
       ylab= nParamO[p], outline=F)
  dev.off()
  
  setwd("E:/CAGECE/Projetos/dados/graficos/histogramas")
  png(filename = paste("ETAOESTEhistograma-", paramO[p],".png", sep=""),width = 1200, height = 1000)
  hist(param$VALOR.RESULTADO, 
       main=paste(paramO[p], "Eta Oeste - 2013 a 2017" ,sep=" "),
       xlab= nParamO[p],
       ylab="Frequência Relativa", freq=T, density=30,
       labels=T)
  dev.off()  
  
  cat(100*p/length(paramO), "% \n")
}

setwd("E:/CAGECE/Projetos/dados")
write.table(file="estatisticas-eta_gaviao",tabelaGa, sep=";", dec=",")
write.table(file="estatisticas-eta_oeste",tabelaOt, sep=";", dec=",")
#===========================================