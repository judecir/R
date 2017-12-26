# LENDO DADOS ====
dados <- read.table(file.choose(), header=TRUE, sep=";", dec=".")

#AJEITANDO CLASSES =====
sapply(dados, class) #INFORMA OS TIPOS DAS COLUNAS
dados$Data <- as.Date(dados$Data, format="%d/%m/%y") #CONFIGURA COLUNA PARA FORMADO DATA
dados$Hora <- dados$Hora/100 #CORRIGE A HORA PARA 0-23
sapply(dados, class) #INFORMA OS TIPOS DE COLUNAS NOVAMENTE

dados[is.na(dados$DirecaoVento), ]$DirecaoVento <- -1
dados[is.na(dados$VelocidadeVento), ]$VelocidadeVento <- -1

#CRIANDO TABELA INICIAL ====
inicio <- "01/01/97" #INICIO DAS OBSERVACOES
fim <- "01/01/17" #FINAL DAS OBSERVACOES
inicio <- as.Date(inicio,format="%d/%m/%y")
fim <- as.Date(fim,format="%d/%m/%y")
fim - inicio

h <- seq(0,23) #CRIANDO VETOR DE HORA
HORA <- rep(h,(fim - inicio)+1) #CRIANDO COLUNA HORA

d <- seq(inicio, fim, by="day") #CRIANDO VETOR DE DIA
DIA <- rep(d, 24) #CRIANDO COLUNA DIA
DIA <- sort(DIA) #ORDENANDO COLUNA

direcao <- rep(-1, length(DIA))
velocidade <-rep(-1, length(DIA))

tabela <- data.frame(DIA, HORA, direcao, velocidade)

rm(DIA, HORA, h, d, direcao, velocidade, inicio, fim)

#ORGANIZAR ====

for (j in 1:length(dados$Estacao)){
  if(tabela[tabela$DIA == dados$Data[j] & tabela$HORA == dados$Hora[j], ]$direcao == -1){
    tabela[tabela$DIA == dados$Data[j] & tabela$HORA == dados$Hora[j], ]$direcao <- dados$DirecaoVento[j]
    tabela[tabela$DIA == dados$Data[j] & tabela$HORA == dados$Hora[j], ]$velocidade <- dados$VelocidadeVento[j]
  }
  cat(j/length(dados$Estacao), "% \n")
}

auxD <- -1
auxV <- -1
for (i in 1:length(tabela$DIA)){
  if(tabela$direcao[i] == -1){
    tabela$direcao[i] <- auxD
    tabela$velocidade[i] <- auxV
  }else{
    auxD <-  tabela$direcao[i] 
    auxV <-  tabela$velocidade[i] 
  }
  cat(100*i/length(tabela$DIA), "% \n")
}


#SALVANDO TABELA ===
write.table(tabela, file="C:/Users/404320/Documents/Projetos/Treinamento R/datele-corrigido.csv", row.names=FALSE)
