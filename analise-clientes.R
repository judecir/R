#Definindo diretorio de trabalho =====================================
setwd("E:/CAGECE/Projetos/subfaturamento")
#=====================================================================

#Instalando pacotes ==================================================
pacote <- c("ggmap", "rgdal", "rgeos", "maptools", "dplyr", "tidyr", "tmap", "outliers")
#install.packages(pacote) # warning: uncommenting this may take a number of minutes
lapply(pacote, library, character.only = TRUE) # load the required packages
#=====================================================================

#Leitura shape =======================================================
lotes <- readOGR("shapes/lotes/lotes - fortaleza/lotes - fortaleza.shp")
sCensitario <- readOGR("shapes/SETORES - PER CAPITA (EDITADO)/SETORES - PER CAPITA (EDITADO).shp")

sCensitario$ECONOMIAS <- as.integer(sCensitario$ECONOMIAS)
lotes$qua_seq_qu <- as.character(lotes$qua_seq_qu) 
lotes$lot_seq_lo <- as.character(lotes$lot_seq_lo)
#=====================================================================

#Leitura arquivos csv ================================================

complementar <- read.table(file="SITUACAO-ABASTECIMENTO/Inscricoes_Tipo_Abastecimento_Fortaleza.csv", header=TRUE, sep=";")
situacao <- read.table(file="SITUACAO-ABASTECIMENTO/SIT_AGUA_FORTALEZA_201601.csv", header=TRUE, sep=";")

#Analise ===============================================================

clientes <- read.table(file="VOL_FORTALEZA_2016/VOLUMES/VOL_FORTALEZA_201601.csv", header=TRUE, sep=";")
clientes <-   clientes[clientes$TIPO_CLIENTE =="RESIDENCIAL",]
clientes0Vol<- data.frame()
clientes0Vol <- rbind(clientes0Vol, clientes[clientes$VOL_CALCULADO == 0, ])

clientes <- clientes[clientes$VOL_CALCULADO > 0, ]
clientes$VOL_CALCULADO <- (100/3)*clientes$VOL_CALCULADO
clientes <- data.frame(clientes, vpe = clientes$VOL_CALCULADO/clientes$NUM_ECONOMIAS)

C <- clientes[,-c(2,3,4,5)]

for(i in 2:12){
  if (i<10){cam = "VOL_FORTALEZA_2016/VOLUMES/VOL_FORTALEZA_20160"
  }else {cam = "VOL_FORTALEZA_2016/VOLUMES/VOL_FORTALEZA_2016"}
  cam = paste(cam, i, ".csv",sep="")
  cat("MES ", i)
  #Leitura csv
  #cat("Iniciando leitura do arquivo INSCRICOES...")
  clientes <- read.table(file=cam, header=TRUE, sep=";")
  
  print("Leitura do arquivo finalizada!")
  
  #Salvando apenas clientes residenciais================================
  #cat("Numero obs= ", length(clientes[,1]), "\n Retirando nao residenciais...")
  clientes <- clientes[clientes$TIPO_CLIENTE =="RESIDENCIAL",]
  #cat("Dados retirados! \n Numero obs atual= =", length(clientes[,1]))
  #=====================================================================
  
  
  #Retirando clientes com VOL_CALCULADO 0 ==============================
  clientes0Vol <- rbind(clientes0Vol, clientes[clientes$VOL_CALCULADO == 0, ])
  clientes <- clientes[clientes$VOL_CALCULADO > 0, ]
  #=====================================================================
 
  #Transformando em l/dia ==============================================
  clientes$VOL_CALCULADO <- (100/3)*clientes$VOL_CALCULADO
  #=====================================================================
  
  clientes <- data.frame(clientes, vpe = clientes$VOL_CALCULADO/clientes$NUM_ECONOMIAS)
  clientes <- clientes[,-c(2,3,4,5)]
  C <- merge(C, clientes, by="INSCRICAO")
 
  
  #Representar no shape ================================================
  
  #sh <- merge(lotes, clientesDisc, by.x="imo_cod_in", by.y="INSCRICAO",all = FALSE)
  #shSetor <- merge(sCensitario, setoresDisc, by.x= "CD_GEOCODI", by.y="ID", all=FALSE)
  
  #plot(sh)
  
  #writeOGR(sh,dsn="shape-clientes-menor3.34x80", layer="sh-clientesDiscrepantes", driver="ESRI Shapefile")
  #writeOGR(shSetor,dsn="shape-setores-menor80", layer="sh-setoresDisc", driver="ESRI Shapefile")
  #=====================================================================
}

#=====================================================================
zeros = rep(0,length(C$INSCRICAO))
C <- data.frame(C, mBaixo = zeros)
for(i in 1:length(C$INSCRICAO)){
  cont = 0
  for(j in 2:13){
      if(C[i,j]< 80){
        cont = cont+1
      }
    
  }
  C[i,14] = cont
  cat(i/length(C$INSCRICAO)*100, "\n %")
}
write.table (C, file="MESES-BAIXOS-CLIENTES.csv", sep=";", dec=",")

#Leitura arquivo após ser gerado alguma vez
C <- read.table(file="E:/CAGECE/Projetos/subfaturamento/MESES-BAIXOS-CLIENTES.csv", header=TRUE, sep=";")
C$INSCRICAO <- as.character(C$INSCRICAO)
cDisc <- C[C$mBaixo > 2, ]

cMDisc <- merge(complementar, cDisc, by="INSCRICAO")
abas = rep("NENHUM", length(cDisc$INSCRICAO))
cDisc <- data.frame(cDisc, TIPO_ABASTECIMENTO=abas)

for (i in 1:length(cMDisc$INSCRICAO)){
  cDisc[cDisc$INSCRICAO == cMDisc$INSCRICAO[i],]$TIPO_ABASTECIMENTO = cMDisc$TIPO_ABASTECIMENTO[i]
  
}
consBaixo <- cDisc[complete.cases(cDisc$TIPO_ABASTECIMENTO),]

cMaisDisc <- cDisc[-cMDisc$INSCRICAO,]
teste <- merge(complementar, cMaisDisc, by="INSCRICAO")

clientesDisc <- merge(lotes, cDisc, by.x="imo_cod_in", by.y="INSCRICAO",all = FALSE)
clientesMaisDisc <- merge(lotes, consBaixo, by.x="imo_cod_in", by.y="INSCRICAO", all=FALSE)

writeOGR(clientesDisc,dsn="17.10.31-consBaixo", layer="clientesDisc", driver="ESRI Shapefile")
writeOGR(clientesMaisDisc,dsn="17.11.01-consMaisBaixo", layer="clientesMaisDisc", driver="ESRI Shapefile")

#=====================================================================
