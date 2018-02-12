###################################################
########### BIBLIOTECAS UTILIZADAS ################

# if(!require(jsonlite,quietly = TRUE)) 
# install.packages('jsonlite',dependencies=TRUE)
# if(!require(cluster,quietly = TRUE)) 
# install.packages('cluster',dependencies=TRUE)
# if(!require(fpc,quietly = TRUE)) 
# install.packages('fpc',dependencies=TRUE)
# if(!require(dplyr,quietly = TRUE)) 
# install.packages('dplyr',dependencies=TRUE)
# if(!require(tidyr,quietly = TRUE)) 
# install.packages('tidyr',dependencies=TRUE)
# if(!require(plotly,quietly = TRUE)) 
# install.packages('plotly',dependencies=TRUE)

library(dplyr)
library(cluster)
library(fpc)
library (xts)
library (forecast)
library(jsonlite)
library(ggplot2)
library(tidyr)
library(plotly)

###################################################
########### LEITURA DOS ARQUIVOS ##################

meses <- seq(as.Date("2011/01/01"), by = "month", length.out = 79)

caged <- data.frame(cnae = numeric(0), 
                    saldo = double(0),
                    mov = double(0),
                    ref = character(0))

data <- NULL
data_sub <- NULL

for(i in 1:length(meses)){
  
  mes <- meses[i]
  
  str_ano <- substr(mes,0,4)
  str_mes <- substr(mes,6,7)
  fname <- paste('CAGEDEST_',str_mes,str_ano,".txt",sep="")
  
  data <- read.csv2(fname)
  
  data_sub <- data %>% 
    group_by(CNAE.2.0.Classe) %>% 
    summarise(saldo = sum(Saldo.Mov), 
              mov = sum(abs(Saldo.Mov)))
  
  data_sub$ref <- mes
  
  caged <- rbind(caged, data_sub)
  
}

colnames(caged) <- c("cnae","saldo","mov","ref")

nrow(caged)
class(caged)
str(caged)

###################################################
########### TRATAMENTO DOS DADOS ##################


which.nonnum <- function(x) {
  which(is.na(suppressWarnings(as.numeric(as.character(x)))))
}

caged <- caged[-c(as.vector(which.nonnum(caged[,c("cnae")]))), ]

caged[] <- lapply(caged, function(x) {gsub(",", "\\.", x)})

caged[, 1:3] <- sapply(caged[, 1:3], as.numeric)


# write.csv2(caged, "caged.csv")

###################################################
########### MODELO PREDITIVO (ARIMA) ##############

caged_ts <- read.csv2("caged.csv", row.names = 1)

which.nonnum <- function(x) {
  which(is.na(suppressWarnings(as.numeric(as.character(x)))))
}

caged_ts <- caged_ts[-c(as.vector(which.nonnum(caged_ts[,c("cnae")]))), ]


uk_cnae <- unique(caged_ts$cnae)
uk_cnaeLen <- length(uk_cnae)

df.arima <- data.frame(cnae = numeric(0), 
                       prev01 = double(0), 
                       prev02 = double(0), 
                       prev03 = double(0))


for(i in 1:uk_cnaeLen){
  df <- caged_ts[caged_ts$cnae == uk_cnae[i],]
  rownames(df) <- df$ref
  df <- df[,c("saldo","ref")]
  cnae_arima <- ts(df$saldo, start = c(2013, 1), frequency = 12)
  previsao <- auto.arima(cnae_arima)
  # # plot(forecast(previsao, h=5))
  
  pred <- forecast(previsao, h=3)
  
  df.arima <- rbind(df.arima,data.frame(cnae= uk_cnae[i],
                                        prev01 = pred$mean[1], 
                                        prev02 = pred$mean[2], 
                                        prev03 = pred$mean[3],
                                        stringsAsFactors=FALSE))
}

head(df.arima)

# write.csv2(df.arima, "df_arima.csv")

###################################################
##### PREPARAÇÃO DO ARQUIVO PARA CLUSTER ##########

caged_cluster <- read.csv2("df_arima.csv")

caged_cluster <- with(caged_cluster, 
                      data.frame(cnae, prev = prev01 + prev02 + prev03))

nrow(caged_cluster)
colnames(caged_cluster) <- c("cnae", "prev")
str(caged_cluster)

cnae_nomes <- read.csv2("cnae_nomes.csv")

caged_cluster <- left_join(caged_cluster,cnae_nomes, by = "cnae")

caged_ts <- read.csv2("caged.csv", row.names = 1)

str(caged_ts)

cnae_mov <- caged_ts %>% 
  group_by(cnae) %>% 
  summarise(mov = sum(mov))

str(cnae_mov)
nrow(cnae_mov)

which.nonnum <- function(x) {
  which(is.na(suppressWarnings(as.numeric(as.character(x)))))
}

cnae_mov<- cnae_mov[-c(as.vector(which.nonnum(cnae_mov[,c("cnae")]))), ]

caged_cluster <- left_join(caged_cluster,cnae_mov, by = c("cnae" = "cnae"))

nrow(caged_cluster)
View(caged_cluster)

caged_cluster$size <- 1+abs((cnae_mov$mov - mean(cnae_mov$mov))/
                              sd(cnae_mov$mov))

max(caged_cluster$size)
min(caged_cluster$size)
head(caged_cluster)

caged_cluster <- caged_cluster[ ,-4]

colnames(caged_cluster) <- c("id","prev","label","size")

# write.csv2(caged_cluster, "caged_cluster.csv")

###################################################
########### ANÁLISE DE CLUSTER ####################

caged_cluster <- read.csv2("caged_cluster.csv")

ukIdDF <- data.frame(id = character(0),
                     label = character(0),
                     scale = integer (0),
                     size = integer (0))

ukIdDFScale <- na.omit(caged_cluster$prev)
ukIdDFScale <- scale(ukIdDFScale)

qtdUkIdDFScale <- nrow(unique(ukIdDFScale))
qtdCluster <- 0


wss <- (nrow(ukIdDFScale)-1)*sum(apply(ukIdDFScale,2,var))


if(!is.na(wss)){
  qtdCluster <- as.numeric(wss[1])
}
qtdCluster
##normalizacao clusters e escalas unicas
if( qtdCluster > qtdUkIdDFScale) {
  qtdCluster <- qtdUkIdDFScale
}	

if(qtdCluster > 5) {
  qtdCluster <- 5
  qtdUkIdDFScale <- 5
}
if(qtdCluster > 2) {
  fit <- kmeans(ukIdDFScale, qtdCluster)
  ukIdDFScale <- data.frame(ukIdDFScale, fit$cluster)
  colnames(ukIdDFScale) <- c( "scale", "cluster" ) 
  ukIdDF <- cbind(caged_cluster,escala=ukIdDFScale$scale,cluster=ukIdDFScale$cluster)
}


# ?plotcluster
# plotcluster(ukIdDF$escala,ukIdDF$cluster,
# xlab = "Escala de Admissão/Desligamento",
# ylab = "Cluster")
# 
# ?clusplot
# clusplot(ukIdDF, ukIdDF$cluster, color=TRUE, shade=TRUE, 
# labels=4, lines=0, ylab = "", xlab = "")


ukIdDF$previsao <- round(ukIdDF$prev,1)
ukIdDF$cluster <- factor(ukIdDF$cluster)


grafico_1 <- ggplot(ukIdDF, aes(previsao, escala, text = ukIdDF$label, colour = cluster)) + 
  geom_point() +
  labs(title = "Previsão de emprego por CNAE nos próximos 3 meses", x = "Previsão de Admissão/Desligamento", y = "Escala / Cluster") +
  scale_color_manual(labels = c(ukIdDF$cluster), values=c("red", "orange", "gray", "light blue", "blue")) +
  theme_bw() +
  guides(color=guide_legend(title = "Cluster"))
# theme(legend.position = c(1,0)) +
# xlab('Previsão de Admissão/Desligamento') +
# ylab('Escala / Cluster') 
ggplotly(grafico_1, tooltip = c("ukIdDF$label", "previsao", "cluster"))

# write.csv2(ukIdDF, "cnae_cluster.csv")