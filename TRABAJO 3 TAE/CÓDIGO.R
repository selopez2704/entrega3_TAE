library(leaflet)
library(kableExtra)
library(dplyr)
library(factoextra)
library(tidyr)
library(readr)
library(cluster)

datos <- read.csv("CollegeScorecard.csv",na = c("NULL", "PrivacySuppressed"))

size <- data.frame(Filas=nrow(datos),Columnas = ncol(datos))

kable(size,caption = "Tamaño de los datos") %>% 
  kable_styling(full_width = F,position = "center") %>% 
  kable_minimal()

set.seed(123)

mapa <- leaflet() %>% 
  addTiles() %>% 
  addMarkers(lng=datos$LONGITUDE[sample(7804,500)],lat=datos$LATITUDE[sample(7804,500)]) 

mapa

inf <- which(datos$region==9)

leaflet() %>% 
  addTiles() %>% 
  addMarkers(lng=datos$LONGITUDE[inf],lat=datos$LATITUDE[inf])

datos <- datos[-inf,]
ciudades <- table(datos$CITY)
ciudades <- ciudades[which(ciudades>40)]

kable(ciudades,col.names =NULL,caption = "Tabla con las ciudades con más de 40 universidades") %>% 
  kable_classic() %>% 
  kable_styling(full_width = F,position = "center")


tipo <- table(datos$CONTROL)
rownames(tipo) <- c("Pública","Privada sin ánimo de lucro","Privada con ánimo de lucro")
kable(tipo,col.names = NULL,caption = "Tipos de Universidades" ) %>% 
  kable_styling(full_width = F,position = "center") %>% 
  kable_classic()

base <- select(datos,c("UNITID","INSTNM","CITY","CONTROL","LATITUDE","LONGITUDE","ADM_RATE","SATVR25","SATVR75","SATMT25","SATMT75","SATVRMID","SATMTMID","SAT_AVG"))

sizebase <- data.frame(Filas=nrow(base),Columnas = ncol(base))

kable(sizebase,caption = "Tamaño de los datos") %>% 
  kable_styling(full_width = F,position = "center") %>% 
  kable_minimal()

datos_con_informacion <- which(rowSums(is.na(base))<6)

informacion <- base[datos_con_informacion,]

kable(t(colSums(is.na(informacion))),caption = "<strong> Cantidad de vacios por columna </strong>") %>% 
  kable_minimal() %>% 
  kable_styling(full_width = F,position = "center") %>% 
  scroll_box(width = "700px",height = "200px")


informacion <- informacion[-c(which(is.na(informacion$LATITUDE)=="TRUE"),which(is.na(informacion$SATVR25)),which(is.na(informacion$SATVR75))),]  # Eliminando todos los vacios


sizebase <- data.frame(Filas=nrow(informacion),Columnas = ncol(informacion))

kable(sizebase,caption = "Tamaño de los datos") %>% 
  kable_styling(full_width = F,position = "center") %>% 
  kable_minimal()

wss <- sapply(4:15,function(k){kmeans(informacion[,5:6], k, nstart = 50, iter.max = 15)$tot.withinss})

plot(4:15,wss,
     type = "b", pch = 19, frame = FALSE,
     xlab = "Número de Clusters (k)",
     ylab = "WSS Total", 
     main = "Método del Codo", col="forestgreen")

clusters <- kmeans(informacion[,5:6],12)
informacion$cluster <- as.factor(clusters$cluster)

qplot(LONGITUDE,LATITUDE,colour=cluster,data=informacion,xlab = "Longitud",ylab = "Latitud")

d <- as.data.frame(matrix(NA,nrow=length(table(informacion$cluster)),ncol=8))

colnames(d) <- colnames(informacion[,c(7:14)])
for (i in 1:nrow(d)) {
  clust <- informacion[informacion$cluster==i,]
  d[i,1] <- sum(clust$ADM_RATE)
  d[i,2] <- sum(clust$SATVR25)
  d[i,3] <- sum(clust$SATVR75)
  d[i,4] <- sum(clust$SATMT25)
  d[i,5] <- sum(clust$SATMT75)
  d[i,6] <- sum(clust$SATVRMID)
  d[i,7] <- sum(clust$SATMTMID)
  d[i,8] <- sum(clust$SAT_AVG)
  
}
d$media <- rowMeans(d)

kable(d,caption = "Datos con su respectiva media grupal",table.attr="style='width:30%;'") %>% 
  kable_minimal() %>% 
  kable_styling(position = "center",full_width = F)

plot(x = 1:length(table(informacion$cluster)),y = d$media,type = "b",xlab = "Número de grupo",ylab = "Promedio en los puntajes")

centros <- as.data.frame(clusters$centers)

leaflet() %>% 
  addTiles() %>% 
  addMarkers(lng=centros$LONGITUDE,lat=centros$LATITUDE,label=paste("Grupo Número:",c(1:nrow(d))))

