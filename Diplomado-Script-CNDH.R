#clear workspace
rm(list = ls(all = TRUE)) 

###  Web scrapping

library(rvest)
library(RCurl)
library(questionr)
library(dygraphs)
library(xts)
library(foreign)
library(stringr)
library(stargazer)
library(reshape)
library(ggplot2)
library(scales)
library(RColorBrewer)
library(dplyr)
library(zoo)
library(raster)

### RECOMENDACIONES DE LA CNDH

## Recomendaciones (RECO)

RCB<-"http://www.cndh.org.mx/Recomendaciones"
RCB2<- read_html(RCB)
RCB3<-RCB2%>%html_node("table")%>%html_table(fill=TRUE)
View(RCB3)

RECO<-RCB3[2:2693,]


## Recomendaciones por violaciones graves (VGRA)

RCB<-"http://www.cndh.org.mx/Recomendaciones_Violaciones_Graves"
RCB2<- read_html(RCB)
RCB3<-RCB2%>%html_node("table")%>%html_table(fill=TRUE)
View(RCB3)

VGRA<-RCB3[2:13,]

## Recomendaciones generales (RGEN)

RCB<-"http://www.cndh.org.mx/Recomendaciones_Generales"
RCB2<- read_html(RCB)
RCB3<-RCB2%>%html_node("table")%>%html_table(fill=TRUE)
View(RCB3)

RGEN<-RCB3[2:32,]

## Recomendaciones del mecanismo nacional de prevención de la tortura (PTOR)

RCB<-"http://www.cndh.org.mx/Recomendaciones_Prevencion_Tortura"
RCB2<- read_html(RCB)
RCB3<-RCB2%>%html_node("table")%>%html_table(fill=TRUE)
View(RCB3)

PTOR<-RCB3[2:14,]

## Acciones de incostitucionalidad (AINC)

RCB<-"http://www.cndh.org.mx/Acciones_Inconstitucionalidad"
RCB2<- read_html(RCB)
RCB3<-RCB2%>%html_node("table")%>%html_table(fill=TRUE)
View(RCB3)

AINC<-RCB3[2:157,]


## Pronunciamientos, estudios e informes especiales (IESP)

RCB<-"http://www.cndh.org.mx/Informes_Especiales"
RCB2<- read_html(RCB)
RCB3<-RCB2%>%html_node("table")%>%html_table(fill=TRUE)
View(RCB3)

IESP<-RCB3[2:77,]

#### SERIES DE TIEMPO 

### Acciones de inconstitucionalidad

## Calculamos las recomendaciones con estado procesal EN TRÁMITE
# Definimos los patrones de búsqueda 

pat_en_tramite<-"En trámite"
pat_resuelta<-"Resuelta"

# Creamos una variable dummy para cada patrón

AINC$en_tramite<-as.numeric(grepl(pat_en_tramite,AINC$`Estado Procesal`))
AINC$resuelta<-as.numeric(grepl(pat_resuelta,AINC$`Estado Procesal`))

# Creamos data frame con las frecuencias de las acciones en trámite y resueltas por año

df_en_tramite<-cast(as.data.frame(wtd.table(AINC$Año,AINC$en_tramite)),Var1~Var2,sum)
df_resuelta<-cast(as.data.frame(wtd.table(AINC$Año,AINC$resuelta)),Var1~Var2,sum)





# Ejemplo serie de tiempo acciones de inconstitucionalidad

mydates<-seq(as.Date("2007/1/1"), as.Date("2018/1/1"), "years")

acinc<- as.data.frame(wtd.table(AINC$Año))[,2]
acinc<-cbind.data.frame(acinc,df_en_tramite$`1`)
acinc<-cbind.data.frame(acinc,df_resuelta$`1`)

# Cambiamos los nombre a las variales 1 de los data frame anteriores

colnames(acinc)[colnames(acinc)=="acinc"]<-"Recomendaciones"
colnames(acinc)[colnames(acinc)=="df_en_tramite$`1`"]<-"En tramite"
colnames(acinc)[colnames(acinc)=="df_resuelta$`1`"]<-"Resuelta"

acinc<- xts(acinc, order.by = mydates)


## Acciones de Inconstitucionalidad

acinc_plot<-dygraph(acinc, main = "Recomendaciones en materia de Acciones de Inconstitucionalidad emitidas por la CNDH (2007-2018)") %>% 
  dyLegend(show = "always", hideOnMouseOut = FALSE)%>%
  dyAxis("x", drawGrid = FALSE) %>%
  dyAxis("y", label = "Recomendaciones") %>%
  
  dyLegend(width = 300)%>%
  dyShading(from = "2007-1-1", to = "2011-1-1", color = "#FFE6E6")%>%
  dyEvent("2011-1-1","2011", labelLoc = "bottom") %>%
  dyOptions(colors = RColorBrewer::brewer.pal(3, "Set1"))


### Recomendaciones

mydates<-seq(as.Date("1990/1/1"), as.Date("2018/1/1"), "years")

reco_ts<-as.data.frame(as.data.frame(wtd.table(RECO$Año))[,2])

colnames(reco_ts)[colnames(reco_ts)=="`as.data.frame(wtd.table(RECO$Año))[, 2]`"]<-"Recomendaciones"
reco_ts<- xts(reco_ts, order.by = mydates)

recom_plot<-dygraph(reco_ts, main = "Recomendaciones emitidas por la CNDH (1990-2018)") %>% 
  dyLegend(show = "always", hideOnMouseOut = FALSE)%>%
  dyAxis("x", drawGrid = FALSE) %>%
  dyAxis("y", label = "Recomendaciones") %>%
  
  dyLegend(width = 300)%>%
  dyShading(from = "1990-1-1", to = "2011-1-1", color = "#FFE6E6")%>%
  dyEvent("2011-1-1","2011", labelLoc = "bottom") 


### Recomendaciones por violaciones graves

mydates<-seq(as.Date("2012/1/1"), as.Date("2018/1/1"), "years")

reco_vgraves<-as.data.frame(as.data.frame(wtd.table(VGRA$Año))[,2])
reco_vgraves<- xts(reco_vgraves, order.by = mydates)

recom_vgraves_plot<-dygraph(reco_vgraves, main = "Recomendaciones por violaciones graves  emitidas por la CNDH (2012-2018)") %>% 
  dyLegend(show = "always", hideOnMouseOut = FALSE)%>%
  dyAxis("x", drawGrid = FALSE) %>%
  dyAxis("y", label = "Recomendaciones") %>%
  dyLegend(width = 300)

bar_vgrav<-data.frame(Recomendaciones=c(1,1,2,1,1,5,1),Año=c(2012,2013,2014,2015,2016,2017,2018))

# Hacemos la gráfica de barras
viola_graves<-ggplot(data=bar_vgrav, aes(x=Año, y=Recomendaciones)) +
  geom_bar(stat="identity", position=position_dodge())+
  geom_text(aes(label=Recomendaciones), vjust=1.6, color="white",
            position = position_dodge(0.9), size=7)+
  scale_fill_manual(values ="#736F6E")+
  theme(axis.text.x=element_text(angle=30, hjust=1))+
  annotate("text", x = 2013, y = .5, label = "San Fernando,",size=3,color="White")+
  annotate("text", x = 2013, y = .3, label = "Tamaulipas",size=3,color="white")+
  annotate("text", x = 2014, y = 1.4, label = "Tlatlaya",size=3,color="White")+
  annotate("text", x = 2014, y = 1.2, label = "Edo.Mex;",size=3,color="white")+
  annotate("text", x = 2014, y = .7, label = "Ocoyucán",size=3,color="White")+
  annotate("text", x = 2014, y = .5, label = "Puebla",size=3,color="white")+
  annotate("text", x = 2015, y = .5, label = "Apatzingán,",size=3,color="White")+
  annotate("text", x = 2015, y = .3, label = "Michoacán",size=3,color="white")+
  annotate("text", x = 2016, y = .5, label = "Tanhuato,",size=3,color="White")+
  annotate("text", x = 2016, y = .3, label = "Michoacán",size=3,color="white")+
  annotate("text", x = 2017, y = .5, label = "Tierra Blanca,",size=3,color="White")+
  annotate("text", x = 2017, y = .3, label = "Veracruz",size=3,color="white")+
  annotate("text", x = 2017, y = 1.4, label = "Papantla,",size=3,color="White")+
  annotate("text", x = 2017, y = 1.2, label = "Veracruz;",size=3,color="white")+
  annotate("text", x = 2017, y = 2.3, label = "Nochixtlán,",size=3,color="White")+
  annotate("text", x = 2017, y = 2.1, label = "Oaxaca;",size=3,color="white")+
  annotate("text", x = 2017, y = 3.2, label = "Cadereyta,",size=3,color="White")+
  annotate("text", x = 2017, y = 3.0, label = "Nuevo León;",size=3,color="white")+
  annotate("text", x = 2017, y = 4.1, label = "Hermosillo,",size=3,color="White")+
  annotate("text", x = 2017, y = 3.9, label = "Sonora;",size=3,color="white")+
  annotate("text", x = 2018, y = 0.5, label = "Allende,",size=3,color="White")+
  annotate("text", x = 2018, y = 0.3, label = "Coahuila",size=3,color="white")


print( viola_graves+labs( title= "Recomendaciones por violaciones graves  emitidas por la CNDH (2012-2018)",caption = ""))

#### BAR CHART

### Recomendaciones por Autoridad 


## Dividiremos la búsqueda por: 
# Procuraduría General
# IMSS
# Secretaría
# comisiones
# Institutos
# Gobierno del Estado
# Gobierno Constitucional
# H.Ayuntamiento
# Municipio
# H.Tribunal Superior de Justicia del Estado
# Juzgado
# Legislatura

# Definimos los patrones de búsqueda. Creamos una lista con los patrones.
lista_pat<-list(
  pat_procu_general<-"Procuraduría General de la República",
  pat_imss<-"IMSS",
  pat_secretaria<-"Secretaría",
  pat_comision<-"Comisión",
  pat_instituto<-"Instituto",
  pat_gob_estado<-"Gobierno del Estado",
  pat_gob_cons<-"Gobierno Constitucional",
  pat_ayuntamiento<-"H.Ayuntamiento",
  pat_municipio<-"Municipio",
  pat_tribunal_estatal<-"H.Tribunal Superior de Justicia del Estado",
  pat_juzgado<-"Juzgado",
  pat_legislatura<-"Legislatura")

#Creamos una función de búsqueda del patrón
feature <- function(data, col){
  data<-cbind.data.frame(data,col=as.numeric(grepl(col,data$Autoridad)))
  colnames(data)[colnames(data)=="col"] <- col
  return(data)
}


#Incorporamos un Loop para aplicar la función anterior a cada elemento de la lista x
for (i in lista_pat){
  RECO<-feature(RECO,i)
}

# Hacemos un data frame resumen para el periodo 1990-2010
RECO_P1<-subset(RECO, Año<=2010)

recom_aut_p1<-data.frame(autoridad=c("Procuraduría General de la República","IMSS","Secretarías","Comisiones","Institutos","Estados","Municipios","Juzgados","Legislaturas","Otras"),
                         recomendacion=c(134,sum(RECO_P1$IMSS),sum(RECO_P1$Secretaría),sum(RECO_P1$Comisión),sum(RECO_P1$Instituto),sum(RECO_P1$`Gobierno del Estado`),sum(RECO_P1$H.Ayuntamiento)+sum(RECO_P1$Municipio),sum(RECO_P1$Juzgado),sum(RECO_P1$Legislatura),599),
                         periodo=c("1990-2010","1990-2010","1990-2010","1990-2010","1990-2010","1990-2010","1990-2010","1990-2010","1990-2010","1990-2010"))

# Hacemos un data frame resumen para el periodo 1990-2010
RECO_P2<-subset(RECO, Año>=2011)

recom_aut_p2<-data.frame(autoridad=c("Procuraduría General de la República","IMSS","Secretarías","Comisiones","Institutos","Estados","Municipios","Juzgados","Legislaturas","Otras"),
                         recomendacion=c(3,47,sum(RECO_P2$Secretaría),sum(RECO_P2$Comisión),sum(RECO_P2$Instituto),sum(RECO_P2$`Gobierno del Estado`),sum(RECO_P2$H.Ayuntamiento)+sum(RECO_P2$Municipio),sum(RECO_P2$Juzgado),sum(RECO_P2$Legislatura),75),
                         periodo=c("2011-2018","2011-2018","2011-2018","2011-2018","2011-2018","2011-2018","2011-2018","2011-2018","2011-2018","2011-2018"))

# Reunimos los data frame en uno sólo

recom_aut<-rbind.data.frame(recom_aut_p1,recom_aut_p2)

# Hacemos la gráfica de barras
bar_autoridades<-ggplot(data=recom_aut, aes(x=autoridad, y=recomendacion, fill=periodo)) +
  geom_bar(stat="identity", position=position_dodge())+
  geom_text(aes(label=recomendacion), vjust=1.6, color="black",
            position = position_dodge(0.9), size=3.5)+
  scale_fill_brewer(palette="Spectral")+
  theme(axis.text.x=element_text(angle=30, hjust=1))

print(bar_autoridades +labs( title= "Recomendaciones por tipo de autoridad  emitidas\npor la CNDH (Periodos 1990-2010 y 2011-2018)",caption = ""))

##### MAP PLOT

### Recomendaciones por Estado

# Definimos los patrones de búsqueda. Creamos una lista con los patrones.
lista_pat<-list(
  pat_Agu<-"Aguascalientes",
  pat_Baj<-"Baja California",
  pat_Baj<-"Baja California Sur",
  pat_Cam<-"Campeche",
  pat_Chi<-"Chiapas",
  pat_Chi<-"Chihuahua",
  pat_Ciu<-"Ciudad de México",
  pat_DF <-"Distrito Federal",
  pat_Coa<-"Coahuila",
  pat_Col<-"Colima",
  pat_Dur<-"Durango",
  pat_Gua<-"Guanajuato",
  pat_Gue<-"Guerrero",
  pat_Hid<-"Hidalgo",
  pat_Jal<-"Jalisco",
  pat_Méx<-"Estado de México",
  pat_Mic<-"Michoacán",
  pat_Mor<-"Morelos",
  pat_Nay<-"Nayarit",
  pat_Nue<-"Nuevo León",
  pat_Oax<-"Oaxaca",
  pat_Pue<-"Puebla",
  pat_Que<-"Querétaro",
  pat_Qui<-"Quintana Roo",
  pat_San<-"San Luis Potosí",
  pat_Sin<-"Sinaloa",
  pat_Son<-"Sonora",
  pat_Tab<-"Tabasco",
  pat_Tam<-"Tamaulipas",
  pat_Tla<-"Tlaxcala",
  pat_Ver<-"Veracruz",
  pat_Yuc<-"Yucatán",
  pat_Zac<-"Zacatecas"
)

#Creamos una función de búsqueda del patrón
feature <- function(data, col){
  data<-cbind.data.frame(data,col=as.numeric(grepl(col,data$Asunto)))
  colnames(data)[colnames(data)=="col"] <- col
  return(data)
}


#Incorporamos un Loop para aplicar la función anterior a cada elemento de la lista x
for (i in lista_pat){
  RECO<-feature(RECO,i)
}

# Hacemos un data frame resumen para el periodo 1990-2010
RECO_P1_EDO<-subset(RECO, Año<=2010)

df<-dplyr::select(RECO_P1_EDO,Año, Aguascalientes,`Baja California`,`Baja California Sur`,Campeche,Chiapas,Chihuahua,`Ciudad de México`,`Distrito Federal`,Coahuila,Colima,Durango,Guanajuato,Guerrero,
                  Hidalgo,
                  Jalisco,
                  `Estado de México`,
                  Michoacán,
                  Morelos,
                  Nayarit,
                  `Nuevo León`,
                  Oaxaca,
                  Puebla,
                  Querétaro,
                  `Quintana Roo`,
                  `San Luis Potosí`,
                  Sinaloa,
                  Sonora,
                  Tabasco,
                  Tamaulipas,
                  Tlaxcala,
                  Veracruz,
                  Yucatán,
                  Zacatecas)


recom_estados_p1<-cast(melt(df, "Año"),variable~value,sum)

recom_estados_p1$`0`<-NULL

colnames(recom_estados_p1)[colnames(recom_estados_p1)=="variable"]<-"Estado"
colnames(recom_estados_p1)[colnames(recom_estados_p1)=="1"]<-"Recomendaciones_P1"

recom_estados_p1$Periodo<-"1990-2010"

# Sumamos la CDMX y el DF
recom_estados_p1[7,2]<-24

# Eliminamos al DF
recom_estados_p1<-recom_estados_p1[-8,]

# Hacemos un data frame resumen para el periodo 1990-2010
RECO_P2_EDO<-subset(RECO, Año>=2011)

df<-dplyr::select(RECO_P2_EDO,Año, Aguascalientes,`Baja California`,`Baja California Sur`,Campeche,Chiapas,Chihuahua,`Ciudad de México`,`Distrito Federal`,Coahuila,Colima,Durango,Guanajuato,Guerrero,
                  Hidalgo,
                  Jalisco,
                  `Estado de México`,
                  Michoacán,
                  Morelos,
                  Nayarit,
                  `Nuevo León`,
                  Oaxaca,
                  Puebla,
                  Querétaro,
                  `Quintana Roo`,
                  `San Luis Potosí`,
                  Sinaloa,
                  Sonora,
                  Tabasco,
                  Tamaulipas,
                  Tlaxcala,
                  Veracruz,
                  Yucatán,
                  Zacatecas)


recom_estados_p2<-cast(melt(df, "Año"),variable~value,sum)

recom_estados_p2$`0`<-NULL

colnames(recom_estados_p2)[colnames(recom_estados_p2)=="variable"]<-"Estado"
colnames(recom_estados_p2)[colnames(recom_estados_p2)=="1"]<-"Recomendaciones_P2"

recom_estados_p2$Periodo<-"2011-2018"

# Sumamos la CDMX y el DF
recom_estados_p2[7,2]<-30

# Eliminamos al DF
recom_estados_p2<-recom_estados_p2[-8,]

# Hacemos el merge de las recomendaciones de los dos periodos
recom_estados<-merge(recom_estados_p1,recom_estados_p2,by="Estado")

# Eliminamos las columnas de periodo
recom_estados$Periodo.x<-NULL
recom_estados$Periodo.y<-NULL

# Cambiamos el nombre de Estados a NOM_ENT, para hacer el merge con el shape
colnames(recom_estados)[colnames(recom_estados)=="Estado"]<-"NOM_ENT"
#Ingresamos la dirección en dónde está ubicada el shape de Estados


shape_edo<-shapefile("C:\\SIGAST\\Datos\\SIGAST_Training\\MEX_estados.shp")

# Ajustamos el id de estados para hacer el Merge

shape_edo@data[7,1]<-"Coahuila"
shape_edo@data[9,1]<-"Ciudad de México"
shape_edo@data[16,1]<-"Michoacán"
shape_edo@data[22,1]<-"Querétaro"
shape_edo@data[30,1]<-"Veracruz"
shape_edo@data[19,1]<-"Nuevo León"
shape_edo@data[15,1]<-"Estado de México"
shape_edo@data[31,1]<-"Yucatán"
shape_edo@data[24,1]<-"San Luis Potosí"

# Eliminamos algunas columnas del shape
shape_edo@data[,2:7]<-NULL
shape_edo@data[,5:9]<-NULL

# Hacemos el merge entre bases 
recom_edo_shape<-merge(shape_edo,recom_estados,by="NOM_ENT")


# Guardamos la base en formato shape

shapefile(recom_edo_shape, "C:\\Users\\End User\\Desktop\\Instituto Jurídicas\\Diplomado Democratización\\Trabajo Final\\recomedos.shp")

## Consultas
# https://www.analyticsvidhya.com/blog/2017/03/beginners-guide-on-web-scraping-in-r-using-rvest-with-hands-on-knowledge/