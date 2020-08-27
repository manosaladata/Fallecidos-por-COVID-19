library(readxl)
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(plotly)
library(extrafont)


getwd()
fallecidos<- readxl::read_xlsx("fallecidos_covid.xlsx")
fallecidos$FECHA_FALLECIMIENTO<-ymd(as.character(fallecidos$FECHA_FALLECIMIENTO))
fallecidos$FECHA_NAC<-ymd(as.character(fallecidos$FECHA_NAC))

View(fallecidos)
sapply(fallecidos,class)
table(fallecidos$SEXO)
table(fallecidos$DEPARTAMENTO)
sapply(fallecidos,class)

## Promedio de edades y numero de muertos por departamento

fallecidos %>% 
  group_by(`DEPARTAMENTO`,`SEXO`) %>%
  summarise(promedio_edad=mean(`EDAD_DECLARADA`),numero=n()) %>%
  View()

## Analizamos mas a profundidad los distritos de nuestro departamento ?

fallecidos %>%
  filter(`DEPARTAMENTO`=="LIMA")%>%
  group_by(`DISTRITO`)%>%
  summarise(promedio_edad=mean(`EDAD_DECLARADA`),numero=n()) %>%
  View()

fallecidos_depar<-fallecidos %>%
  select(FECHA_FALLECIMIENTO,DEPARTAMENTO,EDAD_DECLARADA) %>%
  filter(`DEPARTAMENTO`=="LIMA")
View(fallecidos_depar)


grafico<-ggplot(fallecidos_depar,aes(x=FECHA_FALLECIMIENTO,y=EDAD_DECLARADA))+
  geom_density_2d(aes(color=DEPARTAMENTO),size=1)+
  ggtitle("Densidad de fallecidos por Covid en Lima")+
  theme (plot.title = element_text(family="Comic Sans MS",
                                   size=rel(1.8), 
                                   vjust=2, 
                                   face="bold", 
                                   color="red", 
                                   lineheight=1.5)) +
  labs(x="Fecha de Fallecimiento",y="Edad Declarada")+
  theme(axis.title.x = element_text(face="bold", vjust=-0.5, colour="orange", size=rel(1.5))) +
  theme(axis.title.y = element_text(face="bold", vjust=1.5, colour="blue", size=rel(1.5))) 
  
ggplotly(grafico) %>% layout(legend = list(orientation = "h", x = 0.4, y = -0.2))

#REYNALDO
library(tidyverse)
library(tint)
library(ggthemes)
library(plyr)


# actualizado a 18/08/2020 https://www.datosabiertos.gob.pe/dataset/fallecidos-por-covid-19-ministerio-de-salud-minsa
# actualizado a 18/08/2020 https://www.datosabiertos.gob.pe/dataset/casos-positivos-por-covid-19-ministerio-de-salud-minsa
fallecidos_covid <- readxl::read_xlsx("fallecidos_covid.xlsx")
positivos_covid <- readxl::read_xlsx("positivos_covid.xlsx")

#aumentamos una columna para poder juntar mas adelante
fallecidos_covid$Fallecidos="SI"
fallecidos_covid <-fallecidos_covid %>%  select("UUID","Fallecidos")

#juntamos
X1 <-merge( positivos_covid,fallecidos_covid, by = "UUID", all.x =TRUE)
X1$Fallecidos[is.na(X1$Fallecidos)]="NO" #a todos los NA creados a partir de la mezcla los rellenamos con NO de no estar fallecidos

#Buscamos datos "NA"
X1 %>% count("Fallecidos")
sapply(X1, function(x) sum(is.na(x)))
X1 <- na.omit(X1)

#dibujar en un histograma por edades
theme_recession <- theme(
  rect = element_rect(fill = "grey92"),
  legend.key = element_rect(color = NA),
  axis.ticks = element_blank(),
  panel.grid = element_blank(),
  axis.text = element_text(color = "grey25"),
  plot.title = element_text(face = "italic", size = 16),
  
)
theme_tufte_recession <- theme_tufte() + theme_recession


ggplot(X1,aes(x=EDAD,color=Fallecidos,fill=Fallecidos)) + geom_bar(position="identity",alpha=0.5)+
  labs(x="Edades",y="Numero de casos",title = "Histograma de casos COVID-19")
ggplot(X1, aes(x=EDAD,color=Fallecidos,fill=Fallecidos)) +
  geom_histogram(position="identity",alpha=0.5)  +
  labs(x="Edades",y="Numero de casos",title = "Histograma de casos COVID-19")

X1 %>%  group_by(EDAD,Fallecidos) %>% summarise(conteo_p=count(EDAD))


#--- otro tipo de grafico
library(funModeling)

cross_plot(X1, 
           input="EDAD",   
           target="Fallecidos", plot_type="percentual")+
  labs(x="Edades",y="Numero de casos",title = "Histograma de casos COVID-19")



