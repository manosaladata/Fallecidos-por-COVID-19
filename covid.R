library(readxl)
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(plotly)
library(extrafont)

# actualizado a 18/08/2020 https://www.datosabiertos.gob.pe/dataset/fallecidos-por-covid-19-ministerio-de-salud-minsa
# actualizado a 18/08/2020 https://www.datosabiertos.gob.pe/dataset/casos-positivos-por-covid-19-ministerio-de-salud-minsa
getwd()
fallecidos_covid<- readxl::read_xlsx("fallecidos_covid.xlsx")
positivos_covid <- readxl::read_xlsx("positivos_covid.xlsx")

#--------------------------------------------
# hacemos una limpieza de los datos
## Hacemos una observacion de los datos
#LINK:https://campus.datacamp.com/courses/cleaning-data-in-r/common-data-problems?ex=1
glimpse(fallecidos_covid)
glimpse(positivos_covid)
class(fallecidos_covid$SEXO)
class(fallecidos_covid$SEXO)
assert_is_factor(positivos_covid$SEXO) #si sale algun error es que no es factor y si no sale algo es que si es factor
is.factor(positivos_covid$SEXO)

#transformamos las variables que contienen fechas 
#LINK: https://campus.datacamp.com/courses/working-with-data-in-the-tidyverse/transform-your-data?ex=7
ymd("20200420")
fallecidos_covid <- fallecidos_covid %>% 
  mutate(FECHA_FALLECIMIENTO=ymd(FECHA_FALLECIMIENTO),FECHA_NAC=ymd(FECHA_NAC))
positivos_covid <- positivos_covid %>% 
  mutate(FECHA_RESULTADO=ymd(FECHA_RESULTADO))

#ahora transformamos a factor algunas variables
fallecidos_covid$SEXO <- as.factor(as.character(fallecidos_covid$SEXO))
positivos_covid$SEXO <- as.factor(as.character(positivos_covid$SEXO))

positivos_covid$METODODX <- as.factor(as.character(positivos_covid$METODODX))

is.factor(positivos_covid$METODODX)
is.factor(positivos_covid$SEXO)

levels(fallecidos_covid$SEXO)

#buscamos NA 

sapply(fallecidos_covid, function(x) sum(is.na(x)))
fallecidos_covid <- na.omit(fallecidos_covid)
sapply(positivos_covid, function(x) sum(is.na(x)))
fallecidos_covid <- na.omit(positivos_covid)

summary(fallecidos_covid$EDAD_DECLARADA)
assert_all_are_in_closed_range(fallecidos_covid$EDAD_DECLARADA, lower = 0, upper = 107)
summary(positivos_covid$EDAD)
assert_all_are_in_closed_range(positivos_covid$EDAD, lower = 0, upper = 120)

assert_all_are_in_past(fallecidos_covid$FECHA_FALLECIMIENTO)
assert_all_are_in_past(fallecidos_covid$FECHA_NAC)
assert_all_are_in_past(positivos_covid$FECHA_RESULTADO)
fallecidos_covid %>% filter(FECHA_NAC>today())

sum(duplicated(fallecidos_covid))
sum(duplicated(positivos_covid))

unique(fallecidos_covid$DEPARTAMENTO)

fallecidos_covid %>% group_by(SEXO, EDAD_DECLARADA) %>% count()  #no funciona mi dplyr?
fallecidos_covid %>%  group_by(SEXO,EDAD_DECLARADA) %>% summarise(conteo_p=count(EDAD))
fallecidos_covid %>% count("EDAD","FALLECIO")
#------------------------------------------------------------------

View(fallecidos_covid)
sapply(fallecidos_covid,class)
table(fallecidos_covid$SEXO)
table(fallecidos_covid$DEPARTAMENTO)
sapply(fallecidos_covid,class)

## Promedio de edades y numero de muertos por departamento

fallecidos_covid %>% 
  group_by(`DEPARTAMENTO`,`SEXO`) %>%
  summarise(promedio_edad=mean(`EDAD_DECLARADA`),numero=n()) %>%
  View()

## Analizamos mas a profundidad los distritos de nuestro departamento ?

fallecidos_covid %>%
  filter(`DEPARTAMENTO`=="LIMA")%>%
  group_by(`DISTRITO`)%>%
  summarise(promedio_edad=mean(`EDAD_DECLARADA`),numero=n()) %>%
  View()

fallecidos_depar<-fallecidos_covid %>%
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

#-----------------REYNALDO
library(tidyverse)
library(tint)
library(ggthemes)
library(plyr)


#creamos una base de datos para unir
#y aumentamos una columna para poder juntar en una BBDD que 
#contenga los fallecidos y las personas con COVID positivo

fallecidos1 <- fallecidos_covid
fallecidos1$FALLECIO="SI"
fallecidos1 <- fallecidos1 %>%  select("UUID","FALLECIO")

#juntamos
X1 <-merge( positivos_covid,fallecidos1, by = "UUID", all.x =TRUE)
X1$FALLECIO[is.na(X1$FALLECIO)]="NO" #a todos los NA creados a partir de la mezcla los rellenamos con NO de no estar fallecidos

#buscamos si hay datos duplicados
sum(duplicated(X1))

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


ggplot(X1,aes(x=EDAD,color=FALLECIO,fill=FALLECIO)) + geom_bar(position="identity",alpha=0.5)+
  labs(x="Edades",y="Numero de casos",title = "Histograma de casos COVID-19")
ggplot(X1, aes(x=EDAD,color=FALLECIO,fill=FALLECIO)) +
  geom_histogram(position="identity",alpha=0.5)  +
  labs(x="Edades",y="Numero de casos",title = "Histograma de casos COVID-19")



#--- otro tipo de grafico
library(funModeling)

cross_plot(X1, 
           input="EDAD",   
           target="FALLECIO", plot_type="percentual")+
  labs(x="Edades",y="Numero de casos",title = "Histograma de casos COVID-19")



