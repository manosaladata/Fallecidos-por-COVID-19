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
