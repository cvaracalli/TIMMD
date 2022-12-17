
#### Librerias ####

library(shiny)
library(tidyverse)
library(ggwordcloud)
library(plotly)
library(readxl)
library(gganimate)
library(gifski)
library(ggimage)
library(viridis)
#library(plyr)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(wordcloud)
library(RColorBrewer)
library(wordcloud2)
library(tm)
library(wordcloud)
library(SnowballC)
library(magrittr)
library(extrafont)
library(readxlsb)

#### Funciones ####

"%!in%" <- Negate("%in%")


detach_package <- function(pkg, character.only = FALSE)
{
  if(!character.only)
  {
    pkg <- deparse(substitute(pkg))
  }
  search_item <- paste("package", pkg, sep = ":")
  while(search_item %in% search())
  {
    detach(search_item, unload = TRUE, character.only = TRUE)
  }
}

#### IMPORTO INFORMACI?N VUELOS ####

##https://www.datos.gob.ar/dataset/transporte-aterrizajes-despegues-procesados-por-administracion-nacional-aviacion-civil-anac

Prueba <- read_xlsb(path = "Consolidado2020-2022.xlsb",
                    sheet = "202012-informe-ministerio",
                    range = cell_limits(c(24, 1), c(NA, 18)),
                    col_names = FALSE)
Consolidado <- read_excel("Consolidado2020-2022.xlsx")
View(Consolidado)

#### Din?mico TR?FICO vs FECHA (fondo blanco) ####

df_consolidado2 <- subset(Consolidado, select = c(Fecha,Pasajeros))

df_consolidado3 <-df_consolidado2  %>% 
  group_by(Fecha) %>% 
  summarise(Pasajeros = sum(Pasajeros))

anim <- ggplot(
  data = df_consolidado3, 
  aes(x = Fecha, y = Pasajeros)) +
  geom_line(color = "red") +
  geom_point(shape= 19, color = "#0695DB", size = 5) +
  theme_classic() +
  labs(title = "Evoluci?n del tr?fico a?reo en Argentina",
       subtitle = "Enero 2020 - Septiembre 2022",
       caption = "Fuente: www.datos.gob.ar") +
  theme(
    plot.title = element_text(color = "black", size = 12, 
                              face = "bold", hjust = 0.5),
    plot.subtitle = element_text(color = "#0695DB", hjust = 0.5),
    plot.caption = element_text(color = "black", face = "italic")
  )+
  transition_reveal(Fecha)

animate(anim, 30, fps = 7, 
        renderer = gifski_renderer("Fecha-Pasajeros.gif"))

#### Aerolineas vs Otra (Dom?stico/Internacional) ####

df_data <- Consolidado

df_data$ClasificacionVuelo<-replace(df_data$ClasificacionVuelo,df_data$ClasificacionVuelo=="Dom?stico","Domestico")
df_data$Aerolinea<-replace(df_data$Aerolinea,df_data$Aerolinea !="AEROLINEAS ARGENTINAS SA","Otra")
df_data$Aerolinea<-replace(df_data$Aerolinea,df_data$Aerolinea =="AEROLINEAS ARGENTINAS SA" ,"AerolineasArg")

ggplot(data = df_data) +
  geom_col(mapping = aes(x = Aerolinea, y = Pasajeros)) + 
  facet_wrap(vars(ClasificacionVuelo)) +
  theme_bw()


#### Aerolineas vs JetSmart (Dom?stico/Internacional & AER/EZE)####

df_dataN <- Consolidado %>% 
  filter(
    Aeropuerto %in% c('EZE','AER'),
    Aerolinea %in% c('AEROLINEAS ARGENTINAS SA','JETSMART AIRLINES S.A.')
  )

df_dataN$ClasificacionVuelo<-replace(df_dataN$ClasificacionVuelo,df_dataN$ClasificacionVuelo=="Dom?stico","Domestico")
df_dataN$Aerolinea<-replace(df_dataN$Aerolinea,df_dataN$Aerolinea =="AEROLINEAS ARGENTINAS SA","AerolineasArg")
df_dataN$Aerolinea<-replace(df_dataN$Aerolinea,df_dataN$Aerolinea =="JETSMART AIRLINES S.A.","JetSmart")

ggplot(data = df_dataN) +
  geom_col(mapping = aes(x = Aerolinea, y = Pasajeros)) + 
  facet_wrap(vars(ClasificacionVuelo, Aeropuerto)) +
  theme_bw()

####  EZEoAER vs Otro (Dom?stico/Internacional) ####

df_dataM <- Consolidado

df_dataM$ClasificacionVuelo<-replace(df_dataM$ClasificacionVuelo,df_dataM$ClasificacionVuelo=="Doméstico","Domestico")
df_dataM$Aeropuerto<-replace(df_dataM$Aeropuerto,df_dataM$Aeropuerto =="EZE","Ezeiza o Aeroparque")
df_dataM$Aeropuerto<-replace(df_dataM$Aeropuerto,df_dataM$Aeropuerto =="AER","Ezeiza o Aeroparque")
df_dataM$Aeropuerto<-replace(df_dataM$Aeropuerto,df_dataM$Aeropuerto !="Ezeiza o Aeroparque","Otro")

ggplot(data = df_dataM) +
  geom_col(mapping = aes(x = Aeropuerto, y = Pasajeros/1000000)) + 
  facet_wrap(vars(ClasificacionVuelo)) +
  theme(
    plot.title = element_text(color = "white", size = 12, 
                              face = "bold", hjust = 0.5),
    plot.subtitle = element_text(color = "white", hjust = 0.5),
    plot.caption = element_text(color = "white", face = "italic"),
    panel.background = element_rect(fill = "#1a1a1a",
                                    colour = "#1a1a1a",
                                    size = 0.5, linetype = "solid"),
    plot.background = element_rect(fill = "#1a1a1a"),
    axis.title = element_text(color = "white"),
    axis.text.x = element_text(color = "white"),
    axis.text.y = element_text(color = "white"),
    panel.grid.major = element_line(color = "#282828", size = 0.5),
    panel.grid.minor = element_line(color = "#282828", size = 0.5),
    legend.background = element_rect(fill = "#1a1a1a"),
    legend.key = element_rect(fill = "#1a1a1a", color = NA),
    legend.position = "bottom", 
    legend.title = element_text(colour="white", face="bold"),
    legend.text = element_text(colour="white")
  )+ 
  labs(y="Pasajeros (en millones)")


####  Top Aerolinea sobre Top Aeropuerto (base 100%) ####


df_dataO <- subset(Consolidado, select = c(Aeropuerto, Pasajeros, Aerolinea))

AeropuertoTipico <- c("EZE", "AER", "CBA", "BAR", "DOZ")
AerolineaTipica <- c('AEROLINEAS ARGENTINAS SA','JETSMART AIRLINES S.A.', "FB L?NEAS A?REAS - FLYBONDI", "LAN ARGENTINA S.A. (LATAM AIRLINES)", "AMERICAN AIRLINES INC.")
df_dataO$Aeropuerto<-replace(df_dataO$Aeropuerto,df_dataO$Aeropuerto %!in% AeropuertoTipico,"Otro")
df_dataO$Aerolinea<-replace(df_dataO$Aerolinea,df_dataO$Aerolinea %!in% AerolineaTipica,"Otra")

ggplot(data = df_dataO) +
  geom_col(mapping = aes(x = Aeropuerto, y = Pasajeros, fill = Aerolinea), position = "fill") +
  scale_y_continuous(labels=scales::percent) +
  ylab("Porcentajes") +
  theme_tufte()

#### Top Aerolinea sobre Top Aeropuerto (Pasajeros) #### 

ggplot(data = df_dataO) +
  geom_col(mapping = aes(x = Aeropuerto, y = Pasajeros, fill = Aerolinea)) +
  theme_tufte()

#### Top Aerolinea sobre Top Aeropuerto - Excluyente (Pasajeros) #### 


df_dataC <- subset(Consolidado, select = c(Aeropuerto, Pasajeros, Aerolinea))

df_dataC$Aeropuerto<-replace(df_dataC$Aeropuerto,df_dataC$Aeropuerto %!in% AeropuertoTipico,"Otro")
df_dataC$Aerolinea<-replace(df_dataC$Aerolinea,df_dataC$Aerolinea %!in% AerolineaTipica,"Otra")

df_dataW <- df_dataC%>%
  filter(
    Aeropuerto %in% AeropuertoTipico
  )

ggplot(data = df_dataW) +
  geom_col(mapping = aes(x =  Aeropuerto, y = Pasajeros, fill = Aerolinea)) +
  
  theme_tufte()

#### Top Aerolinea sobre Top Aeropuerto - ordenado #### 

df_dataQ <- subset(Consolidado, select = c(Aeropuerto, Pasajeros, Aerolinea))%>%
  filter(
    Aeropuerto %in% c("EZE", "AER", "CBA", "BAR", "DOZ")
  )

AerolineaTipica <- c('AEROLINEAS ARGENTINAS SA','JETSMART AIRLINES S.A.', "FB LÍNEAS AÉREAS - FLYBONDI", "LAN ARGENTINA S.A. (LATAM AIRLINES)", "AMERICAN AIRLINES INC.")
df_dataQ$Aerolinea<-replace(df_dataQ$Aerolinea,df_dataQ$Aerolinea %!in% AerolineaTipica,"Otra")
df_dataQ$Aerolinea<-replace(df_dataQ$Aerolinea,df_dataQ$Aerolinea=='AEROLINEAS ARGENTINAS SA',"Aerolíneas Argentinas")
df_dataQ$Aerolinea<-replace(df_dataQ$Aerolinea,df_dataQ$Aerolinea=='JETSMART AIRLINES S.A.',"JetSmart")
df_dataQ$Aerolinea<-replace(df_dataQ$Aerolinea,df_dataQ$Aerolinea=="FB LÍNEAS AÉREAS - FLYBONDI","FlyBondi")
df_dataQ$Aerolinea<-replace(df_dataQ$Aerolinea,df_dataQ$Aerolinea=="LAN ARGENTINA S.A. (LATAM AIRLINES)","LAN Argentina")
df_dataQ$Aerolinea<-replace(df_dataQ$Aerolinea,df_dataQ$Aerolinea=="AMERICAN AIRLINES INC.","American Airlines")

df_dataQ$Aeropuerto<-replace(df_dataQ$Aeropuerto,df_dataQ$Aeropuerto=="EZE","Ezeiza")
df_dataQ$Aeropuerto<-replace(df_dataQ$Aeropuerto,df_dataQ$Aeropuerto=="AER","Aeroparque")
df_dataQ$Aeropuerto<-replace(df_dataQ$Aeropuerto,df_dataQ$Aeropuerto=="BAR","Bariloche")
df_dataQ$Aeropuerto<-replace(df_dataQ$Aeropuerto,df_dataQ$Aeropuerto=="CBA","Córdoba")
df_dataQ$Aeropuerto<-replace(df_dataQ$Aeropuerto,df_dataQ$Aeropuerto=="DOZ","Mendoza")

ggplot(data = df_dataQ) +
  geom_col(mapping = aes(x = reorder(Aeropuerto, -Pasajeros), y = Pasajeros/1000000, fill = Aerolinea)) +
  labs(title = "Tráfico aéreo en Argentina TOP 5 Aeropuertos",
       subtitle = "Enero 2020 - Septiembre 2022",
       caption = "Fuente: www.datos.gob.ar", x = "Aeropuerto", colour = ""
  )+
  scale_fill_manual( values = c("#0A7CB2",
                             "#D51920",
                             "#F9BA15",
                             "#153E69",
                             "#908F8E",
                             "#525252"))+
  theme(    plot.title = element_text(color = "white", size = 12, 
                                      face = "bold", hjust = 0.5),
            plot.subtitle = element_text(color = "white", hjust = 0.5),
            plot.caption = element_text(color = "white", face = "italic"),
            panel.background = element_rect(fill = "#1a1a1a",
                                            colour = "#1a1a1a",
                                            size = 0.5, linetype = "solid"),
            plot.background = element_rect(fill = "#1a1a1a"),
            axis.title = element_text(color = "white"),
            axis.text.x = element_text(color = "white"),
            axis.text.y = element_text(color = "white"),
            panel.grid.major = element_line(color = "#282828", size = 0.5),
            panel.grid.minor = element_line(color = "#282828", size = 0.5),
            legend.background = element_rect(fill = "#1a1a1a"),
            legend.key = element_rect(fill = "#1a1a1a", color = NA),
            legend.position = "bottom", 
            legend.title = element_text(colour="white", face="bold"),
            legend.text = element_text(colour="white")
  )+
labs(y="Pasajeros (en millones)")


#### Din?mico TR?FICO vs FECHA (fondo gris) ####

anim2 <- ggplot(
  data = df_consolidado3, 
  aes(x = Fecha, y = Pasajeros)) +
  geom_line(color = "white") +
  geom_point(shape= 19, color = "white", size = 5) +
  theme_classic() +
  labs(title = "Evoluci?n del tr?fico a?reo en Argentina",
       subtitle = "Enero 2020 - Septiembre 2022",
       caption = "Fuente: www.datos.gob.ar", y = "Pasajeros/dia") +
  theme(
    plot.title = element_text(color = "white", size = 16, 
                              face = "bold", hjust = 0.5),
    plot.subtitle = element_text(color = "white", hjust = 0.5),
    plot.caption = element_text(color = "white", face = "italic"),
    panel.background = element_rect(fill = "#1a1a1a",
                                    colour = "#1a1a1a",
                                    size = 0.5, linetype = "solid"),
    plot.background = element_rect(fill = "#1a1a1a"),
    axis.title = element_text(color = "white"),
    axis.text.x = element_text(color = "white"),
    axis.text.y = element_text(color = "white"),
    panel.grid.major = element_line(color = "#282828", size = 0.5),
    panel.grid.minor = element_line(color = "#282828", size = 0.5),
  )+
  #geom_hline(yintercept = 50000, color = "white") +
  #geom_vline(xintercept = 2021-01-01, color = "white") +
  transition_reveal(Fecha)

animate(anim2, 100, fps = 15, 
        renderer = gifski_renderer("Fecha-Pasajeros.gif"))

#### Din?mico TR?FICO Aerolineas/Otra vs FECHA (fondo gris) ####

df_consolidado4 <- Consolidado
df_consolidado4$Aerolinea<-replace(df_consolidado4$Aerolinea,df_consolidado4$Aerolinea %!in% 'AEROLINEAS ARGENTINAS SA',"Otra")

df_consolidadoAerolineas <- df_consolidado4  %>%
  filter(
    Aerolinea %in% c('AEROLINEAS ARGENTINAS SA')
  )%>%
  group_by(Fecha) %>% 
  summarise(Pasajeros = sum(Pasajeros))

df_consolidadoOtras <- df_consolidado4  %>%
  filter(
    Aerolinea %in% c('Otra')
  )%>%
  group_by(Fecha) %>% 
  summarise(Pasajeros = sum(Pasajeros))


total <- merge(df_consolidadoAerolineas,df_consolidadoOtras, by = "Fecha")


anim3 <- ggplot(
  data = total, aes(x=Fecha)) +
  geom_line(aes(y = Pasajeros.x, colour = "Aerolineas Argentinas S.A.")) +
  geom_point(aes(y = Pasajeros.x), color = "white", size = 5) +
  geom_line(aes(y = Pasajeros.y, colour = "Otras")) +
  geom_point(aes(y = Pasajeros.y), color = "white", size = 5) +
  theme_classic() +
  labs(title = "Evoluci?n del tr?fico a?reo en Argentina",
       subtitle = "Enero 2020 - Septiembre 2022",
       caption = "Fuente: www.datos.gob.ar", y = "Pasajeros/dia", colour = ""
       ) +
  theme(
    plot.title = element_text(color = "white", size = 12, 
                              face = "bold", hjust = 0.5),
    plot.subtitle = element_text(color = "white", hjust = 0.5),
    plot.caption = element_text(color = "white", face = "italic"),
    panel.background = element_rect(fill = "#1a1a1a",
                                    colour = "#1a1a1a",
                                    size = 0.5, linetype = "solid"),
    plot.background = element_rect(fill = "#1a1a1a"),
    axis.title = element_text(color = "white"),
    axis.text.x = element_text(color = "white"),
    axis.text.y = element_text(color = "white"),
    panel.grid.major = element_line(color = "#282828", size = 0.5),
    panel.grid.minor = element_line(color = "#282828", size = 0.5),
    legend.background = element_rect(fill = "#1a1a1a"),
    legend.key = element_rect(fill = "#1a1a1a", color = NA),
    legend.position = "bottom", #no lo toma
    legend.title = element_text(colour="white", face="bold"),
    legend.text = element_text(colour="white")
    
  )+
  transition_reveal(Fecha)

animate(anim3, 30, fps = 15, 
        renderer = gifski_renderer("Fecha-Pasajeros.gif"))
animate(anim3, 30, fps = 15, 
        renderer = gifski_renderer("Fecha-Pasajeros.gif"), duration = 5)


#### WORDCLOUDS Aeropuerto ####

mooncloud <- Consolidado$Aeropuerto

wordcloud(mooncloud
          , scale=c(5,0.5)     # Set min and max scale
          , max.words=100      # Set top n words
          , random.order=FALSE # Words in decreasing freq
          , rot.per=0.35       # % of vertical words
          , use.r.layout=FALSE 
          , colors=brewer.pal(8, "Dark2"))

#### WORDCLOUDS Aerolinea ####

mooncloud2 <- Consolidado$Aerolinea

wordcloud(mooncloud2
          , scale=c(5,0.5)     # Set min and max scale
          , max.words=100      # Set top n words
          , random.order=FALSE # Words in decreasing freq
          , rot.per=0.35       # % of vertical words
          , use.r.layout=FALSE 
          , colors=brewer.pal(8, "Dark2"))



#### PIECHART Aerolinea ####


df_Consolidadotorta <- subset(Consolidado, select = c(Aerolinea,Pasajeros))  
#detach_package(plyr)

df_Consolidadotorta$Aerolinea<-replace(df_Consolidadotorta$Aerolinea,df_Consolidadotorta$Aerolinea %!in% AerolineaTipica,"Otra")

df_torta <- df_Consolidadotorta  %>%
group_by(Aerolinea) %>% summarise(Pasajeros = sum(Pasajeros))


ggplot(df_torta, aes(x = "", y = Pasajeros, fill=Aerolinea)) +
  geom_bar(stat="identity", width=1, color="white") +
  #geom_text(aes(label = Pasajeros),
  #          position = position_stack(vjust = 0.5)) +
  coord_polar("y", start=0) +
  theme_void() # remove background, grid, numeric labels


#### TOP Aerolineas ####

df_ordenAerolinea <- subset(Consolidado, select = c(Aerolinea,Pasajeros)) %>%
  group_by(Aerolinea) %>% summarise(Pasajeros = sum(Pasajeros)) 

df_ordenAerolinea2 <- df_ordenAerolinea[order(df_ordenAerolinea$Pasajeros, decreasing = TRUE), ]  # Order data descending


ggplot(data = df_ordenAerolinea2 %>% head(4)) +
  geom_col(mapping = aes(x = reorder(Aerolinea, -Pasajeros), y = Pasajeros)) + 
  #scale_x_discrete(guide = guide_axis(n.dodge=3))+
  scale_x_discrete(guide = guide_axis(angle = 45))+
labs(title = "Tr?fico a?reo en Argentina",
       subtitle = "Enero 2020 - Septiembre 2022",
       caption = "Fuente: www.datos.gob.ar", x = "Aerolinea", colour = ""
  )+
  theme_bw()


#### TOP Aeropuertos ####


df_ordenAeropuerto <- subset(Consolidado, select = c(Aeropuerto,Pasajeros)) %>%
  group_by(Aeropuerto) %>% summarise(Pasajeros = sum(Pasajeros)) 

df_ordenAeropuerto2 <- df_ordenAeropuerto[order(df_ordenAeropuerto$Pasajeros, decreasing = TRUE), ]  # Order data descending


ggplot(data = df_ordenAeropuerto2 %>% head(5)) +
  geom_col(mapping = aes(x = reorder(Aeropuerto, -Pasajeros), y = Pasajeros)) + 
  labs(title = "Tr?fico a?reo en Argentina",
       subtitle = "Enero 2020 - Septiembre 2022",
       caption = "Fuente: www.datos.gob.ar", x = "Aeropuerto", colour = ""
  )+
  theme_bw()


#### Dinamico Aerolineas vs Total (Principal) ####

df_consolidado5 <- Consolidado
df_consolidado5$Aerolinea<-replace(df_consolidado5$Aerolinea,df_consolidado5$Aerolinea %!in% 'AEROLINEAS ARGENTINAS SA',"Otra")

df_consolidadoAerolineas2 <- df_consolidado5  %>%
  filter(
    Aerolinea %in% c('AEROLINEAS ARGENTINAS SA')
  )%>%
  group_by(Fecha) %>% 
  summarise(Pasajeros = sum(Pasajeros))

df_consolidadoOtras2 <- df_consolidado5  %>%
  filter(
    Aerolinea %in% c('Otra')
  )%>%
  group_by(Fecha) %>% 
  summarise(Pasajeros = sum(Pasajeros))


total2 <- merge(df_consolidadoAerolineas2,df_consolidadoOtras2, by = "Fecha")


anim3 <- ggplot(
  data = total2, aes(x=Fecha)) +
  geom_line(aes(y = Pasajeros.x, colour = "Aerolineas Argentinas S.A.")) +
  geom_point(aes(y = Pasajeros.x), color = "white", size = 5) +
  geom_line(aes(y = Pasajeros.y + Pasajeros.x, colour = "Totalizado")) +
  geom_point(aes(y = Pasajeros.y + Pasajeros.x), color = "white", size = 5) +
  theme_classic() +
  labs(title = "Evoluci?n del tr?fico a?reo en Argentina",
       subtitle = "Enero 2020 - Septiembre 2022",
       caption = "Fuente: www.datos.gob.ar - Datos de 852.938 vuelos", y = "Pasajeros/dia", colour = ""
  ) +
  theme(
    plot.title = element_text(color = "white", size = 12, 
                              face = "bold", hjust = 0.5),
    plot.subtitle = element_text(color = "white", hjust = 0.5),
    plot.caption = element_text(color = "white", face = "italic"),
    panel.background = element_rect(fill = "#1a1a1a",
                                    colour = "#1a1a1a",
                                    size = 0.5, linetype = "solid"),
    plot.background = element_rect(fill = "#1a1a1a"),
    axis.title = element_text(color = "white"),
    axis.text.x = element_text(color = "white"),
    axis.text.y = element_text(color = "white"),
    panel.grid.major = element_line(color = "#282828", size = 0.5),
    panel.grid.minor = element_line(color = "#282828", size = 0.5),
    legend.background = element_rect(fill = "#1a1a1a"),
    legend.key = element_rect(fill = "#1a1a1a", color = NA),
    legend.position = "bottom", 
    legend.title = element_text(colour="white", face="bold"),
    legend.text = element_text(colour="white")
    
  )+
  transition_reveal(Fecha)

animate(anim3, 200, fps = 15, 
        renderer = gifski_renderer("Fecha-Pasajeros.gif"), duration = 10)

#### NUEVO ####

df_consolidadoX <- Consolidado
df_consolidadoX$Aerolinea<-replace(df_consolidadoX$Aerolinea,df_consolidadoX$Aerolinea %!in% 'AEROLINEAS ARGENTINAS SA',"Otra")

df_consolidadoAerolineas2X <- df_consolidadoX  %>%
  filter(
    Aerolinea %in% c('AEROLINEAS ARGENTINAS SA')
  )%>%
  group_by(Fecha) %>% 
  summarise(Pasajeros = sum(Pasajeros))

df_consolidadoOtras2X <- df_consolidadoX  %>%
  filter(
    Aerolinea %in% c('Otra')
  )%>%
  group_by(Fecha) %>% 
  summarise(Pasajeros = sum(Pasajeros))


total2X <- merge(df_consolidadoAerolineas2X,df_consolidadoOtras2X, by = "Fecha")

ggplot(data = total2X) +
  geom_col(mapping = aes(x = Fecha, y=Pasajeros.x+Pasajeros.y)) +
  geom_col(mapping = aes(x = Fecha, y=Pasajeros.x),color="#0A7CB2") +
  labs(title = "Tráfico Aéreo Aerolíneas vs Total",
       subtitle = "Enero 2020 - Septiembre 2022",
       caption = "Fuente: www.datos.gob.ar", x = "Fecha", colour = "", y = "Pasajeros"
  )+
  theme(plot.title = element_text(color = "white", size = 12, 
                                  face = "bold", hjust = 0.5),
        plot.subtitle = element_text(color = "white", hjust = 0.5),
        plot.caption = element_text(color = "white", face = "italic"),
        panel.background = element_rect(fill = "#1a1a1a",
                                        colour = "#1a1a1a",
                                        size = 0.5, linetype = "solid"),
        plot.background = element_rect(fill = "#1a1a1a"),
        axis.title = element_text(color = "white"),
        axis.text.x = element_text(color = "white"),
        axis.text.y = element_text(color = "white"),
        panel.grid.major = element_line(color = "#282828", size = 0.5),
        panel.grid.minor = element_line(color = "#282828", size = 0.5),
        legend.background = element_rect(fill = "#1a1a1a"),
        legend.key = element_rect(fill = "#1a1a1a", color = NA),
        legend.position = "bottom", 
        legend.title = element_text(colour="white", face="bold"),
        legend.text = element_text(colour="white")
  )


#### 2019-2022 Din?mico ####

Consolidado2019 <- read_excel("Consolidado2019.xlsx") 
Consolidado20192022 <- rbind(Consolidado2019,Consolidado)

Consolidado20192022$Aerolinea<-replace(Consolidado20192022$Aerolinea,Consolidado20192022$Aerolinea %!in% 'AEROLINEAS ARGENTINAS SA',"Otra")

Consolidado20192022_Aerolineas <- Consolidado20192022  %>%
  filter(
    Aerolinea %in% c('AEROLINEAS ARGENTINAS SA')
  )%>%
  group_by(Fecha) %>% 
  summarise(Pasajeros = sum(Pasajeros))

Consolidado20192022_Otras <- Consolidado20192022  %>%
  filter(
    Aerolinea %in% c('Otra')
  )%>%
  group_by(Fecha) %>% 
  summarise(Pasajeros = sum(Pasajeros))


total20192022 <- merge(Consolidado20192022_Aerolineas,Consolidado20192022_Otras, by = "Fecha")

anim4 <- ggplot(
  data = total20192022, aes(x=Fecha)) +
  geom_line(aes(y = Pasajeros.x, colour = "Aerolineas Argentinas S.A.")) +
  geom_point(aes(y = Pasajeros.x), color = "white", size = 5) +
  geom_line(aes(y = Pasajeros.y + Pasajeros.x, colour = "Totalizado")) +
  geom_point(aes(y = Pasajeros.y + Pasajeros.x), color = "white", size = 5) +
  theme_classic() +
  labs(title = "Evoluci?n del tr?fico a?reo en Argentina",
       subtitle = "Enero 2019 - Septiembre 2022",
       caption = "Fuente: www.datos.gob.ar - Datos de 1.433.712 vuelos", y = "Pasajeros/dia", colour = ""
  ) +
  theme(
    plot.title = element_text(color = "white", size = 12, 
                              face = "bold", hjust = 0.5),
    plot.subtitle = element_text(color = "white", hjust = 0.5),
    plot.caption = element_text(color = "white", face = "italic"),
    panel.background = element_rect(fill = "#1a1a1a",
                                    colour = "#1a1a1a",
                                    size = 0.5, linetype = "solid"),
    plot.background = element_rect(fill = "#1a1a1a"),
    axis.title = element_text(color = "white"),
    axis.text.x = element_text(color = "white"),
    axis.text.y = element_text(color = "white"),
    panel.grid.major = element_line(color = "#282828", size = 0.5),
    panel.grid.minor = element_line(color = "#282828", size = 0.5),
    legend.background = element_rect(fill = "#1a1a1a"),
    legend.key = element_rect(fill = "#1a1a1a", color = NA),
    legend.position = "bottom", 
    legend.title = element_text(colour="white", face="bold"),
    legend.text = element_text(colour="white")
    
  )+
  transition_reveal(Fecha)

animate(anim4, 20, fps = 15, 
        renderer = gifski_renderer("Fecha-Pasajeros.gif"), duration = 15)

#### 2019-2022 Est?tico barras ####

Consolidado20192022_Estatico <- Consolidado20192022  %>%
  group_by(Fecha) %>% 
  summarise(Pasajeros = sum(Pasajeros))

ggplot(data = Consolidado20192022_Estatico) +
  geom_col(mapping = aes(x = Fecha, y = Pasajeros)) +
  labs(title = "Est?tico tr?fico A?reo",
       subtitle = "Enero 2019 - Septiembre 2022",
       caption = "Fuente: www.datos.gob.ar", x = "Fecha", colour = "", y = "Pasajeros"
  )+
  theme_tufte()

#### 2019-2022 Est?tico puntos ####

ggplot(data = Consolidado20192022_Estatico) +
  geom_point(mapping = aes(x = Fecha, y = Pasajeros)) +
  labs(title = "Est?tico tr?fico A?reo",
       subtitle = "Enero 2019 - Septiembre 2022",
       caption = "Fuente: www.datos.gob.ar", x = "Fecha", colour = "", y = "Pasajeros"
  )+
  theme_tufte()

#### 2019-2022 Est?tico barras con trend ####

ggplot(data = Consolidado20192022_Estatico) +
  geom_col(mapping = aes(x = Fecha, y = Pasajeros)) +
  labs(title = "Est?tico tr?fico A?reo",
       subtitle = "Enero 2019 - Septiembre 2022",
       caption = "Fuente: www.datos.gob.ar", x = "Fecha", colour = "", y = "Pasajeros"
  )+
  geom_smooth(mapping = aes(x = Fecha, y = Pasajeros))+
  theme_tufte()

#### 2019-2022 Est?tico barras con LM ####

ggplot(data = Consolidado20192022_Estatico) +
  geom_col(mapping = aes(x = Fecha, y = Pasajeros)) +
  labs(title = "Est?tico tr?fico A?reo",
       subtitle = "Enero 2019 - Septiembre 2022",
       caption = "Fuente: www.datos.gob.ar", x = "Fecha", colour = "", y = "Pasajeros"
  )+
  geom_smooth(mapping = aes(x = Fecha, y = Pasajeros), method = lm)+
  theme_tufte()

#### 2019-2022 Est?tico polinomico ####

ggplot(data = Consolidado20192022_Estatico) +
  geom_col(mapping = aes(x = Fecha, y = Pasajeros)) +
  labs(title = "Est?tico tr?fico A?reo",
       subtitle = "Enero 2019 - Septiembre 2022",
       caption = "Fuente: www.datos.gob.ar", x = "Fecha", colour = "", y = "Pasajeros"
  )+
  geom_smooth(mapping = aes(x = Fecha, y = Pasajeros), method = lm, formula = y ~ splines::bs(x, degree = 3))+  # parametro indicativo del ajuste polinomico
  theme_tufte()

