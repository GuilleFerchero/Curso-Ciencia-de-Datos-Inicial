####Limpiamos nuestro ambiente de trabajo####

rm(list = ls())

####Instalamos y activamos paquetes de trabajo####

library("openxlsx")
library("tidyverse")


setwd("C:/Users/Guille/Dropbox/R/Curso Ciencia de Datos con R/Curso R Nivel Inicial")

####Levantamos base de trasplantes####

link <- "http://cdn.buenosaires.gob.ar/datosabiertos/datasets/donacion-y-trasplante-de-organos-y-tejidos-en-la-ciudad-de-buenos-aires/trasplantes.csv"

Basetrasplantes <- read.csv(link, sep = ";", encoding = "UTF-8") #Leemos datos desde link

Basetrasplantesrecod <- Basetrasplantes %>% 
  mutate("SexoRecod" = case_when(SEXO == "MASC" ~ "Masculino",
                                 SEXO == "FEM" ~ "Femenino",
                                 SEXO == "MASC " ~ "Masculino",
                                 SEXO == "FEM " ~ "Femenino"),
         "Región" = case_when(COMUNA %in% c("COMUNA 12","COMUNA 13","COMUNA 14","COMUNA 02") ~ "Norte",
                              COMUNA %in% c("COMUNA 05","COMUNA 06","COMUNA 07","COMUNA 10","COMUNA 11","COMUNA 15","COMUNA 01","COMUNA 03") ~ "Centro",
                              COMUNA %in% c("COMUNA 08","COMUNA 04","COMUNA 09") ~ "Sur",
                              COMUNA == "" ~ "Sin Informar"),
         "Edad" = case_when(EDAD >= 0 & EDAD < 10 ~ "Menores de 10",
                            EDAD >= 10 & EDAD < 20 ~ "de 10 hasta 20",
                            EDAD >= 20 & EDAD < 30 ~ "de 20 hasta 30",
                            EDAD >= 30 & EDAD < 40 ~ "de 30 hasta 40",
                            EDAD >= 40 & EDAD < 50 ~ "de 40 hasta 50",
                            EDAD >= 50 & EDAD < 60 ~ "de 50 hasta 60",
                            TRUE ~ "Mayores de 60"))




########Visualización##############


#Paletas https://colors.muz.li/

ggplot(Basetrasplantesrecod %>% 
         filter(Región != "Sin Informar"), aes(x = Región , fill = SexoRecod))+
  geom_bar(position = "dodge")+
  scale_fill_manual(values = c("#7c4739","#bae8e8"))+
  theme_minimal()+
  labs(title = "Casos según sexo",
       caption = "Datos Abiertos CABA")+
  theme(
    plot.title = element_text( hjust = 0.5 , vjust = 0.5)
  )


ggplot(Basetrasplantesrecod,aes(x = Región , y = EDAD))+
  geom_boxplot()+
  theme_minimal()


#Graficar a partir de tablas


Tabla1 <- Basetrasplantesrecod %>% 
  select(PERIODO) %>% 
  group_by(PERIODO) %>% 
  summarise(Casos = n())

ggplot(Tabla1, aes(x = PERIODO, y = Casos))+
  geom_line(color = "#f8aa4b")+
  geom_point(color = "#f8aa4b")+
  theme_minimal()



