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
         

########Relaciones##############


###Tablas de Contigencia

##Vamos a generar una tabla de Tipo de Gestión según Sexo utilizando métodos relacionales


Tabla_F <- Basetrasplantesrecod %>% 
  select(SexoRecod, GESTION) %>% 
  filter(SexoRecod == "Femenino") %>% 
  group_by(GESTION) %>% 
  summarise(Femenino = n()) %>% 
  rename("Gestión" = GESTION)#Renombramos para pasar a minúscula

Tabla_M <- Basetrasplantesrecod %>% 
  select(SexoRecod, GESTION) %>% 
  filter(SexoRecod == "Masculino") %>% 
  group_by(GESTION) %>% 
  summarise(Masculino = n()) %>% 
  rename("Gestión" = GESTION)#Renombramos para pasar a minúscula

Tabla_Join <- left_join(Tabla_F,Tabla_M)


##Ahora vamos a generar una tabla de casos por región a la cual le agregaremos información externa


Tabla_Reg <- Basetrasplantesrecod %>% 
  select(Región) %>% 
  group_by(Región) %>% 
  summarise(Casos = n()) %>% 
  filter(Región != "Sin Informar") #Quito los casos sin informar

#Levantamos base con habitantes

Base_Reg <- read.csv("Base Región.csv", sep =";", encoding = "UTF-8")

Base_Reg <- Base_Reg %>% 
  rename( "Región" = X.U.FEFF.Región)


Tabla_Reg_Hab <- left_join(Tabla_Reg,Base_Reg)

Tabla_Reg_Hab <- Tabla_Reg_Hab %>% 
  mutate(Prop = Casos/Población)

##Ahora vamos a recodificar la Tabla Original con información externa


# Tabla_Barrio <- Basetrasplantesrecod %>% 
#   select(BARRIO) %>% 
#   group_by(BARRIO) %>% 
#   summarise(Casos = n()) %>% 
#   filter(BARRIO != "")

Base_Zonas <- read.csv("Base Barrios Zonas.csv",sep =";", encoding = "UTF-8")

Base_Zonas <-  Base_Zonas %>% 
  rename("BARRIO" = X.U.FEFF.BARRIO)

Basetrasplantesrecod <- left_join(Basetrasplantesrecod,Base_Zonas)

Basetrasplantesrecod <- Basetrasplantesrecod %>% 
  mutate("ZonaRecod" = case_when(is.na(Zona) ~ "Sin Informar",
                          TRUE ~ Zona))
 


