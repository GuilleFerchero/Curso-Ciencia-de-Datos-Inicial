####Limpiamos nuestro ambiente de trabajo####

rm(list = ls())

####Instalamos y activamos paquetes de trabajo####

library("openxlsx")
library("tidyverse")

####Levantamos base de trasplantes####

link <- "http://cdn.buenosaires.gob.ar/datosabiertos/datasets/donacion-y-trasplante-de-organos-y-tejidos-en-la-ciudad-de-buenos-aires/trasplantes.csv"

Basetrasplantes <- read.csv(link, sep = ";", encoding = "UTF-8") #Leemos datos desde link


#####Solución Ejercicios#######

#1) Necesito una tabla en excel que contenga los trasplantes de casos mayores a 10 años y menores a 50, 
#ocurridos en el período 2013-2016. 
#En esa tabla no me interesa ver las siguientes variables: DIRECCIÓN, COD POSTAL, LAT, LNG, ID. 

basetransplentes3 <- Basetrasplantes %>% 
  select(PERIODO,GESTION,INSTITUCION,SEXO,EDAD,BARRIO, ID) %>% 
  filter(PERIODO==2013:2016)

#,EDAD>10,EDAD<50

Tabla_ej_1 <- Basetrasplantes %>%
  select(!c(DIRECCION,COD_POSTAL, LAT, LNG))%>% # seleccion de variables
  #filter(EDAD > 10 & EDAD <50) %>% #filtro de edad
  filter(PERIODO >= 2013 & PERIODO <= 2016) 
  

Tabla_ej_2 <- Basetrasplantes %>%
  select(!c(DIRECCION,COD_POSTAL, LAT, LNG, ID))%>% # seleccion de variables
  filter(EDAD >= 10 & EDAD <=50) %>% #filtro de edad
  filter(PERIODO >= 2013 & PERIODO <= 2016) %>% 
  arrange(desc(EDAD))

#2) OPCIONAL : Quisiera que la tabla generada se encuentre ordenada por edad. 
#Esto se hace con la función arrange (funciona igual que select y filter )
#que no la llegamos a ver pero es muy sencilla. Si pueden investiguen un poco en el sitio de dplyr.

#3) OPCIONAL: Quisiera otra tabla de excel que contenga los casos acontecidos en almagro,

Tabla_ej_3 <- Basetrasplantes %>%
  select(!c(DIRECCION,COD_POSTAL, LAT, LNG, ID))%>%
  filter(BARRIO == "ALMAGRO")# seleccion de variables


Baseejercicio <- createWorkbook() #creamos hoja de trabajo
addWorksheet(Baseejercicio, sheetName = "Solución EJ 1")#creamos hoja
writeDataTable(Baseejercicio, sheet = "Solución EJ 1", Tabla_ej_1)#escribimos los datos
addWorksheet(Baseejercicio, sheetName = "Solución EJ 2")#creamos hoja
writeDataTable(Baseejercicio, sheet = "Solución EJ 2", Tabla_ej_2)#escribimos los datos
addWorksheet(Baseejercicio, sheetName = "Solución EJ 3")#creamos hoja
writeDataTable(Baseejercicio, sheet = "Solución EJ 3", Tabla_ej_3)
saveWorkbook(Baseejercicio, file = "Solución Ejercicio.xlsx")#salvamos archivo

#Distribución de frecuencias

Tabla_1 <- Basetrasplantes %>% 
  select(GESTION) %>% 
  group_by(GESTION) %>% 
  summarise(Total = n())

Tabla_2 <- Basetrasplantes %>% 
  select(PERIODO) %>% 
  group_by(PERIODO) %>% 
  summarise(Total = n()) %>% 
  mutate("%" = round(Total/sum(Total)*100,2))

Tabla_3 <- Basetrasplantes %>% 
  select(GESTION, EDAD) %>% 
  group_by(GESTION) %>% 
  summarise(Total = n(),
            PROMEDIO_EDAD = round(mean(EDAD),2),
            Desvío_edad = round(sd(EDAD),2))
            
Tabla_4 <- Basetrasplantes %>% 
  select(SEXO) %>% 
  group_by(SEXO) %>% 
  summarise(Total = n())

Tabla_5 <- Basetrasplantes %>% 
  select(GESTION) %>% 
  group_by(GESTION) %>% 
  summarise(Total = n()) %>% 
  mutate("%" = round(Total/sum(Total)*100,2))

Basetrasplantesrecod <- Basetrasplantes %>% 
  mutate("SexoRecod" = case_when(SEXO == "MASC" ~ "Masculino",
                                 SEXO == "FEM" ~ "Femenino",
                                 SEXO == "MASC " ~ "Masculino",
                                 SEXO == "FEM " ~ "Femenino"),
         "Región" = case_when(COMUNA %in% c("COMUNA 12","COMUNA 13","COMUNA 14","COMUNA 02") ~ "Norte",
                              COMUNA %in% c("COMUNA 05","COMUNA 06","COMUNA 07","COMUNA 10","COMUNA 11","COMUNA 15","COMUNA 01","COMUNA 03") ~ "Centro",
                              COMUNA %in% c("COMUNA 08","COMUNA 04","COMUNA 09") ~ "Sur",
                              COMUNA == "" ~ "Sin Informar"),
         "Edad" = case_when(EDAD >= 0 & EDAD < 15 ~ "Menores de 15",
                            TRUE ~ SEXO))

                  
# Basetrasplantesrecod <- Basetrasplantes %>% 
#         mutate("Región" = case_when(COMUNA %in% c("COMUNA 12",
#                                             "COMUNA 13",
#                                             "COMUNA 14",
#                                             "COMUNA 02") ~ "Norte",
#                               COMUNA %in% c("COMUNA 05",
#                                             "COMUNA 06",
#                                             "COMUNA 07",
#                                             "COMUNA 10",
#                                             "COMUNA 11",
#                                             "COMUNA 15",
#                                             "COMUNA 01",
#                                             "COMUNA 03") ~ "Centro",
#                               COMUNA %in% c("COMUNA 08",
#                                             "COMUNA 04",
#                                             "COMUNA 09") ~ "Sur",
#                               COMUNA == "" ~ "Sin Informar"))
# 
# 
# Basetrasplantesrecod <- Basetrasplantes %>% 
#   mutate("Edad" = case_when(EDAD >= 0 & EDAD < 10 ~ "Menores de 10",
#                             EDAD >= 10 & EDAD < 20 ~ "de 10 hasta 20",
#                             EDAD >= 20 & EDAD < 30 ~ "de 20 hasta 30",
#                             EDAD >= 30 & EDAD < 40 ~ "de 30 hasta 40",
#                             EDAD >= 40 & EDAD < 50 ~ "de 40 hasta 50",
#                             EDAD >= 50 & EDAD < 60 ~ "de 50 hasta 60",
#                             TRUE ~ "Mayores de 60"))

Tabla_Edad <- Basetrasplantesrecod %>% 
  select(Edad) %>% 
  group_by(Edad) %>% 
  summarise(Trasplantes = n())


Tabla_6 <- Basetrasplantes %>% 
  select(COMUNA) %>% 
  group_by(COMUNA) %>% 
  summarise(Total = n())

Tabla_7 <- Basetrasplantes %>% 
  select(BARRIO) %>% 
  group_by(BARRIO) %>% 
  summarise(Total = n())

Tabla_8 <- Basetrasplantesrecod %>% 
  select(Región) %>% 
  group_by(Región) %>% 
  summarise(Total = n()) %>% 
  mutate("%" = round(Total/sum(Total)*100,2))
  
Tabla_9 <- Tabla_8 %>% 
  select(!Total) %>% 
  filter(Región == "Norte") %>% 
  rename("Porcentaje de trasplantes" = "%")

     
#########PRUEBA PASTE#############

Barrio <- "ALMAGRO"

Tabla_paste <- Basetrasplantes %>% 
  select(!c(DIRECCION,COD_POSTAL,LAT,LNG,ID))%>% 
  filter(BARRIO == Barrio)

Baseejercicio <- createWorkbook() #creamos hoja de trabajo
addWorksheet(Baseejercicio, sheetName = Barrio)#creamos hoja
writeDataTable(Baseejercicio, sheet = Barrio, Tabla_paste)#escribimos los datos
saveWorkbook(Baseejercicio, file = paste("Trasplantes-",Barrio,".xlsx"))#salvamos archivo


getwd()


#########################################################################


