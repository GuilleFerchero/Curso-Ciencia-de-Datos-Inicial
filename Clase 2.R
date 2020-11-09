####Limpiamos nuestro ambiente de trabajo####

rm(list = ls())

####Instalamos y activamos paquetes de trabajo####

install.packages("openxlsx")
library("openxlsx")
install.packages("tidyverse")
library("tidyverse")


####Probamos el paquete openxlsx####


peso <- c(56,78,89,76)

nombre <- c("A","B","C","D")

altura <- c(1.68,1.76,1.80,1.65)

IMC <- round(peso/altura^2,2)

Base <- data.frame(nombre,peso,altura,IMC)

Basexlsx <- createWorkbook() #creamos hoja de trabajo
addWorksheet(Basexlsx, sheetName = "Base Prueba")#creamos hoja
writeDataTable(Basexlsx, sheet = "Base Prueba", Base)#escribimos los datos
saveWorkbook(Basexlsx, file = "Basepruebaclase.xlsx")#salvamos archivo


####Levantamos base de trasplantes####

link <- "http://cdn.buenosaires.gob.ar/datosabiertos/datasets/donacion-y-trasplante-de-organos-y-tejidos-en-la-ciudad-de-buenos-aires/trasplantes.csv"

Basetrasplantes <- read.csv(link, sep = ";", encoding = "UTF-8") #Leemos datos desde link

Basetrasplantes2 <- read.csv("trasplantes.csv", sep = ";") #Leemos datos descargados

rm(Basetrasplantes2)
rm(Base)

summary(Basetrasplantes)

Basetrasplantes %>% #ctrl + shift + M 
    summary()

####FUNCIONES TIDYVERSE####

#select
#filter

Basetrasplantes3 <- Basetrasplantes %>%
  select(PERIODO,GESTION) %>% 
  filter(PERIODO %in% c(2012,2013,2014))
  
Basetrasplantes4 <- Basetrasplantes %>%
  filter(PERIODO == "2012")%>% 
  select(INSTITUCION,BARRIO)
  
Basetras <- createWorkbook() #creamos hoja de trabajo
addWorksheet(Basetras, sheetName = "Base Trasplantes")#creamos hoja
writeDataTable(Basetras, sheet = "Base Trasplantes", Basetrasplantes3)
addWorksheet(Basetras, sheetName = "Base Trasplantes2")#creamos hoja
writeDataTable(Basetras, sheet = "Base Trasplantes2", Basetrasplantes4)#escribimos los datos
saveWorkbook(Basetras, file = "BaseTrasplantes.xlsx")#salvamos archivo


# link2 <- "https://cdn.buenosaires.gob.ar/datosabiertos/datasets/salud/reporte-covid/dataset_reporte_covid_sitio_gobierno.csv"
# 
# 
# 
# basecovid <- read.csv(link2)


Basetrasplantes$sexofac <- factor(Basetrasplantes$SEXO)

Basetrasplantes %>% 
  summary()

