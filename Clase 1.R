# Limpiamos nuestro ambiente de trabajo

rm(list = ls())

# Seleccionamos nuestro directorio de trabajo

getwd()

# Generamos objetos de trabajo

a <- 8

b <- 7

sumaayb <- a+b

multiplic <- a*b

c <- sumaayb + multiplic

a <- c(4,5,6,7)

b <- c(4:7)

c <- a+b


# Creamos nuestro Data Frame

peso <- c(56,78,89,76)

nombre <- c("A","B","C","D")

altura <- c(1.68,1.76,1.80,1.65)

IMC <- round(peso/altura^2,2)

Base <- data.frame(nombre,peso,altura,IMC)


mean(Base$peso)



número <- c(1,2,3,4,5)

nombre <- c("jorge","pablo","marcela","romina","andrés")

edad <- c(45,36,29,40,23)

región <- c("norte","sur","sur","oeste","norte")

tiene_trabajo <- c("no","si","si","si","no")
#los nombres de los objetos no tienen que tener espacios, por eso el _

base2 <- data.frame(número,nombre,edad,región,tiene_trabajo)

colnames(base2) <- c("N","Nombre","Edad","Región","Tiene trabajo?")

nrow(base2)

ncol(base2)

str(base2)

summary(base2)

write.csv(base2,"base.csv")

base3 <- read.csv(file = "base.csv")



