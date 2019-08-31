#pasar datos de diesel bajo en azufre a diesel o todos los dieseles a diesel
#setwd("C:/Users/Cristian/Documents/uvg2019 2do semestre/data science/labs/lab3/data")
setwd("C:/Users/Usuario Dell/Desktop/S8/Data Science/lab3-datascience/data")

#install.packages("forecast")
#install.packages("fUnitRoots")
#install.packages("ggfortify")

importaciones <- read.csv("datosImp.csv")
summary(importaciones$GLP)
qqnorm(importaciones$GLP, main="GLP", col = 2)
qqline(importaciones$GLP)
hist(importaciones$GLP, col = "blue")
#gasolina aviacion
summary(importaciones$GasAviacion)
qqnorm(importaciones$GasAviacion, main="Gas aviación", col = 2)
qqline(importaciones$GasAviacion)
#gasolina superior
summary(importaciones$GasSuperior)
qqnorm(importaciones$GasSuperior, main="Gas superior", col = 2)
qqline(importaciones$GasSuperior)
hist(importaciones$GasSuperior, col = "red", prob= T)
lines(density(importaciones$GasSuperior))
#gasolina regular
summary(importaciones$GasRegular)
qqnorm(importaciones$GasRegular, main="Gas Regular", col = 2)
qqline(importaciones$GasRegular)
hist(importaciones$GasRegular, col = "red", prob= T)
lines(density(importaciones$GasRegular))
#Kerosina
summary(importaciones$Kerosina)
qqnorm(importaciones$Kerosina, main="Kersoina", col = 2)
qqline(importaciones$Kerosina)
hist(importaciones$Kerosina, col = "red", prob= T)
lines(density(importaciones$Kerosina))
#diesel
importaciones$Diesel[is.na(importaciones$Diesel)] <- 0
importaciones$DieselLS[is.na(importaciones$DieselLS)] <- 0
importaciones$DieselULS[is.na(importaciones$DieselULS)] <- 0
#sumar columnas
importaciones$Diesel <- importaciones$Diesel + importaciones$DieselLS + importaciones$DieselULS
#analisis diesel
summary(importaciones$Diesel)
hist(importaciones$Diesel, col = "red", prob= T)
lines(density(importaciones$Diesel))
ks.test(importaciones$Diesel, pnorm, mean(importaciones$Diesel, sd(importaciones$Diesel)))
#bunker
summary(importaciones$Bunker)
hist(importaciones$Bunker, col = "red", prob= T)
lines(density(importaciones$Bunker))
ks.test(importaciones$Bunker, pnorm, mean(importaciones$Bunker, sd(importaciones$Bunker)))
#asfalto
summary(importaciones$Asfalto)
hist(importaciones$Asfalto, col = "red", prob= T)
lines(density(importaciones$Asfalto))
#petroleo reconstituido
summary(importaciones$PetroleoReconst)
hist(importaciones$PetroleoReconst, col = "red", prob= T)
lines(density(importaciones$PetroleoReconst))
#MTBE
summary(importaciones$MTBE)
hist(importaciones$MTBE, col = "red", prob= T)
lines(density(importaciones$MTBE))

#serie de tiempo
class(importaciones$Diesel)
diesel_tseries <- ts(importaciones$Diesel, start = c(2001,1), end = c(2019,6), frequency = 12)
plot(diesel_tseries)
start(diesel_tseries)
end(diesel_tseries)
frequency(diesel_tseries)
# descomponer la serie
diesel_tseries.desc <- decompose(diesel_tseries)
plot(diesel_tseries.desc)


class(importaciones$GasSuperior)
superior_tseries <- ts(importaciones$GasSuperior, start = c(2001,1), end = c(2019,6), frequency = 12)
plot(superior_tseries)
start(superior_tseries)
end(superior_tseries)
frequency(superior_tseries)
# descomponer la serie
superior_tseries.desc <- decompose(superior_tseries)
plot(superior_tseries.desc)

class(importaciones$GasRegular)
regular_tseries <- ts(importaciones$GasRegular, start = c(2001,1), end = c(2019,6), frequency = 12)
plot(regular_tseries)
start(regular_tseries)
end(regular_tseries)
frequency(regular_tseries)
# descomponer la serie
regular_tseries.desc <- decompose(regular_tseries)
plot(regular_tseries.desc)
