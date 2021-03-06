#Base de datos de datasets  "cars"
data(cars)

#clase de cars
class(cars)
 


#resumir el conjunto de datos
summary(cars)


#calcular la desviaci�n est�ndar para todos los atributos
sapply(cars[,1:2], sd)



#Graficos de densidad
#crear un dise�o de parcelas de densidad m�s simples por atributo
par(mfrow = c(1, 2)) 
for(i in 1:2) {
  plot(density(cars[,i]), xlab = names(cars)[i], ylab = "Densidad", main = "")
}



# cargar paquete
library(corrplot)
# calcular grafica de correlacion
M<- cor(cars)
corrplot(M, method = "circle")


# Scatter plot
plot(cars[,1], cars[,2], xlab = names(cars)[1], ylab = names(cars)[2])

#install.packages("car")
library("car")
#scatterplot(speed ~ dist, data = cars)

scatterplot(dist ~ speed, data = cars)

#tabla
table(cars)

colSums(cars)




#grafica de caja bigotes (boxplot)

boxplot(x = cars$speed)
boxplot(x = cars$dist)
