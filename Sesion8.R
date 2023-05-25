set.seed(123)
numvar <- 150;
X <- rnorm(numvar);      print(X)
Y <- 2*X +rnorm(numvar); print(Y)


plot(X,Y)

plot(X,Y,main= 'Ejemplo de Regresion Lineal',
     xlab = 'Variable Independiente',
     ylab = 'Variable Dependiente')

plot(X,Y,main= 'Ejemplo de Regresion Lineal',
     xlab = 'Variable Independiente',
     ylab = 'Variable Dependiente',
     col = 'blue')
# >>==<< >>==<< >>==<< >>==<< >>==<< >>==<< >>==<<
# >>==<< >>==<< >>==<< >>==<< >>==<< >>==<< >>==<<
set.seed(123)
numvar <- 150;
# >>==<< >>==<< >>==<< >>==<< >>==<< >>==<< >>==<<
# >>==<< >>==<< >>==<< >>==<< >>==<< >>==<< >>==<<
X1 <- rnorm(numvar);      print(X1)
Y1 <- X1+rnorm(numvar); print(Y1)
Regresion <- MiRegresion(X1,Y1)
plot(X1,Y1,main= 'Ejemplo 1 de Regresion Lineal',
     xlab = 'Variable Independiente',
     ylab = 'Variable Dependiente',
     col = 'blue',
     type = 'p')
par(new=TRUE)
plot(X1,Regresion$Yest,col='red',type='l')
Ygorro <- Regresion$Yest
hist(Ygorro, main='Distribucion de los Errores de estimacion',
     xlab='Errores',
     ylab= 'Frecuencia',
     col = rainbow(15, 0.25),
     border = 'green')

# >>==<< >>==<< >>==<< >>==<< >>==<< >>==<< >>==<<
# >>==<< >>==<< >>==<< >>==<< >>==<< >>==<< >>==<<
X2 <- rnorm(numvar);      print(X2)
Y2 <- 9*X2 +rnorm(numvar); print(Y2)
Regresion <- MiRegresion(X2,Y2)
plot(X2,Y2,main= 'Ejemplo 2 de Regresion Lineal',
     xlab = 'Variable Independiente',
     ylab = 'Variable Dependiente',
     col = 'pink',
     type = 'p')
par(new=TRUE)
plot(X2,Regresion$Yest,col='red',type='l')
Ygorro <- Regresion$Yest
hist(Ygorro, main='Ejmplo 2: Distribucion de los Errores de estimacion',
     xlab='Errores',
     ylab= 'Frecuencia',
     col = rainbow(15, 0.25),
     border = 'blue')
# >>==<< >>==<< >>==<< >>==<< >>==<< >>==<< >>==<<
X3 <- rnorm(numvar);      print(X3)
Y3 <- -2*X3 +rnorm(numvar); print(Y3)
Regresion <- MiRegresion(X3,Y3)
plot(X3,Y3,main= 'Ejemplo 3 de Regresion Lineal',
     xlab = 'Variable Independiente',
     ylab = 'Variable Dependiente',
     col = 'green',
     type = 'p')
par(new=TRUE)
plot(X3,Regresion$Yest,col='red',type='l')
Ygorro <- Regresion$Yest
hist(Ygorro, main='Ejemplo 3: Distribucion de los Errores de estimacion',
     xlab='Errores',
     ylab= 'Frecuencia',
     col = rainbow(15, 0.25),
     border = 'pink')

# >>==<< >>==<< >>==<< >>==<< >>==<< >>==<< >>==<<
X4 <- rnorm(numvar);      print(X4)
Y4 <- -2*X4 - rnorm(numvar); print(Y4)
Regresion <- MiRegresion(X4,Y4)
plot(X4,Y4,main= 'Ejemplo 4 de Regresion Lineal',
     xlab = 'Variable Independiente',
     ylab = 'Variable Dependiente',
     col = 'orange',
     type = 'p')
par(new=TRUE)
plot(X4,Regresion$Yest,col='red',type='l')
Ygorro <- Regresion$Yest
hist(Ygorro, main='Ejempolo 4: Distribucion de los Errores de estimacion',
     xlab='Errores',
     ylab= 'Frecuencia',
     col = rainbow(15, 0.25),
     border = 'brown')

# >>==<< >>==<< >>==<< >>==<< >>==<< >>==<< >>==<<
X5 <- rnorm(numvar);      print(X5)
Y5 <- -2*X5 - 2*rnorm(numvar); print(Y5)
Regresion <- MiRegresion(X5,Y5)
plot(X5,Y5,main= 'Ejemplo 5 de Regresion Lineal',
     xlab = 'Variable Independiente',
     ylab = 'Variable Dependiente',
     col = 'yellow',
     type = 'p')
par(new=TRUE)
plot(X5,Regresion$Yest,col='red',type='l')
Ygorro <- Regresion$Yest
hist(Ygorro, main='Ejemplo 5: Distribucion de los Errores de estimacion',
     xlab='Errores',
     ylab= 'Frecuencia',
     col = rainbow(15, 0.25),
     border = 'red')

# >>==<< >>==<< >>==<< >>==<< >>==<< >>==<< >>==<<
suma <- 0;
for(i in 1:numvar){suma <- suma +X[i]}
xbarra <- suma/numvar;

suma <- 0;
for(i in 1:numvar){suma <- suma +Y[i]}
ybarra <- suma/numvar;

suma <- 0;
for(i in 1:numvar){suma <- suma +(X[i]-xbarra)^2}
Sxx <- suma/numvar

suma <- 0;
for(i in 1:numvar){suma <- suma+((X[i]-xbarra)*(Y[i]-ybarra))}
Sxy <- suma/numvar;

Beta1 <- Sxy/Sxx;
Beta0 <- ybarra-Beta1*xbarra

Ygorro <- Beta0+Beta1*X; print(Ygorro)
ErrorEst <- Y-Ygorro
hist(ErrorEst)
# >>==<< >>==<< >>==<< >>==<< >>==<< >>==<< >>==<<
source('C:/Users/Alumno/Desktop/ClaseProba/MiRegresion.R')
x <- MiRegresion(X1,Y1)
# >>==<< >>==<< >>==<< >>==<< >>==<< >>==<< >>==<<
X1 <- rnorm(numvar);      print(X1)
Y1 <- X1+rnorm(numvar); print(Y1)
Regresion <- MiRegresion(X1,Y1)
plot(X1,Y1,main= 'Ejemplo 1 de Regresion Lineal',
     xlab = 'Variable Independiente',
     ylab = 'Variable Dependiente',
     col = 'blue',
     type = 'p')
par(new=TRUE)
plot(X1,Regresion$Yest,col='red',type='l')
Ygorro <- Regresion$Yest
hist(Ygorro, main='Distribucion de los Errores de estimacion',
     xlab='Errores',
     ylab= 'Frecuencia',
     col = rainbow(15, 0.25),
     border = 'green')

# >>==<< >>==<< >>==<< >>==<< >>==<< >>==<< >>==<<