#----------------------------------
setwd("C:/CursoEyP")
#----------------------------------
x <- c(1,12,30,54,5); 
print(x)
y <- runif(10,0,100)
print(y)
y <- floor(y); print(y)
# defino una constante aleatoria k
k <- floor(runif(1,0,100)); print(k)

z <- k+y; print(z)

x1 <- floor(runif(15,0,50))
x2 <- ceiling(runif(15,0,50))

y1 <- x1+x2; print(y1)
y2 <- x1-x2; print(y2)
y3 <- x1%*%x2; print(y3)

# k*y1, y1/k,

w <- floor(runif(1500,0,150));
print(w)
head(w,30)
tail(w,30)

A <- matrix(0,5,5); print(A)


n <- 5; m <- 5
B <- matrix(0,n,m); print(B)
for(i in 1:n){
  for(j in 1:m){
    B[i,j] <- ceiling(runif(1,0,15))
  }
}
print(B)
View(B)
colnames(B)<- c('Lunes','Martes',
                'Miercoles','Jueves',
                'Viernes')
rownames(B)<- c('Enero','Febrero',
                'Marzo','Abril',
                'Mayo')
View(B)
B
B[,3]
B[3,]
B[1:3,]
B[,1:3]
indices <- c(2,4)
B[indices,]
B[,indices]
#----------------------------------
# EJERCICIO (1)  GENERAR UNA MATRIZ ALEATORIA
# DE 7X7, ETIQUETAR LAS COLUMNAS, LOS RENGLONES
#GENERAR UNA SUBMATRIZ 
# 1) U QUE CONTENGA LOS PRIMEROS 3 RENGLONES Y LAS PRIMERAS 4 COLUMNAS
# 2) V QUE CONTENGA LAS ULTIMAS 4 COLUMNAS Y LOS ULTIMOS 4 RENGLONES
# 3) W QUE CONTENGA LAS COLUMNAS IMPARES
# 4) R QUE CONTENGA LOS RENGLONES PARES
# 5) S QUE CONTENGA RENGLONES Y COLUMNAS IMPARES
# 6) T QUE CONTENGA RENGLONES Y COLUMNAS PARES
# 7) Q QUE CONTENGA RENGLONES PARES Y COLUMNAS IMPARES
# 8) P QUE CONTENGA RENGLONES IMPARES Y COLUMNAS PARES


# EJERCICIO (2) GENERAR 4 MATRICES ALEATORIAS CON VALORES
# ENTRE -50 Y 50, UTILIZANDO LOS COMANDOS DE: TECHO, PISO, REDONDEO
# TRUNCAMIENTO, COMPARAR Y HACER OPERACIONES ELEMENTALES.

# EJERCICIO (3) GENERAR UNA MATRIZ DIAGONAL NxN

# Graficas


#----------------------------------
# generacion de tablas