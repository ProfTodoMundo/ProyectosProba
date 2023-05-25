setwd("C://Temporal")

n <- 5
m <- 5

B <- matrix(0,n,m); print(B)
for(i in 1:n){
  for(j in 1:m){
    B[i,j] <- floor(runif(1,-15,15));
    print(B[i,j])
  }
}
colnames(B) <- c('Lunes','Martes','Miercoles',
                 'jueves','Viernes')
rownames(B) <- c('Enero','Febrero','Marzo',
                 'Abril','Mayo')
View(B)

# -------------------------------------------------
n <- 7
m <- 7

B <- matrix(0,n,m); print(B)
for(i in 1:n){
  for(j in 1:m){
    B[i,j] <- floor(runif(1,0,15));
    print(B[i,j])
  }
}
colnames(B) <- c('Lunes','Martes','Miercoles',
                 'jueves','Viernes','Sabado',
                 'Domingo')
rownames(B) <- c('Enero','Febrero','Marzo',
                 'Abril','Mayo','Junio',
                 'Julio')
View(B)

indRenglonPar   <- c(2,4,6)
indRenglonImpar <- c(1,3,5,7)
indColPar       <- c(2,4,6)
indColImpar     <- c(1,3,5,7)

M1 <- B[1:3,1:4]; print(M1)
M2 <- B[4:7,4:7]; print(M2)
M3 <- B[,indColImpar]; print(M3)
M4 <- B[indRenglonPar,]; print(M4)
M5 <- B[indRenglonImpar,indColImpar]; print(M5)
M6 <- B[indRenglonPar,indColPar]; print(M6)
M7 <- B[indRenglonPar,indColImpar]; print(M7)
M8 <- B[indRenglonImpar,indColPar]; print(M8)
# -------------------------------------------------
n <- 7; m <- 7
A1 <- matrix(0,n,m); print(A1)
A2 <- matrix(0,n,m); print(A2)
A3 <- matrix(0,n,m); print(A3)
A4 <- matrix(0,n,m); print(A4)
for(i in 1:n){
  for(j in 1:m){
    entrada <- runif(1,-50,50)
    A1[i,j] <- floor(entrada)
    A2[i,j] <- ceiling(entrada)
    A3[i,j] <- trunc(entrada)
    A4[i,j] <- round(entrada)
  }
}
print(A1)
print(A2)
print(A3)
print(A4)

A4-A1
A4-A2
A4-A3
# -------------------------------------------------
D <-  matrix(0,n,m)
L <-  matrix(0,n,m)
U <-  matrix(0,n,m)
for(i in 1:n){
  for(j in 1:m){
    if(i==j){D[i,j]<- ceiling(runif(1,-50,50))}
    if(j<=i){L[i,j]<- ceiling(runif(1,-50,50))}
    if(j>=i){U[i,j]<- ceiling(runif(1,-50,50))}
  }
}
print(D)
print(L)
print(U)
L <-  matrix(0,n,m)
for(i in 1:n){
  for(j in 1:i){
    L[i,j]<- ceiling(runif(1,-50,50))
  }
}
print(L)

for(i in 1:n){
  for(j in i:m){
    U[i,j]<- ceiling(runif(1,-50,50))
  }
}
print(U)
# -------------------------------------------------
n <- 15
x <- matrix(0,1,n)
for(i in 1:n){
  x[i] <- round(runif(1,0,100))
}
head(x)

suma   <- 0
xbarra <- 0
for(i in 1:n){
  suma <- suma + x[i]
}
xbarra <- suma/n; print(xbarra)

suma2    <- 0
varianza <- 0
for(i in 1:n){
  termino <- x[i]-xbarra
  termino2 <- termino^2
  suma2 <- suma2 + termino2
}
varianza <- suma2/(n-1); print(varianza)




