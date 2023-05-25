# Respuestas Binomial

dbinom(1,5,0.1)
# Comprobando
5*(0.1)*(0.9^4)

# Probabilidad de que 3 sean defectuosos
dbinom(3,5,0.1)

# Probabilidad de que 2 sean defectuosos
dbinom(2,5,0.1)


dbinom(4,5,0.1)

dbinom(5,5,0.1)
x <- c()
n <- 10
p <- 0.47
for(i in 1:10){
  x[i] <- dbinom(i,n,p)
  
}
print(x)
sum(x[6:10])


n<- 100
p <- 0.2

# P[0<X<9]
x <- c()
n <- 100
p <- 0.2
for(i in 0:100){
  x[i+1] <- dbinom(i,n,p)
  
}
print(x)
respuesta <- sum(x[1:10]); 
print(respuesta)


# ejercicio 10

n <- 50
p <- 0.3
#P[X>=10] = 1-P[X<10]
x <- c()
for(i in 0:(n-1)){
  x[i+1] <- dbinom(i,n,p)
  
}
print(x)
respuesta <- 1 - sum(x[1:10]); 
print(respuesta)


# ejercicio 8

n <- 200
p <- 0.05
x <- c()
for(i in 0:(n-1)){
  x[i+1] <- dbinom(i,n,p)
  
}
print(x)
print(x[16])

























