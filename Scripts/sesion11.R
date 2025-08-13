#1. X dist. normal con media = 2 y DesvEst = 1/3
#a) P[X>3]
#b) P[2<X<3]
#c) Encontrar x0 tal que P[X>x0]= 0.45
#d) Encontrar x* tal que P[X<x*]= alfa
#d') Encontrar x* talque P[|X|<x*]=alfa
#alfa = 0.5;   alfa = 0.75;  alfa = 0.8;
#alfa = 0.85;  alfa = 0.9;   alfa = 0.95
#alfa = 0.99
#e) P[X<0]
#f) P[-1<X<1]
# - - - - - - -  - - - - - - -  - - - - - - - 
#SOLUCIONES:
# - - - - - - -  - - - - - - -  - - - - - - - 
#a) P[X>3]
# - - - - - - -  - - - - - - -  - - - - - - - 
x0 <- 3
mu <- 2;   desvest <- 1/3;
z0 <- (x0-mu)/desvest; print(z0)
probabilidad <- pnorm(z0,0,1);  print(probabilidad)
probbuscada  <- 1-probabilidad; print(probbuscada)
# - - - - - - -  - - - - - - -  - - - - - - - 
#b) P[2<X<3]
# - - - - - - -  - - - - - - -  - - - - - - - 
mu <- 2;   desvest <- 1/3;
x1 <- 2;   
z1 <- (x1-mu)/desvest; print(z1)
p1 <- pnorm(z1,0,1);   print(p1)
x2 <- 3;
z2 <- (x2-mu)/desvest; print(z2)
p2 <- pnorm(z2,0,1);   print(p2)
probbuscada <- p2-p1;  print(probbuscada)
# - - - - - - -  - - - - - - -  - - - - - - - 
#e) P[X<0]
# - - - - - - -  - - - - - - -  - - - - - - - 
x3 <- 0
mu <- 2;   desvest <- 1/3;
z3 <- (x3-mu)/desvest; print(z3)
probabilidad <- pnorm(z3,0,1);  print(probabilidad)
# - - - - - - -  - - - - - - -  - - - - - - - 
#f) P[-1<X<1]
# - - - - - - -  - - - - - - -  - - - - - - - 
mu <- 2;   desvest <- 1/3;
x4 <- -1;   
z4 <- (x4-mu)/desvest; print(z4)
p4 <- pnorm(z4,0,1);   print(p4)
x5 <- 1;
z5 <- (x5-mu)/desvest; print(z5)
p5 <- pnorm(z5,0,1);   print(p5)
probbuscada <- p5-p4;  print(probbuscada)
# - - - - - - -  - - - - - - -  - - - - - - - 
#c) Encontrar x0 tal que P[X>x0]= 0.45
# P[X>x*]= p*, entonces buscamos en tablas el valor x*, tal que 
# P[X<x*] = q*= 1-p*, esto se hace con la funcion qnorm(q*,0,1)
pestrella <- 0.45; qestrella = 1-pestrella; print(qestrella)
zestrella <- qnorm(qestrella,0,1); print(zestrella)
#corroboramos
pnorm(zestrella,0,1) # s? es la respuesta correcta!!!!
mu <- 2;   desvest <- 1/3;
xestrella <- mu+desvest*zestrella; print(xestrella)
# corroboramos
pnorm(xestrella,2,1/3)
# - - - - - - -  - - - - - - -  - - - - - - - 
#d) Encontrar x* tal que P[X<x*]= alfa
#i)   alfa = 0.5;   ii) alfa = 0.75;  iii) alfa = 0.8;
#iv)  alfa = 0.85;   v) alfa = 0.9;    vi) alfa = 0.95
#vii) alfa = 0.99
mu <- 2;   desvest <- 1/3;


# << >> << >> << >> << >> << >> << >> << >> << >> << >> << >>
mu <- 2;   desvest <- 1/3;
# << >> << >> << >> << >> << >> << >> << >> << >> << >> << >>
alfa1 <- 0.5;
zest1 <- qnorm(alfa1,0,1); print(zest1)
xest1 <- mu+desvest*zest1; print(xest1)
# ----------------------------- ----------------------------- ------------------
alfa2 <- 0.75;
zest2 <- qnorm(alfa2,0,1); print(zest2)
xest2 <- mu+desvest*zest2; print(xest2)
# ----------------------------- ----------------------------- ------------------
alfa3 <- 0.8;
zest3 <- qnorm(alfa3,0,1); print(zest3)
xest3 <- mu+desvest*zest3; print(xest3)
# ----------------------------- ----------------------------- ------------------
alfa4 <- 0.85;
zest4 <- qnorm(alfa4,0,1); print(zest4)
xest4 <- mu+desvest*zest4; print(xest4)
# ----------------------------- ----------------------------- ------------------
alfa5 <- 0.9;
zest5 <- qnorm(alfa5,0,1); print(zest5)
xest5 <- mu+desvest*zest5; print(xest5)
# ----------------------------- ----------------------------- ------------------
alfa6 <- 0.95;
zest6 <- qnorm(alfa6,0,1); print(zest6)
xest6 <- mu+desvest*zest6; print(xest6)
# ----------------------------- ----------------------------- ------------------
alfa7 <- 0.99;
zest7 <- qnorm(alfa7,0,1); print(zest7)
xest7 <- mu+desvest*zest7; print(xest7)
# ----------------------------- ----------------------------- ------------------
Resultados <- matrix(0,7,3)
Resultados[,1] <- c(alfa1, alfa2, alfa3, alfa4, alfa5, alfa6, alfa7)
Resultados[,2] <- c(zest1,zest2,zest3,zest4,zest5,zest6,zest7); print(Resultados)
Resultados[,3] <- c(xest1,xest2,xest3,xest4,xest5,xest6,xest7); print(Resultados)
colnames(Resultados) <- c('Confianza','z*','x*'); print(Resultados)
rownames(Resultados) <- c(1,2,3,4,5,6,7); print(Resultados)
# ----------------------------- ----------------------------- ------------------
mu <- 2;   desvest <- 1/3;
# << >> << >> << >> << >> << >> << >> << >> << >> << >> << >>
alfa1 <- 0.75;
zest1 <- qnorm(alfa1,0,1); print(zest1)
xest1 <- mu+desvest*zest1; print(xest1)
# ----------------------------- ----------------------------- ------------------
alfa2 <- 0.875;
zest2 <- qnorm(alfa2,0,1); print(zest2)
xest2 <- mu+desvest*zest2; print(xest2)
# ----------------------------- ----------------------------- ------------------
alfa3 <- 0.9;
zest3 <- qnorm(alfa3,0,1); print(zest3)
xest3 <- mu+desvest*zest3; print(xest3)
# ----------------------------- ----------------------------- ------------------
alfa4 <- 0.925;
zest4 <- qnorm(alfa4,0,1); print(zest4)
xest4 <- mu+desvest*zest4; print(xest4)
# ----------------------------- ----------------------------- ------------------
alfa5 <- 0.95;
zest5 <- qnorm(alfa5,0,1); print(zest5)
xest5 <- mu+desvest*zest5; print(xest5)
# ----------------------------- ----------------------------- ------------------
alfa6 <- 0.975;
zest6 <- qnorm(alfa6,0,1); print(zest6)
xest6 <- mu+desvest*zest6; print(xest6)
# ----------------------------- ----------------------------- ------------------
alfa7 <- 0.995;
zest7 <- qnorm(alfa7,0,1); print(zest7)
xest7 <- mu+desvest*zest7; print(xest7)
# ----------------------------- ----------------------------- ------------------
Resultados <- matrix(0,7,5)
Resultados[,1] <- c(alfa1, alfa2, alfa3, alfa4, alfa5, alfa6, alfa7); print(Resultados)
Resultados[,3] <- c(zest1,zest2,zest3,zest4,zest5,zest6,zest7); print(Resultados)
Resultados[,2] <- -Resultados[,3]
Resultados[,5] <- c(xest1,xest2,xest3,xest4,xest5,xest6,xest7); print(Resultados)
Resultados[,4] <- -Resultados[,5]
colnames(Resultados) <- c('Confianza','*-z*','z*','-x*','x*'); print(Resultados)
rownames(Resultados) <- c(1,2,3,4,5,6,7); print(Resultados)
# ----------------------------- ----------------------------- ------------------
indices <- c(1,3)
renglones <- c(5,6,7)
ValoresZ <- Resultados[renglones,indices]
print(ValoresZ)
rownames(ValoresZ) <- c('90%','95%','99%')
print(ValoresZ)
# ----------------------------- ----------------------------- ------------------
# Mini rutina para calcular intervalos de confianza para 
# una media, para muestras grandes
# Ejercicio 1
DesvEst <- 8
n <- 100
xbarra <- 170
alpha <- 90; Zalpha1 <- 1.64
alpha <- 95; Zalpha2 <- 1.96
alpha <- 99; Zalpha3 <- 2.57

mu1 <- xbarra - Zalpha2*(DesvEst/sqrt(n)); print(mu1)
mu2 <- xbarra + Zalpha2*(DesvEst/sqrt(n)); print(mu2)

Intervalo <- paste0("mu ∈  ","(",mu1,",",mu2,")"); 
print(Intervalo)

# Ejercicio 2

DesvEst <- 10
n <- 50
xbarra <- 400
alpha <- 90; Zalpha1 <- 1.64
alpha <- 95; Zalpha2 <- 1.96
alpha <- 99; Zalpha3 <- 2.57

mu1 <- xbarra - Zalpha1*(DesvEst/sqrt(n)); print(mu1)
mu2 <- xbarra + Zalpha1*(DesvEst/sqrt(n)); print(mu2)

Intervalo <- paste0("mu ∈  ","(",mu1,",",mu2,")"); 
print(Intervalo)


Intervalo <- paste0("mu ∈  ","(",round(mu1,2),",",round(mu2,2),")"); 
print(Intervalo)


# ejercicio 3
# Intervalo de confianza del   95
# para la media de las alturas de una población de   2000
# estudiantes universitarios:
#  Media muestral : 170 cm
# Desviación estándar muestral s: 10 cm

DesvEst <- 10
n <- 2000
xbarra <- 170
alpha <- 90; Zalpha1 <- 1.64
alpha <- 95; Zalpha2 <- 1.96
alpha <- 99; Zalpha3 <- 2.57

mu1 <- xbarra - Zalpha2*(DesvEst/sqrt(n)); print(mu1)
mu2 <- xbarra + Zalpha2*(DesvEst/sqrt(n)); print(mu2)

Intervalo <- paste0("mu ∈  ","(",round(mu1,2),",",round(mu2,2),")"); 
print(Intervalo)


# ------------------------------------------------------------
#tamaño de la muestra (n): 10
#Media muestral ( xˉ): 170 cm
#Desviación estándar muestral ( s): 8 cm
#Nivel de confianza:  95
n <- 10
xbarra <- 170
DesvEst <- 8
gl <- n-1
nc <- 0.975

Talpha2 <- round(qt(nc, gl),4)
mu1 <- xbarra - Talpha2*(DesvEst/sqrt(n)); 
mu2 <- xbarra + Talpha2*(DesvEst/sqrt(n)); 
Intervalo <- paste0("mu ∈  ","(",round(mu1,2),",",
                    round(mu2,2),")"); 
print(Intervalo)

# ------------------------------------------------------------
#tamaño de la muestra (n): 10
#Media muestral ( xˉ): 170 cm
#Desviación estándar muestral ( s): 8 cm
#Nivel de confianza:  95
n <- 15
xbarra <- 3.5
DesvEst <- 0.5
gl <- n-1
nc <- 0.95

Talpha2 <- round(qt(nc, gl),4)
mu1 <- xbarra - Talpha2*(DesvEst/sqrt(n)); 
mu2 <- xbarra + Talpha2*(DesvEst/sqrt(n)); 
Intervalo <- paste0("mu ∈  ","(",round(mu1,2),",",round(mu2,2),")"); 
print(Intervalo)
























































