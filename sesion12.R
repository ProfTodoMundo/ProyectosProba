# Varianzas conocidas e iguales
DesvEst <- 0.8
n1 <- 50
n2 <- 50
xbarra1 <- 3.5
xbarra2 <- 4.2
xbarra <- xbarra1-xbarra2
alpha <- 90; Zalpha1 <- 1.64
alpha <- 95; Zalpha2 <- 1.96
alpha <- 99; Zalpha3 <- 2.57

nc <- Zalpha2
ErrorStd <- DesvEst*sqrt((n1+n2)/(n1*n2))
ErrorStd <- DesvEst*sqrt((1/n1)+(1/n2))

LimInf <- xbarra - nc*ErrorStd; print(LimInf)
LimSup <- xbarra + nc*ErrorStd; print(LimSup)

Intervalo <- paste0("mu ∈  ","(",round(LimInf,2),",",
                    round(LimSup,2),")"); 
print(Intervalo)
# ------------------------------------------------------------
# Varianzas conocidas y distintas


# ------------------------------------------------------------
# Varianzas desconocidas e iguales
n1 <- 10
n2 <- 12
xbarra1 <- 6.5
xbarra2 <- 5.8
S1 <- 0.7
S2 <- 0.9
xbarra <- xbarra1-xbarra2
nc <- 0.975
Sd <- sqrt(((n1-1)*S1^2+(n2-1)*S2^2)/(n1+n2-2))
gl = n1+n2-2
Talpha2 <- round(qt(nc, gl),4)
LimInf <- xbarra - Talpha2*(Sd); 
LimSup <- xbarra + Talpha2*(Sd); 
Intervalo <- paste0("mu ∈  ","(",
                    round(LimInf,2),
                    ",",
                    round(LimSup,2),
                    ")"); 
print(Intervalo)



# ------------------------------------------------------------
# Varianzas desconocidas y distintas
# Se quiere comparar la efectividad de dos medicamentos para el dolor de cabeza. 
# Se toma una muestra de 10 pacientes para el medicamento A y otra 
# muestra de 12 pacientes para el medicamento B. Los resultados muestran que la 
# media del medicamento A es de 6.5 y la media del medicamento B es de 5.8. 
# Las desviaciones estándar son de 0.7 para el medicamento A y
# 0.9 para el medicamento B. 
#Construya un intervalo de confianza del 95% para la diferencia de medias.

n1 <- 10
n2 <- 12
xbarra1 <- 6.5
xbarra2 <- 5.8
S1 <- 0.7
S2 <- 0.9
xbarra <- xbarra1-xbarra2
nc <- 0.975
Sd <- sqrt(((S1^2)/n1)+((S2^2)/n2))

numerador <- (((S1^2)/n1)+((S2^2)/n2))^2
denominador <- ((((S1^2)/n1)^2)/(n1-1))+((((S2^2)/n2)^2)/(n2-1))
gl = ceiling(numerador/denominador)
Talpha2 <- round(qt(nc, gl),4)

LimInf <- xbarra - Talpha2*(Sd); 
LimSup <- xbarra + Talpha2*(Sd); 
Intervalo <- paste0("mu ∈  ","(",
                    round(LimInf,2),
                    ",",
                    round(LimSup,2),
                    ")"); 
print(Intervalo)


















