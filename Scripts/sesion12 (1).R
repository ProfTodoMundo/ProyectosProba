
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