#---- Preparamos todo ----
rm(list = ls())
cat("\014")
dev.off()
#---- Lectura de los datos ----
kgs <- c(100.5, 91.3, 60.4, 83.2, 49.6, 103.1, 60.3, 109.3, 83.5,
         63.9, 106, 50, 47, 89.7, 108.5, 78.9, 82.7, 60.7, 98.7, 85.2, 48.7, 106.7, 63.9, 84.1,
         69.5, 53.3, 108.9, 91.8, 108.6, 54.5, 95.1, 90.6, 115.9, 88.5, 67.7, 115.1, 108.3, 76.8,
         81.4, 102.6, 63.9, 105.9, 106.7, 76.3, 113.7, 50.3, 105.8, 81.4, 67.9, 91.3, 68.9, 93.9,
         113.7, 87.7, 92.8, 76.2, 104.7, 109.7, 72.6, 81.6, 112.2, 79.8, 60.7, 95.7, 100.1, 94,
         60.5, 117.1, 45.5, 112.7, 51.7, 107.8, 86.6, 90.3, 105.9, 64.7, 48, 55.4, 52.9, 58.2,
         117.1, 59.6, 69.9, 96.9, 97, 66.5, 67.4, 77.2, 73.7, 113)
#---- Procesamiento de los datos ----
summary(kgs)
kgs_ord <- sort(kgs)
cte <- 9
indices <- 1:10; (indices)
t <- cte*indices; (t)
mis_deciles <- kgs_ord[t]; (mis_deciles)
cuantiles <- quantile(kgs); (cuantiles)
Q1 <- cuantiles[2]; (Q1)
Q2 <- cuantiles[3]; (Q2)
Q3 <- cuantiles[4]; (Q3)
x_media <- mean(kgs)
mi_min <- min(kgs); 
mi_max <- max(kgs);
#---- Grafica: Plot -----
##---- Instruccion simple ----
plot(kgs,main="Grafica de peso en kilogramos")
##---- Grafica mas elaborada ----
plot(
  kgs,
  main = "Gráfica de peso en kilogramos",
  xlab = "Índice del individuo",
  ylab = "Peso (kg)",
  col = "blue",       
  pch = 19,           
  cex = 0.5,          
  cex.main = 1.5,     
  cex.lab = 1.2,      
  col.main = "darkred")
legend(
  "topright",                  # ubicación
  legend = c(mi_min,mi_max),    # etiquetas
  pch = 17,
  col=c("purple","orange"),
  pt.cex = 1.5,                # tamaño de los símbolos
  cex = 0.7,
  bty = "n"                    # sin borde en la caja
)
abline(h = x_media, col = "red", lwd = 1, lty = 2)     
abline(h = Q2, col = "lightgreen", lwd = 1, lty = 3) 
abline(h = mis_deciles, col = "lightgray", lty = 1.5)
points(which.min(kgs), min(kgs), col = "purple", pch = 17, cex = 1.5)
points(which.max(kgs), max(kgs), col = "orange", pch = 17, cex = 1.5)
#---- Grafica de histograma ----
nbreaks=10;
miscolores <- rainbow(25,0.85);
h <- hist(kgs, breaks = nbreaks, col= miscolores, 
          main = 'Peso en kilos',
          xlab="Peso",
          ylab="Frecuencias")
text(h$mids, h$counts, labels = h$counts, pos = 3, cex = 0.4, 
     col = "black")
nl <- length(h$counts); (nl)
legend(
  "topright",                  # ubicación
  legend = h$counts,    # etiquetas
  fill = miscolores,
  col = miscolores,    # colores (uno por cada símbolo)
  pt.cex = 1.5,                # tamaño de los símbolos
  cex = 0.7,
  bty = "n"                    # sin borde en la caja
)
##---- Mejora del grafico ----
h2 <- hist(kgs, breaks = nbreaks, col= miscolores, 
          main = 'Peso en kilos',
          xlab="Peso",
          ylab="Frecuencias",
          ylim=c(0,max(h$counts)*1.5))
text(h$mids, h$counts, labels = h$counts, pos = 3, cex = 0.8, 
     col = "black")
nl <- length(h$counts); (nl)
legend(
  "topright",                  # ubicación
  legend = h$counts,    # etiquetas
  fill = miscolores,
  col = miscolores,    # colores (uno por cada símbolo)
  pt.cex = 1.5,                # tamaño de los símbolos
  cex = 0.5,
  bty = "n"                    # sin borde en la caja
)
##---- Grafico con porcentajes ----
h3 <- hist(kgs, 
           breaks = nbreaks, 
           col= miscolores, 
           main = 'Peso en kilos',
           xlab="Peso",
           ylab="Porcentaje",
           ylim=c(0,max(h$density)*1.5),
           probability=TRUE)
porcentajes <- h3$counts/sum(h3$counts)*100

text(h3$mids, 
     h3$density, 
     labels = paste0(round(porcentajes,1),"%"),
     pos = 3, 
     cex = 0.8, 
     col = "black")

nl <- length(h3$counts); (nl)
legend(
  "topright",                  # ubicación
  legend = paste0(round(porcentajes,1),"%"),
  fill = miscolores,
  cex = 0.5,
  bty = "n"                    # sin borde en la caja
)
#---- Fin ----
