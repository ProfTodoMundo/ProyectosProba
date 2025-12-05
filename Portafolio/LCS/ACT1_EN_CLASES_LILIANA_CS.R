#---- Procesamiento de datos----
##----Datos categoricos nominales----
Nacionalidad <-  c("Boliviana", "Peruana", "Venezolana", "Cubana", "Mexicana",
                   "Colombiana", "Colombiana", "Brasileña", "Colombiana", "Venezolana", "Chilena",
                   "Argentina", "Mexicana", "Mexicana", "Argentina", "Mexicana", "Argentina",
                   "Brasileña", "Mexicana", "Uruguaya", "Argentina", "Argentina", "Colombiana",
                   "Cubana", "Boliviana", "Peruana", "Boliviana", "Boliviana", "Peruana", "Uruguaya",
                   "Chilena", "Uruguaya", "Venezolana", "Uruguaya", "Argentina", "Venezolana",
                   "Uruguaya", "Cubana", "Venezolana", "Cubana", "Chilena", "Argentina", "Peruana",
                   "Boliviana", "Cubana", "Venezolana", "Colombiana", "Mexicana", "Uruguaya",
                   "Argentina")
(Nacionalidad)
sort(Nacionalidad)
table(Nacionalidad)
contador <- table(Nacionalidad)
barplot(contador)
barplot(contador, xlab="Frecuencias",
        ylab="Nacionalidad",
        main="Encuesta Nacionacionalidades", 
        col=rainbow(10))