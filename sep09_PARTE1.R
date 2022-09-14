f <- function(x){-x^2+x+1/2}#Aquí se define la función objetivo
# f <- function(x){-(x-1)^2+1/4}
a <- 0
b <- 2 
curve(f,a,b,lwd=6,col='magenta',asp=1)
abline(h=0,v=0)
set.seed(2022)  # semilla para reproducir los valores aleatorios
candidatos <- runif(10,a,b) #proponemos algunos valores del dominio
candidatos
candidatos <- sort(candidatos)
candidatos
points(candidatos,rep(0,10),pch=16)
y <- f(candidatos)
points(candidatos,y,pch=16,cex=1.2,col=4)
segments(candidatos,rep(0,10),candidatos,y,lty = 3)

################################################################
#   1< -2 ejemplo de sentencia lógica
y[1:9]*y[2:10]<0 # y[1]; y[c(5,7)]
indice_de_cambio <- which(y[1:9]*y[2:10]<0)
x_izq <- candidatos[indice_de_cambio[1]]
x_der <- candidatos[indice_de_cambio[1]+1]
#x_izq <- 1/2
#x_der <- 1.9
##############
curve(f,x_izq,x_der,lwd=6,col='magenta',asp=1)
abline(h=0,v=0)
##############
x_aux <- (x_izq+x_der)/2 # primer aproximación a la raíz
nuevos <- c(x_izq,x_aux,x_der) # nuevos candidatos
y <- f(nuevos)
points(nuevos,y,pch=16,cex=1.5,col=3)
segments(nuevos,rep(0,3),nuevos,y,lty = 2)

###  DESDE AQUÍ COMIENZA A REPETIRSE EL CÓDIGO   ###
indice_de_cambio <- which(y[1:2]*y[2:3]<0)
x_izq <- nuevos[indice_de_cambio]
x_der <- nuevos[indice_de_cambio+1]
##############
curve(f,x_izq,x_der,lwd=6,col='magenta',asp=1)
abline(h=0,v=0)
##############
x_aux <- (x_izq+x_der)/2 #Segunda aproximación
nuevos <- c(x_izq,x_aux,x_der)
y <- f(nuevos)
points(nuevos,y,pch=16,cex=1.5,col=3)
segments(nuevos,rep(0,3),nuevos,y,lty = 2)
###  HASTA AQUÍ TERMINA DE REPETIRSE EL CÓDIGO   ###
