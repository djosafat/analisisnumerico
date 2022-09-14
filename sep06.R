f <- function(x){x-1}
curve(f,0,2,lwd=6,col='magenta',asp=1)
abline(h=0,v=0)
set.seed(2022)
candidatos <- runif(10,0,2) #proponemos algunos valores del dominio
candidatos
candidatos <- sort(candidatos)
candidatos
points(candidatos,rep(0,10),pch=16)
y <- f(candidatos)
points(candidatos,y,pch=16,cex=1.2,col=3)
segments(candidatos,rep(0,10),candidatos,y,lty = 2)
################################################################
y[1:9]*y[2:10]<0
indice_de_cambio <- which(y[1:9]*y[2:10]<0)
#x_izq <- candidatos[indice_de_cambio[1]]
#x_der <- candidatos[indice_de_cambio[1]+1]
x_izq <- 1/2
x_der <- 1.9
##############
curve(f,x_izq,x_der,lwd=6,col=4)
abline(h=0,v=0)
##############
x_aux <- (x_izq+x_der)/2 # primer aproximación a la raíz
nuevos <- c(x_izq,x_aux,x_der)
y <- f(nuevos)
points(nuevos,y,pch=16,cex=1.5,col=3)
segments(nuevos,rep(0,3),nuevos,y,lty = 2)
################################################################
y[1:2]*y[2:3]<0
indice_de_cambio <- which(y[1:2]*y[2:3]<0)
x_izq <- nuevos[indice_de_cambio]
x_der <- nuevos[indice_de_cambio+1]
##############
curve(f,x_izq,x_der,lwd=6,col=4)
abline(h=0,v=0)
##############
x_aux <- (x_izq+x_der)/2 #Segunda aproximación
nuevos <- c(x_izq,x_aux,x_der)
y <- f(nuevos)
points(nuevos,y,pch=16,cex=1.5,col=3)
segments(nuevos,rep(0,3),nuevos,y,lty = 2)
################################################################
y[1:2]*y[2:3]<0
indice_de_cambio <- which(y[1:2]*y[2:3]<0)
x_izq <- nuevos[indice_de_cambio]
x_der <- nuevos[indice_de_cambio+1]
##############
curve(f,x_izq,x_der,lwd=6,col=4)
abline(h=0,v=0)
##############
x_aux <- (x_izq+x_der)/2
nuevos <- c(x_izq,x_aux,x_der)
y <- f(nuevos)
points(nuevos,y,pch=16,cex=1.5,col=3)
segments(nuevos,rep(0,3),nuevos,y,lty = 2)
################################################################
y[1:2]*y[2:3]<0
indice_de_cambio <- which(y[1:2]*y[2:3]<0)
x_izq <- nuevos[indice_de_cambio]
x_der <- nuevos[indice_de_cambio+1]
##############
curve(f,x_izq,x_der,lwd=6,col=4)
abline(h=0,v=0)
##############
x_aux <- (x_izq+x_der)/2
nuevos <- c(x_izq,x_aux,x_der)
y <- f(nuevos)
points(nuevos,y,pch=16,cex=1.5,col=3)
segments(nuevos,rep(0,3),nuevos,y,lty = 2)
################################################################
y[1:2]*y[2:3]<0
indice_de_cambio <- which(y[1:2]*y[2:3]<0)
x_izq <- nuevos[indice_de_cambio]
x_der <- nuevos[indice_de_cambio+1]
##############
curve(f,x_izq,x_der,lwd=6,col=4)
abline(h=0,v=0)
##############
x_aux <- (x_izq+x_der)/2
nuevos <- c(x_izq,x_aux,x_der)
y <- f(nuevos)
points(nuevos,y,pch=16,cex=1.5,col=3)
segments(nuevos,rep(0,3),nuevos,y,lty = 2)
################################################################
y[1:2]*y[2:3]<0
indice_de_cambio <- which(y[1:2]*y[2:3]<0)
x_izq <- nuevos[indice_de_cambio]
x_der <- nuevos[indice_de_cambio+1]
##############
curve(f,x_izq,x_der,lwd=6,col=4)
abline(h=0,v=0)
##############
x_aux <- (x_izq+x_der)/2
nuevos <- c(x_izq,x_aux,x_der)
y <- f(nuevos)
points(nuevos,y,pch=16,cex=1.5,col=3)
segments(nuevos,rep(0,3),nuevos,y,lty = 2)
################################################################
y[1:2]*y[2:3]<0
indice_de_cambio <- which(y[1:2]*y[2:3]<0)
x_izq <- nuevos[indice_de_cambio]
x_der <- nuevos[indice_de_cambio+1]
##############
curve(f,x_izq,x_der,lwd=6,col=4)
abline(h=0,v=0)
##############
x_aux <- (x_izq+x_der)/2
nuevos <- c(x_izq,x_aux,x_der)
y <- f(nuevos)
points(nuevos,y,pch=16,cex=1.5,col=3)
segments(nuevos,rep(0,3),nuevos,y,lty = 2)
################################################################
y[1:2]*y[2:3]<0
indice_de_cambio <- which(y[1:2]*y[2:3]<0)
x_izq <- nuevos[indice_de_cambio]
x_der <- nuevos[indice_de_cambio+1]
##############
curve(f,x_izq,x_der,lwd=6,col=4)
abline(h=0,v=0)
##############
x_aux <- (x_izq+x_der)/2
nuevos <- c(x_izq,x_aux,x_der)
y <- f(nuevos)
points(nuevos,y,pch=16,cex=1.5,col=3)
segments(nuevos,rep(0,3),nuevos,y,lty = 2)
################################################################
y[1:2]*y[2:3]<0
indice_de_cambio <- which(y[1:2]*y[2:3]<0)
x_izq <- nuevos[indice_de_cambio]
x_der <- nuevos[indice_de_cambio+1]
##############
curve(f,x_izq,x_der,lwd=6,col=4)
abline(h=0,v=0)
##############
x_aux <- (x_izq+x_der)/2
nuevos <- c(x_izq,x_aux,x_der)
y <- f(nuevos)
points(nuevos,y,pch=16,cex=1.5,col=3)
segments(nuevos,rep(0,3),nuevos,y,lty = 2)
################################################################
#  etc ...

##########################################
##########################################
# Automatización
##########################################
f <- function(x){x-1/4}
curve(f,-1,1,lwd=6,col=4)
abline(h=0,v=0)
set.seed(2022)
candidatos <- runif(10,-1,1) #proponemos algunos valores del dominio
candidatos
candidatos <- sort(candidatos)
candidatos
points(candidatos,rep(0,10),pch=16)
######################################
m <- 60 #número de iteraciones (repeticiones) >=2
z <- numeric(m) #para almacenar las aproximaciones
######################################
y <- f(candidatos)
points(candidatos,y,pch=16,cex=1.5,col=3)
segments(candidatos,rep(0,10),candidatos,y,lty = 2)
################################################################
y[1:9]*y[2:10]<0
indice_de_cambio <- which(y[1:9]*y[2:10]<0)
x_izq <- candidatos[indice_de_cambio[1]]
x_der <- candidatos[indice_de_cambio[1]+1]
x_aux <- (x_izq+x_der)/2
##############
k <- 1 #contador de iteraciones (repeticiones)
z[k] <- x_aux
sigue <- TRUE #para arrancar la instrucción while
while(sigue){
    k <- k + 1 #se aumenta una unidad a k cada iteración.
    nuevos <- c(x_izq,x_aux,x_der)
    y <- f(nuevos)
    ################################################################
    y[1:2]*y[2:3]<0
    indice_de_cambio <- which(y[1:2]*y[2:3]<0)
    x_izq <- nuevos[indice_de_cambio]
    x_der <- nuevos[indice_de_cambio+1]
    x_aux <- (x_izq+x_der)/2
    z[k] <- x_aux
    if( f(x_aux)==0 | k==m ){sigue <- FALSE}
    # sigue <- FALSE # para finalizar la instrucción while.
}
z[1:k]
f(z[1:k])
