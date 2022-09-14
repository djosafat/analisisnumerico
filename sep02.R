# Gráfico
curve(x**2,0,2,lwd=6,col=2)
# Gráfico con misma escala en los ejes
curve(x**2,0,2,lwd=6,col=2,asp=1)
# Agregamos ejes y línea de referencia
abline(h=0,v=c(0,2))
# Valor del área bajo la curva
f <- function(x){x**2}
ValorIntegral <- integrate(f,0,2)
ValorIntegral
####################################
####################################
# Aproximación usando rectángulos 
x <- seq(0,2,1)
y <- x**2
x
y
####
rect(x[1],y[1],x[2],y[2],col=rainbow(1,alpha = 0.3))
rect(x[2],y[1],x[3],y[3],col=rainbow(1,alpha = 0.3))
A1 <- (x[2]-x[1])*y[2]
A2 <- (x[3]-x[2])*y[3]
A <- A1+A2
####
rect(x[1],y[1],x[2],y[1],border = 'blue',col=rainbow(2,alpha = 0.3)[2])
rect(x[2],y[1],x[3],y[2],border = 'blue',col=rainbow(2,alpha = 0.3)[2])
a1 <- (x[2]-x[1])*y[1]
a2 <- (x[3]-x[2])*y[2]
a <- a1+a2

################################
# Automatización
# Gráfico
curve(x**2,0,2,lwd=6,col=2,asp=1)
#
abline(h=0,v=c(0,2))
####################################
particiones <- 4 #al menos debe ser 2
delta <- 2/particiones 
x <- seq(0,2,by=delta)
y <- x**2
ymax <- y[2:(particiones+1)]
ymin <- y[1:particiones]
####
A <- numeric(particiones)
for(i in 1:particiones){
    rect(x[i],y[1],x[i+1],ymax[i],col=rainbow(1,alpha = 0.3))
    A[i] <- (x[i+1]-x[i])*ymax[i]
}
sum(A) # Área total
# Nos ahorramos:
A1 <- (x[2]-x[1])*y[2]
A2 <- (x[3]-x[2])*y[3]
A3 <- (x[4]-x[3])*y[4]
A4 <- (x[5]-x[4])*y[5]
A <- A1+A2+A3+A4
A
########################
a <- numeric(particiones)
for(i in 1:particiones){
    rect(x[i],y[1],x[i+1],ymin[i],border = 'blue',col=rainbow(2,alpha = 0.3)[2])
    a[i] <- (x[i+1]-x[i])*ymin[i]
}
sum(a)
# Nos ahorramos:
a1 <- (x[2]-x[1])*y[1]
a2 <- (x[3]-x[2])*y[2]
a3 <- (x[4]-x[3])*y[3]
a4 <- (x[5]-x[4])*y[4]
a <- a1+a2+a3+a4
a

#############################################
# Más automatizado aún
# Gráfico
curve(x**2,0,2,lwd=6,col=2,asp=1)
#
abline(h=0,v=c(0,2))
####################################
####################################
particiones <- 400 #al menos debe ser 2
delta <- 2/particiones 
x <- seq(0,2,by=delta)
y <- x**2
ymax <- y[2:(particiones+1)]
ymin <- y[1:particiones]
####
A <- numeric(particiones)
a <- numeric(particiones)
for(i in 1:particiones){
    rect(x[i],y[1],x[i+1],ymax[i],col=rainbow(1,alpha = 0.3))
    A[i] <- (x[i+1]-x[i])*ymax[i]
    rect(x[i],y[1],x[i+1],ymin[i],border = 'blue',col=rainbow(2,alpha = 0.3)[2])
    a[i] <- (x[i+1]-x[i])*ymin[i]
}
sum(A)
sum(a)
sum(A)-sum(a)
(sum(A)+sum(a))/2
ValorIntegral
