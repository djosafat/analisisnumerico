##########################################
# Automatización del método de bisección
##########################################
f <- function(x){-x^2+x+1/2} #Aquí se define la función objetivo
elcolor <- 'red'
a <- 0; b <- 2
curve(f,a,b,lwd=6,col=elcolor)
abline(h=0,v=0)
##################################################################
set.seed(2022)
candidatos <- runif(10,a,b) #proponemos algunos valores del dominio
candidatos
candidatos <- sort(candidatos)
candidatos
y <- f(candidatos)
points(candidatos,rep(0,10),pch=16)
points(candidatos,y,pch=16,cex=1.5)
segments(candidatos,rep(0,10),candidatos,y,lty = 2)
################################################################
y[1:9]*y[2:10]<0 # para detectar el cambio de signo
indice_de_cambio <- which(y[1:9]*y[2:10]<0) # índice del cambio
x_izq <- candidatos[indice_de_cambio[1]]
x_der <- candidatos[indice_de_cambio[1]+1]
x_aux <- (x_izq+x_der)/2 #primera aproximación
curve(f,x_izq,x_der,lwd=6,col=elcolor)
abline(h=0)
points(x_aux,f(x_aux),pch=16,cex=1.5,col='blue')
segments(x_aux,0,x_aux,f(x_aux),lty=2)
######################################
m <- 10 #número de iteraciones (repeticiones) >=2
z <- numeric(m) #para almacenar las aproximaciones
k <- 1 #contador de iteraciones (repeticiones)
z[1] <- x_aux 
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
    points(x_aux,f(x_aux),pch=16,cex=1.5,col='yellow')
    segments(x_aux,0,x_aux,f(x_aux),lty=2)
    if( f(x_aux)==0 | k==m ){sigue <- FALSE}
    # sigue <- FALSE # para finalizar la instrucción while.
}

z[1:k] # algunas ocasiones k será menor a el valor de m.
f(z[1:k])
points(z[k],f(z[k]),pch=16,cex=1.5)
z[k] #raíz aproximada

#################################################
x <- 0
repeat{
    x <- x + 1
    if(x==20){break}
}
#
x <- 0
for(i in 1:20){
    x <- x + 1
}
#################################################

##########################################################################
## Función aproxima_raiz() necesita que ya haya
## sido definida la función objetivo f(x)
aproxima_raiz <- function(a,b,m){
    set.seed(123)
    candi <- sort(runif(10,a,b)) #proponemos algunos valores del dominio
    y <- f(candi)
    ic <- which(y[1:9]*y[2:10]<0) #índice de cambio
    x_aux <- (candi[ic]+candi[ic+1])/2
    k <- 1
    while(f(x_aux)!=0 & k<m){ #sigue si aún no has encontrado la raíz y te faltan iteraciones
        k <- k + 1 #se aumenta una unidad a k cada iteración.
        candi <- c(candi[ic],x_aux,candi[ic+1])
        y <- f(candi)
        ic <- which(y[1:2]*y[2:3]<0)
        x_aux <- (candi[ic]+candi[ic+1])/2
    }
    return(c(x_aux,f(x_aux)))
}
#Ejemplo
f <- function(x){exp(x)-2}#Aquí se define la función objetivo
# curve(f,0,1); abline(h=0)
## 
aproxima_raiz(0,1,10)
##
aproxima_raiz(0,1,20)[1]  # sólo la raíz
# Valor real de la raíz del ejemplo:
log(2)
##
aproxima_raiz(0,1,20)[1]  # sólo la raíz
# Valor real de la raíz del ejemplo:
log(2)
format(log(2), digits = 10)
as.numeric(format(log(2), digits = 10))
format(log(2), digits = 10)
format(aproxima_raiz(0,1,20)[1], digits = 10)

