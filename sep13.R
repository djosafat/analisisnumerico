#Para que funcione debe estar definida f(x)
m_bisec <- function(a,b,epsilon){ 
  x <- numeric(2); k <- 0; set.seed(123)
  while(!(prod(f(x)) < 0)){
    x <- sort(runif(2,a,b))
  }
  iniciales <- x
  while(!(x[2]-x[1] < epsilon)){
    z <- mean(x); k <- k + 1
    zcomox2 <- prod(f(c(x[1],z)))<0 # estudiar
    if(zcomox2){x[2] <- z}else{x[1] <- z} # estudiar
  }
  cat('raíz aprox =',z,
      '\nimagen',f(z),
      '\niteraciones =',k,
      '\nvalores iniciales ',iniciales)  
}

#Ejemplo
f <- function(x){x^2-2}
a <- 0; b <- 2; epsilon <- 0.000001
m_bisec(a,b,epsilon)

#######################################
#Para que funcione debe estar definida f(x)
#x es un vector de valores iniciales c(x1,x2)
m_bisec_2 <- function(x,epsilon){ 
  k <- 0
  while(!(x[2]-x[1] < epsilon)){
    z <- mean(x); k <- k + 1
    if(prod(f(c(x[1],z)))<0){x[2] <- z}else{x[1] <- z}
  }
  cat('raíz aprox =',z,'\nimagen',f(z),'\niteraciones =',k)  
}

#Ejemplo
f <- function(x){x^2-2}
m_bisec_2(c(0,2),0.0000001)

#Ejemplo
f <- function(x){x^3+4*x^2-10}
m_bisec_2(c(1,2),0.001)


#######################################
#Para que funcione debe estar definida f(x)
#x es un vector de valores iniciales c(x1,x2)
m_bisec_3 <- function(x,m){ 
    k <- 0
    repeat{
        k <- k + 1
        z <- mean(x)
        if(prod(f(c(x[1],z)))<0){x[2] <- z}else{x[1] <- z}
        if(k==m){break}
    }
    cat('raíz aprox =',z,'\nimagen',f(z),'\nerror = +-',(x[2]-x[1])/2)  
}

#Ejemplo
f <- function(x){x^3+4*x^2-10}
m_bisec_3(c(1,2),9)
