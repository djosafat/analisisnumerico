newton <- function(x0,n){
    k <- 0
    repeat{
        k <- k + 1
        x0 <- x0 - f(x0)/fprima(x0)   
        if(k==n){break}
    }
    return(x0)
}

#ejemplo
f <- function(x){x**2-3}
fprima <- function(x){2*x}
newton(0.5,10)
sqrt(3)
newton(0.5,10)==sqrt(3)
print(newton(0.5,10),digits = 22)
print(sqrt(3),digits = 22)


newton2 <- function(x1,epsilon){
    x0 <- x1
    x1 <- x1 - f(x1)/fprima(x1)
    k <- 1
    while(abs(x1-x0)>=epsilon){
        x0 <- x1
        x1 <- x1 - f(x1)/fprima(x1)
        k <- k + 1
    }
    cat('z aprox ',x1, 'con ',k,'iteraciones\n')
}


#ejemplo
f <- function(x){x**2-3}
fprima <- function(x){2*x}
newton2(0.5,0.0001)
sqrt(3)

?options
options(digits = 22)
newton2(2,0.01)
sqrt(3)
