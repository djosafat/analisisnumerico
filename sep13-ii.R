f <- function(x){x^3-2*x^2+1}
a <- 0.77
b <- 1.45
f(a)*f(b)<0
curve(f,a,b,lwd=5); abline(h=0,v=0)
p <- uniroot(f,c(a,b))$root  # raíz con error numérico
p <- 1 #raíz exacta
############################3
N <- 7
p_star <- numeric(N)
distancia <- numeric(N)
epsilon <- (b-a)/(2^(1:N))
###########################################
for(i in 1:N){
    p_star[i] <- (a+b)/2
    distancia[i] <- abs(p-p_star[i])
    if(f(p_star[i])*f(a)>0){a <- p_star[i]}else{b <- p_star[i]}
    rect(a,0,b,(b-a),col=rainbow(N,alpha = 0.5)[i])
}
p_star
distancia
epsilon
distancia < epsilon
text(p_star,rep(-0.05,N),paste('p',1:N),col=4,cex=0.75)

##############################
##############################
f <- function(x){x^3-2*x^2-x/4+1}
a <- -1  #a <- 0      #a <- 1.5
b <- 2   #b <- 1.7    #b <- 2
f(a)*f(b)<0
curve(f,a,b,lwd=5); abline(h=0,v=0)
#curve(f,a,b,lwd=5,ylim=c(-0.5,1)); abline(h=0,v=0)
p <- uniroot(f,c(a,b))$root
f(p)
############################
N <- 7
p_star <- numeric(N)
distancia <- numeric(N)
epsilon <- (b-a)/(2^(1:N))
###########################################
for(i in 1:N){
    p_star[i] <- (a+b)/2
    distancia[i] <- abs(p-p_star[i])
    if(f(p_star[i])*f(a)>0){a <- p_star[i]}else{b <- p_star[i]}
    rect(a,0,b,(b-a),col=rainbow(N,alpha = 0.5)[i])
}
p_star 
distancia
epsilon
distancia < epsilon
text(p_star,rep(-0.05,N),paste('p',1:N),col=4,cex=0.75)


