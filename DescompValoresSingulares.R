m <- sample(1:6,1)
n <- sample(1:6,1)
A <- matrix(sample((-100):100,m*n),m,n)
# A <- matrix(c(-76,60,14,-55,40,75,9,64,47,-81),2,5)
# A <- matrix(c(4,8,11,7,14,-2),2,3)
mn <- dim(A)
m <- mn[1]
t(A)
#A%*%t(A)
B <- t(A)%*%A
# Cálculo de valores y vectores propios
eigenB <- eigen(B)
lambda <- round(eigenB$values,4)
###  Los valores propios ya están en orden decreciente
V_lambda <- round(eigenB$vectors,4)
###  Los vectores propios ya están normalizados
r <- sum(lambda>0)
sigma <- sqrt(lambda[1:r])  # quito el valor propio igual a cero
############################
Sigma_r <- diag(sigma)
############################
V <- eigenB$vectors   # round(V%*%t(V),4)  # Igual a matrix identidad
V_r <- V[,1:r]
############################
U_r <- matrix(0,m,r) 
for(j in 1:r){
    U_r[,j] <- (1/sigma[j])*A%*%V[,j]
}
##########################################
##Descomposición en valores singulares: ##
U_r; Sigma_r; t(V_r)
##########################################
##Comprobación:
U_r%*%Sigma_r%*%t(V_r)
################
A
#############################################
