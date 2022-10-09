## Función determinante
det_nxn <- function(A){ # A debe ser cuadrada
    m <- dim(A)[1]
    if(m==1){
        return(A[1,1])
    }else if(m==2){
        return(A[1,1]*A[2,2]-A[2,1]*A[1,2])
    }else{
        aux <- 0 #en aux se almacenará el determinante
        for(i in 1:m){
            menorA <- A[2:m,-i]
            aux <- aux + (-1)**(i+1)*A[1,i]*det_nxn(menorA)
        }
        return(aux)
    }
}
### Ejemplos
M <- matrix(2,1,1)
det_nxn(M)
#
M <- matrix(c(2,0,3,4),2,2)
det_nxn(M)
#
M <- matrix(c(2,0,3,0,1,5,3,-1,7),3,3)
M
det_nxn(M)
det(M) # función de base en R
##
M <- matrix(c(2,0,3,0,1,5,3,-1,7,4,2,3,1,6,4,-2),4,4)
M
det_nxn(M)
det(M) # función de base en R

##################################################################
M <- matrix(c(0,1,2,4,1,5,-1,-1,7),3,3)
M
det(M)!=0 # distinto a
det(M)
cof <- matrix(0,3,3)
cof[1,1] <- det(M[c(2,3),c(2,3)])
cof[1,2] <- -det(M[c(2,3),c(1,3)])
cof[1,3] <- det(M[c(2,3),c(1,2)])
cof[2,1] <- -det(M[c(1,3),c(2,3)])
cof[2,2] <- det(M[c(1,3),c(1,3)])
cof[2,3] <- -det(M[c(1,3),c(1,2)])
cof[3,1] <- det(M[c(1,2),c(2,3)])
cof[3,2] <- -det(M[c(1,2),c(1,3)])
cof[3,3] <- det(M[c(1,2),c(1,2)])
cof
#################################
cof2 <- matrix(0,3,3)
for(i in 1:3){
    for(j in 1:3){
        cof2[i,j] <- (-1)**(i+j)*det(M[-i,-j])
    }
}
cof2
###################################
M
M_inversa <- t(cof)/det(M)
M_inversa
round(M_inversa,2)
round(solve(M),2) # Función base de R

##########################
eigen(M)
#
eigen(M)$values
#
eigen(M)$vectors
#
D <- diag(eigen(M)$values)
round(D,2)
#
Q <- eigen(M)$vectors
#
Q_inv <- solve(Q)
#
M_diagonalizada <- Q%*%D%*%Q_inv
M_diagonalizada
Re(round(M_diagonalizada,2))
M
M%*%M%*%M%*%M%*%M%*%M%*%M%*%M%*%M    ### M^9
Re(Q%*%D^9%*%Q_inv)     ### Md^9
#################################

###################################
#  Función potencia de matrices:  #
###################################
"%**%" <- function(A,n){
    m <- dim(A)[1]
    Anew <- diag(1,m,m) # matriz identidad
    for(i in 1:n){
        Anew <- Anew%*%A
    }
    return(Anew)
}

M%**%9
Re(Q%*%D^9%*%Q_inv)
###################################################

M <- matrix(c(2,0,3,0,1,5,3,-1,7,4,2,3,1,6,4,-2),4,4)
M
D <- diag(eigen(M)$values)
Q <- eigen(M)$vectors
Q_inv <- solve(Q)
Q%*%D^5%*%Q_inv  ### M^5   
M%**%5
