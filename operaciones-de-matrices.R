matrix(0,5,4)
matrix(c(0,1,2,3),5,4)
matrix(c(0,1,2,3),5,4,byrow = TRUE)
matrix(c(0,1,2,3),5,4,byrow = T)
?matrix
matrix(0,ncol=3,nrow=4)
##############################
matrix(c(15,-5,28,13))
matrix(c(15,-5,28,13),nrow = 2,ncol = 2)
matrix(c(15,-5,28,13),nrow = 4,ncol = 1)
matrix(c(15,-5,28,13),nrow = 1,ncol = 4)

#matrix(c(15,-5,28,13),nrow = 1,ncol = 5) error de dimensiones
A <- matrix(c(15,-5,28,13,32,42),2,3)
B <- matrix(c(15,-5,28,13,32,42),3,2)
C <- matrix(sample(1:100,9),3,3)
D <- matrix(sample(1:100,9),3,3)
sample(1:6,1)
A
B
C
D
########
A+B
C+D
A+A
B
t(B) #transpuesta
A+t(B)
C*D # multiplicación de elemento por elemento
C%*%D # Multiplicación de matrices
##    %*%   %%   %/%
"%p%" <- function(a,b){a*b}
13%p%2
##########################
diag(C) # elementos de la diagonal de C
sum(diag(C)) # traza de C
det(C) # determinante de C
solve(C) #matriz inversa
round(C%*%solve(C),digits = 5)
round(solve(C)%*%C,digits = 5)
#########################################
# ¿Cómo usar secciones de las matrices? #
#########################################
A
A[2,2]
A[1,2]
A[2,1]
A[,1]
A[,2]
A[,3]
A[,c(2,3)]
A[,2:3]
A[,c(1,3)]
D
D[3,]
D[c(1,3),c(1,3)]
sum(D[c(1,3),c(1,3)])
prod(D[c(1,3),c(1,3)])

#################################
M <- matrix(c(2,0,3,4,1,5,3,-1,7),3,3)
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
M
Madj <- t(cof)/det(M)
round(Madj,2)
round(solve(M),2)
##########################
