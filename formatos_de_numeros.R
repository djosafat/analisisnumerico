pi
round(pi,2)
round(pi,3)
round(pi,4)
round(pi,5)
round(pi,6)
round(pi,10)
###########################
signif(pi,digits = 4)
signif(pi,digits = 5)
signif(pi,digits = 6)
signif(pi,digits = 7)
signif(pi,digits = 8)
signif(pi,digits = 10)
#############################
print(pi,digits = 4)
print(pi,digits = 6)
print(pi,digits = 7)
print(pi,digits = 8)
print(pi,digits = 10)
print(pi,digits = 22)
print(pi,digits = 23)
#
print(exp(1),digits = 22)
#############################
sprintf("%.6f",pi)
sprintf("%.7f",pi)
sprintf("%.8f",pi)
sprintf("%.9f",pi) # comparar con print(pi,digits = 10)
sprintf("%.21f",pi) # comparar con print(pi,digits = 22) 
sprintf("%.44f",pi) 
sprintf("%.48f",pi) 
sprintf("%.49f",pi) 
sprintf("%.50f",pi) 
sprintf("%.100f",pi) 
sprintf("%.48f",sqrt(2))
##############################
sprintf("%.48f",pi^2/6)
sprintf("%.48f",exp(1))
sprintf("%.50f",exp(1))
sprintf("%.55f",exp(1))
sprintf("%.44f",1/9)
sprintf("%.60f",1/9)
###############################
library(Rmpfr)
npi <- mpfr(pi, precBits = 256)
formatMpfr(npi,digits = 10)# comparar con print(pi,digits = 10)
formatMpfr(npi,digits = 40)# comparar con sprintf("%.39f",pi)
formatMpfr(npi,digits = 50)# comparar con sprintf("%.49f",pi)
formatMpfr(npi^2,digits = 50)# comparar con sprintf("%.49f",pi^2)
npi^2
###################################
npi <- mpfr(pi, precBits = 1024)
formatMpfr(npi,digits = 10)# comparar con print(pi,digits = 10)
formatMpfr(npi,digits = 40)# comparar con sprintf("%.39f",pi)
formatMpfr(npi,digits = 50)# comparar con sprintf("%.49f",pi)
formatMpfr(npi^2,digits = 50)# comparar con sprintf("%.49f",pi^2)
npi^2
###################################
formatC(pi,digits = 55)
formatC(pi,digits = 50)
formatMpfr(npi,digits = 49)
sprintf("%.48f",pi)

##
str(formatC(pi,digits = 50))
str(formatMpfr(npi,digits = 49))
str(sprintf("%.48f",pi))
str(formatMpfr(npi,digits = 50))
########################
options(digits = 22)
pi
pi^2
sprintf("%.21f",pi^2)
formatMpfr(npi^2,digits = 22)
####################################
options(digits = 16)
pi
pi^2
sprintf("%.15f",pi^2)
formatMpfr(npi^2,digits = 16)
####################
pi^4
sprintf("%.15f",pi^4)
formatMpfr(npi^4,digits = 16)
##################
options(digits = 22)
1/9
sprintf("%.22f",1/9)
n1sobre9 <- mpfr(1/9,precBits = 256)
n1sobre9
############
n1 <- mpfr(pi-3,precBits = 256)
n2 <- mpfr(pi-3,precBits = 1024)
n1**2
n2**2
##########################################
exp(-5)
format(exp(-5), scientific = T )
as.numeric(format(exp(-5), scientific = T ))
exp(-15)
format(exp(-15), scientific = F )
