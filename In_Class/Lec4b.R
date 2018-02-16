n <- 10000 
x <- rexp(n,1) 
hist(x,freq=FALSE) 
t <- c(0:1200)/100 
lines(t,dexp(t,1)) 

y <- pexp(x,1)            # probability-integral transformation 
hist(y,freq=FALSE) 
s <- c(0:100)/100 
lines(s,dunif(s,0,1)) 

#  Now generating U~UNIF(0,1) and creating Z~EXP(1) using 
# inverse-probability-integral transformation 
u <- runif(n,0,1) 
z <- qexp(u,1)            # Using quantile function for Exponentials 
hist(z,freq=FALSE) 
t <- c(0:1200)/100 
lines(t,dexp(t,1)) 

