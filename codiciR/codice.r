N = 100
a = 0.2
L = 0.8
n = 6000

test <- function(N, a, L){
    X <- runif(N)
    Z <- (X >= a) & (X <= a + L)
    Y <- sum(Z)
    return(Y)
}

Y <- replicate(n, test(N,a,L))
print(Y)
hist(Y, main = "Grafico delle distribuzioni con a = 0.2 e L = 0.8", xlab = "successi", 
    ylab = "probabilità", freq=FALSE, xlim = c(0,100))
lines(0:N, dbinom(0:N, size=N, prob=L), col='red', lw=3)
legend("topright", c("distribuzione teorica", "distribuzione con dati effettivi"), 
    bty = "n", density = c(100, 100),
    fill=c("red", "gray"))
