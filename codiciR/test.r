N = 7000
L = 0.1

#definisco una funzione Y_max che crea un segmento [0,1] e inserisce all'interno N punti uniformemente a caso.
#successivamente, data una finestra di larghezza L, fa scorrere la finestra lungo tutto il segmento 
#e salva il valore della massima concentrazione di punti trovata e la relativa posizione.

Y_max <- function(N, L, delta=0.001) {
    temp <- c(0.0, 0.0)
    X <- runif(N)
    for(a in seq(from = 0.0, to = 1-L, by = delta)) {

        Z <- (X >= a) & (X <= a+L)
        Y = sum(Z)
        if (Y > temp[1]){
            temp[1] <- Y
            temp[2] <- a
        }
    }
    return(temp)
}
#itero la funzione Y_max e salvo tutti i valori massimi trovati in un vettore M
maxima <- function(N, L, times=1000) {
    M <- c()
    for(i in 1:times){
        M[i] <- Y_max(N,L)[1]
    }
    return(M)
}

M <- maxima(N, L)
hist(M, main = "Grafico della distribuzione di M con N = 7000")

times = 1000
windows <- c()
means <- c()
expected <- c()

for(l in seq(from = 0.05, to = 0.95, by = 0.05)) {
    M <- maxima(N, l, times)
    # hist(M, main = "grafico della distribuzione del massimo con N = 5000")
    print(c(l, sum(M)/times, N*(l + 0.01)))
    
    windows <- append(windows, l)
    means <- append(means, sum(M)/times)
    expected <- append(expected, N*(l + 0.01))
}
plot(means, xlab="L")
lines(expected, col='red')
legend("bottomright", c("media campionaria", "N * (L + 0.01)"), density=c(100,100), fill=c("white", "red"))

