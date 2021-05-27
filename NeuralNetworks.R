#install.packages("ggplot2")
library(ggplot2)

# Wczytujemy dane
dane <- read.csv('C:\\Users\\...\\Downloads\\iris.data', header = FALSE, sep=",")
# Tasujemy dane
rows <- sample(nrow(dane))
dane <- dane[rows, ]

# Interpretujemy dane 
X <- data.matrix(dane[,-ncol(dane)])
y <- data.matrix(dane[,ncol(dane)])

mozliweWyniki <- list("Iris-setosa", "Iris-versicolor", "Iris-virginica")

# Podmieniamy ceche jakosciowa na wartosc liczbowa
Y <- matrix(0, nrow = length(y), ncol = length(mozliweWyniki))
for(i in 1:length(y)) {
  Y[i, match(y[i], mozliweWyniki)] <- 1
}

# Rozdzielamy dane
podzialDanych <- 0.8*nrow(dane)

X_uczenie <- X[1:podzialDanych,]
Y_uczenie <- Y[1:podzialDanych,]

X_test <- X[(podzialDanych+1):nrow(X),]
Y_test <- Y[(podzialDanych+1):nrow(Y),]

sigmoid <- function(x) {
  w <- 1/(1+exp(-x))
  w[is.na(w)] <- 0
  return (w)
}

sigmoidBipolar <- function(x) {
  w <- (1-exp(-x))/(1+exp(-x))
  w[is.na(w)] <- -1
  return (w)
}

linear <- function(x) {
  x[x < 0] <- 0
  x[x > 1] <- 1
  return (x)
}

# Wybieramy parametry
warstwyUkryte <- c(4, 6)
maxIter <- 50000
wspolczynnikUczenia <- 0.02

# Wybierz jedno:
#funkcjeAktywacji <- list(sigmoid, sigmoidBipolar, sigmoid)   # rozne funkcje aktywacji dla warstw
funkcjeAktywacji <- sigmoid                                      # jedna funkcja aktywacji dla wszystkich warstw
##################

minSSE <- 0.05

utworzWektorFunkcji <- function(fa, warstwyUkryte) {
  funkcjeAktywacji <- list()
  for (i in 1:(length(warstwyUkryte) + 1)) {
    funkcjeAktywacji[[i]] <- fa
  }
  return (funkcjeAktywacji)
}

procesUczeniaSieci <- function(X, Y, warstwyUkryte, maxIter, wspolczynnikUczenia, funkcjeAktywacji, minSSE){
  
  # Wektor kolejnych SSE
  kolejneSSE <- rep(0, maxIter)
  
  liczbaNeuronowWWarstwach <- c(length(X[1,]), warstwyUkryte, length(Y[1,]))
  liczbaWarstw = length(liczbaNeuronowWWarstwach)
  
  # Losujemy wagi poczatkowe 
  listaWag <- list()
  for (i in 1:(liczbaWarstw-1)) {
    a <- liczbaNeuronowWWarstwach[i]
    b <- liczbaNeuronowWWarstwach[i+1]
    listaWag[[i]] <- matrix(runif((a+1)*b, 0, 1), a+1, b)
  }
  
  # Ta sama funkcja dla kazdej warstwy
  if (is.function(funkcjeAktywacji)) {
    funkcjeAktywacji <- utworzWektorFunkcji(funkcjeAktywacji, warstwyUkryte)
  }
  
  # Do wartosci wejsciowych dodajemy x0 = 1
  X <- cbind(rep(1, nrow(X)), X)
  
  warstwy <- list()
  warstwy[[1]] <- X
  
  for(k in 1:maxIter){
    
    # Wartosci neuronow w kolejnych warstwach
    for(i in 2:(liczbaWarstw)) {
      warstwy[[i]] <- cbind(matrix(1, nrow = nrow(warstwy[[i-1]])), funkcjeAktywacji[[i-1]](warstwy[[i-1]] %*% listaWag[[i-1]]))
    }
    warstwaWyjsciowa <- warstwy[[liczbaWarstw]][,-1]
    
    # Wartosci sygnalow bledow w kolejnych warstwach
    listaSygnalowBledu <- list()
    for(i in liczbaWarstw:2) {
      
      # Warstwa wyjsciowa
      if (i == liczbaWarstw) {
        listaSygnalowBledu[[i]] <- (Y-warstwaWyjsciowa)*(warstwaWyjsciowa*(1-warstwaWyjsciowa))
      }
      
      # Pozostale warstwy
      else {
        listaSygnalowBledu[[i]] <- (listaSygnalowBledu[[i+1]] %*% t(listaWag[[i]])[,-1]) * (warstwy[[i]][,-1]*(1-warstwy[[i]][,-1]))
      }
    }
    
    # Aktualizujemy wagi
    for(i in 1:(liczbaWarstw-1)) {
      listaWag[[i]] <- listaWag[[i]] + wspolczynnikUczenia * t(warstwy[[i]]) %*% listaSygnalowBledu[[i+1]]
    }
    
    # Srednia wartosc SSE dla danych wejsciowych 
    kolejneSSE[k] <- sum((Y-warstwaWyjsciowa)^2) / nrow(Y)
    
    # Warunek zatrzymania
    if (kolejneSSE[k] < minSSE) {
      print("Osiagnieto minimalna sume kwadratow bledow")
      break
    }
  }
  
  # Wykres procesu uczenia
  x <- seq(1, k, length = 100)
  print(qplot(x, kolejneSSE[x], geom = "line", main = "Proces uczenia", ylab = "Wartosc SSE", xlab = "Iteracja"))
  
  wynik <- list("wyjscie" = warstwaWyjsciowa,
                "noweWagi" = listaWag)
  return (wynik)
}

testSieci <- function(X, Y, warstwyUkryte, listaWag, funkcjeAktywacji) {
  liczbaNeuronowWWarstwach <- c(length(X[1,]), warstwyUkryte, length(Y[1,]))
  liczbaWarstw = length(liczbaNeuronowWWarstwach)
  
  # Ta sama funkcja dla kazdej warstwy
  if (is.function(funkcjeAktywacji)) {
    funkcjeAktywacji <- utworzWektorFunkcji(funkcjeAktywacji, warstwyUkryte)
  }
  
  X <- cbind(rep(1, nrow(X)), X)
  
  warstwy <- list()
  warstwy[[1]] <- X
  for(i in 2:(liczbaWarstw)) {
    warstwy[[i]] <- cbind(matrix(1, nrow = nrow(warstwy[[i-1]])), funkcjeAktywacji[[i-1]](warstwy[[i-1]] %*% listaWag[[i-1]]))
  }
  warstwaWyjsciowa <- warstwy[[liczbaWarstw]][,-1]
  
  # Srednia wartosc SSE dla danych wejsciowych 
  SSE <- sum((Y-warstwaWyjsciowa)^2) / nrow(Y)
  
  wynik <- list("wyjscie" = warstwaWyjsciowa,
                "SSE" = SSE)
  return(wynik)
}

#####################

# Uczymy siec
wynikUczenia <- procesUczeniaSieci(X_uczenie, Y_uczenie, warstwyUkryte, maxIter, wspolczynnikUczenia, funkcjeAktywacji, minSSE)

# Testujemy siec
wynikTestow <- testSieci(X_test, Y_test, warstwyUkryte, wynikUczenia$noweWagi, funkcjeAktywacji)

# Porownujemy wyjscia oczekiwane z przewidywanymi
przewidywane <- apply(wynikTestow$wyjscie, 1, which.max)
oczekiwane <- apply(Y_test, 1, which.max)

porownanie <- cbind(mozliweWyniki[przewidywane], mozliweWyniki[oczekiwane])
colnames(porownanie) <- c("Wartosc przewidywana", "Wartosc oczekiwana")
rownames(porownanie) <- paste(c(1:30),c(rep("#", 30)))
print(porownanie)

# Skutecznosc testow
skutecznosc <- (sum(przewidywane == oczekiwane) / nrow(Y_test)) * 100
cat("Skutecznosc przeprowadzonych testow: ", skutecznosc, "%")
cat("Srednie SSE wykonanego testu: ", wynikTestow$SSE)
