#Instalacion de paquetes
install.packages("ggplot2")
install.packages("nortest")
install.packages("fitdistrplus")
#Inclusion de librerias
library(ggplot2)
library("nortest")
library(MASS)
library(fitdistrplus)

# Leer datos de uso
user_data= (read.csv(file = "datosUso.csv", header=T, encoding = "UTF-8"))

# 1 - Calcular Tasa de Arribo
pob_total = 4320000000
music_perc = 5 / 10000
music_use_perc = 0.1
# Calcular numero de usuarios
users = pob_total * music_perc * music_use_perc 

# Obtencion de la tasa de arribo.
lambda_a_mes = (mean(user_data$tasa_uso) * users)
lambda_a = lambda_a_mes/(30*24*60*60) # tasa de arribo en segundos


# 2 - Analisis de normalidad y obtencion de parametros de la distribucion de los tiempos de espera

# 2.1 - Tiempos de espera sin modificaciones
# Histograma de tiempos de espera
ggplot(user_data, aes(x = t_espera)) +
  geom_histogram(breaks=seq(0, 90000, by=50), fill = "white", colour = "black") +
  labs(title="Histograma de tiempos de espera", x="Tiempo de espera (s)", y="Frecuencia")

# Grafico Q-Q de tiempos de espera 
qqnorm(user_data$t_espera, pch = 1, frame = FALSE, main = "Grafico Q-Q de tiempos de espera (s)")
qqline(user_data$t_espera, col = "steelblue", lwd = 2)

# Prueba de normalidad 
shapiro.test(user_data$t_espera)
# Los tiempos de espera sin modificaciones no tienen dist. normal.


# 2.2 - Tiempos de espera usando la regla 1.5 IQR para eliminar valores atipicos
# Obtener cuartiles de los tiempos de espera
summary(user_data$t_espera)
Q1 = 300 # primer cuartil
Q3 = 3600 # tercer cuartil
IQR = Q3 - Q1 # rango intercuantilico
# Calcular limites inferior y superior para la regla 1.5 IQR
bottom_limit = Q1 - 1.5 * IQR
upper_limit = Q3 + 1.5 * IQR

# Obtener tiempos de espera sin valores atipicos
subset = user_data[bottom_limit < user_data$t_espera && user_data$t_espera < upper_limit,]

# Histograma de tiempos de espera sin valores atipicos
ggplot(subset, aes(x = t_espera)) +
  geom_histogram(breaks=seq(0, 5000, by=20), fill = "white", colour = "black") +
  labs(title="Histograma de tiempos de espera sin valores atipicos", x="Tiempo de espera (s)", y="Frecuencia")

# Grafico Q-Q de tiempos de espera sin valores atipicos
qqnorm(subset$t_espera, pch = 1, frame = FALSE,  main = "Grafico Q-Q de tiempos de espera sin valores atipicos (s)")
qqline(subset$t_espera, col = "steelblue", lwd = 2)

# Prueba de normalidad 
shapiro.test(subset$t_espera)
# Los tiempos de espera sin valores atipicos no tienen dist. normal.


# 2.3 - Tiempos de espera usando la regla 1.5 IQR para eliminar valores atipicos y transformados a escala logaritmica
# Pasar los tiempos de espera a escala logaritmica
subsetLog = subset
subsetLog$t_espera = log(subset$t_espera)

# Histograma de tiempos de espera en escala logaritmica
ggplot(subsetLog, aes(x = t_espera)) +
  geom_histogram(breaks=seq(0, 15, by=0.25), fill = "white", colour = "black") +
  labs(title="Histograma de tiempos de espera en escala logaritmica", x="Tiempo de espera (log(s))", y="Frecuencia")


# Grafico Q-Q de tiempos de espera en escala logaritmica
qqnorm(subsetLog$t_espera, pch = 1, frame = FALSE, main = "Grafico Q-Q de tiempos de espera en escalara logaritmica (log(s))")
qqline(subsetLog$t_espera, col = "steelblue", lwd = 2)

# Prueba de normalidad
shapiro.test(subsetLog$t_espera)
# Los tiempos de espera en escala logaritmica no tienen dist. normal.


# 2.4 - Ajustar tiempos de espera a distribucion log normal para obtener la media y desviacion estandar de la dist.
dist = fitdist(subsetLog$t_espera,"norm")
summary(subsetLog$t_espera)
summary(dist)
# Parametros de la distribucion log normal
prom_t_espera = 7 #6.52 
desv_t_espera = 1.8 #1.12



# 3 - Simulacion
# 3.1 Modelo lineal de tiempo de procesamiento de los computadoras
# Leer datos de tiempo de procesamiento
processing_data = (read.csv(file = "Processing time.csv", header=T, encoding = "UTF-8"))

# MBP_2019
m2019 <- lm(processing_data$MBP_2019~processing_data$Seconds)
summary(m2019)
# pendiente: 0.09, intercepcion: 0

# MBP_2015_2_9
m2012_2_9 <- lm(processing_data$MBP_2015_2_9~processing_data$Seconds)
summary(m2012_2_9)
# pendiente: 0.2, intercepcion: 0 

# MBP_2019_2_7
m2012_2_7 <- lm(processing_data$MBP_2015_2_7~processing_data$Seconds)
summary(m2012_2_7)
# pendiente: 0.22, intercepcion: 0 

# 3.2 Codigo de la simulacion
# Calcular numero de pruebas requeridas
p = 0.9
e = 0.05
n = 3 #round(p * (1 - p) * (qnorm(1 - (e/2))/e)^2)

# Parametros de la simulacion
k <- 9 # numero de servidores
testTime = 604800/7  # segundos en una semana
ttMultiplier = 52 *7
mu <- 1/lambda_a # tiempo promedio entre arribos
ms <- rep(0.09, 7)
ms <- c(ms, 0.2, 0.22) # pendientes de los tiempos de servicio de las computadoras
bs <- rep(0, 9) # intercepciones de los tiempos de servicio de las computadoras

# Parametros de ventas
comissionLimit = 1000000
goal = 1000000

# Distribucion gamma de duracion de canciones
shape = 6.76
rate = 0.03

# Arreglos para guardar resultados de la simulacion
Twork <- matrix(rep(0, n*k), n)
Njobs <- matrix(rep(0, n*k), n)
Wmean <- rep(0, n) # tiempo promedio de espera
Wmax <- rep(0, n) # tiempo maximode espera
Nwithdr <- rep(0, n)
Nav <- rep(0, n)
Nat10 <- rep(0, n)
Stimes <- rep(0, n) # tiempo promedio de servicio
Smax <- rep(0,n)

sales = rep(0,n) #vector de ventas totales por corrida
longSongSales = rep(0,n) # vector de ventas de canciones largas por corrida
longSongFraction = rep(0,n) # vector de canciones largas por corrida

for (i in 1:n) {
  arrival <- c()
  start <- c()
  finish <- c()
  server <- c()
  j <- 0
  T <- 0
  s_max <- -1
  A <- rep(0,k)
  
  songCounter = 0
  
  while (T < testTime) {
    j <- j+1
    wait_time <- exp(rnorm(1, mean=prom_t_espera, sd=desv_t_espera))
    T <- T + rexp(n = 1, rate = lambda_a) # 0.0086
    arrival <- c(arrival, T)
    Nfree <- sum( A < T )
    u <- 1
    if (Nfree == 0) {
      
      for (v in 2:k) {
        if (A[v] < A[u]) {
          u <- v
        }
      }
      if (A[u]-T > wait_time) {
        start <- c(start, T+wait_time)
        finish <- c(finish, T+wait_time)
        u <- 0
      } else {
        start <- c(start, A[u])
      }
    } else { # NFree > 0
      u <- ceiling(runif(1)*k)
      while (A[u] > T) { 
        u <- ceiling(runif(1)*k)
      }
      start <- c(start, T)
    }
    server <- c(server, u)
    if (u > 0) {
      songCounter = songCounter + 1
      song_duration = rgamma(1, shape, rate=rate)
      S <- ms[u]*song_duration + bs[u]
      Stimes[i] <- Stimes[i] + S
      finish <- c(finish, start[j]+S)
      A[u] <- start[j] + S
      if (s_max < S) {
        s_max = S
      }
      
      # calculo de metricas de ventas
      if (song_duration < 360) {
        sales[i] = sales[i] + 0.99
      }
      else {
        sale = ceiling(song_duration/360) * 0.99
        sales[i] = sales[i] + sale
        longSongSales[i] = longSongSales[i] + sale
        longSongFraction[i] = longSongFraction[i] + 1 
      }
    }
  }
   for (u in 1:k) { 
     Twork[i,u] <- sum((server == u)*(finish-start)) 
     Njobs[i,u] <- sum(server == u)
   }
   Wmean[i] <- mean(start-arrival) 
   Wmax[i] <- max(start-arrival)
   Nwithdr[i] <- sum(server == 0)
   Nav[i] <- sum(start == arrival)
   Nat10[i] <- sum(finish > testTime)
   
   Smax[i] <- s_max
   sim_jobs = sum(Njobs[i])
   Stimes[i] <- Stimes[i] / sim_jobs
   
   longSongFraction[i] = longSongFraction[i]/songCounter
}

# Resultados
# 1 - Cantidad esperada de canciones en un año
n_jobs = colMeans(Njobs) * ttMultiplier
total_jobs =  sum(n_jobs)
cat("Cantidad total de canciones procesadas en un año: ", total_jobs, "\n")

# 1 - Fraccion de canciones procesadas por cada computador 
server_fraction = n_jobs / total_jobs
cat("Fraccion de canciones procesadas por cada computador: ", server_fraction, "\n")

# 2 - Fracci?n de solicitudes que fueron retiradas por el cliente debido a que se le agot? la paciencia.
retired_jobs_fraction = mean(Nwithdr) / total_jobs
cat("Fracci?n de solicitudes que fueron retiradas: ", retired_jobs_fraction, "\n")

# 3 - Tiempo promedio y maximo de espera
mean_waitTime = mean(Wmean)
cat("Tiempo promedio de espera: ", mean_waitTime, "\n")

max_waitTime = mean(Wmax)
cat("Tiempo maximo de espera: ", max_waitTime, "\n")

# 3 - Tiempo promedio y maximo de servicio
mean_serviceTime = mean(Stimes)
cat("Tiempo promedio de servicio: ", mean_serviceTime, "\n")
max_serviceTime = mean(Smax)
cat("Tiempo maximo de servicio: ", max_serviceTime, "\n")

# 3 -Tiempo promedio y maximo de respuesta
mean_responseTime = mean_waitTime + mean_serviceTime
cat("Tiempo promedio de respuesta: ", mean_responseTime, "\n")
max_responseTime = max_waitTime + max_serviceTime
cat("Tiempo maximo de respuesta: ", max_responseTime, "\n")


# 4- Fraccion de tiempo de espera
frac_waitTime = mean_waitTime / mean_responseTime
cat("Fraccion de tiempo de espera del tiempo de respuesta: ", frac_waitTime, "\n")

# 5 - Metricas de ventas
cat("Valor esperado de ventas: ", mean(sales*ttMultiplier), "\n")
cat("Error estandar: ",sd(sales*ttMultiplier), "\n")
earnings = ifelse(sales>comissionLimit, sales - sales*0.3, sales - sales*0.15)
cat("Ganancias esperadas: ",mean(earnings*ttMultiplier), "\n")
cat("Fraccion de canciones mayores a 6  min: ",mean(longSongFraction), "\n")
cat("Fraccion de ventas mayores a 6  min: ",mean(longSongSales)/mean(sales), "\n")
cat("Probabilidad de cumplir la meta de ventas en 1 año: ",(sum(sales*ttMultiplier>goal)/n), "\n")




