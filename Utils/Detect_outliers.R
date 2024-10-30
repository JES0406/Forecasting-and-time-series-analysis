
# Importamos los datos
fdata<-read.table("ARIMA_04.dat", sep = "")
TT <-  12
y <- ts(fdata, frequency = TT)
# Para la demostración
y[51] <- 1000
# Vemos y para ver los posibles outliers
ggtsdisplay(y,lag.max = 50)

# Para detectarlos programáticamente
q1 <- quantile(y, 0.01)
q3 <- quantile(y, 0.99)
iqr <- q3 - q1

lower_bound <- q1 - 1.5 * iqr
upper_bound <- q3 + 1.5 * iqr

# Te los identifica
outliers <- which(y < lower_bound | y > upper_bound)
outliers


# Ejemplo con el 51, pero se puede hacer un bucle
index <- 51
y[index] <- (y[index-1] + y[index+1])/2
ggtsdisplay(y,lag.max = 50)
