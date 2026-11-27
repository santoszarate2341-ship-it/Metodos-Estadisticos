# 1. Cargar los datos
RW <- c(1.42, 1.58, 1.71, 1.65, 1.85, 1.94, 2.06, 1.76, 1.68, 1.89,
        2.14, 1.53, 1.75, 1.97, 2.23, 1.61, 1.82, 2.08, 1.73, 1.87,
        1.91, 2.04, 1.76, 1.83, 2.12, 1.69, 3.57, 1.88, 2.21, 1.95,
        1.74, 1.81, 2.09, 1.66, 1.92, 2.17, 1.72, 1.86, 2.02, 3.48)
# 2. Cálculos estadísticos
n <- length(RW)               # Observaciones: 40
min_val <- min(RW)            # Mínimo: 1.42
max_val <- max(RW)            # Máximo: 3.57
media <- mean(RW)             # Media: 1.922
mediana <- median(RW)         # Mediana: 1.845
desv_est <- sd(RW)            # Desviación estándar: 0.422
varianza <- var(RW)           # Varianza: 0.178
rango <- max_val - min_val    # Rango: 2.15
cv <- (desv_est / media) * 100 # Coeficiente de variación: 21.96%