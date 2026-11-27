# Tarea 5: Correlación
# Dr. Marco Aurelio Gonzalez Tagle
# Alumno: Jose santos zarate Bocanegra
# Matricula: 2141856
# Librerías
library(corrplot)

# Ejercicio 1
velocidad <- c(2, 3, 5, 9, 14, 24, 29, 34)
abundancia <- c(6, 3, 5, 23, 16, 12, 48, 43)

plot(velocidad, abundancia, 
     main = "Efímeras vs Velocidad",
     xlab = "Velocidad", ylab = "Abundancia",
     pch = 19, col = "blue")
grid()
abline(lm(abundancia ~ velocidad), col = "red", lwd = 2)

cor.test(velocidad, abundancia)

# --- RESPUESTAS / PREGUNTAS DEL EJERCICIO 1 ---
# Pregunta: ¿Es estadísticamente significativa la correlación?
# Respuesta: Sí.
# - Hipótesis Nula (H0): No existe una correlación entre la velocidad del arroyo y la abundancia de efímeras (r = 0).
# - Hipótesis Alternativa (H1): Existe una correlación positiva entre la velocidad de los arroyos y la abundancia de efímeras (Ecdyonurus dispar).
# - Datos a reportar: r = 0.844, df = 6, p-value = 0.0084.
# - Conclusión: Como el valor de p es menor a 0.05, la correlación es estadísticamente significativa. 
#   A mayor velocidad de la corriente, mayor abundancia de efímeras.

# Ejercicio 2
datos <- read.csv(Sueloo) file(file)


vars <- datos[, c("pH", "N", "Dens", "P", "Ca", "Mg", "K", "Na")]
mat_cor <- cor(vars, use = "complete.obs")


corrplot(mat_cor, method = "circle", type = "upper", 
         tl.col = "black", tl.srt = 45, addCoef.col = "black")

# Para ver las correlaciones del pH
for(v in setdiff(names(vars), "pH")) {
  print(v)
  print(cor.test(vars$pH, vars[[v]]))
}

# --- RESPUESTAS / CUADRO DEL EJERCICIO 2 ---
# Valores de correlación y significancia (pH vs Variables):
# - pH - N   : r = 0.637,  p = 1.15e-06 (Altamente significativo)
# - pH - Dens: r = -0.589, p = 1.06e-05 (Altamente significativo)
# - pH - P   : r = 0.591,  p = 9.74e-06 (Altamente significativo)
# - pH - Ca  : r = 0.809,  p = 3.61e-12 (Altamente significativo)
# - pH - Mg  : r = -0.396, p = 0.0054   (Significativo)
# - pH - K   : r = 0.580,  p = 1.58e-05 (Altamente significativo)
# - pH - Na  : r = -0.693, p = 4.72e-08 (Altamente significativo)
# - Conclusión del correlograma: El Calcio (Ca) muestra una fuerte correlación positiva con el pH (círculos azules), 
#   mientras que el Sodio (Na) y la densidad presentan correlaciones negativas marcadas (círculos rojos).

# Ejercicio 3
x1 = c(10, 8, 13, 9, 11, 14, 6, 4, 12, 7, 5)
y1 = c(8.04, 6.95, 7.58, 8.81, 8.33, 9.96, 7.24, 4.26, 10.84, 4.82, 5.68)
x2 = c(10, 8, 13, 9, 11, 14, 6, 4, 12, 7, 5)
y2 = c(9.14, 8.14, 8.74, 8.77, 9.26, 8.10, 6.13, 3.10, 9.13, 7.26, 4.74)
x3 = c(10, 8, 13, 9, 11, 14, 6, 4, 12, 7, 5)
y3 = c(7.46, 6.77, 12.74, 7.11, 7.81, 8.84, 6.08, 5.39, 8.15, 6.42, 5.73)
x4 = c(8, 8, 8, 8, 8, 8, 8, 19, 8, 8, 8)
y4 = c(6.58, 5.76, 7.71, 8.84, 8.47, 7.04, 5.25, 12.50, 5.56, 7.91, 6.80)

par(mfrow = c(2, 2))

plot(x1, y1, main = "Conjunto 1", pch = 19, col = "darkgreen")
abline(lm(y1 ~ x1), col = "red", lwd = 2)

plot(x2, y2, main = "Conjunto 2", pch = 19, col = "darkgreen")
abline(lm(y2 ~ x2), col = "red", lwd = 2)

plot(x3, y3, main = "Conjunto 3", pch = 19, col = "darkgreen")
abline(lm(y3 ~ x3), col = "red", lwd = 2)

plot(x4, y4, main = "Conjunto 4", pch = 19, col = "darkgreen")
abline(lm(y4 ~ x4), col = "red", lwd = 2)

par(mfrow = c(1, 1))

# --- RESPUESTAS / EXPLICACIÓN DEL EJERCICIO 3 ---
# - Moraleja general: Los cuatro conjuntos comparten exactamente las mismas estadísticas descriptivas 
#   (medias, varianzas y una correlación de 0.816), pero son visualmente totalmente distintos.
# - Conjunto I: Relación lineal simple y normal.
# - Conjunto II: Relación no lineal (curvilínea), donde el coeficiente de Pearson por sí solo no es adecuado.
# - Conjunto III: Relación lineal afectada por un valor atípico (outlier) que reduce artificialmente la fuerza de r.
# - Conjunto IV: Sin relación real entre las variables, donde un solo punto extremo y aislado basta para inflar artificialmente la correlación.
# - Conclusión: Demuestra la importancia fundamental de inspeccionar y graficar siempre los datos antes de analizarlos.