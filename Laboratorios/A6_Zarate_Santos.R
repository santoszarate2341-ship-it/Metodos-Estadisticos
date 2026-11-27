# ============================================================
# ASIGNACION 6: REGRESION LINEAL
# Relacion entre el DAP y la edad de Pinus arizonica
# Alumno: José Santos Zarat Bocanegra
# Matricula: 2141856
# ============================================================


# ------------------------------------------------------------------------------
# 0. Preparación
# ------------------------------------------------------------------------------

# En esta actividad se utilizará una regresión lineal simple para analizar
# la relación entre el diámetro a la altura del pecho (DAP) y la edad
# de los árboles de Pinus arizonica.

# Definimos la dirección de los datos proporcionados por el profesor.

direccion <- "https://www.dropbox.com/s/nxoijhgmutuho0s/datos_control_Rascon.csv?dl=1"

# Definimos el nivel de significancia.
alpha <- 0.05


# ------------------------------------------------------------------------------
# 1. Importación y exploración de los datos
# ------------------------------------------------------------------------------

# Importamos la base de datos y la guardamos en un objeto llamado datos.

datos <- read.csv(direccion, header = TRUE)

# Revisamos las primeras observaciones.
head(datos)

# Revisamos la estructura de la base de datos.
str(datos)

# Revisamos los nombres de las variables.
names(datos)

# Comprobamos el número de árboles registrados.
n <- nrow(datos)

# Obtenemos estadísticos descriptivos de DAP y EDAD.
summary(datos[, c("DAP", "EDAD")])

# Calculamos algunas medidas adicionales.
sd(datos$DAP, na.rm = TRUE)
sd(datos$EDAD, na.rm = TRUE)

# Número de árboles (n):
cat("Número de árboles:", n, "\n")

# Comentario sobre los datos:
# La base de datos contiene información de árboles de Pinus arizonica.
# Las variables principales son DAP, que representa el diámetro a la
# altura del pecho en centímetros, y EDAD, que representa la edad
# de los árboles en años.
#
# Se revisó la estructura y los estadísticos descriptivos para conocer
# cómo se distribuyen los datos antes de realizar la regresión.


# ------------------------------------------------------------------------------
# 2. Diagrama de dispersión
# ------------------------------------------------------------------------------

# Elaboramos un diagrama de dispersión para observar la relación entre
# el DAP (variable independiente) y la EDAD (variable dependiente).

plot(
  datos$DAP,
  datos$EDAD,
  main = "Relación entre DAP y edad de Pinus arizonica",
  xlab = "Diámetro a la altura del pecho (cm)",
  ylab = "Edad del árbol (años)",
  pch = 19,
  col = "darkgreen",
  cex = 1.2
)

# Calculamos la correlación para apoyar la interpretación de la gráfica.
correlacion <- cor(datos$DAP, datos$EDAD, use = "complete.obs")

cat("Correlación entre DAP y EDAD:", correlacion, "\n")

# ¿Qué patrón observa? (forma, dirección, valores atípicos):
#
# Se espera observar una tendencia positiva entre las variables,
# ya que conforme aumenta el diámetro del árbol, generalmente también
# aumenta su edad. La dispersión de los puntos permite identificar
# qué tan cercana es esta relación a una línea recta.
#
# La presencia de posibles valores atípicos debe revisarse visualmente
# en el diagrama, sin asumir que existen solamente por la dispersión.


# ------------------------------------------------------------------------------
# 3. Hipótesis
# ------------------------------------------------------------------------------

# H0 (en palabras):
# No existe una relación lineal estadísticamente significativa entre
# el DAP y la edad de Pinus arizonica.

# H0 (en notación):
# H0: beta1 = 0
#
# H1 (en palabras):
# Existe una relación lineal estadísticamente significativa entre
# el DAP y la edad de Pinus arizonica.

# H1 (en notación):
# H1: beta1 != 0
#
# Nivel de significancia:
# alpha = 0.05
#
# Se utilizará un nivel de significancia del 5%, por lo que se rechazará
# la hipótesis nula cuando el valor p sea menor que 0.05.


# ------------------------------------------------------------------------------
# 4. Ajuste del modelo de regresión lineal simple
# ------------------------------------------------------------------------------

# Modelo: EDAD = alpha + beta * DAP + error

# Ajustamos el modelo de regresión lineal simple.
modelo <- lm(EDAD ~ DAP, data = datos)

# Mostramos el resumen estadístico del modelo.
summary(modelo)

# Guardamos el resumen para utilizar sus resultados posteriormente.
resumen <- summary(modelo)


# ------------------------------------------------------------------------------
# 5. Coeficientes
# ------------------------------------------------------------------------------

# Extraemos los coeficientes del modelo.
coeficientes <- coef(modelo)

# Intercepto (alpha):
intercepto <- coeficientes[1]

cat("Intercepto (alpha):", intercepto, "\n")

# Interpretación:
# Representa la edad estimada cuando el DAP es igual a cero.
# Sin embargo, este valor puede no tener una interpretación biológica
# práctica, ya que un árbol con DAP igual a cero no representa
# una condición normal dentro del estudio.

# Pendiente (beta):
pendiente <- coeficientes[2]

cat("Pendiente (beta):", pendiente, "\n")

# Interpretación (años por cada cm de DAP):
# Por cada centímetro que aumenta el DAP, la edad estimada del árbol
# cambia en promedio según el valor de la pendiente.
# Si la pendiente es positiva, significa que los árboles con mayor
# diámetro tienden a presentar mayor edad.

# Ecuación ajustada:
cat(
  "EDAD estimada =",
  round(intercepto, 4),
  "+",
  round(pendiente, 4),
  "* DAP\n"
)


# ------------------------------------------------------------------------------
# 6. Significancia
# ------------------------------------------------------------------------------

# Extraemos los valores p de los coeficientes.
tabla_coeficientes <- resumen$coefficients

# a) Intercepto -> p-value:
p_intercepto <- tabla_coeficientes[1, 4]

cat("a) p-value del intercepto:", p_intercepto, "\n")

# ¿Significativo? (Sí/No):
if (p_intercepto < alpha) {
  cat("El intercepto es significativo.\n")
} else {
  cat("El intercepto no es significativo.\n")
}

# b) Pendiente -> p-value:
p_pendiente <- tabla_coeficientes[2, 4]

cat("b) p-value de la pendiente:", p_pendiente, "\n")

# ¿Significativo? (Sí/No):
if (p_pendiente < alpha) {
  cat("La pendiente es significativa.\n")
} else {
  cat("La pendiente no es significativa.\n")
}

# c) Regresión -> F, grados de libertad y p-value:
estadistico_F <- resumen$fstatistic[1]
gl1 <- resumen$fstatistic[2]
gl2 <- resumen$fstatistic[3]

p_global <- pf(
  estadistico_F,
  gl1,
  gl2,
  lower.tail = FALSE
)

cat("c) Estadístico F:", estadistico_F, "\n")
cat("Grados de libertad:", gl1, "y", gl2, "\n")
cat("p-value global:", p_global, "\n")

# ¿Es significativa la regresión? (Sí/No):
if (p_global < alpha) {
  cat("La regresión es estadísticamente significativa.\n")
} else {
  cat("La regresión no es estadísticamente significativa.\n")
}

# d) R2:
R2 <- resumen$r.squared

cat("d) R2:", R2, "\n")
cat("Porcentaje explicado:", R2 * 100, "%\n")

# Interpretación:
# El coeficiente de determinación R2 indica qué porcentaje de la
# variación observada en la edad puede ser explicado por el DAP
# mediante el modelo de regresión lineal.
#
# La parte restante corresponde a variación que el modelo no explica
# y que puede estar relacionada con otros factores.

# Decisión sobre H0:
if (p_pendiente < alpha) {
  cat("Se rechaza H0: existe evidencia de una relación lineal significativa.\n")
} else {
  cat("No se rechaza H0: no existe evidencia suficiente de una relación lineal.\n")
}


# ------------------------------------------------------------------------------
# 7. Gráfica con la recta de regresión
# ------------------------------------------------------------------------------

# Elaboramos nuevamente el diagrama de dispersión y agregamos
# la recta ajustada del modelo de regresión.

plot(
  datos$DAP,
  datos$EDAD,
  main = "Regresión lineal entre DAP y edad",
  xlab = "Diámetro a la altura del pecho (cm)",
  ylab = "Edad del árbol (años)",
  pch = 19,
  col = "darkgreen",
  cex = 1.2
)

# Agregamos la recta de regresión.
abline(
  modelo,
  col = "red",
  lwd = 2
)

# Agregamos una leyenda.
legend(
  "topleft",
  legend = "Recta de regresión",
  col = "red",
  lwd = 2,
  bty = "n"
)


# ------------------------------------------------------------------------------
# 8. Predicción de la edad
# ------------------------------------------------------------------------------

# Creamos un objeto con los DAP que solicita la actividad.
nuevos_datos <- data.frame(
  DAP = c(12, 20, 26, 34, 45)
)

# Calculamos las edades estimadas mediante la ecuación ajustada.
# Este procedimiento corresponde al cálculo manual:
# EDAD estimada = intercepto + pendiente * DAP.

edad_manual <- intercepto + pendiente * nuevos_datos$DAP

# Verificamos los resultados utilizando la función predict()
# y el modelo de regresión ajustado.
edad_modelo <- predict(
  modelo,
  newdata = nuevos_datos
)

# Presentamos los resultados en una tabla.
tabla_predicciones <- data.frame(
  No = 1:5,
  DAP_cm = nuevos_datos$DAP,
  Edad_manual = round(edad_manual, 2),
  Edad_modelo = round(edad_modelo, 2)
)

cat("\nTABLA DE PREDICCIONES:\n")
print(tabla_predicciones)

# Comprobamos que ambos procedimientos producen los mismos resultados.
cat(
  "\n¿Coinciden los cálculos manuales y los del modelo?:",
  isTRUE(all.equal(edad_manual, edad_modelo)),
  "\n"
)

# | No. | DAP (cm) | Edad estimada (años) |
# |-----|----------|----------------------|
# Los resultados exactos se muestran en tabla_predicciones.

# ¿Algún DAP queda fuera del intervalo observado en los datos?

dap_minimo <- min(datos$DAP, na.rm = TRUE)
dap_maximo <- max(datos$DAP, na.rm = TRUE)

cat("DAP mínimo observado:", dap_minimo, "cm\n")
cat("DAP máximo observado:", dap_maximo, "cm\n")

fuera_intervalo <- nuevos_datos$DAP[
  nuevos_datos$DAP < dap_minimo |
    nuevos_datos$DAP > dap_maximo
]

if (length(fuera_intervalo) > 0) {
  cat("DAP fuera del intervalo observado:", fuera_intervalo, "\n")
  cat("Estas predicciones son extrapolaciones y deben interpretarse con precaución.\n")
} else {
  cat("Ningún DAP solicitado queda fuera del intervalo observado.\n")
}


# ------------------------------------------------------------------------------
# 9. Conclusión
# ------------------------------------------------------------------------------

# ¿Es el DAP un buen predictor de la edad en Pinus arizonica?

# La utilidad del DAP como predictor se evaluará considerando
# la significancia estadística de la pendiente y el porcentaje
# de variación explicado por el modelo (R2).

if (p_pendiente < alpha) {
  
  cat("\nCONCLUSIÓN:\n")
  cat("El DAP presenta una relación lineal estadísticamente significativa",
      "con la edad de Pinus arizonica.\n")
  
  cat("Por cada centímetro adicional de DAP, la edad estimada cambia",
      "aproximadamente", round(pendiente, 2), "años.\n")
  
  cat("El modelo explica el", round(R2 * 100, 2),
      "% de la variación observada en la edad.\n")
  
} else {
  
  cat("\nCONCLUSIÓN:\n")
  cat("No se encontró evidencia estadística suficiente para afirmar",
      "que el DAP tenga una relación lineal significativa con la edad.\n")
}

# Una limitación del modelo:
#
# Una limitación es que el modelo considera únicamente el DAP para
# explicar la edad de los árboles. Sin embargo, el crecimiento también
# puede estar influido por factores como las condiciones del suelo,
# la disponibilidad de agua, la competencia y las condiciones climáticas.
#
# Además, las predicciones fuera del intervalo de DAP observado
# representan extrapolaciones, por lo que deben interpretarse
# con precaución.


# ==============================================================================
# FIN DE LA ASIGNACIÓN 6
# ==============================================================================


