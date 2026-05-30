# Modelación SARIMA - Licencias de Construcción en Bogotá
Poster de Econometría II.

David Santiago Pazmiño Cortés | Juliana Pulido Gómez | Vanessa Muñoz Cañas

### Contenido del repositorio:
* El script de R con la metodología Box-Jenkins completa.
* La base de datos histórica utilizada.

### Resumen de resultados:
Tras evaluar las pruebas de raíz unitaria y fuerza estacional (0.212), se determinó que la serie no requería diferenciación estacional. El modelo ganador según el criterio BIC fue un **ARIMA(2,1,1)** puro. Los residuos fueron validados como Ruido Blanco mediante la prueba de Ljung-Box (p-valor de 0.889).
