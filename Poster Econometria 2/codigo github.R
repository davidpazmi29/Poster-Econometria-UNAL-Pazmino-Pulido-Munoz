

mis_paquetes <- c("tidyverse", "fs", "tsibble", "feasts", "fable", "ggtime","tseries","forecast" )


paquetes_faltantes <- mis_paquetes[!(mis_paquetes %in% installed.packages()[,"Package"])]


if(length(paquetes_faltantes)) {
  install.packages(paquetes_faltantes)
}
# 1. CARGA DE LIBRERÍAS
library(tidyverse) 
library(fs)       
library(tsibble)   
library(fable)     
library(feasts)    
library(ggtime)
library(tseries)
library(forecast)

# 2. DEFINIR RUTA RELATIVA
ruta_csv <- "Base_de_datos_elic.csv"

# 3. LECTURA ADAPTADA (Saltando los encabezados corruptos)
datos_crudos <- read_csv(ruta_csv, 
                         col_names = FALSE, 
                         skip = 1, 
                         locale = locale(encoding = "LATIN1"),
                         show_col_types = FALSE)

# 4. ASIGNAR MANUALMENTE NOMBRES LIMPIOS EN MINÚSCULAS
# Respetamos el orden exacto de las 15 columnas que tiene la base del DANE
colnames(datos_crudos) <- c("año", "mes", "cod_depto", "cod_muni", "obj_tra", 
                            "clase_suelo", "modalidad", "destino", "tipo_vivi", 
                            "vis_novis", "estrato", "area", "unidades", 
                            "licencias", "cobertura")

# 5. FILTRAR BOGOTÁ Y COLAPSAR A SERIE MENSUAL
serie_bogota <- datos_crudos %>%
  # Filtrar Bogotá (Depto 11, Municipio 1) convirtiendo a número 
  filter(as.numeric(cod_depto) == 11 & as.numeric(cod_muni) == 1) %>%
  mutate(
    mes_num = as.numeric(mes),
    # Crear formato año-mes de tsibble
    fecha = yearmonth(paste(año, mes_num, sep = "-"))
  ) %>%
  # Quitar posibles filas vacías
  filter(!is.na(fecha)) %>%
  # Agrupar y sumar los microdatos de la ciudad
  group_by(fecha) %>%
  summarise(
    area_total = sum(as.numeric(area), na.rm = TRUE),
    viviendas_totales = sum(as.numeric(unidades), na.rm = TRUE)
  ) %>%
  as_tsibble(index = fecha)

# 6. VERIFICAR EL RESULTADO EN CONSOLA
print(serie_bogota)

# 8. graficas FAC y FACP
serie_bogota %>%
  gg_tsdisplay(area_total, plot_type = "partial", lag_max = 36) +
  labs(title = " Serie Original (Bogotá D.C.)")

# 8. PRUEBA ADF A LA SERIE ORIGINAL
cat("--- PRUEBA ADF: SERIE ORIGINAL ---\n")
prueba_original <- adf.test(serie_bogota$area_total)
print(prueba_original)

# a. PRUEBA ADF A LA SERIE DIFERENCIADA
cat("\n--- PRUEBA ADF: SERIE DIFERENCIADA (1 Diferencia) ---\n")
serie_dif <- diff(serie_bogota$area_total)

# Hacemos la prueba omitiendo el dato vacío (na.omit)
prueba_diferenciada <- adf.test(na.omit(serie_dif))
print(prueba_diferenciada)

#9. serie Logaritmica  
serie_transformada <- diff(diff(log(serie_bogota$area_total), lag = 12), lag = 1)

# 2. Graficar con la transformación completa (Log + Diferencia estacional + Diferencia regular)
serie_bogota %>%
  gg_tsdisplay(difference(difference(log(area_total), 12), 1), plot_type = "partial")
serie_bogota %>%
  features(log(area_total), unitroot_nsdiffs)
serie_bogota %>%
  features(log(area_total), feat_stl) %>%
  select(seasonal_strength_year)
ajuste_modelos <- serie_bogota %>%
  model(
    modelo_ar_estacional = fable::ARIMA(log(area_total) ~ pdq(1,1,1) + PDQ(1,0,0)),
    modelo_ma_estacional = fable::ARIMA(log(area_total) ~ pdq(1,1,1) + PDQ(0,0,1)),
    modelo_completo_est  = fable::ARIMA(log(area_total) ~ pdq(1,1,1) + PDQ(1,0,1)),
    modelo_puro_regular  = fable::ARIMA(log(area_total) ~ pdq(2,1,1) + PDQ(0,0,0))
  )

# 10. NUEVA TABLA COMPARATIVA
tabla_criterios <- ajuste_modelos %>% 
  glance() %>% 
  select(.model, AIC, AICc, BIC) %>%
  arrange(BIC)

print(tabla_criterios)

# 11. DIAGNÓSTICO GRÁFICO DE LOS RESIDUOS
ajuste_modelos %>%
  select(modelo_puro_regular) %>%
  gg_tsresiduals()

# 12. PRUEBA FORMAL DE LJUNG-BOX
augment(ajuste_modelos) %>%
  filter(.model == "modelo_puro_regular") %>%
  features(.innov, ljung_box, lag = 24, dof = 3)


# 13. GENERAR EL PRONOSTICO A 12 MESES
pronostico_bogota <- ajuste_modelos %>%
  select(modelo_puro_regular) %>%
  forecast(h = "12 months")

# 14. GRAFICA FINAL DEL PRONOSTICO (Para el centro del Poster)
pronostico_bogota %>%
  autoplot(serie_bogota %>% filter_index("2021 Jan" ~ .), level = c(80, 95)) +
  labs(
    title = "Etapa 4: Pronostico del Area Aprobada para Construccion",
    subtitle = "Modelo ARIMA(2,1,1) con intervalos de confianza del 80% y 95% - Bogota D.C.",
    x = "Meses",
    y = "Area Total (Metros Cuadrados)"
  ) +
  theme_minimal()


# 15. VER LOS PARAMETROS ESTIMADOS Y ERRORES ESTANDAR

# Opcion A: Reporte completo en texto 
ajuste_modelos %>% 
  select(modelo_puro_regular) %>% 
  report()

# Opcion B: Tabla limpia en formato de datos 
tabla_parametros <- ajuste_modelos %>% 
  select(modelo_puro_regular) %>% 
  tidy()

print(tabla_parametros)
# 16. EXTRAER LOS RESIDUOS DEL MODELO 
residuos_modelo <- augment(ajuste_modelos) %>%
  filter(.model == "modelo_puro_regular") %>%
  pull(.innov) %>%
  na.omit() 

# 17. PRUEBA DE NORMALIDAD (Jarque-Bera)
print("--- Prueba de Normalidad de Jarque-Bera ---")
jarque.bera.test(residuos_modelo)

# 18. PRUEBA DE HOMOSCEDASTICIDAD (ARCH - Multiplicador de Lagrange)
print("--- Prueba de Efectos ARCH (Homoscedasticidad) ---")
ArchTest(residuos_modelo, lags = 12)
