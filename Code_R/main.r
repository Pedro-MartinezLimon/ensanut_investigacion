library(haven)
library(dplyr)

#  Datos Sangre -----------------------------------------------------------
sangre_filtros <- c("FOLIO_INT", 
                          "valor_AC_URICO", "valor_COL_LDL", "valor_CREAT",
                          "valor_GLU_SUERO", "valor_INSULINA","valor_TRIG")

datos_sangre_2021 <- read_dta("ENSANUT/2021/datos_sangre_2021.dta")
datos_sangre_2022 <- read_dta("ENSANUT/2022/datos_sangre_2022.dta")

datos_sangre_2021 <- datos_sangre_2021[sangre_filtros]
datos_sangre_2022 <- datos_sangre_2022[sangre_filtros]

datos_sangre <- bind_rows(datos_sangre_2021, datos_sangre_2022)

datos_sangre[is.na(datos_sangre)] <- 0
#  Datos Presón Arterial --------------------------------------------------

presion_filtros <- c("FOLIO_INT",
                     "an27_01s", "an27_01d",
                     "an27_02s", "an27_02d",
                     "an27_03s", "an27_03d")

datos_presion_2021 <- read_dta("ENSANUT/2021/datos_tension_arterial_2021.dta")
datos_presion_2022 <- read_dta("ENSANUT/2022/datos_tension_arterial_2022.dta")

datos_presion_2021 <- datos_presion_2021[presion_filtros]
datos_presion_2022 <- datos_presion_2022[presion_filtros]

datos_presion <- bind_rows(datos_presion_2021, datos_presion_2022)
datos_presion[is.na(datos_presion)] <- 0

datos_presion <- datos_presion %>% 
  mutate(Sistolica_Promedio = trunc(rowMeans(select(.,an27_01s, an27_02s, an27_03s))),
         Diastolica_Promedio = trunc(rowMeans(select(.,an27_01d, an27_02d, an27_03d)))
  ) %>% 
  select(-starts_with("an27_"))

#  Datos Salud ------------------------------------------------------------

salud_filtros <- c("FOLIO_INT", "edad", "sexo",
                   "a0301", "a0401", 
                   "a0601b", "a0601c",
                   "a0604", "a0606")

datos_salud_2021 <- read_dta("ENSANUT/2021/datos_salud_adultos_2021.dta")
datos_salud_2022 <- read_dta("ENSANUT/2022/datos_salud_adultos_2022.dta")

datos_salud_2021 <- datos_salud_2021[salud_filtros]
datos_salud_2022 <- datos_salud_2022[salud_filtros]

datos_salud <- bind_rows(datos_salud_2021, datos_salud_2022)
datos_salud[is.na(datos_salud)] <- 0
# Datos Residentes --------------------------------------------------------

residentes_filtros <- c("FOLIO_INT", "FOLIO_I",
                        "entidad", "estrato",
                        "H0310A", "H0310B",
                        "H0310C","h0317a", "h0404")

datos_residentes_2021 <- read_dta("ENSANUT/2021/datos_residentes_2021.dta")
datos_residentes_2022 <- read_dta("ENSANUT/2022/datos_residentes_2022.dta")

datos_residentes_2021 <- datos_residentes_2021[residentes_filtros]
datos_residentes_2022 <- datos_residentes_2022[residentes_filtros]

# IMPORTANTE: SE CASTEA A CHAR PARA JUNTARLA A 2022
datos_residentes_2021 <- datos_residentes_2021 %>%
  mutate(across(c(H0310A, H0310B, H0310C), as.character))

datos_residentes_2021 <- datos_residentes_2021 %>%
  mutate(across(c(entidad), as.numeric))

datos_residentes_2022 <- datos_residentes_2022 %>%
  mutate(across(c(entidad), as.numeric))



datos_residentes <- bind_rows(datos_residentes_2021,datos_residentes_2022)
datos_residentes <- datos_residentes %>% 
  mutate(across(c(H0310A, H0310B, H0310C), as.numeric))

datos_residentes <- datos_residentes %>% 
  mutate(Institucion_Medica = pmax(H0310A, H0310B, H0310C, na.rm = TRUE)) %>% 
  select(-starts_with("H0310"))
# Datos del hogar ---------------------------------------------------------

datos_hogar_filtros <- c("FOLIO_I", "h0204", "h0327")

datos_hogar_2021 <- read_dta("ENSANUT/2021/datos_hogar_2021.dta")
datos_hogar_2022 <- read_dta("ENSANUT/2022/datos_hogar_2022.dta")

datos_hogar_2021 <- datos_hogar_2021[datos_hogar_filtros]
datos_hogar_2022 <- datos_hogar_2022[datos_hogar_filtros]


datos_hogar <- bind_rows(datos_hogar_2021, datos_hogar_2022)

datos_hogar[is.na(datos_hogar)] <- 0
#  Unión datos ------------------------------------------------------------

folios_integrantes <- as.data.frame(unique(c(datos_sangre$FOLIO_INT, datos_presion$FOLIO_INT)))
folios_integrantes <- folios_integrantes %>% rename(FOLIO_INT = `unique(c(datos_sangre$FOLIO_INT, datos_presion$FOLIO_INT))`)

# ORDEN : Sangre -> Presion -> Salud -> Residentes : FOLIO_INT : integrante
datos_exportar <- folios_integrantes %>%
  left_join(datos_sangre, by = "FOLIO_INT")

datos_exportar <- datos_exportar %>%
  left_join(datos_presion, by = "FOLIO_INT")

datos_exportar <- datos_salud %>% 
  left_join(datos_exportar, by = "FOLIO_INT")


datos_exportar <- datos_exportar %>% 
  left_join(datos_residentes, by = "FOLIO_INT")

# Incorporamos por vivienda [Hogar] con : FOLIO_I 

datos_exportar <- datos_exportar %>%
  left_join(datos_hogar, by = "FOLIO_I")

datos_exportar[is.na(datos_exportar)] <- 0
# Renombrando datos ------------------------------------------------------

# SANGRE # 

# SALUD #
datos_exportar <- datos_exportar %>% 
  mutate(sexo = case_when(
    sexo == 1 ~ "Hombre",
    sexo == 2 ~ "Mujer",
  ))
# Residentes #
datos_exportar <- datos_exportar %>% 
  mutate(region = case_when(
    (entidad == 9 | entidad == 15) ~ "Cd./Edo México",
    (entidad == 13 | entidad == 29 | entidad == 30 | entidad == 17 | entidad == 21) ~ "Centro",
    (entidad == 1 | entidad == 10 | entidad == 11 | entidad == 22 | entidad == 24 | entidad == 32)  ~ "Centro Norte",
    (entidad == 5 | entidad == 8 | entidad == 19 | entidad == 28) ~ "Frontera",
    (entidad == 12 | entidad == 20) ~ "Pacífico Sur",
    (entidad == 6 | entidad == 14 | entidad == 16) ~ "Pacífico-Centro",
    (entidad == 2 | entidad == 3 | entidad == 18 | entidad == 25 | entidad == 26) ~ "Pacifico-Norte",
    (entidad == 4 | entidad == 7 | entidad == 23 | entidad == 27 | entidad == 31) ~ "Península"
))
datos_exportar <- datos_exportar %>% 
  mutate( Estrato = case_when(
   estrato == 1 ~ "Rural",
   estrato == 2 ~ "Urbano",
   estrato == 3 ~ "Métropolitano"
  ))

datos_exportar <- datos_exportar %>% 
  mutate(Institucion_Medica = case_when(
    Institucion_Medica == 1 ~ "IMSS",
    Institucion_Medica == 2 ~ "ISSSTE",
    Institucion_Medica == 3 ~ "PEMEX",
    Institucion_Medica == 4 ~ "Defensa",
    Institucion_Medica == 5 ~ "Marina",
    Institucion_Medica == 6 ~ "Secretaria de Salud",
    Institucion_Medica == 7 ~ "IMSS Bienestar",
    Institucion_Medica == 8 ~ "DIF",
    Institucion_Medica == 9 ~ "Cruz Roja",
    Institucion_Medica == 10 ~ "Instituto Nacional de Salud",
    Institucion_Medica == 11 ~ "ONG/OSC",
    Institucion_Medica == (12:18) ~ "Instituciones Privadas",
    Institucion_Medica == 19 ~ "Medico laboral",
    Institucion_Medica == 20 ~ "Curandero/Naturista",
    Institucion_Medica == 21 ~ "Homeópata",
    Institucion_Medica == 22 ~ "Otro",
    Institucion_Medica == 23 ~ "No especifico",
    Institucion_Medica == 99 ~ "No sabe",
  ))
datos_exportar <- datos_exportar %>% 
  mutate(Maximo_Grado_Estudios = case_when(
    h0317a == 1 ~ "Preescolar",
    h0317a == 2 ~ "Primaria",
    h0317a == 3 ~ "Secundaria",
    h0317a == 4 ~ "Preparatoria",
    h0317a == 5 ~ "Normal básica",
    h0317a == 6 ~ "Estudios técnicos con primaria terminada",
    h0317a == 7 ~ "Estudios técnicos con secundaria terminada",
    h0317a == 8 ~ "Estudios técnicos con preparatoria terminada",
    h0317a == 9 ~ "Normal de licenciatura",
    h0317a == 10 ~ "Licenciatura o profesional",
    h0317a == 11 ~ "Maestría",
    h0317a == 12 ~ "Doctorado",
  ))
datos_exportar <- datos_exportar %>% 
  mutate(Buscaron_Atencion_Medica = case_when(
    h0404 == 0 ~ "No tuvo una necesidad médica",
    h0404 == 1 ~ "Tuvo una necesidad médica y sí fue al médico",
    h0404 == 2 ~ "Tuvo una necesidad médica y no fue al médico"
  ))
datos_exportar <- datos_exportar %>% 
  select(-entidad, -estrato, -h0317a, -h0404)
# Hogar #

datos_exportar <- datos_exportar %>%
  mutate(Habitantes_En_Vivienda = case_when(
    (h0204 <= 3) ~ "De 1 a 2 personas",
    (h0204 <= 5) ~ "De 3 a 4 personas",
    (h0204 <= 7) ~ "De 5 a 6 personas",
    h0204 <= 11 ~ "De 7 a 10 personas",
    h0204 <= 20 ~ "De 11 a 20 personas",
    TRUE ~ "Más 21 personas",
  ))

datos_exportar <- datos_exportar %>%
  mutate(IngresoMedio = case_when(
    h0327 == 1 ~ "1 a 5,999 mxn",
    h0327 == 2 ~ "6,000 a 9,999 mxn",
    h0327 == 3 ~ "10,000 a 13,999 mxn",
    h0327 == 4 ~ "14,000 a 21,999 mxn",
    h0327 == 5 ~ "22,000 o más",
    h0327 == 6 ~ "No percibierón ingresos",
    h0327 == 8 ~ "No quiso responder",
    h0327 == 9 ~ "No sabe",
  ))

datos_exportar <- datos_exportar %>% 
  select(-h0327, -h0204)

#  Diagnosticos Preventivos -----------------------------------------------

library(lubridate)

# PRESIÓN (Eliminamos a las personas con valor alto durante periodo de lactancia)#
datos_exportar <- datos_exportar %>% 
  filter(a0301 != 2 | a0401 != 2)

datos_exportar <- datos_exportar %>% 
  mutate(Presion = case_when(
    (Sistolica_Promedio <= 120 & Diastolica_Promedio <= 79) ~ 1,
    (Sistolica_Promedio >= 129 & Diastolica_Promedio >= 79) ~ 2,
    TRUE ~ 0
  ))
# SANGRE Y SALUD #


#eliminar los que tengan valor < al nivel normal (EN TODOS)
datos_exportar <- datos_exportar %>% 
  mutate(Diagnostico_Calculos_Renales = case_when(
    (valor_AC_URICO < 3.4 & a0601b == 0) ~ "Ácido Úrico bajo",
    (valor_AC_URICO > 8 & a0601b == 0) ~ "Ácido Úrico elevado",
    (valor_AC_URICO > 8 & a0601b == 1) | (a0601b == 1) ~ "Ácido Úrico alto diagnosticado con cálculos renales",
    (valor_AC_URICO > 8 | a0601b == 2) ~ "Ácido Úrico alto NO diagnostico de cálculos renales"),
    
    Diagnostico_Enfermedad_Renal = case_when(
      (valor_CREAT > 1.5 & a0601c == 2) ~ "Creatinina elevada y sin diagnostico",
      ((valor_CREAT > 1.5 & a0601c == 1) | (a0601c == 1)) ~ "Creatinina elevada y diagnosticado",
      (valor_CREAT == 0 | a0601c == 2) ~ "No se sabe si tiene enfermedad renal"),
    
   Diagnostico_Diabetes = case_when(
    (valor_GLU_SUERO > 125 & a0301 == 3) ~ "Glucosa elevada sin diagnostico médico",
    ((valor_GLU_SUERO > 125 & a0301 == 1) | (a0301 == 1)) ~ "Glucosa elevada con diagnostico médico",
    (valor_GLU_SUERO == 0 | a0301 == 3) ~ "No conoce sus niveles",),
    
   Diagnostico_Hipertension = case_when(
    (Presion == 2 & a0401 == 1) ~ "Presión Elevada con diagnostico de hipertensión",
    (Presion == 2 & a0401 == 3) ~ "Presión Elevada sin diagnostico de hipertensión",
    TRUE ~ "No sabe si tiene hipertensión",),
    

   Diagnostico_Colesterol = case_when(
    (valor_COL_LDL > 200 & a0604 == 2) ~ "Colesterol elevado sin diagnostico médico",
    ((valor_COL_LDL > 200 & a0604 == 1) | (a0604 == 1) )~ "Colesterol elevado y diagnosticado",
    (valor_COL_LDL == 0 | a0604 == 2) ~ "No se sabe si colesterol alto"),
  
   Diagnostico_Trigliceridos = case_when(
    (valor_TRIG > 150 & a0606 == 2) ~ "Trigliceridos elevados sin diagnostico médico",
    ((valor_TRIG > 150 & a0606 == 1) | (a0606 == 1)) ~ "Trigliceridos altos con diagnostico médico",
    (valor_TRIG == 0 | a0606 == 2) ~ "No se sabe si trigliceridos altos",)
)

datos_exportar <- datos_exportar %>% 
  select(-starts_with("valor_"), -starts_with("a0"),-Presion, -Sistolica_Promedio, -Diastolica_Promedio)
# Exportanción a CSV ------------------------------------------------------
datos_exportar[is.na(datos_exportar)] <- "Sin registro"

library(readr)
library(data.table)
write_excel_csv(datos_exportar, "RESULTADOS/DatosENSANUT.csv");