# CÓDIGO ENSANUT

# IMPORTANTE

Es importante remarcar que este es unicamente el código fuente de la aplicación, en caso de querer ejecutarse es elemental emplear R Studio e incluir todas las liberias correspondientes

## Variables de ENSANUT Consideradas

El código toma datos de ENSANUT a partir del periodo de años de 2021-2023. Los 
datos recompilados son los siguientes. El documento marca el año y las tablas 
estudiadas así como el contenido seleccionado para trabajar.


### Datos Sangre

- Identificador único [FOLIO_INT]
- Horas transcurridas desde el último alimento [sc02]
- Valor del Ácido Úrico (mg/dl) [valor_AC_URICO]
- Valor de la Albúmina (g/dl) [valor_ALBUM]
- Valor Colesterol LDL 'malo' (mg/dl) [valor_COL_LDL]
- Valor Creatinina (mg/dl) [valor_CREAT]
- Valor Glucosa (mg/dl) [valor_GLU_SUERO]
- Valor Insulina (ul/ml) [valor_INSULINA]
- Valor Triglicéridos (mg/dl) [valor_TRIG]
  
### Datos de Tensión Arterial
- Identificador único [FOLIO_INT]
- 1ra Toma de Presión sístole [AN27_01S]
- 1ra Toma de Presión diastole [AN27_01D]
- 2ra Toma de Presión sístole [AN27_02S]
- 2ra Toma de Presión diastole [AN27_02D]
- 3ra Toma de Presión Sístole [AN27_03S]
- 3ra Toma de Presión diastole [AN27_03D]
  
### Datos Salud
- Identificador único [FOLIO_INT]
- Edad [edad]
- Sexo [sexo]
- El médico diagnostico diabetes [a0301]
- El médico diagnostico presión alta [a0401] 
- El médico diagnostico enf riñon (infección vias urinarias) [a0601a]
- El médico diagnostico enf riñon (cálculos renales) [a0601b]
- El médico diagnostico enf riñon (insf o enfermedad renal cronica) [a0601c]
- El médico indicó nvs altos de colesterol [a0604]
- El médico indicó nvs altos de triglicéridos [a0606]
  
### Datos Residentes
- Identificador único [FOLIO_INT] 
- Identificdor vivienda [FOLIO_I]
- Entidad [entidad]
- Estrato [estrato]
- Tiene derecho a servicio médico [H0310A, H0310B, H0310C]
- Grado de estudios [h0317a]
- Busca atención médica [h0404]
  
### Datos Hogar
- Identificador de vivienda [FOLIO_I]
- Residentes por vivienda [h0204]
- Ingreso Medio por vivienda [h0327] 
  
  

### Valores de presión considerados

        SISTOLICA       DIASTOLICA
Normal     <120         <80
Elevada    >120<129     <80   
Hip. 1     >130<139     >80<89
Hip. 2     >140<180     >90
Crisis     >180         >120

#### Fuente
https://www.heart.org/-/media/files/health-topics/answers-by-heart/answers-by-heart-spanish/what-is-highbloodpressure_span.pdf