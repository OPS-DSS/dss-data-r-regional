# ============================================================
# Indicador DSS
# ============================================================
# Fuente: World Bank
# Indicador: Gasto público en educación, total (% del PIB)
# Codigo indicador: SE.XPD.TOTL.GD.ZS
# Nivel geografico: País
# Frecuencia: Anual
# Region: America
# ============================================================

library(here)
library(jsonlite)
library(dplyr)
library(arrow)
library(readr)
library(fs)
library(glue)

source(here("herramientas/descargas/banco-mundial.R"))

codigo_indicador <- "SE.XPD.TOTL.GD.ZS"

  educ_raw <- descargar_banco_mundial(
    indicator_code = codigo_indicador
  )
  
  educ_PIB <- educ_raw |>
    
    transmute(
      iso3 = countryiso3code,
      anio = as.integer(date),
      valor = as.numeric(value),
      
      indicador =
        "Gasto público en educación, total (% del PIB)"
    ) 
  
  codigos_paises <- codelist |>
    select(
      country.name.es,
      country.name.en,
      iso3c,
      continent,
      region
    )|>
    filter(continent=="Americas")
  
  educ_PIB <- educ_PIB |>
    inner_join(codigos_paises,by=c("iso3"="iso3c"))|>
    mutate(pais=country.name.es)|>
    select(
      pais,
      iso3,
      indicador,
      anio,
      valor
    ) |>
    
    arrange(
      pais,
      anio
    )
  
 