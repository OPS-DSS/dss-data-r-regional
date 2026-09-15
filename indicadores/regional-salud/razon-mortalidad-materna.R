# ============================================================
# Indicador DSS
# ============================================================
# Fuente: Naciones Unidas - Global SDG Database
# Indicador: Razón de mortalidad materna
# Codigo indicador: SH_STA_MORT
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
library(httr)
library(countrycode) #country.name.es country.name.en country.name.pt iso3c continent
source(here("herramientas/descargas/ods.R"))

codigo_indicador <- "SH_STA_MORT"

mortalidad_materna_raw <- descargar_un_ods(
  indicator_code = codigo_indicador
)

mortalidad_materna <- mortalidad_materna_raw |>
  
  transmute(
    country = geoAreaName,
    anio = as.integer(timePeriodStart),
    valor = as.numeric(value),
    indicador =
      "Razón de mortalidad materna"
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


mortalidad_materna <- mortalidad_materna |>
  inner_join(codigos_paises,by=c("country"="country.name.en"))|>
  mutate(pais=country.name.es,iso3=iso3c)|>
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
