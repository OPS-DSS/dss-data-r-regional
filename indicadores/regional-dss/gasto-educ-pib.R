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
    valor = as.numeric(value)
  )

codigos_paises <- codelist |>
  select(
    country.name.es,
    country.name.en,
    iso3c,
    continent,
    region
  ) |>
  filter(continent == "Americas")

# Dashboard contract:
# iso3, Territorio, cod_local, anio, valor.
# At regional level each country is a territory; cod_local does not apply.
educ_PIB <- educ_PIB |>
  inner_join(codigos_paises, by = c("iso3" = "iso3c")) |>
  transmute(
    iso3,
    Territorio = country.name.es,
    cod_local = NA_character_,
    anio,
    valor
  ) |>
  filter(!is.na(anio), !is.na(valor)) |>
  arrange(
    Territorio,
    anio
  )
