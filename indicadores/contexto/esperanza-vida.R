# ============================================================
# Indicador de contexto regional
# Fuente: World Bank
# Indicador: Esperanza de vida al nacer, total (años)
# Codigo indicador: SP.DYN.LE00.IN
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
library(countrycode)

source(here("herramientas/descargas/banco-mundial.R"))

process_esperanza_vida <- function(output_dir = here("outputs")) {
  codigo_indicador <- "SP.DYN.LE00.IN"

  raw <- descargar_banco_mundial(indicator_code = codigo_indicador)

  indicador <- raw |>
    transmute(
      iso3 = countryiso3code,
      anio = as.integer(date),
      valor = as.numeric(value)
    )

  codigos_paises <- codelist |>
    select(country.name.es, country.name.en, iso3c, continent, region) |>
    filter(continent == "Americas")

  indicador <- indicador |>
    inner_join(codigos_paises, by = c("iso3" = "iso3c")) |>
    transmute(
      iso3,
      territorio = country.name.es,
      cod_local = NA_character_,
      anio,
      valor
    ) |>
    filter(!is.na(anio), !is.na(valor)) |>
    arrange(territorio, anio)

  dir_create(file.path(output_dir, "csv"))
  dir_create(file.path(output_dir, "parquet"))

  csv_file <- file.path(output_dir, "csv", "esperanza-vida.csv")
  parquet_file <- file.path(output_dir, "parquet", "esperanza-vida.parquet")

  write_csv(indicador, csv_file)
  write_parquet(indicador, parquet_file)

  message(glue("Context indicator processed: Esperanza de vida al nacer, total (años)"))
  message(glue("CSV: {csv_file}"))
  message(glue("Parquet: {parquet_file}"))

  invisible(list(data = indicador, output_files = c(csv_file, parquet_file)))
}

result <- process_esperanza_vida()
