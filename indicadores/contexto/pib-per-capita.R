# ============================================================
# Indicador de contexto regional
# Fuente: World Bank
# Indicador: PIB per cápita (US$ corrientes)
# Codigo indicador: NY.GDP.PCAP.CD
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

process_pib_per_capita <- function(output_dir = here("outputs")) {
  codigo_indicador <- "NY.GDP.PCAP.CD"

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

  csv_file <- file.path(output_dir, "csv", "pib-per-capita.csv")
  parquet_file <- file.path(output_dir, "parquet", "pib-per-capita.parquet")

  write_csv(indicador, csv_file)
  write_parquet(indicador, parquet_file)

  message(glue("Context indicator processed: PIB per cápita (US$ corrientes)"))
  message(glue("CSV: {csv_file}"))
  message(glue("Parquet: {parquet_file}"))

  invisible(list(data = indicador, output_files = c(csv_file, parquet_file)))
}

result <- process_pib_per_capita()
