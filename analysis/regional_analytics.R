# ============================================================
# Regional analytics: prioritized health indicators x DSS
# ============================================================
# Statistical computation belongs to the R data pipeline.
# Countries are the unit of analysis and indicators are joined by iso3 + anio.
# ============================================================

library(here)
library(dplyr)
library(tidyr)
library(readr)
library(arrow)
library(fs)
library(purrr)

MIN_COUNTRIES <- 10L

spearman_summary <- function(x, y) {
  ok <- is.finite(x) & is.finite(y)
  x <- x[ok]
  y <- y[ok]
  n <- length(x)
  if (n < MIN_COUNTRIES || length(unique(x)) < 2L || length(unique(y)) < 2L) {
    return(NULL)
  }
  test <- suppressWarnings(cor.test(x, y, method = "spearman", exact = FALSE))
  rho <- unname(test$estimate)

  # Fisher-z approximation retained for compatibility with PAM-DSS correlation chart.
  rho_clamped <- max(-0.9999, min(0.9999, rho))
  z <- atanh(rho_clamped)
  se <- 1 / sqrt(n - 3)

  tibble(
    correlacion = rho,
    ci_lower = tanh(z - 1.96 * se),
    ci_upper = tanh(z + 1.96 * se),
    p_value = test$p.value,
    n = as.integer(n)
  )
}

read_regional_indicator <- function(path, slug) {
  read_parquet(path) |>
    transmute(
      iso3 = as.character(iso3),
      territorio = as.character(territorio),
      anio = as.integer(anio),
      valor = as.numeric(valor),
      slug = slug
    ) |>
    filter(!is.na(iso3), !is.na(anio), is.finite(valor))
}

process_regional_analytics <- function(
    output_dir = here("outputs"),
    priority_slugs = c("razon-mortalidad-materna"),
    dss_slugs = c("gasto-educ-pib")) {

  parquet_dir <- file.path(output_dir, "parquet")
  csv_dir <- file.path(output_dir, "csv")
  dir_create(parquet_dir)
  dir_create(csv_dir)

  required <- c(priority_slugs, dss_slugs)
  missing <- required[!file.exists(file.path(parquet_dir, paste0(required, ".parquet")))]
  if (length(missing) > 0L) {
    stop("Missing regional indicator parquet(s): ", paste(missing, collapse = ", "))
  }

  indicators <- set_names(
    map(required, ~ read_regional_indicator(
      file.path(parquet_dir, paste0(.x, ".parquet")), .x
    )),
    required
  )

  scatter <- crossing(priorizado = priority_slugs, dss = dss_slugs) |>
    mutate(data = map2(priorizado, dss, function(p, d) {
      health <- indicators[[p]] |>
        select(iso3, territorio, anio, valor_salud = valor)
      determinant <- indicators[[d]] |>
        select(iso3, anio, valor_dss = valor)
      inner_join(health, determinant, by = c("iso3", "anio")) |>
        mutate(priorizado = p, dss = d, .before = 1)
    })) |>
    select(data) |>
    unnest(data) |>
    arrange(priorizado, dss, anio, territorio)

  correlations <- scatter |>
    group_by(priorizado, dss, anio) |>
    group_split() |>
    map_dfr(function(df) {
      result <- spearman_summary(df$valor_dss, df$valor_salud)
      if (is.null(result)) return(tibble())
      bind_cols(
        df |> slice(1) |> select(priorizado, dss, anio),
        result
      )
    }) |>
    group_by(priorizado, anio) |>
    arrange(desc(abs(correlacion)), .by_group = TRUE) |>
    mutate(rank_abs = row_number(), top10 = rank_abs <= 10L) |>
    ungroup()

  trends <- bind_rows(
    map_dfr(priority_slugs, function(slug) {
      indicators[[slug]] |>
        transmute(tipo = "priorizado", indicador = slug, iso3, territorio, anio, valor)
    }),
    map_dfr(dss_slugs, function(slug) {
      indicators[[slug]] |>
        transmute(tipo = "dss", indicador = slug, iso3, territorio, anio, valor)
    })
  ) |>
    arrange(tipo, indicador, anio, territorio)

  files <- c(
    correlations_csv = file.path(csv_dir, "regional-correlations.csv"),
    correlations_parquet = file.path(parquet_dir, "regional-correlations.parquet"),
    scatter_csv = file.path(csv_dir, "regional-scatter.csv"),
    scatter_parquet = file.path(parquet_dir, "regional-scatter.parquet"),
    trends_csv = file.path(csv_dir, "regional-trends.csv"),
    trends_parquet = file.path(parquet_dir, "regional-trends.parquet")
  )

  write_csv(correlations, files[["correlations_csv"]])
  write_parquet(correlations, files[["correlations_parquet"]])
  write_csv(scatter, files[["scatter_csv"]])
  write_parquet(scatter, files[["scatter_parquet"]])
  write_csv(trends, files[["trends_csv"]])
  write_parquet(trends, files[["trends_parquet"]])

  message("Regional analytics saved: correlations, scatter and trends.")
  invisible(list(
    correlations = correlations,
    scatter = scatter,
    trends = trends,
    output_files = unname(files)
  ))
}

if (!interactive()) {
  process_regional_analytics()
}
