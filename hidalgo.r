# ============================================================
# HIDALGO | LEYES DE INGRESOS MUNICIPALES 2025 Y 2026
# ------------------------------------------------------------
# 2025 fiscal -> publicados el 31/12/2024 -> alcances 5:24
# 2026 fiscal -> publicados el 31/12/2025 -> alcances 5:22
# (23 y 24 ya son cuotas/tarifas de agua)
#
# Guarda todo en:
# C:/Users/lmart/Downloads/Leyes Ingreso/hidalgo
# ============================================================

rm(list = ls())
options(error = NULL)
options(warn = 1)

# -----------------------------
# 1) PAQUETES
# -----------------------------
packs <- c(
  "httr2", "rvest", "xml2", "stringr", "stringi",
  "purrr", "dplyr", "tibble", "readr",
  "pdftools", "fs", "writexl"
)

instalar <- packs[!vapply(packs, requireNamespace, logical(1), quietly = TRUE)]
if (length(instalar) > 0) install.packages(instalar)

invisible(lapply(packs, library, character.only = TRUE))

# -----------------------------
# 2) CONFIG
# -----------------------------
`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0 || all(is.na(x))) y else x
}

ua <- paste(
  "Mozilla/5.0 (Windows NT 10.0; Win64; x64)",
  "AppleWebKit/537.36 (KHTML, like Gecko)",
  "Chrome/145.0.0.0 Safari/537.36"
)

base_site <- "https://periodico.hidalgo.gob.mx"

dir_base <- "C:/Users/lmart/Downloads/Leyes Ingreso/hidalgo"
dir_pdfs <- fs::path(dir_base, "pdfs")
dir_logs <- fs::path(dir_base, "logs")

fs::dir_create(dir_base, recurse = TRUE)
fs::dir_create(dir_pdfs, recurse = TRUE)
fs::dir_create(dir_logs, recurse = TRUE)

# 2025 fiscal = publicados 31 dic 2024, Número 53
# 2026 fiscal = publicados 31 dic 2025, Número 52
cfg <- tibble::tribble(
  ~ejercicio_fiscal, ~anio_publicacion, ~fecha_slug,                    ~numero_periodico, ~alcance_ini, ~alcance_fin,
  2025L,             2024L,             "31-de-diciembre-de-2024",      53L,               5L,           24L,
  2026L,             2025L,             "31-de-diciembre-de-2025",      52L,               5L,           22L
)

alcances_tbl <- purrr::map_dfr(seq_len(nrow(cfg)), function(i) {
  fila <- cfg[i, ]

  tibble::tibble(
    ejercicio_fiscal = fila$ejercicio_fiscal,
    anio_publicacion = fila$anio_publicacion,
    fecha_slug = fila$fecha_slug,
    numero_periodico = fila$numero_periodico,
    alcance = seq.int(fila$alcance_ini, fila$alcance_fin)
  )
}) |>
  dplyr::mutate(
    event_url = sprintf(
      "%s/?tribe_events=periodico-oficial-alcance-%d-del-%s",
      base_site, alcance, fecha_slug
    ),
    wpdmpro_url = sprintf(
      "%s/?wpdmpro=periodico-oficial-alcance-%d-del-%s",
      base_site, alcance, fecha_slug
    ),
    pdf_local = fs::path(
      dir_pdfs,
      sprintf("HGO_EJ%s_PUB%s_ALC%02d.pdf", ejercicio_fiscal, anio_publicacion, alcance)
    )
  )

# -----------------------------
# 3) HELPERS DE RED
# -----------------------------
req_base <- function(url) {
  httr2::request(url) |>
    httr2::req_user_agent(ua) |>
    httr2::req_timeout(90) |>
    httr2::req_retry(max_tries = 2)
}

safe_resp_raw <- function(url) {
  tryCatch({
    resp <- req_base(url) |>
      httr2::req_headers(
        Accept = "application/pdf,application/octet-stream,text/html,application/xhtml+xml,*/*"
      ) |>
      httr2::req_perform()

    list(
      ok = TRUE,
      url = resp$url %||% url,
      status = httr2::resp_status(resp),
      content_type = tolower(resp$headers[["content-type"]] %||% ""),
      body_raw = httr2::resp_body_raw(resp),
      error = NA_character_
    )
  }, error = function(e) {
    list(
      ok = FALSE,
      url = url,
      status = NA_integer_,
      content_type = NA_character_,
      body_raw = raw(),
      error = conditionMessage(e)
    )
  })
}

safe_html <- function(url) {
  out <- safe_resp_raw(url)
  if (!isTRUE(out$ok)) return(NA_character_)

  tryCatch(
    rawToChar(out$body_raw),
    error = function(e) NA_character_
  )
}

es_pdf_raw <- function(raw_vec) {
  if (length(raw_vec) < 5) return(FALSE)
  encabezado <- tryCatch(rawToChar(raw_vec[1:5]), error = function(e) "")
  identical(encabezado, "%PDF-")
}

# -----------------------------
# 4) HELPERS HTML / LINKS
# -----------------------------
normaliza_ascii <- function(x) {
  x <- enc2utf8(x)
  x <- stringi::stri_trans_general(x, "Latin-ASCII")
  x <- stringr::str_replace_all(x, "\u00A0", " ")
  x <- stringr::str_replace_all(x, "[\r\n\t]+", " ")
  x <- stringr::str_replace_all(x, "\\s+", " ")
  stringr::str_squish(x)
}

extrae_links_html <- function(html_txt, base_url) {
  if (is.na(html_txt) || !nzchar(html_txt)) return(character(0))

  out <- character(0)

  doc <- tryCatch(xml2::read_html(html_txt), error = function(e) NULL)

  if (!is.null(doc)) {
    attrs <- c(
      rvest::html_elements(doc, "a[href]") |> rvest::html_attr("href"),
      rvest::html_elements(doc, "iframe[src]") |> rvest::html_attr("src"),
      rvest::html_elements(doc, "embed[src]") |> rvest::html_attr("src"),
      rvest::html_elements(doc, "object[data]") |> rvest::html_attr("data"),
      rvest::html_elements(doc, "form[action]") |> rvest::html_attr("action"),
      rvest::html_elements(doc, "link[href]") |> rvest::html_attr("href"),
      rvest::html_elements(doc, "[data-href]") |> rvest::html_attr("data-href"),
      rvest::html_elements(doc, "[data-url]") |> rvest::html_attr("data-url"),
      rvest::html_elements(doc, "[data-downloadurl]") |> rvest::html_attr("data-downloadurl")
    )

    attrs <- attrs[!is.na(attrs) & nzchar(attrs)]
    out <- c(out, attrs)
  }

  patrones <- c(
    "(https?://[^\"'\\s<>()]+\\.pdf(?:\\?[^\"'\\s<>()]*)?)",
    "(https?://[^\"'\\s<>()]+\\?wpdmpro=[^\"'\\s<>()]+)",
    "(https?://[^\"'\\s<>()]+\\?wpdmdl=\\d+[^\"'\\s<>()]*)",
    "(/[^\"'\\s<>()]+\\.pdf(?:\\?[^\"'\\s<>()]*)?)",
    "(/\\?wpdmpro=[^\"'\\s<>()]+)",
    "(/\\?wpdmdl=\\d+[^\"'\\s<>()]*)"
  )

  regex_hits <- unlist(
    lapply(patrones, function(p) stringr::str_extract_all(html_txt, p)[[1]]),
    use.names = FALSE
  )

  regex_hits <- regex_hits[!is.na(regex_hits) & nzchar(regex_hits)]
  out <- c(out, regex_hits)

  # Variantes wpdmpro
  slug <- stringr::str_match(html_txt, "\\?wpdmpro=([a-zA-Z0-9\\-_%]+)")[, 2]
  if (!is.na(slug)) {
    out <- c(
      out,
      paste0(base_site, "/?wpdmpro=", slug),
      paste0(base_site, "/?wpdmpro=", slug, "&download=1"),
      paste0(base_site, "/?wpdmpro=", slug, "&ind=0&download=1")
    )
  }

  # IDs wpdmdl
  ids <- stringr::str_match_all(html_txt, "wpdmdl=(\\d+)")[[1]]
  if (nrow(ids) > 0) {
    out <- c(out, paste0(base_site, "/?wpdmdl=", unique(ids[, 2])))
  }

  out <- out[!is.na(out) & nzchar(out)]
  out <- tryCatch(xml2::url_absolute(out, base_url), error = function(e) out)
  out <- unique(out)

  out <- out[
    !stringr::str_detect(
      out,
      "file-type-icons|\\.svg$|\\.png$|\\.jpg$|\\.jpeg$|\\.webp$|facebook|twitter|instagram|youtube|forms\\.gle|google\\.com/calendar|outlook\\.office|outlook\\.live"
    )
  ]

  out
}

# Construye fallbacks directos al repositorio público
construye_fallbacks_pdf <- function(anio_publicacion, alcance, numero_periodico) {
  unique(c(
    sprintf("%s/POEHpdfpublic/%d_dic_31_alc%d_%d.pdf", base_site, anio_publicacion, alcance, numero_periodico),
    sprintf("%s/POEHpdfpublic/%d_dic_31_alc%02d_%d.pdf", base_site, anio_publicacion, alcance, numero_periodico),
    sprintf("%s/POEHpdfpublic/%d_dic_31_al%d_%d.pdf", base_site, anio_publicacion, alcance, numero_periodico),
    sprintf("%s/POEHpdfpublic/%d_dic_31_al%02d_%d.pdf", base_site, anio_publicacion, alcance, numero_periodico)
  ))
}

resolver_pdf_real <- function(urls_iniciales, max_visitas = 40) {
  cola <- unique(urls_iniciales[!is.na(urls_iniciales) & nzchar(urls_iniciales)])
  visitados <- character(0)

  if (length(cola) == 0) {
    return(list(ok = FALSE, pdf_raw = NULL, pdf_url = NA_character_, detalle = "Sin URLs candidatas"))
  }

  while (length(cola) > 0 && length(visitados) < max_visitas) {
    u <- cola[1]
    cola <- cola[-1]

    if (u %in% visitados) next
    visitados <- c(visitados, u)

    resp <- safe_resp_raw(u)
    if (!isTRUE(resp$ok)) next

    # Caso PDF real
    if (es_pdf_raw(resp$body_raw) || stringr::str_detect(resp$content_type %||% "", "application/pdf")) {
      return(list(
        ok = TRUE,
        pdf_raw = resp$body_raw,
        pdf_url = resp$url,
        detalle = "PDF valido detectado"
      ))
    }

    # Caso HTML
    html_txt <- tryCatch(rawToChar(resp$body_raw), error = function(e) NA_character_)
    if (!is.na(html_txt)) {
      nuevos <- extrae_links_html(html_txt, resp$url)
      nuevos <- setdiff(nuevos, visitados)
      if (length(nuevos) > 0) {
        cola <- unique(c(nuevos, cola))
      }
    }
  }

  list(ok = FALSE, pdf_raw = NULL, pdf_url = NA_character_, detalle = "No se pudo resolver un PDF real")
}

guardar_pdf_resuelto <- function(urls_candidatas, destfile) {
  urls_candidatas <- unique(urls_candidatas[!is.na(urls_candidatas) & nzchar(urls_candidatas)])

  if (length(urls_candidatas) == 0) {
    return(list(ok = FALSE, pdf_url = NA_character_, detalle = "Sin candidatos"))
  }

  res <- resolver_pdf_real(urls_candidatas)

  if (isTRUE(res$ok) && !is.null(res$pdf_raw)) {
    if (fs::file_exists(destfile)) fs::file_delete(destfile)
    writeBin(res$pdf_raw, destfile)

    if (fs::file_exists(destfile) && fs::file_info(destfile)$size > 1024) {
      return(list(ok = TRUE, pdf_url = res$pdf_url, detalle = res$detalle))
    }
  }

  list(ok = FALSE, pdf_url = NA_character_, detalle = res$detalle %||% "No se encontró PDF real")
}

# -----------------------------
# 5) HELPERS DE PDF / TEXTO
# -----------------------------
leer_pdf_normalizado <- function(path_pdf) {
  txt <- pdftools::pdf_text(path_pdf)
  txt <- paste(txt, collapse = "\n")
  txt <- normaliza_ascii(txt)
  toupper(txt)
}

extraer_bloques_ley <- function(texto, ejercicio_fiscal) {
  patron_ley <- paste0(
    "(DECRETO\\s+NUMERO\\.?\\s*\\d+\\s*[-–.]?\\s*LXVI\\s+)?",
    "(QUE\\s+CONTIENE\\s+LA\\s+)?",
    "LEY\\s+DE\\s+INGRESOS\\s+PARA\\s+EL\\s+MUNICIPIO\\s+DE\\s+[A-Z .'-]+?,?\\s+HIDALGO,?\\s+CORRESPONDIENTE\\s+AL\\s+EJERCICIO\\s+FISCAL\\s+",
    ejercicio_fiscal
  )

  patron_vigencia <- paste0(
    "DECRETO\\s+NUMERO\\.?\\s*\\d+\\s*[-–.]?\\s*LXVI\\s+QUE\\s+DECLARA\\s+LA\\s+VIGENCIA\\s+DE\\s+LA\\s+LEY\\s+DE\\s+INGRESOS\\s+DEL\\s+MUNICIPIO\\s+DE\\s+[A-Z .'-]+?,?\\s+HIDALGO.*?REGIRA\\s+DURANTE\\s+EL\\s+EJERCICIO\\s+FISCAL\\s+DEL\\s+ANO\\s+",
    ejercicio_fiscal
  )

  patron_inicio <- paste0("(", patron_ley, "|", patron_vigencia, ")")

  locs <- stringr::str_locate_all(texto, patron_inicio)[[1]]

  if (is.null(locs) || nrow(locs) == 0) return(character(0))

  starts <- locs[, 1]
  ends <- c(starts[-1] - 1, nchar(texto))

  purrr::map2_chr(starts, ends, ~ stringr::str_sub(texto, .x, .y))
}

extraer_decreto_bloque <- function(bloque) {
  stringr::str_match(bloque, "DECRETO\\s+NUMERO\\.?\\s*(\\d{1,3})")[, 2]
}

extraer_municipio_bloque <- function(bloque) {
  m <- stringr::str_match(
    bloque,
    "LEY\\s+DE\\s+INGRESOS\\s+PARA\\s+EL\\s+MUNICIPIO\\s+DE\\s+([A-Z .'-]+?),?\\s+HIDALGO"
  )[, 2]

  if (is.na(m)) {
    m <- stringr::str_match(
      bloque,
      "VIGENCIA\\s+DE\\s+LA\\s+LEY\\s+DE\\s+INGRESOS\\s+DEL\\s+MUNICIPIO\\s+DE\\s+([A-Z .'-]+?),?\\s+HIDALGO"
    )[, 2]
  }

  stringr::str_squish(m)
}

extraer_total_bloque <- function(bloque) {
  num_pat <- "([0-9]{1,3}(?:,\\s?[0-9]{3})+(?:\\.\\d{2})|[0-9]+(?:\\.\\d{2}))"

  patrones <- c(
    paste0("TOTAL\\s+DE\\s+INGRESOS\\s*[:\\$]?\\s*", num_pat),
    paste0("TOTAL\\s+INGRESOS\\s*[:\\$]?\\s*", num_pat),
    paste0("INGRESOS\\s+TOTALES\\s*[:\\$]?\\s*", num_pat),
    paste0("IMPORTE\\s+TOTAL\\s*[:\\$]?\\s*", num_pat),
    paste0("MONTO\\s+TOTAL\\s*[:\\$]?\\s*", num_pat),
    paste0("SE\\s+PERCIBIRAN\\s+LOS\\s+INGRESOS\\s+POR\\s+UN\\s+TOTAL\\s+DE\\s*[:\\$]?\\s*", num_pat),
    paste0("PERCIBIRA\\s+LOS\\s+INGRESOS.*?TOTAL\\s+DE\\s*[:\\$]?\\s*", num_pat)
  )

  for (p in patrones) {
    m <- stringr::str_match(bloque, p)[, 2]
    if (!is.na(m)) return(stringr::str_replace_all(m, "\\s+", ""))
  }

  ventanas <- c(
    stringr::str_extract(bloque, "ARTICULO\\s+1.{0,1000}"),
    stringr::str_extract(bloque, "ART\\.?\\s*1.{0,1000}"),
    paste(stringr::str_extract_all(bloque, "TOTAL.{0,300}")[[1]], collapse = " "),
    paste(stringr::str_extract_all(bloque, "PERCIBIRA.{0,400}")[[1]], collapse = " ")
  )

  ventanas <- ventanas[!is.na(ventanas) & nzchar(ventanas)]

  if (length(ventanas) > 0) {
    todos <- unlist(stringr::str_extract_all(paste(ventanas, collapse = " "), num_pat), use.names = FALSE)
    todos <- todos[!is.na(todos)]

    if (length(todos) > 0) {
      todos_limpios <- stringr::str_replace_all(todos, "\\s+", "")
      nums <- suppressWarnings(as.numeric(gsub(",", "", todos_limpios)))
      if (!all(is.na(nums))) {
        return(todos_limpios[which.max(nums)])
      }
    }
  }

  NA_character_
}

convierte_monto <- function(x) {
  if (length(x) == 0 || is.na(x) || !nzchar(x)) return(NA_real_)
  suppressWarnings(as.numeric(gsub(",", "", x)))
}

extraer_desde_pdf <- function(path_pdf, ejercicio_fiscal, pdf_url = NA_character_) {
  texto <- leer_pdf_normalizado(path_pdf)
  bloques <- extraer_bloques_ley(texto, ejercicio_fiscal)

  if (length(bloques) == 0) {
    return(
      tibble::tibble(
        decreto = NA_character_,
        municipio = NA_character_,
        tipo_decreto = NA_character_,
        total_ingresos_texto = NA_character_,
        total_ingresos = NA_real_,
        pdf_local = path_pdf,
        pdf_url = pdf_url,
        hallo_total = FALSE
      )
    )
  }

  purrr::map_dfr(bloques, function(b) {
    tibble::tibble(
      decreto = extraer_decreto_bloque(b),
      municipio = extraer_municipio_bloque(b),
      tipo_decreto = ifelse(
        stringr::str_detect(b, "VIGENCIA\\s+DE\\s+LA\\s+LEY\\s+DE\\s+INGRESOS"),
        "Vigencia de ley previa",
        "Ley de ingresos"
      ),
      total_ingresos_texto = extraer_total_bloque(b),
      total_ingresos = convierte_monto(extraer_total_bloque(b)),
      pdf_local = path_pdf,
      pdf_url = pdf_url,
      hallo_total = !is.na(extraer_total_bloque(b))
    )
  })
}

# -----------------------------
# 6) DESCARGAR LOS ALCANCES
# -----------------------------
descargas_list <- vector("list", nrow(alcances_tbl))

for (i in seq_len(nrow(alcances_tbl))) {
  fila <- alcances_tbl[i, ]

  message(sprintf(
    "[%s] Ejercicio %s | alcance %s",
    i, fila$ejercicio_fiscal, fila$alcance
  ))

  html_event <- safe_html(fila$event_url)

  links_evento <- extrae_links_html(html_event, fila$event_url)

  candidatos <- unique(c(
    construye_fallbacks_pdf(
      anio_publicacion = fila$anio_publicacion,
      alcance = fila$alcance,
      numero_periodico = fila$numero_periodico
    ),
    fila$wpdmpro_url,
    paste0(fila$wpdmpro_url, "&download=1"),
    paste0(fila$wpdmpro_url, "&ind=0&download=1"),
    fila$event_url,
    links_evento
  ))

  descarga <- guardar_pdf_resuelto(candidatos, fila$pdf_local)

  descargas_list[[i]] <- tibble::tibble(
    ejercicio_fiscal = fila$ejercicio_fiscal,
    anio_publicacion = fila$anio_publicacion,
    alcance = fila$alcance,
    numero_periodico = fila$numero_periodico,
    event_url = fila$event_url,
    wpdmpro_url = fila$wpdmpro_url,
    pdf_local = fila$pdf_local,
    pdf_url = descarga$pdf_url,
    descargado = descarga$ok,
    detalle_descarga = descarga$detalle
  )
}

descargas <- dplyr::bind_rows(descargas_list)

# -----------------------------
# 7) EXTRAER TOTALES
# -----------------------------
resultados_list <- vector("list", nrow(descargas))

for (i in seq_len(nrow(descargas))) {
  fila <- descargas[i, ]

  message(sprintf(
    "[%s] Extrayendo ejercicio %s | alcance %s",
    i, fila$ejercicio_fiscal, fila$alcance
  ))

  if (!isTRUE(fila$descargado) || !fs::file_exists(fila$pdf_local)) {
    resultados_list[[i]] <- tibble::tibble(
      ejercicio_fiscal = fila$ejercicio_fiscal,
      anio_publicacion = fila$anio_publicacion,
      alcance = fila$alcance,
      decreto = NA_character_,
      municipio = NA_character_,
      tipo_decreto = NA_character_,
      total_ingresos_texto = NA_character_,
      total_ingresos = NA_real_,
      pdf_local = fila$pdf_local,
      pdf_url = fila$pdf_url,
      hallo_total = FALSE
    )
    next
  }

  tmp <- tryCatch(
    extraer_desde_pdf(
      path_pdf = fila$pdf_local,
      ejercicio_fiscal = fila$ejercicio_fiscal,
      pdf_url = fila$pdf_url
    ),
    error = function(e) {
      tibble::tibble(
        decreto = NA_character_,
        municipio = NA_character_,
        tipo_decreto = NA_character_,
        total_ingresos_texto = NA_character_,
        total_ingresos = NA_real_,
        pdf_local = fila$pdf_local,
        pdf_url = fila$pdf_url,
        hallo_total = FALSE
      )
    }
  )

  resultados_list[[i]] <- tmp |>
    dplyr::mutate(
      ejercicio_fiscal = fila$ejercicio_fiscal,
      anio_publicacion = fila$anio_publicacion,
      alcance = fila$alcance,
      .before = 1
    )
}

resultados <- dplyr::bind_rows(resultados_list) |>
  dplyr::distinct(ejercicio_fiscal, decreto, municipio, .keep_all = TRUE) |>
  dplyr::arrange(ejercicio_fiscal, suppressWarnings(as.numeric(decreto)), municipio)

# -----------------------------
# 8) EXPORTAR
# -----------------------------
path_csv_todos <- fs::path(dir_base, "HIDALGO_LEYES_INGRESOS_2025_2026_TOTALES.csv")
path_csv_2025  <- fs::path(dir_base, "HIDALGO_LEYES_INGRESOS_2025_TOTALES.csv")
path_csv_2026  <- fs::path(dir_base, "HIDALGO_LEYES_INGRESOS_2026_TOTALES.csv")
path_xlsx      <- fs::path(dir_base, "HIDALGO_LEYES_INGRESOS_2025_2026_TOTALES.xlsx")
path_log_desc  <- fs::path(dir_logs, "log_descargas.csv")
path_pend      <- fs::path(dir_logs, "pendientes.csv")

readr::write_csv(descargas, path_log_desc, na = "")
readr::write_csv(
  resultados |>
    dplyr::filter(is.na(total_ingresos) | !hallo_total),
  path_pend,
  na = ""
)

readr::write_csv(resultados, path_csv_todos, na = "")
readr::write_csv(
  resultados |>
    dplyr::filter(ejercicio_fiscal == 2025),
  path_csv_2025,
  na = ""
)
readr::write_csv(
  resultados |>
    dplyr::filter(ejercicio_fiscal == 2026),
  path_csv_2026,
  na = ""
)

writexl::write_xlsx(
  list(
    totales_todos = resultados,
    ejercicio_2025 = resultados |>
      dplyr::filter(ejercicio_fiscal == 2025),
    ejercicio_2026 = resultados |>
      dplyr::filter(ejercicio_fiscal == 2026),
    descargas = descargas,
    pendientes = resultados |>
      dplyr::filter(is.na(total_ingresos) | !hallo_total)
  ),
  path_xlsx
)

# -----------------------------
# 9) RESUMEN
# -----------------------------
cat("\n========================================\n")
cat("HIDALGO | LEYES DE INGRESOS 2025 y 2026\n")
cat("========================================\n")
cat("Carpeta base: ", dir_base, "\n", sep = "")
cat("PDFs descargados: ", sum(descargas$descargado, na.rm = TRUE), " de ", nrow(descargas), "\n", sep = "")
cat("Registros detectados: ", nrow(resultados), "\n", sep = "")
cat("Con total detectado: ", sum(!is.na(resultados$total_ingresos)), "\n", sep = "")
cat("Sin total detectado: ", sum(is.na(resultados$total_ingresos)), "\n", sep = "")
cat("CSV todos: ", path_csv_todos, "\n", sep = "")
cat("CSV 2025: ", path_csv_2025, "\n", sep = "")
cat("CSV 2026: ", path_csv_2026, "\n", sep = "")
cat("XLSX: ", path_xlsx, "\n", sep = "")
cat("LOG descargas: ", path_log_desc, "\n", sep = "")
cat("Pendientes: ", path_pend, "\n", sep = "")

cat("\n--- Primeros resultados ---\n")
print(
  resultados |>
    dplyr::select(
      ejercicio_fiscal, alcance, decreto, municipio,
      total_ingresos_texto, total_ingresos
    ) |>
    head(20)
)

cat("\n--- Pendientes ---\n")
print(
  resultados |>
    dplyr::filter(is.na(total_ingresos) | !hallo_total) |>
    dplyr::select(ejercicio_fiscal, alcance, decreto, municipio, pdf_url)
)
