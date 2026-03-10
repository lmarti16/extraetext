# ============================================================
# HIDALGO | LEYES DE INGRESOS MUNICIPALES 2025 y 2026
# ------------------------------------------------------------
# Qué hace:
# 1) Recorre los alcances 5 a 24 del 31 de diciembre de 2024
#    (ejercicio fiscal 2025) y del 31 de diciembre de 2025
#    (ejercicio fiscal 2026).
# 2) Descarga el PDF real de cada alcance.
# 3) Valida que el archivo descargado sea PDF de verdad.
# 4) Extrae por municipio el bloque de Ley de Ingresos.
# 5) Busca el TOTAL DE INGRESOS con varias redacciones.
# 6) Exporta CSV, XLSX y logs.
# ------------------------------------------------------------
# Guarda todo en:
# C:/Users/lmart/Downloads/Leyes Ingreso/hidalgo
# ============================================================

rm(list = ls())

# -----------------------------
# 1) PAQUETES
# -----------------------------
packs <- c(
  "httr2", "rvest", "xml2", "stringr", "stringi",
  "purrr", "dplyr", "tibble", "tidyr", "readr",
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

# Configuración de años:
# 2025 fiscal -> publicado 31 dic 2024
# 2026 fiscal -> publicado 31 dic 2025
configuraciones <- tibble::tribble(
  ~ejercicio_fiscal, ~anio_publicacion, ~fecha_slug,          ~mes_abrev, ~dia, ~numero_periodico_esperado,
  2025L,             2024L,             "31-de-diciembre-de-2024", "dic",     31L, 53L,
  2026L,             2025L,             "31-de-diciembre-de-2025", "dic",     31L, 52L
)

alcances_tbl <- tidyr::expand_grid(
  configuraciones,
  alcance = 5:24
) |>
  dplyr::mutate(
    event_url = sprintf(
      "%s/?tribe_events=periodico-oficial-alcance-%d-del-%s",
      base_site, alcance, fecha_slug
    ),
    slug_wpdmpro = sprintf(
      "%s/?wpdmpro=periodico-oficial-alcance-%d-del-%s",
      base_site, alcance, fecha_slug
    ),
    pdf_local = fs::path(
      dir_pdfs,
      sprintf("HGO_%d_%d_ALC%02d.pdf", ejercicio_fiscal, anio_publicacion, alcance)
    )
  )

# -----------------------------
# 3) HELPERS DE RED
# -----------------------------
req_base <- function(url) {
  httr2::request(url) |>
    httr2::req_user_agent(ua) |>
    httr2::req_timeout(120) |>
    httr2::req_retry(max_tries = 3)
}

obtener_html <- function(url) {
  req_base(url) |>
    httr2::req_headers(
      Accept = "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8"
    ) |>
    httr2::req_perform() |>
    httr2::resp_body_string()
}

obtener_respuesta <- function(url) {
  resp <- req_base(url) |>
    httr2::req_headers(
      Accept = "application/pdf,application/octet-stream,text/html,application/xhtml+xml,*/*"
    ) |>
    httr2::req_perform()

  list(
    url_final = resp$url %||% url,
    status = httr2::resp_status(resp),
    content_type = tolower(resp$headers[["content-type"]] %||% ""),
    body_raw = httr2::resp_body_raw(resp)
  )
}

es_pdf_raw <- function(raw_vec) {
  if (length(raw_vec) < 5) return(FALSE)
  encabezado <- tryCatch(rawToChar(raw_vec[1:5]), error = function(e) "")
  identical(encabezado, "%PDF-")
}

# -----------------------------
# 4) HELPERS DE TEXTO / HTML
# -----------------------------
normaliza_ascii <- function(x) {
  x <- enc2utf8(x)
  x <- stringi::stri_trans_general(x, "Latin-ASCII")
  x <- stringr::str_replace_all(x, "\u00A0", " ")
  x <- stringr::str_replace_all(x, "[\r\n\t]+", " ")
  x <- stringr::str_replace_all(x, "\\s+", " ")
  stringr::str_squish(x)
}

html_a_texto_normalizado <- function(html_txt) {
  doc <- xml2::read_html(html_txt)
  txt <- rvest::html_text2(doc)
  normaliza_ascii(toupper(txt))
}

extrae_links_html <- function(html_txt, base_url) {
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
      rvest::html_elements(doc, "[data-downloadurl]") |> rvest::html_attr("data-downloadurl"),
      rvest::html_elements(doc, "[onclick]") |> rvest::html_attr("onclick")
    )

    attrs <- attrs[!is.na(attrs) & nzchar(attrs)]
    out <- c(out, attrs)
  }

  bloques_texto <- c(html_txt)

  if (!is.null(doc)) {
    scripts <- rvest::html_elements(doc, "script") |> rvest::html_text2()
    scripts <- scripts[!is.na(scripts) & nzchar(scripts)]
    bloques_texto <- c(bloques_texto, scripts)
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
    lapply(bloques_texto, function(txt) {
      unlist(lapply(patrones, function(p) stringr::str_extract_all(txt, p)[[1]]), use.names = FALSE)
    }),
    use.names = FALSE
  )

  regex_hits <- regex_hits[!is.na(regex_hits) & nzchar(regex_hits)]
  out <- c(out, regex_hits)

  # Si detecta un slug wpdmpro suelto, genera variantes
  slug <- stringr::str_match(
    html_txt,
    "\\?wpdmpro=([a-zA-Z0-9\\-_%]+)"
  )[, 2]

  if (!is.na(slug)) {
    out <- c(
      out,
      paste0(base_site, "/?wpdmpro=", slug),
      paste0(base_site, "/?wpdmpro=", slug, "&download=1"),
      paste0(base_site, "/?wpdmpro=", slug, "&ind=0&download=1")
    )
  }

  # IDs wpdmdl
  ids_wpdmdl <- stringr::str_match_all(html_txt, "wpdmdl=(\\d+)")[[1]]
  if (nrow(ids_wpdmdl) > 0) {
    ids_u <- unique(ids_wpdmdl[, 2])
    out <- c(out, paste0(base_site, "/?wpdmdl=", ids_u))
  }

  # Construye absolutos
  out <- out[!is.na(out) & nzchar(out)]
  out <- tryCatch(xml2::url_absolute(out, base_url), error = function(e) out)
  out <- unique(out)

  # Filtra basura
  out <- out[
    !stringr::str_detect(
      out,
      "file-type-icons|\\.svg$|\\.png$|\\.jpg$|\\.jpeg$|\\.webp$|facebook|twitter|instagram|youtube|forms\\.gle|outlook\\.office|outlook\\.live|google\\.com/calendar"
    )
  ]

  out
}

extrae_numero_periodico <- function(html_txt, numero_esperado = NA_integer_) {
  txt <- html_a_texto_normalizado(html_txt)

  m <- stringr::str_match(txt, "NUMERO\\s*:?\\s*(\\d{1,3})")[, 2]
  numero <- suppressWarnings(as.integer(m))

  if (is.na(numero) && !is.na(numero_esperado)) {
    numero <- as.integer(numero_esperado)
  }

  numero
}

construye_fallbacks_pdf <- function(anio_publicacion, mes_abrev, dia, alcance, numero_periodico) {
  if (is.na(numero_periodico)) return(character(0))

  c(
    sprintf("%s/POEHpdfpublic/%d_%s_%02d_alc%d_%d.pdf", base_site, anio_publicacion, mes_abrev, dia, alcance, numero_periodico),
    sprintf("%s/POEHpdfpublic/%d_%s_%02d_alc%02d_%d.pdf", base_site, anio_publicacion, mes_abrev, dia, alcance, numero_periodico),
    sprintf("%s/POEHpdfpublic/%d_%s_%d_alc%d_%d.pdf", base_site, anio_publicacion, mes_abrev, dia, alcance, numero_periodico),
    sprintf("%s/POEHpdfpublic/%d_%s_%d_alc%02d_%d.pdf", base_site, anio_publicacion, mes_abrev, dia, alcance, numero_periodico)
  ) |>
    unique()
}

resolver_pdf_real <- function(urls_iniciales, max_visitas = 50) {
  cola <- unique(urls_iniciales[!is.na(urls_iniciales) & nzchar(urls_iniciales)])
  visitados <- character(0)

  if (length(cola) == 0) {
    return(list(
      ok = FALSE,
      pdf_raw = NULL,
      pdf_url = NA_character_,
      detalle = "Sin URLs candidatas"
    ))
  }

  while (length(cola) > 0 && length(visitados) < max_visitas) {
    u <- cola[1]
    cola <- cola[-1]

    if (u %in% visitados) next
    visitados <- c(visitados, u)

    resp <- tryCatch(obtener_respuesta(u), error = function(e) NULL)
    if (is.null(resp)) next

    # PDF real
    if (es_pdf_raw(resp$body_raw) || stringr::str_detect(resp$content_type, "application/pdf")) {
      return(list(
        ok = TRUE,
        pdf_raw = resp$body_raw,
        pdf_url = resp$url_final,
        detalle = "PDF valido detectado"
      ))
    }

    # HTML -> seguir buscando más links
    html_txt <- NA_character_
    if (length(resp$body_raw) > 0) {
      html_txt <- tryCatch(rawToChar(resp$body_raw), error = function(e) NA_character_)
    }

    if (!is.na(html_txt)) {
      nuevos <- extrae_links_html(html_txt, resp$url_final)
      nuevos <- setdiff(nuevos, visitados)
      if (length(nuevos) > 0) {
        cola <- unique(c(nuevos, cola))
      }
    }
  }

  list(
    ok = FALSE,
    pdf_raw = NULL,
    pdf_url = NA_character_,
    detalle = "No se pudo resolver un PDF real"
  )
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

    tam <- fs::file_info(destfile)$size
    if (isTRUE(tam > 1024)) {
      return(list(ok = TRUE, pdf_url = res$pdf_url, detalle = res$detalle))
    }
  }

  list(ok = FALSE, pdf_url = NA_character_, detalle = "No se encontró PDF real")
}

# -----------------------------
# 5) HELPERS DE PDF / EXTRACCIÓN
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
    "DECRETO\\s+NUMERO\\.?\\s*\\d+\\s*[-–.]?\\s*LXVI\\s+QUE\\s+DECLARA\\s+LA\\s+VIGENCIA\\s+DE\\s+LA\\s+LEY\\s+DE\\s+INGRESOS\\s+DEL\\s+MUNICIPIO\\s+DE\\s+[A-Z .'-]+?,?\\s+HIDALGO.*?REGIRA\\s+DURANTE\\s+EL\\s+EJERCICIO\\s+FISCAL\\s+",
    ejercicio_fiscal
  )

  patron_inicio <- paste0("(", patron_ley, "|", patron_vigencia, ")")

  locs <- stringr::str_locate_all(texto, patron_inicio)[[1]]

  if (is.null(locs) || nrow(locs) == 0) return(character(0))

  starts <- locs[, 1]
  ends <- c(starts[-1] - 1, nchar(texto))

  purrr::map2_chr(starts, ends, ~ stringr::str_sub(texto, .x, .y))
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

extraer_decreto_bloque <- function(bloque) {
  stringr::str_match(bloque, "DECRETO\\s+NUMERO\\.?\\s*(\\d{3})")[, 2]
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
    stringr::str_extract(bloque, "ARTICULO\\s+1.{0,900}"),
    stringr::str_extract(bloque, "ART\\.?\\s*1.{0,900}"),
    paste(stringr::str_extract_all(bloque, "TOTAL.{0,250}")[[1]], collapse = " "),
    paste(stringr::str_extract_all(bloque, "PERCIBIRA.{0,350}")[[1]], collapse = " ")
  )

  ventanas <- ventanas[!is.na(ventanas) & nzchar(ventanas)]

  if (length(ventanas) > 0) {
    todos <- unlist(
      stringr::str_extract_all(paste(ventanas, collapse = " "), num_pat),
      use.names = FALSE
    )
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
    tipo_dec <- if (stringr::str_detect(b, "VIGENCIA\\s+DE\\s+LA\\s+LEY\\s+DE\\s+INGRESOS")) {
      "Vigencia de ley previa"
    } else {
      "Ley de ingresos"
    }

    total_txt <- extraer_total_bloque(b)

    tibble::tibble(
      decreto = extraer_decreto_bloque(b),
      municipio = extraer_municipio_bloque(b),
      tipo_decreto = tipo_dec,
      total_ingresos_texto = total_txt,
      total_ingresos = convierte_monto(total_txt),
      pdf_local = path_pdf,
      pdf_url = pdf_url,
      hallo_total = !is.na(total_txt)
    )
  })
}

# -----------------------------
# 6) DESCARGA DE ALCANCES
# -----------------------------
descargas <- purrr::pmap_dfr(
  list(
    alcances_tbl$ejercicio_fiscal,
    alcances_tbl$anio_publicacion,
    alcances_tbl$mes_abrev,
    alcances_tbl$dia,
    alcances_tbl$alcance,
    alcances_tbl$event_url,
    alcances_tbl$slug_wpdmpro,
    alcances_tbl$pdf_local,
    alcances_tbl$numero_periodico_esperado
  ),
  function(ejercicio_fiscal, anio_publicacion, mes_abrev, dia, alcance, event_url, slug_wpdmpro, pdf_local, numero_periodico_esperado) {

    html_event <- tryCatch(obtener_html(event_url), error = function(e) NA_character_)

    numero_periodico <- if (!is.na(html_event)) {
      extrae_numero_periodico(html_event, numero_esperado = numero_periodico_esperado)
    } else {
      numero_periodico_esperado
    }

    links_evento <- if (!is.na(html_event)) {
      extrae_links_html(html_event, event_url)
    } else {
      character(0)
    }

    candidatos <- unique(c(
      event_url,
      links_evento,
      slug_wpdmpro,
      paste0(slug_wpdmpro, "&download=1"),
      paste0(slug_wpdmpro, "&ind=0&download=1"),
      construye_fallbacks_pdf(anio_publicacion, mes_abrev, dia, alcance, numero_periodico)
    ))

    descarga <- guardar_pdf_resuelto(candidatos, pdf_local)

    tibble::tibble(
      ejercicio_fiscal = ejercicio_fiscal,
      anio_publicacion = anio_publicacion,
      alcance = alcance,
      event_url = event_url,
      slug_wpdmpro = slug_wpdmpro,
      numero_periodico = numero_periodico,
      pdf_local = pdf_local,
      pdf_url = descarga$pdf_url,
      descargado = descarga$ok,
      detalle_descarga = descarga$detalle
    )
  }
)

# -----------------------------
# 7) EXTRAER TOTALES
# -----------------------------
resultados <- purrr::pmap_dfr(
  list(
    descargas$ejercicio_fiscal,
    descargas$anio_publicacion,
    descargas$alcance,
    descargas$pdf_local,
    descargas$pdf_url,
    descargas$descargado
  ),
  function(ejercicio_fiscal, anio_publicacion, alcance, pdf_local, pdf_url, descargado) {

    if (!isTRUE(descargado) || !fs::file_exists(pdf_local)) {
      return(
        tibble::tibble(
          ejercicio_fiscal = ejercicio_fiscal,
          anio_publicacion = anio_publicacion,
          alcance = alcance,
          decreto = NA_character_,
          municipio = NA_character_,
          tipo_decreto = NA_character_,
          total_ingresos_texto = NA_character_,
          total_ingresos = NA_real_,
          pdf_local = pdf_local,
          pdf_url = pdf_url,
          hallo_total = FALSE
        )
      )
    }

    out <- tryCatch(
      extraer_desde_pdf(pdf_local, ejercicio_fiscal = ejercicio_fiscal, pdf_url = pdf_url),
      error = function(e) {
        tibble::tibble(
          decreto = NA_character_,
          municipio = NA_character_,
          tipo_decreto = NA_character_,
          total_ingresos_texto = NA_character_,
          total_ingresos = NA_real_,
          pdf_local = pdf_local,
          pdf_url = pdf_url,
          hallo_total = FALSE
        )
      }
    )

    out |>
      dplyr::mutate(
        ejercicio_fiscal = ejercicio_fiscal,
        anio_publicacion = anio_publicacion,
        alcance = alcance,
        .before = 1
      )
  }
) |>
  dplyr::distinct(ejercicio_fiscal, decreto, municipio, .keep_all = TRUE) |>
  dplyr::arrange(ejercicio_fiscal, suppressWarnings(as.numeric(decreto)), municipio)

# -----------------------------
# 8) EXPORTAR
# -----------------------------
path_csv_todos <- fs::path(dir_base, "HIDALGO_LEYES_INGRESOS_2025_2026_TOTALES.csv")
path_xlsx_todos <- fs::path(dir_base, "HIDALGO_LEYES_INGRESOS_2025_2026_TOTALES.xlsx")
path_csv_2025 <- fs::path(dir_base, "HIDALGO_LEYES_INGRESOS_2025_TOTALES.csv")
path_csv_2026 <- fs::path(dir_base, "HIDALGO_LEYES_INGRESOS_2026_TOTALES.csv")
path_log_desc <- fs::path(dir_logs, "log_descargas.csv")
path_log_pend <- fs::path(dir_logs, "pendientes.csv")

readr::write_csv(descargas, path_log_desc, na = "")
readr::write_csv(
  resultados |>
    dplyr::filter(is.na(total_ingresos) | !hallo_total),
  path_log_pend,
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
  path_xlsx_todos
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
cat("XLSX: ", path_xlsx_todos, "\n", sep = "")
cat("LOG descargas: ", path_log_desc, "\n", sep = "")
cat("Pendientes: ", path_log_pend, "\n", sep = "")

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
