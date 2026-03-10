# ============================================================
# HIDALGO | LEYES DE INGRESOS MUNICIPALES 2026
# ------------------------------------------------------------
# Qué hace:
# 1) Recorre los alcances 5 a 24 del 31 de diciembre de 2025
#    del Periódico Oficial de Hidalgo.
# 2) Descarga los PDFs en:
#    C:/Users/lmart/Downloads/Leyes Ingreso/hidalgo
# 3) Detecta cada Ley de Ingresos municipal 2026.
# 4) Extrae el TOTAL DE INGRESOS aunque la redacción cambie.
# 5) Exporta CSV + XLSX con resultados y un log.
# ------------------------------------------------------------
# Referencias oficiales:
# - Alcance 5 del 31/12/2025: decrees 454 a 457. :contentReference[oaicite:0]{index=0}
# - Compendio municipal 2026: lista los decretos 454 a 537
#   y señala que el 537 es vigencia de Mixquiahuala 2025
#   para regir en 2026. :contentReference[oaicite:1]{index=1}
# ============================================================

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

ua <- "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/145.0.0.0 Safari/537.36"
base_site <- "https://periodico.hidalgo.gob.mx"

dir_base <- "C:/Users/lmart/Downloads/Leyes Ingreso/hidalgo"
dir_pdfs <- fs::path(dir_base, "pdfs")
dir_logs <- fs::path(dir_base, "logs")

fs::dir_create(dir_base, recurse = TRUE)
fs::dir_create(dir_pdfs, recurse = TRUE)
fs::dir_create(dir_logs, recurse = TRUE)

# -----------------------------
# 3) HELPERS WEB
# -----------------------------
obtener_html <- function(url) {
  httr2::request(url) |>
    httr2::req_user_agent(ua) |>
    httr2::req_timeout(90) |>
    httr2::req_retry(max_tries = 3) |>
    httr2::req_perform() |>
    httr2::resp_body_string()
}

extrae_urls_relevantes <- function(html_txt, base_url) {
  doc <- xml2::read_html(html_txt)

  hrefs <- c(
    rvest::html_elements(doc, "a[href]") |> rvest::html_attr("href"),
    rvest::html_elements(doc, "iframe[src]") |> rvest::html_attr("src"),
    rvest::html_elements(doc, "embed[src]") |> rvest::html_attr("src"),
    rvest::html_elements(doc, "object[data]") |> rvest::html_attr("data"),
    rvest::html_elements(doc, "form[action]") |> rvest::html_attr("action")
  )

  hrefs <- hrefs[!is.na(hrefs) & nzchar(hrefs)]
  hrefs_abs <- tryCatch(xml2::url_absolute(hrefs, base_url), error = function(e) hrefs)

  hits_raw <- stringr::str_extract_all(
    html_txt,
    "(https?://[^\"'\\s>]+(?:\\.pdf(?:\\?[^\"'\\s>]*)?|\\?wpdmpro=[^\"'\\s>]+|\\?wpdmdl=\\d+[^\"'\\s>]*|download=[^\"'\\s>]+)|/[^\"'\\s>]+(?:\\.pdf(?:\\?[^\"'\\s>]*)?|\\?wpdmpro=[^\"'\\s>]+|\\?wpdmdl=\\d+[^\"'\\s>]*))"
  )[[1]]

  hits_raw <- hits_raw[!is.na(hits_raw) & nzchar(hits_raw)]
  hits_abs <- tryCatch(xml2::url_absolute(hits_raw, base_url), error = function(e) hits_raw)

  urls <- unique(c(hrefs_abs, hits_abs))
  urls <- unique(urls)

  urls <- urls[
    stringr::str_detect(
      urls,
      "POEHpdfpublic/.+\\.pdf(?:\\?.*)?$|\\?wpdmpro=|\\?wpdmdl=|download="
    )
  ]

  urls <- urls[
    !stringr::str_detect(
      urls,
      "file-type-icons|\\.svg$|\\.png$|\\.jpg$|\\.jpeg$|\\.webp$"
    )
  ]

  urls
}

es_pdf_valido <- function(path_pdf) {
  tryCatch({
    info <- pdftools::pdf_info(path_pdf)
    !is.null(info$pages) && info$pages >= 1
  }, error = function(e) FALSE)
}

descargar_primero_que_funcione <- function(candidatos, destfile) {
  candidatos <- unique(candidatos[!is.na(candidatos) & nzchar(candidatos)])

  if (length(candidatos) == 0) {
    return(list(
      ok = FALSE,
      url = NA_character_,
      intentos = tibble::tibble(url = character(), ok = logical(), detalle = character())
    ))
  }

  intentos <- tibble::tibble(
    url = candidatos,
    ok = FALSE,
    detalle = NA_character_
  )

  for (i in seq_along(candidatos)) {
    u <- candidatos[i]
    ok_i <- FALSE
    detalle_i <- NA_character_

    tryCatch({
      if (fs::file_exists(destfile)) fs::file_delete(destfile)

      httr2::request(u) |>
        httr2::req_user_agent(ua) |>
        httr2::req_timeout(180) |>
        httr2::req_retry(max_tries = 2) |>
        httr2::req_perform(path = destfile)

      ok_i <- fs::file_exists(destfile) &&
        fs::file_info(destfile)$size > 1024 &&
        es_pdf_valido(destfile)

      if (!ok_i) detalle_i <- "Descargó algo, pero no fue PDF válido."
    }, error = function(e) {
      detalle_i <<- conditionMessage(e)
      ok_i <<- FALSE
    })

    intentos$ok[i] <- ok_i
    intentos$detalle[i] <- detalle_i

    if (ok_i) {
      return(list(
        ok = TRUE,
        url = u,
        intentos = intentos
      ))
    }
  }

  list(
    ok = FALSE,
    url = NA_character_,
    intentos = intentos
  )
}

# Rutas típicas del portal; quedan como respaldo si el botón de descarga cambia
candidatos_fallback_pdf <- function(alcance) {
  unique(c(
    sprintf("%s/POEHpdfpublic/2025_dic_31_alc%s_52.pdf", base_site, alcance),
    sprintf("%s/POEHpdfpublic/2025_dic_31_alc%02d_52.pdf", base_site, alcance),
    sprintf("%s/POEHpdfpublic/2025_dic_31_al%s_52.pdf", base_site, alcance),
    sprintf("%s/POEHpdfpublic/2025_dic_31_al%02d_52.pdf", base_site, alcance)
  ))
}

resolver_candidatos_pdf_evento <- function(event_url, alcance) {
  html_evt <- obtener_html(event_url)
  urls_evt <- extrae_urls_relevantes(html_evt, event_url)

  directos <- urls_evt[stringr::str_detect(
    urls_evt,
    "POEHpdfpublic/.+\\.pdf(?:\\?.*)?$|\\.pdf(?:\\?.*)?$|\\?wpdmdl="
  )]

  wpdmpro <- urls_evt[stringr::str_detect(urls_evt, "\\?wpdmpro=")]

  out <- directos

  if (length(wpdmpro) > 0) {
    for (w in wpdmpro) {
      html_w <- tryCatch(obtener_html(w), error = function(e) NA_character_)
      if (is.na(html_w)) next

      urls_w <- extrae_urls_relevantes(html_w, w)

      out <- c(
        out,
        urls_w[stringr::str_detect(
          urls_w,
          "POEHpdfpublic/.+\\.pdf(?:\\?.*)?$|\\.pdf(?:\\?.*)?$|\\?wpdmdl="
        )]
      )
    }
  }

  unique(c(out, candidatos_fallback_pdf(alcance)))
}

# -----------------------------
# 4) HELPERS TEXTO/PDF
# -----------------------------
normaliza_texto_pdf <- function(x) {
  x |>
    enc2utf8() |>
    stringi::stri_trans_general("Latin-ASCII") |>
    stringr::str_replace_all("\u00A0", " ") |>
    stringr::str_replace_all("[\r\n\t]+", " ") |>
    stringr::str_replace_all("\\s+", " ") |>
    stringr::str_squish() |>
    toupper()
}

leer_pdf_normalizado <- function(path_pdf) {
  txt <- pdftools::pdf_text(path_pdf)
  txt <- paste(txt, collapse = "\n")
  normaliza_texto_pdf(txt)
}

extraer_bloques_ley <- function(texto) {
  patron_inicio <- paste0(
    "(",
    "(DECRETO\\s+NUMERO\\.?\\s*\\d+\\s*[-–.]?\\s*LXVI\\s+)?",
    "(QUE\\s+CONTIENE\\s+LA\\s+)?",
    "LEY\\s+DE\\s+INGRESOS\\s+PARA\\s+EL\\s+MUNICIPIO\\s+DE\\s+[A-Z .'-]+?,?\\s+HIDALGO,?\\s+CORRESPONDIENTE\\s+AL\\s+EJERCICIO\\s+FISCAL\\s+2026",
    "|",
    "DECRETO\\s+NUMERO\\.?\\s*537\\s*[-–.]?\\s*LXVI\\s+QUE\\s+DECLARA\\s+LA\\s+VIGENCIA\\s+DE\\s+LA\\s+LEY\\s+DE\\s+INGRESOS\\s+DEL\\s+MUNICIPIO\\s+DE\\s+MIXQUIAHUALA\\s+DE\\s+JUAREZ"
    ,")"
  )

  locs <- stringr::str_locate_all(texto, patron_inicio)[[1]]

  if (is.null(locs) || nrow(locs) == 0) return(character(0))

  starts <- locs[, 1]
  ends <- c(starts[-1] - 1, nchar(texto))

  purrr::map2_chr(starts, ends, ~ stringr::str_sub(texto, .x, .y))
}

extraer_municipio <- function(bloque) {
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

  m <- stringr::str_squish(m)
  ifelse(is.na(m), NA_character_, m)
}

extraer_decreto <- function(bloque) {
  stringr::str_match(
    bloque,
    "DECRETO\\s+NUMERO\\.?\\s*(\\d{3})"
  )[, 2]
}

limpia_numero_monetario <- function(x) {
  if (length(x) == 0 || is.na(x) || !nzchar(x)) return(NA_real_)
  x <- gsub("\\s+", "", x)
  x <- gsub(",", "", x)
  suppressWarnings(as.numeric(x))
}

# Busca TOTAL DE INGRESOS y variantes cercanas
extraer_total_bloque <- function(bloque) {
  patrones <- c(
    "TOTAL\\s+DE\\s+INGRESOS\\s*[:\\$]?\\s*([0-9]{1,3}(?:,\\s?[0-9]{3})*(?:\\.\\d{2})?)",
    "TOTAL\\s+INGRESOS\\s*[:\\$]?\\s*([0-9]{1,3}(?:,\\s?[0-9]{3})*(?:\\.\\d{2})?)",
    "INGRESOS\\s+TOTALES\\s*[:\\$]?\\s*([0-9]{1,3}(?:,\\s?[0-9]{3})*(?:\\.\\d{2})?)",
    "IMPORTE\\s+TOTAL\\s*[:\\$]?\\s*([0-9]{1,3}(?:,\\s?[0-9]{3})*(?:\\.\\d{2})?)",
    "MONTO\\s+TOTAL\\s*[:\\$]?\\s*([0-9]{1,3}(?:,\\s?[0-9]{3})*(?:\\.\\d{2})?)",
    "SE\\s+PERCIBIRAN\\s+LOS\\s+INGRESOS\\s+POR\\s+UN\\s+TOTAL\\s+DE\\s*[:\\$]?\\s*([0-9]{1,3}(?:,\\s?[0-9]{3})*(?:\\.\\d{2})?)",
    "LA\\s+HACIENDA\\s+PUBLICA\\s+DEL\\s+MUNICIPIO.*?PERCIBIRA.*?TOTAL\\s+DE\\s*[:\\$]?\\s*([0-9]{1,3}(?:,\\s?[0-9]{3})*(?:\\.\\d{2})?)"
  )

  for (p in patrones) {
    m <- stringr::str_match(bloque, p)[, 2]
    if (!is.na(m)) return(stringr::str_replace_all(m, "\\s+", ""))
  }

  # Ventanas alrededor de palabras clave
  ventanas <- c(
    stringr::str_extract_all(bloque, "TOTAL.{0,220}")[[1]],
    stringr::str_extract_all(bloque, "ARTICULO\\s+1.{0,450}")[[1]],
    stringr::str_extract_all(bloque, "PERCIBIRA.{0,300}")[[1]]
  )

  ventanas <- ventanas[!is.na(ventanas)]

  if (length(ventanas) > 0) {
    nums <- stringr::str_match_all(
      ventanas,
      "([0-9]{1,3}(?:,\\s?[0-9]{3})*(?:\\.\\d{2})?)"
    )

    nums <- unlist(purrr::map(nums, ~ .x[, 2]))
    nums <- nums[!is.na(nums)]

    if (length(nums) > 0) {
      nums_limpios <- stringr::str_replace_all(nums, "\\s+", "")
      nums_num <- suppressWarnings(as.numeric(gsub(",", "", nums_limpios)))
      if (all(is.na(nums_num))) return(NA_character_)
      return(nums_limpios[which.max(nums_num)])
    }
  }

  NA_character_
}

extraer_desde_pdf <- function(path_pdf, alcance, pdf_url = NA_character_) {
  txt <- leer_pdf_normalizado(path_pdf)
  bloques <- extraer_bloques_ley(txt)

  if (length(bloques) == 0) {
    return(
      tibble::tibble(
        alcance = alcance,
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
    total_txt <- extraer_total_bloque(b)

    tipo_dec <- if (stringr::str_detect(
      b,
      "VIGENCIA\\s+DE\\s+LA\\s+LEY\\s+DE\\s+INGRESOS"
    )) {
      "Vigencia 2025 que regira 2026"
    } else {
      "Ley de ingresos 2026"
    }

    tibble::tibble(
      alcance = alcance,
      decreto = extraer_decreto(b),
      municipio = extraer_municipio(b),
      tipo_decreto = tipo_dec,
      total_ingresos_texto = total_txt,
      total_ingresos = limpia_numero_monetario(total_txt),
      pdf_local = path_pdf,
      pdf_url = pdf_url,
      hallo_total = !is.na(total_txt)
    )
  })
}

# -----------------------------
# 5) ALCANCES 5 A 24
# -----------------------------
alcances <- tibble::tibble(
  alcance = 5:24,
  event_url = sprintf(
    "https://periodico.hidalgo.gob.mx/?tribe_events=periodico-oficial-alcance-%d-del-31-de-diciembre-de-2025",
    5:24
  ),
  pdf_local = fs::path(dir_pdfs, sprintf("HGO_2025_12_31_ALCANCE_%02d.pdf", 5:24))
)

# -----------------------------
# 6) DESCARGAR PDFS
# -----------------------------
descargas <- alcances |>
  dplyr::mutate(
    candidatos = purrr::map2(event_url, alcance, resolver_candidatos_pdf_evento),
    descarga = purrr::map2(candidatos, pdf_local, descargar_primero_que_funcione),
    pdf_url = purrr::map_chr(descarga, ~ .x$url %||% NA_character_),
    descargado = purrr::map_lgl(descarga, "ok")
  )

log_descargas <- descargas |>
  dplyr::transmute(
    alcance,
    event_url,
    pdf_url,
    descargado,
    pdf_local,
    n_candidatos = purrr::map_int(candidatos, length)
  )

readr::write_csv(log_descargas, fs::path(dir_logs, "log_descargas.csv"), na = "")

# Log detallado de intentos
log_intentos <- purrr::map2_dfr(
  descargas$alcance,
  descargas$descarga,
  function(alcance, obj) {
    tb <- obj$intentos
    if (nrow(tb) == 0) {
      tibble::tibble(
        alcance = alcance,
        url = NA_character_,
        ok = FALSE,
        detalle = "Sin candidatos detectados"
      )
    } else {
      dplyr::mutate(tb, alcance = alcance, .before = 1)
    }
  }
)

readr::write_csv(log_intentos, fs::path(dir_logs, "log_intentos_descarga.csv"), na = "")

# -----------------------------
# 7) EXTRAER TOTALES
# -----------------------------
res <- purrr::pmap_dfr(
  list(descargas$pdf_local, descargas$alcance, descargas$pdf_url, descargas$descargado),
  function(pdf_local, alcance, pdf_url, descargado) {
    if (!descargado || !fs::file_exists(pdf_local)) {
      return(
        tibble::tibble(
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

    extraer_desde_pdf(
      path_pdf = pdf_local,
      alcance = alcance,
      pdf_url = pdf_url
    )
  }
) |>
  dplyr::distinct(alcance, decreto, municipio, .keep_all = TRUE) |>
  dplyr::arrange(alcance, suppressWarnings(as.numeric(decreto)), municipio)

# -----------------------------
# 8) EXPORTAR RESULTADOS
# -----------------------------
salida_csv  <- fs::path(dir_base, "HIDALGO_LEYES_INGRESOS_2026_TOTALES.csv")
salida_xlsx <- fs::path(dir_base, "HIDALGO_LEYES_INGRESOS_2026_TOTALES.xlsx")

readr::write_csv(res, salida_csv, na = "")

writexl::write_xlsx(
  list(
    totales = res,
    descargas = log_descargas,
    intentos = log_intentos,
    pendientes = dplyr::filter(res, is.na(total_ingresos) | !hallo_total)
  ),
  salida_xlsx
)

# -----------------------------
# 9) RESUMEN EN CONSOLA
# -----------------------------
cat("\n========================================\n")
cat("HIDALGO | LEYES DE INGRESOS 2026\n")
cat("========================================\n")
cat("Carpeta base: ", dir_base, "\n", sep = "")
cat("PDFs descargados: ", sum(descargas$descargado), " de ", nrow(descargas), "\n", sep = "")
cat("Registros detectados: ", nrow(res), "\n", sep = "")
cat("Con total detectado: ", sum(!is.na(res$total_ingresos)), "\n", sep = "")
cat("Sin total detectado: ", sum(is.na(res$total_ingresos)), "\n", sep = "")
cat("CSV:  ", salida_csv, "\n", sep = "")
cat("XLSX: ", salida_xlsx, "\n", sep = "")

cat("\n--- Muestra: ACATLAN / MIXQUIAHUALA ---\n")
print(
  res |>
    dplyr::filter(
      stringr::str_detect(dplyr::coalesce(municipio, ""), "ACATLAN|MIXQUIAHUALA")
    ) |>
    dplyr::select(
      alcance, decreto, municipio, tipo_decreto,
      total_ingresos_texto, total_ingresos
    )
)

cat("\n--- Pendientes por revisar ---\n")
print(
  res |>
    dplyr::filter(is.na(total_ingresos) | !hallo_total) |>
    dplyr::select(alcance, decreto, municipio, tipo_decreto, pdf_url)
)
