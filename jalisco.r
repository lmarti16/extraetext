# ============================================================
# SCRAPER: LEYES DE INGRESOS MUNICIPALES JALISCO 2025 Y 2026
# Fuente:
# https://congresoweb.congresojal.gob.mx/BibliotecaVirtual/busquedasleyes/Listado%272.cfm
# ============================================================

# ---------------------------
# 1) PAQUETES
# ---------------------------
packs <- c(
  "curl", "xml2", "rvest", "dplyr", "stringr",
  "purrr", "tibble", "readr", "writexl"
)

instalar <- packs[!vapply(packs, requireNamespace, logical(1), quietly = TRUE)]
if (length(instalar) > 0) install.packages(instalar)

invisible(lapply(packs, library, character.only = TRUE))

# ---------------------------
# 2) CONFIG
# ---------------------------
url_listado <- "https://congresoweb.congresojal.gob.mx/BibliotecaVirtual/busquedasleyes/Listado%272.cfm"
base_url    <- "https://congresoweb.congresojal.gob.mx"

# carpeta donde se guardarán los PDFs
dir_pdf <- "JALISCO_LEYES_INGRESOS_2025_2026_PDF"
dir.create(dir_pdf, showWarnings = FALSE, recursive = TRUE)

# ---------------------------
# 3) FUNCIONES AUXILIARES
# ---------------------------

leer_html_robusto <- function(url) {
  # Baja el HTML en binario y prueba varias codificaciones
  raw <- curl::curl_fetch_memory(url)$content
  txt0 <- rawToChar(raw)

  candidatos <- list(
    utf8   = txt0,
    latin1 = iconv(txt0, from = "latin1",       to = "UTF-8"),
    cp1252 = iconv(txt0, from = "windows-1252", to = "UTF-8")
  )

  score_txt <- function(x) {
    if (is.na(x) || length(x) == 0) return(-Inf)
    pats <- c(
      "Ley de Ingresos",
      "Municipio",
      "Ingresos",
      "Documentos_PDF-Ingresos",
      "2025",
      "2026"
    )
    sum(stringr::str_detect(x, stringr::fixed(pats, ignore_case = TRUE)), na.rm = TRUE)
  }

  scores <- vapply(candidatos, score_txt, numeric(1))
  mejor  <- candidatos[[names(which.max(scores))[1]]]

  # Si aún viene raro, intenta limpiar caracteres nulos
  mejor <- gsub("\u0000", "", mejor, fixed = TRUE)

  list(
    html_text = mejor,
    doc = xml2::read_html(mejor)
  )
}

normaliza_url <- function(href, base = base_url) {
  href <- trimws(href)

  dplyr::case_when(
    is.na(href) ~ NA_character_,
    href == "" ~ NA_character_,
    stringr::str_detect(href, "^https?://") ~ href,
    stringr::str_detect(href, "^//") ~ paste0("https:", href),
    stringr::str_detect(href, "^/") ~ paste0(base, href),
    TRUE ~ paste0(base, "/", href)
  )
}

limpia_titulo_archivo <- function(x) {
  x |>
    utils::URLdecode() |>
    basename() |>
    stringr::str_remove("\\.pdf$") |>
    stringr::str_remove("-\\d{6,}$") |>
    stringr::str_replace_all("%20", " ") |>
    stringr::str_squish()
}

extrae_anio <- function(x) {
  y <- stringr::str_extract(x, "20(25|26)")
  suppressWarnings(as.integer(y))
}

extrae_municipio <- function(titulo) {
  titulo |>
    stringr::str_replace_all("\\s+", " ") |>
    stringr::str_squish() |>
    stringr::str_remove(
      regex("^Ley de Ingresos del Municipio de\\s*", ignore_case = TRUE)
    ) |>
    stringr::str_remove(
      regex(",?\\s*Jalisco\\s*para\\s*el\\s*ejercicio\\s*fiscal.*$", ignore_case = TRUE)
    ) |>
    stringr::str_remove(
      regex("\\s*para\\s*el\\s*ejercicio\\s*fiscal.*$", ignore_case = TRUE)
    ) |>
    stringr::str_remove(
      regex(",?\\s*Jalisco.*$", ignore_case = TRUE)
    ) |>
    stringr::str_squish()
}

extraer_links_regex <- function(html_text) {
  # Fallback: por si el HTML trae ligas en script/texto y no como <a>
  rx <- "(?i)(?:https?:)?//[^\"'<>\\s]+Documentos_PDF-Ingresos/[^\"'<>\\s]+\\.pdf|/[^\"'<>\\s]+Documentos_PDF-Ingresos/[^\"'<>\\s]+\\.pdf"
  out <- stringr::str_extract_all(html_text, rx)[[1]]

  if (length(out) == 0) return(tibble::tibble(href = character(), texto = character()))

  tibble::tibble(
    href  = unique(out),
    texto = NA_character_
  )
}

# ---------------------------
# 4) FUNCIÓN PRINCIPAL
# ---------------------------
extraer_leyes_ingresos_jalisco <- function(anios = c(2025, 2026),
                                           descargar_pdf = TRUE,
                                           dir_descarga = dir_pdf) {

  html_obj <- leer_html_robusto(url_listado)
  doc      <- html_obj$doc
  html_txt <- html_obj$html_text

  # A) Links tomados de etiquetas <a>
  nodos_a <- rvest::html_elements(doc, "a")

  links_a <- tibble::tibble(
    href  = rvest::html_attr(nodos_a, "href"),
    texto = rvest::html_text2(nodos_a)
  )

  # B) Links tomados con regex directo del HTML (fallback)
  links_rx <- extraer_links_regex(html_txt)

  links <- dplyr::bind_rows(links_a, links_rx) |>
    dplyr::mutate(
      href_abs = normaliza_url(href),
      texto    = stringr::str_squish(texto)
    ) |>
    dplyr::filter(!is.na(href_abs)) |>
    dplyr::filter(stringr::str_detect(
      href_abs,
      regex("Documentos_PDF-Ingresos/.+\\.pdf$", ignore_case = TRUE)
    )) |>
    dplyr::distinct(href_abs, .keep_all = TRUE) |>
    dplyr::mutate(
      titulo_archivo = limpia_titulo_archivo(href_abs),
      titulo = dplyr::if_else(
        !is.na(texto) &
          texto != "" &
          stringr::str_detect(texto, regex("Ley de Ingresos del Municipio de", ignore_case = TRUE)),
        texto,
        titulo_archivo
      ),
      titulo = stringr::str_squish(titulo),
      anio = extrae_anio(titulo),
      municipio = extrae_municipio(titulo)
    ) |>
    dplyr::filter(stringr::str_detect(
      titulo,
      regex("^Ley de Ingresos del Municipio de", ignore_case = TRUE)
    )) |>
    dplyr::filter(anio %in% anios) |>
    dplyr::select(anio, municipio, titulo, pdf_url = href_abs) |>
    dplyr::distinct() |>
    dplyr::arrange(anio, municipio)

  if (nrow(links) == 0) {
    stop("No se encontraron PDFs de 'Ley de Ingresos del Municipio de' para los años solicitados.")
  }

  # Descargar PDFs
  if (isTRUE(descargar_pdf)) {
    dir.create(dir_descarga, showWarnings = FALSE, recursive = TRUE)

    links <- links |>
      dplyr::mutate(
        archivo_pdf = file.path(
          dir_descarga,
          paste0(
            anio, "_",
            stringr::str_replace_all(municipio, "[^A-Za-zÁÉÍÓÚáéíóúÑñ0-9]+", "_"),
            ".pdf"
          )
        )
      )

    purrr::walk2(links$pdf_url, links$archivo_pdf, function(u, f) {
      try(
        download.file(
          url = u,
          destfile = f,
          mode = "wb",
          quiet = TRUE
        ),
        silent = TRUE
      )
    })
  }

  links
}

# ---------------------------
# 5) EJECUCIÓN
# ---------------------------
leyes_jalisco <- extraer_leyes_ingresos_jalisco(
  anios = c(2025, 2026),
  descargar_pdf = TRUE,
  dir_descarga = dir_pdf
)

# ver resultados
print(leyes_jalisco, n = nrow(leyes_jalisco))

# ---------------------------
# 6) EXPORTAR RESULTADOS
# ---------------------------
readr::write_csv(leyes_jalisco, "leyes_ingresos_municipios_jalisco_2025_2026.csv")

writexl::write_xlsx(
  list(
    leyes_ingresos_2025_2026 = leyes_jalisco,
    solo_2025 = dplyr::filter(leyes_jalisco, anio == 2025),
    solo_2026 = dplyr::filter(leyes_jalisco, anio == 2026)
  ),
  path = "leyes_ingresos_municipios_jalisco_2025_2026.xlsx"
)

cat("\nListo.\n")
cat("CSV:  leyes_ingresos_municipios_jalisco_2025_2026.csv\n")
cat("XLSX: leyes_ingresos_municipios_jalisco_2025_2026.xlsx\n")
cat("PDFs descargados en:", normalizePath(dir_pdf), "\n")
