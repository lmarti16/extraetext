# ============================================================
# JALISCO | LEYES DE INGRESOS MUNICIPALES 2025 Y 2026
# Extrae desde:
# https://congresoweb.congresojal.gob.mx/BibliotecaVirtual/busquedasleyes/Listado%272.cfm#Ingresos
# ============================================================

# ---------------------------
# 1) PAQUETES
# ---------------------------
packs <- c(
  "curl", "dplyr", "stringr", "purrr",
  "tibble", "readr", "writexl"
)

instalar <- packs[!vapply(packs, requireNamespace, logical(1), quietly = TRUE)]
if (length(instalar) > 0) install.packages(instalar)

invisible(lapply(packs, library, character.only = TRUE))

# ---------------------------
# 2) CONFIG
# ---------------------------
url_listado <- "https://congresoweb.congresojal.gob.mx/BibliotecaVirtual/busquedasleyes/Listado%272.cfm"
base_url    <- "https://congresoweb.congresojal.gob.mx"

dir_pdf <- "JALISCO_LEYES_INGRESOS_2025_2026_PDF"
dir.create(dir_pdf, showWarnings = FALSE, recursive = TRUE)

# ---------------------------
# 3) FUNCIONES AUXILIARES
# ---------------------------

bajar_html_crudo <- function(url) {
  h <- curl::new_handle()
  curl::handle_setopt(
    h,
    useragent = "Mozilla/5.0",
    followlocation = TRUE,
    ssl_verifypeer = TRUE,
    ssl_verifyhost = 2
  )
  
  raw <- curl::curl_fetch_memory(url, handle = h)$content
  
  # Esto evita el problema con bytes raros / nulos
  txt <- paste(rawToChar(raw, multiple = TRUE), collapse = "")
  txt
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

limpia_titulo_pdf <- function(url_pdf) {
  titulo <- basename(url_pdf)
  titulo <- utils::URLdecode(titulo)
  titulo <- stringr::str_remove(titulo, "\\.pdf$")
  titulo <- stringr::str_remove(titulo, "-\\d{6,}$")
  titulo <- stringr::str_replace_all(titulo, "\\s+", " ")
  titulo <- stringr::str_squish(titulo)
  titulo
}

extrae_anio <- function(titulo) {
  out <- stringr::str_extract(titulo, "20(25|26)")
  suppressWarnings(as.integer(out))
}

extrae_municipio <- function(titulo) {
  titulo |>
    stringr::str_remove(
      stringr::regex("^Ley de Ingresos del Municipio de\\s*", ignore_case = TRUE)
    ) |>
    stringr::str_remove(
      stringr::regex(",?\\s*Jalisco\\s*para\\s*el\\s*ejercicio\\s*fiscal.*$", ignore_case = TRUE)
    ) |>
    stringr::str_remove(
      stringr::regex("\\s*para\\s*el\\s*ejercicio\\s*fiscal.*$", ignore_case = TRUE)
    ) |>
    stringr::str_remove(
      stringr::regex(",?\\s*Jalisco.*$", ignore_case = TRUE)
    ) |>
    stringr::str_squish()
}

archivo_seguro <- function(x) {
  x |>
    stringr::str_replace_all("[/\\\\:*?\"<>|]", " ") |>
    stringr::str_replace_all("[^A-Za-zÁÉÍÓÚáéíóúÑñ0-9 ]", " ") |>
    stringr::str_replace_all("\\s+", "_") |>
    stringr::str_squish()
}

# ---------------------------
# 4) EXTRAER LINKS DE PDF DESDE HTML CRUDO
# ---------------------------
extraer_pdfs_desde_html <- function(html_text) {
  
  # Busca rutas o urls a pdfs del apartado de Ingresos
  rx_pdf <- "(?i)(https?://[^\"'<>\\s]+\\.pdf|/[^\"]+?\\.pdf)"
  
  candidatos <- stringr::str_extract_all(html_text, rx_pdf)[[1]]
  
  if (length(candidatos) == 0) {
    return(tibble::tibble(pdf_url = character()))
  }
  
  tibble::tibble(
    href = unique(candidatos)
  ) |>
    dplyr::mutate(
      pdf_url = normaliza_url(href)
    ) |>
    dplyr::filter(
      stringr::str_detect(pdf_url, stringr::regex("Documentos_PDF-Ingresos", ignore_case = TRUE))
    ) |>
    dplyr::filter(
      stringr::str_detect(
        pdf_url,
        stringr::regex("Ley%20de%20Ingresos%20del%20Municipio%20de|Ley de Ingresos del Municipio de",
                       ignore_case = TRUE)
      )
    ) |>
    dplyr::select(pdf_url) |>
    dplyr::distinct()
}

# ---------------------------
# 5) FUNCIÓN PRINCIPAL
# ---------------------------
extraer_leyes_ingresos_jalisco <- function(anios = c(2025, 2026),
                                           descargar_pdf = TRUE,
                                           dir_descarga = dir_pdf) {
  
  html_text <- bajar_html_crudo(url_listado)
  
  pdfs <- extraer_pdfs_desde_html(html_text)
  
  if (nrow(pdfs) == 0) {
    stop("No encontré PDFs en el HTML del listado.")
  }
  
  out <- pdfs |>
    dplyr::mutate(
      titulo    = purrr::map_chr(pdf_url, limpia_titulo_pdf),
      anio      = purrr::map_int(titulo, extrae_anio),
      municipio = purrr::map_chr(titulo, extrae_municipio)
    ) |>
    dplyr::filter(anio %in% anios) |>
    dplyr::filter(
      stringr::str_detect(
        titulo,
        stringr::regex("^Ley de Ingresos del Municipio de", ignore_case = TRUE)
      )
    ) |>
    dplyr::distinct(anio, municipio, titulo, pdf_url, .keep_all = TRUE) |>
    dplyr::arrange(anio, municipio)
  
  if (nrow(out) == 0) {
    stop("Sí encontré PDFs, pero ninguno coincidió con 2025/2026.")
  }
  
  if (isTRUE(descargar_pdf)) {
    dir.create(dir_descarga, showWarnings = FALSE, recursive = TRUE)
    
    out <- out |>
      dplyr::mutate(
        archivo_pdf = file.path(
          dir_descarga,
          paste0(anio, "_", archivo_seguro(municipio), ".pdf")
        )
      )
    
    purrr::walk2(out$pdf_url, out$archivo_pdf, function(u, f) {
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
  
  out
}

# ---------------------------
# 6) EJECUCIÓN
# ---------------------------
leyes_jalisco <- extraer_leyes_ingresos_jalisco(
  anios = c(2025, 2026),
  descargar_pdf = TRUE,
  dir_descarga = dir_pdf
)

print(leyes_jalisco, n = nrow(leyes_jalisco))

# ---------------------------
# 7) EXPORTAR
# ---------------------------
readr::write_csv(
  leyes_jalisco,
  "leyes_ingresos_municipios_jalisco_2025_2026.csv"
)

writexl::write_xlsx(
  list(
    leyes_2025_2026 = leyes_jalisco,
    leyes_2025 = dplyr::filter(leyes_jalisco, anio == 2025),
    leyes_2026 = dplyr::filter(leyes_jalisco, anio == 2026)
  ),
  "leyes_ingresos_municipios_jalisco_2025_2026.xlsx"
)

cat("\nListo.\n")
cat("Total encontrados:", nrow(leyes_jalisco), "\n")
cat("CSV:  leyes_ingresos_municipios_jalisco_2025_2026.csv\n")
cat("XLSX: leyes_ingresos_municipios_jalisco_2025_2026.xlsx\n")
cat("PDFs en:", normalizePath(dir_pdf), "\n")
