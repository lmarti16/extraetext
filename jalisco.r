# ============================================================
# JALISCO | LEYES DE INGRESOS MUNICIPALES 2025 Y 2026
# VERSIÓN ROBUSTA:
# - NO depende del listado roto del Congreso
# - Busca PDFs oficiales por municipio y año
# - Usa Bing y DuckDuckGo como respaldo
# - Descarga PDFs y exporta CSV/XLSX
# ============================================================

# ---------------------------
# 1) PAQUETES
# ---------------------------
packs <- c(
  "httr2", "xml2", "rvest", "stringr", "stringi",
  "dplyr", "purrr", "tibble", "readr", "writexl"
)

instalar <- packs[!vapply(packs, requireNamespace, logical(1), quietly = TRUE)]
if (length(instalar) > 0) install.packages(instalar)

invisible(lapply(packs, library, character.only = TRUE))

# ---------------------------
# 2) CONFIG
# ---------------------------
dir_pdf <- "JALISCO_LEYES_INGRESOS_2025_2026_PDF"
dir.create(dir_pdf, showWarnings = FALSE, recursive = TRUE)

# ---------------------------
# 3) CATÁLOGO DE MUNICIPIOS
# ---------------------------
municipios_jalisco <- c(
  "Acatic","Acatlán de Juárez","Ahualulco de Mercado","Amacueca","Amatitán","Ameca",
  "San Juanito de Escobedo","Arandas","El Arenal","Atemajac de Brizuela","Atengo","Atenguillo",
  "Atotonilco el Alto","Atoyac","Autlán de Navarro","Ayotlán","Ayutla","La Barca","Bolaños",
  "Cabo Corrientes","Casimiro Castillo","Cihuatlán","Zapotlán el Grande","Cocula","Colotlán",
  "Concepción de Buenos Aires","Cuautitlán de García Barragán","Cuautla","Cuquío","Chapala",
  "Chimaltitán","Chiquilistlán","Degollado","Ejutla","Encarnación de Díaz","Etzatlán","El Grullo",
  "Guachinango","Guadalajara","Hostotipaquillo","Huejúcar","Huejuquilla el Alto","La Huerta",
  "Ixtlahuacán de los Membrillos","Ixtlahuacán del Río","Jalostotitlán","Jamay","Jesús María",
  "Jilotlán de los Dolores","Jocotepec","Juanacatlán","Juchitlán","Lagos de Moreno","El Limón",
  "Magdalena","Santa María del Oro","La Manzanilla de la Paz","Mascota","Mazamitla","Mexticacán",
  "Mezquitic","Mixtlán","Ocotlán","Ojuelos de Jalisco","Pihuamo","Poncitlán","Puerto Vallarta",
  "Villa Purificación","Quitupan","El Salto","San Cristóbal de la Barranca","San Diego de Alejandría",
  "San Juan de los Lagos","San Julián","San Marcos","San Martín de Bolaños","San Martín Hidalgo",
  "San Miguel el Alto","Gómez Farías","San Sebastián del Oeste","Santa María de los Ángeles","Sayula",
  "Tala","Talpa de Allende","Tamazula de Gordiano","Tapalpa","Tecalitlán","Techaluta de Montenegro",
  "Tecolotlán","Tenamaxtlán","Teocaltiche","Teocuitatlán de Corona","Tepatitlán de Morelos","Tequila",
  "Teuchitlán","Tizapán el Alto","Tlajomulco de Zúñiga","San Pedro Tlaquepaque","Tolimán","Tomatlán",
  "Tonalá","Tonaya","Tonila","Totatiche","Tototlán","Tuxcacuesco","Tuxcueca","Tuxpan",
  "Unión de San Antonio","Unión de Tula","Valle de Guadalupe","Valle de Juárez","San Gabriel",
  "Villa Corona","Villa Guerrero","Villa Hidalgo","Cañadas de Obregón","Yahualica de González Gallo",
  "Zacoalco de Torres","Zapopan","Zapotiltic","Zapotitlán de Vadillo","Zapotlán del Rey",
  "Zapotlanejo","San Ignacio Cerro Gordo"
)

# ---------------------------
# 4) HELPERS
# ---------------------------
normaliza_txt <- function(x) {
  x |>
    stringi::stri_trans_general("Latin-ASCII") |>
    tolower() |>
    stringr::str_replace_all("[^a-z0-9]+", " ") |>
    stringr::str_squish()
}

archivo_seguro <- function(x) {
  x |>
    stringi::stri_trans_general("Latin-ASCII") |>
    stringr::str_replace_all("[/\\\\:*?\"<>|]", " ") |>
    stringr::str_replace_all("[^A-Za-z0-9 ]", " ") |>
    stringr::str_squish() |>
    stringr::str_replace_all("\\s+", "_")
}

decode_url_multi <- function(x) {
  y <- x
  for (i in 1:3) y <- utils::URLdecode(y)
  y
}

extrae_url_ddg <- function(x) {
  if (is.na(x) || x == "") return(NA_character_)
  if (grepl("duckduckgo.com/l/\\?", x, perl = TRUE)) {
    x <- sub(".*[?&]uddg=([^&]+).*", "\\1", x, perl = TRUE)
    x <- decode_url_multi(x)
  }
  x
}

es_pdf_congreso_jalisco <- function(url) {
  if (is.na(url) || url == "") return(FALSE)
  grepl("congresoweb\\.congresojal\\.gob\\.mx", url, ignore.case = TRUE, perl = TRUE) &&
    grepl("Documentos_PDF-Ingresos", url, ignore.case = TRUE, perl = TRUE) &&
    grepl("\\.pdf($|\\?)", url, ignore.case = TRUE, perl = TRUE)
}

titulo_desde_url <- function(url) {
  x <- sub("\\?.*$", "", url, perl = TRUE)
  x <- basename(x)
  x <- decode_url_multi(x)
  x <- sub("\\.pdf$", "", x, ignore.case = TRUE, perl = TRUE)
  x <- sub("-[0-9]{6,}$", "", x, perl = TRUE)
  x <- stringr::str_replace_all(x, "\\s+", " ")
  stringr::str_squish(x)
}

extrae_anio <- function(titulo) {
  m <- stringr::str_extract(titulo, "20(25|26)")
  suppressWarnings(as.integer(m))
}

score_candidato <- function(url, municipio, anio) {
  if (!es_pdf_congreso_jalisco(url)) return(-Inf)
  
  titulo <- titulo_desde_url(url)
  tnorm  <- normaliza_txt(titulo)
  munorm <- normaliza_txt(municipio)
  toks   <- unique(unlist(strsplit(munorm, " ", fixed = TRUE)))
  toks   <- toks[nchar(toks) >= 3]
  
  score <- 0
  score <- score + 5
  score <- score + ifelse(grepl("ley de ingresos", tnorm, fixed = TRUE), 4, 0)
  score <- score + ifelse(grepl("municipio", tnorm, fixed = TRUE) || grepl("para el municipio", tnorm, fixed = TRUE), 2, 0)
  score <- score + ifelse(grepl(as.character(anio), tnorm, fixed = TRUE), 5, 0)
  score <- score + sum(vapply(
    toks,
    function(tok) grepl(paste0("\\b", tok, "\\b"), tnorm, perl = TRUE),
    logical(1)
  )) * 2
  
  score
}

hacer_request <- function(url, query = list()) {
  httr2::request(url) |>
    httr2::req_url_query(!!!query) |>
    httr2::req_user_agent("Mozilla/5.0") |>
    httr2::req_retry(max_tries = 3) |>
    httr2::req_timeout(30)
}

buscar_bing <- function(query, n = 8) {
  req <- hacer_request(
    "https://www.bing.com/search",
    query = list(
      q = query,
      count = n,
      setlang = "es-MX"
    )
  )
  
  resp <- try(httr2::req_perform(req), silent = TRUE)
  if (inherits(resp, "try-error")) {
    return(tibble::tibble(
      engine = character(),
      query = character(),
      title = character(),
      url = character()
    ))
  }
  
  html <- httr2::resp_body_string(resp)
  doc  <- xml2::read_html(html)
  
  links <- rvest::html_elements(doc, "li.b_algo h2 a")
  
  tibble::tibble(
    engine = "bing",
    query  = query,
    title  = rvest::html_text2(links),
    url    = rvest::html_attr(links, "href")
  ) |>
    dplyr::filter(!is.na(url), url != "")
}

buscar_ddg <- function(query, n = 8) {
  req <- hacer_request(
    "https://html.duckduckgo.com/html/",
    query = list(
      q = query,
      kl = "mx-es"
    )
  )
  
  resp <- try(httr2::req_perform(req), silent = TRUE)
  if (inherits(resp, "try-error")) {
    return(tibble::tibble(
      engine = character(),
      query = character(),
      title = character(),
      url = character()
    ))
  }
  
  html <- httr2::resp_body_string(resp)
  doc  <- xml2::read_html(html)
  
  links <- rvest::html_elements(doc, "a.result__a")
  
  tibble::tibble(
    engine = "ddg",
    query  = query,
    title  = rvest::html_text2(links),
    url    = rvest::html_attr(links, "href")
  ) |>
    dplyr::mutate(url = vapply(url, extrae_url_ddg, character(1))) |>
    dplyr::filter(!is.na(url), url != "") |>
    dplyr::slice_head(n = n)
}

buscar_candidatos <- function(municipio, anio) {
  q1 <- paste0(
    'site:congresoweb.congresojal.gob.mx/BibliotecaVirtual/legislacion/Ingresos/Documentos_PDF-Ingresos ',
    'filetype:pdf ',
    '"Ley de Ingresos del Municipio de ', municipio, '" ',
    '"Jalisco" ',
    '"', anio, '"'
  )
  
  q2 <- paste0(
    'site:congresoweb.congresojal.gob.mx/BibliotecaVirtual/legislacion/Ingresos/Documentos_PDF-Ingresos ',
    'filetype:pdf ',
    '"', municipio, '" ',
    '"Jalisco" ',
    '"ejercicio fiscal ', anio, '" ',
    '"Ley de Ingresos"'
  )
  
  q3 <- paste0(
    'site:congresoweb.congresojal.gob.mx/BibliotecaVirtual/legislacion/Ingresos/Documentos_PDF-Ingresos ',
    'filetype:pdf ',
    '"', municipio, '" ',
    '"', anio, '"'
  )
  
  consultas <- c(q1, q2, q3)
  
  purrr::map_dfr(consultas, function(q) {
    Sys.sleep(runif(1, 0.7, 1.3))
    dplyr::bind_rows(
      buscar_bing(q, n = 8),
      buscar_ddg(q, n = 8)
    )
  }) |>
    dplyr::distinct(url, .keep_all = TRUE) |>
    dplyr::filter(vapply(url, es_pdf_congreso_jalisco, logical(1))) |>
    dplyr::mutate(
      municipio = municipio,
      anio = anio,
      titulo_pdf = vapply(url, titulo_desde_url, character(1)),
      score = vapply(url, score_candidato, numeric(1), municipio = municipio, anio = anio)
    ) |>
    dplyr::arrange(dplyr::desc(score), titulo_pdf)
}

buscar_pdf_municipio <- function(municipio, anio) {
  cand <- buscar_candidatos(municipio, anio)
  
  if (nrow(cand) == 0) {
    return(tibble::tibble(
      municipio = municipio,
      anio = anio,
      titulo = NA_character_,
      pdf_url = NA_character_,
      engine = NA_character_,
      query = NA_character_,
      score = NA_real_
    ))
  }
  
  mejor <- cand |>
    dplyr::slice(1) |>
    dplyr::transmute(
      municipio = municipio,
      anio = anio,
      titulo = titulo_pdf,
      pdf_url = url,
      engine = engine,
      query = query,
      score = score
    )
  
  mejor
}

descargar_pdf_seguro <- function(url, destfile) {
  req <- httr2::request(url) |>
    httr2::req_user_agent("Mozilla/5.0") |>
    httr2::req_retry(max_tries = 3) |>
    httr2::req_timeout(60)
  
  try({
    resp <- httr2::req_perform(req)
    writeBin(httr2::resp_body_raw(resp), destfile)
    TRUE
  }, silent = TRUE)
}

# ---------------------------
# 5) FUNCIÓN PRINCIPAL
# ---------------------------
extraer_leyes_ingresos_jalisco <- function(
    anios = c(2025, 2026),
    municipios = municipios_jalisco,
    descargar_pdf = TRUE,
    dir_descarga = dir_pdf
) {
  
  universo <- tidyr::expand_grid(
    municipio = municipios,
    anio = anios
  )
  
  resultados <- purrr::pmap_dfr(
    universo,
    function(municipio, anio) {
      cat("Buscando:", municipio, "-", anio, "\n")
      buscar_pdf_municipio(municipio, anio)
    }
  )
  
  resultados <- resultados |>
    dplyr::mutate(
      encontrado = !is.na(pdf_url) & pdf_url != ""
    ) |>
    dplyr::arrange(anio, municipio)
  
  if (isTRUE(descargar_pdf)) {
    dir.create(dir_descarga, showWarnings = FALSE, recursive = TRUE)
    
    resultados <- resultados |>
      dplyr::mutate(
        archivo_pdf = dplyr::if_else(
          encontrado,
          file.path(
            dir_descarga,
            paste0(anio, "_", archivo_seguro(municipio), ".pdf")
          ),
          NA_character_
        )
      )
    
    idx <- which(resultados$encontrado)
    
    for (i in idx) {
      cat("Descargando:", resultados$municipio[i], "-", resultados$anio[i], "\n")
      descargar_pdf_seguro(resultados$pdf_url[i], resultados$archivo_pdf[i])
      Sys.sleep(runif(1, 0.4, 0.9))
    }
  }
  
  resultados
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
    resultados = leyes_jalisco,
    encontrados = dplyr::filter(leyes_jalisco, encontrado),
    faltantes = dplyr::filter(leyes_jalisco, !encontrado),
    leyes_2025 = dplyr::filter(leyes_jalisco, anio == 2025),
    leyes_2026 = dplyr::filter(leyes_jalisco, anio == 2026)
  ),
  path = "leyes_ingresos_municipios_jalisco_2025_2026.xlsx"
)

cat("\nListo.\n")
cat("Encontrados:", sum(leyes_jalisco$encontrado, na.rm = TRUE), "\n")
cat("Faltantes:", sum(!leyes_jalisco$encontrado, na.rm = TRUE), "\n")
cat("CSV: leyes_ingresos_municipios_jalisco_2025_2026.csv\n")
cat("XLSX: leyes_ingresos_municipios_jalisco_2025_2026.xlsx\n")
cat("PDFs en:", normalizePath(dir_pdf), "\n")
