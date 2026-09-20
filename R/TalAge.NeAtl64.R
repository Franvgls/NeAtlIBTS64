#' Grafico Talla-Edad por anyo (varios anyos) con lattice
#'
#' Descarga datos CA de DATRAS para uno o varios anyos y quarters con
#' \code{icesDatras::getDATRAS(record="CA", ...)} — a diferencia de
#' \code{\link{GetAlkDTR.NeAtl64}}, que usa \code{getCAdata()} y solo
#' admite un survey/anyo/quarter por llamada. Los representa con
#' \code{lattice::xyplot} (\code{Lngt ~ Age | Year}), un panel por anyo,
#' replicando el grafico exploratorio:
#' \preformatted{
#' CAdat <- dplyr::filter(icesDatras::getDATRAS("CA","SP-NORTH",
#'            years=c(2020:2024), quarters=4), SpecCode==127146)
#' lattice::xyplot(LngtClass~Age|as.factor(Year), groups=Maturity>61, CAdat,
#'                  pch=21, auto.key=T, cex=1.2,
#'                  scales=list(alternating=F, tck=c(1,0)),
#'                  xlab="Age", ylab="Length class (mm)")
#' }
#' A diferencia de \code{\link{GrafAlk.NeAtl64}} (ALK en barras apiladas
#' para un unico survey/anyo/quarter), esta funcion muestra la nube de
#' puntos talla-edad, comparando anyos en paneles.
#'
#' \strong{Sobre combos anyo/quarter inexistentes:} la descarga se hace en
#' cascada: primero se intenta bajar todo de una vez; si falla, se baja
#' anyo a anyo; si un anyo concreto falla, se baja quarter a quarter dentro
#' de ese anyo. Los anyos/quarters sin datos simplemente se saltan (con
#' aviso si \code{verbose=TRUE}). El filtrado por especie se hace siempre
#' en local sobre \code{Valid_Aphia}/\code{SpecCode} tras la descarga, NO
#' pasando \code{species=} a \code{getDATRAS(record="CA", ...)} — ese
#' filtro no es fiable para registros CA (solo aparece documentado para
#' HL).
#'
#' \strong{Sobre anyos con Age = NA:} un panel vacio puede significar dos
#' cosas muy distintas: (a) no hay lances CA para ese anyo/especie, o (b)
#' SI hay lances CA pero no se han leido los otolitos (Age = NA en todos
#' los registros de esa especie ese anyo) — la campana perdio esa parte
#' del valor del muestreo biologico. La funcion distingue ambos casos: si
#' \code{verbose=TRUE}, avisa explicitamente de los anyos del segundo tipo
#' antes de descartar las filas sin Age.
#'
#' @param camp Survey de DATRAS, ej. "SP-NORTH". Ignorado si se pasa `data`.
#' @param years Vector de anyos (obligatorio si no se pasa `data`), ej.
#'   1996:2022 o un unico anyo.
#' @param quarter Quarter o vector de quarters a descargar/incluir, ej. 4 o
#'   c(1,4). NULL (por defecto) = 1:4. (Nombre alineado con
#'   \code{icesDatras}, que usa "quarter"/"quarters"; no es especifico de
#'   Espana ni de un unico equipo.)
#' @param aphia SpecCode/Valid_Aphia de la especie, ej. 127146.
#' @param maturity Logico. TRUE (por defecto): separa por \code{Maturity >
#'   61} (maduros/inmaduros) con \code{groups=} y \code{auto.key}. FALSE:
#'   un solo grupo, sin distincion de madurez.
#' @param by_cm Logico. TRUE (por defecto): representa la talla en cm
#'   enteros (\code{trunc(LngtClass/10)}), \code{ylab} = "Length class
#'   (cm)". FALSE: representa \code{LngtClass} tal cual, en mm, \code{ylab}
#'   = "Length class (mm)" (como en el script exploratorio original).
#' @param maxlength Talla maxima a representar (subset \code{Lngt <
#'   maxlength}), en las mismas unidades que marque \code{by_cm} (cm si
#'   TRUE, mm si FALSE). NULL (por defecto) = sin filtro.
#' @param data data.frame ya construido (p.ej. tu \code{ldbCAs}, el CSV ya
#'   bajado, o la salida de \code{getDATRAS}/\code{GetAlkDTR.NeAtl64}). Si
#'   se proporciona, NO se descarga nada de DATRAS — pensado para trabajar
#'   offline mientras DATRAS esta caido. Debe traer \code{LngtClass} (o ya
#'   \code{Lngt}) y \code{Age}.
#' @param ncol_max Numero maximo de columnas del layout de paneles. Por
#'   defecto 5 (con 20 anyos da un 5x4).
#' @param as_table Logico pasado a lattice. FALSE (por defecto) replica el
#'   orden de paneles del script original (se llenan de abajo hacia
#'   arriba). TRUE = orden cronologico normal, de arriba hacia abajo.
#' @param cex Tamano de punto. 1.2 por defecto (con cex=1 se ven demasiado
#'   pequenos).
#' @param scales Lista pasada a \code{lattice::xyplot}. Por defecto
#'   \code{list(alternating=FALSE, tck=c(1,0))}: ejes/numeros solo abajo e
#'   izquierda, en vez de en los cuatro lados.
#'
#' @param verbose Logico. TRUE (por defecto): avisa con \code{message()}
#'   en que nivel de la cascada tuvo que caer, que anyos/quarters se
#'   saltaron por falta de datos, y que anyos tienen lances CA pero Age
#'   siempre NA (otolitos sin leer).
#' @param out.dat Logico, TRUE (F por defecto) devuelve los datos utilizados en el gráfico.
#' @param ... Argumentos adicionales pasados a \code{lattice::xyplot}
#'   (col, main, etc.)
#' @return Devuelve (invisible) el objeto trellis; lo pinta con
#'   \code{print()}.
#' @seealso \code{\link{GetAlkDTR.NeAtl64}}, \code{\link{GrafAlk.NeAtl64}}
#' @family ALK DATRAS
#' @export
#' @examples
#' \dontrun{
#' # Descarga de una serie completa de anyos de merluza en SP-NORTH Q4
#' TalAge.NeAtl64("SP-NORTH", 2020:2024, quarter = 4, aphia = 127146)
#'
#' # En mm, como el script exploratorio original
#' TalAge.NeAtl64("SP-NORTH", 2020:2024, quarter = 4, aphia = 127146,
#'                 by_cm = FALSE)
#'
#' # Offline, con un CSV ya bajado (ej. mientras DATRAS esta caido)
#' ldbCAs <- read.csv("ldbCAs.csv")
#' TalAge.NeAtl64(years = 1996:2022, aphia = 127145, data = ldbCAs,
#'                 maxlength = 40)
#' }
TalAge.NeAtl64 <- function(camp = NULL, years = NULL, quarter = NULL, aphia,
                            maturity = TRUE, by_cm = TRUE,
                            maxlength = NULL, data = NULL,
                            ncol_max = 5, as_table = FALSE,
                            cex = 1.2,
                            scales = list(alternating = FALSE, tck = c(1, 0)),
                            out.dat = FALSE,
                            verbose = TRUE, ...) {

  # Helper interno: una llamada a getDATRAS, NULL si falla o viene vacia.
  # OJO: 'species' NO se pasa a getDATRAS(record="CA", ...) -- en todos los
  # ejemplos oficiales del paquete ese filtro solo se usa con record="HL"; con
  # CA se descarga todo y se filtra Valid_Aphia/SpecCode en local despues
  # (igual que hace GetAlkDTR.NeAtl64 con getCAdata()). Pasar species=aphia
  # aqui puede devolver vacio o fallar aunque los datos existan.
  .get_ca_chunk <- function(camp, y, q, aphia) {
    out <- tryCatch(
      icesDatras::getDATRAS(record = "CA", survey = camp, years = y, quarters = q),
      error = function(e) NULL
    )
    if (is.null(out) || !is.data.frame(out) || nrow(out) == 0) return(NULL)

    spcol <- if ("Valid_Aphia" %in% names(out)) "Valid_Aphia" else "SpecCode"
    if (!is.null(aphia) && spcol %in% names(out)) out <- out[out[[spcol]] %in% aphia, ]

    if (nrow(out) == 0) NULL else out
  }

  # --- 1. Obtener datos CA, en cascada: bloque -> anyo -> anyo+quarter -------
  if (is.null(data)) {
    if (is.null(camp) || is.null(years)) {
      stop("Faltan 'camp' y/o 'years' (o pasa 'data' ya construido si ",
           "DATRAS no esta disponible).")
    }
    qs <- if (is.null(quarter)) 1:4 else quarter

    all_ca <- list()

    # Nivel 1: todo de una vez
    bulk <- .get_ca_chunk(camp, years, qs, aphia)

    if (!is.null(bulk)) {
      all_ca[[1]] <- bulk
    } else {
      if (verbose) message("Descarga conjunta de todos los anyos ha fallado (o no hay datos); ",
                            "bajando anyo a anyo...")
      # Nivel 2: anyo a anyo (todos los quarters pedidos juntos)
      for (y in years) {
        yr_ca <- .get_ca_chunk(camp, y, qs, aphia)
        if (!is.null(yr_ca)) {
          all_ca[[length(all_ca) + 1]] <- yr_ca
        } else {
          if (verbose) message(sprintf("  %s %d: fallo con los quarters juntos, probando uno a uno...", camp, y))
          # Nivel 3: anyo + quarter individual
          for (q in qs) {
            q_ca <- .get_ca_chunk(camp, y, q, aphia)
            if (!is.null(q_ca)) {
              all_ca[[length(all_ca) + 1]] <- q_ca
            } else if (verbose) {
              message(sprintf("    %s %d-Q%d: sin datos, salto.", camp, y, q))
            }
          }
        }
      }
    }

    if (length(all_ca) == 0) {
      stop("No se ha podido descargar ningun dato CA para esta seleccion ",
           "(ni en bloque, ni anyo a anyo, ni anyo+quarter individual).")
    }

    data <- do.call(rbind, all_ca)
  }

  if (!is.data.frame(data) || nrow(data) == 0) {
    stop("'data' esta vacio o no es un data.frame.")
  }

  df <- data

  # --- 2. Unidades: Lngt (cm o mm) segun by_cm, y su ylab ---------------------
  if (!"Lngt" %in% names(df)) {
    if (!"LngtClass" %in% names(df)) {
      stop("'data' no tiene columna 'LngtClass' (ni 'Lngt' ya calculada).")
    }
    df$Lngt <- if (isTRUE(by_cm)) trunc(df$LngtClass / 10) else df$LngtClass
  }
  ylab_txt <- if (isTRUE(by_cm)) "Length class (cm)" else "Length class (mm)"

  # --- 3. Filtros --------------------------------------------------------------
  spcol <- if ("Valid_Aphia" %in% names(df)) "Valid_Aphia" else "SpecCode"
  if (!missing(aphia) && !is.null(aphia) && spcol %in% names(df)) {
    df <- df[df[[spcol]] %in% aphia, ]
  }
  if (!is.null(camp)    && "Survey"  %in% names(df)) df <- df[df$Survey  %in% camp,    ]
  if (!is.null(years)   && "Year"    %in% names(df)) df <- df[df$Year    %in% years,   ]
  if (!is.null(quarter) && "Quarter" %in% names(df)) df <- df[df$Quarter %in% quarter, ]

  if (nrow(df) == 0) {
    stop("Sin registros tras aplicar filtros (revisa camp/years/quarter/aphia).")
  }

  # --- 4. Aviso: anyos con lances CA pero Age = NA en todos los casos ---------
  # (distinto de "no hay datos ese anyo": aqui SI hay muestreo, pero no se
  # han leido los otolitos, y el panel del lattice saldria vacio sin avisar).
  if (isTRUE(verbose) && "Year" %in% names(df)) {
    yr_all_na <- tapply(df$Age, df$Year, function(x) length(x) > 0 && all(is.na(x)))
    na_years <- names(yr_all_na)[yr_all_na]
    if (length(na_years) > 0) {
      message(sprintf(
        "Aviso: %s tiene(n) lances CA para esta especie pero Age = NA en TODOS los registros ",
        paste(na_years, collapse = ", ")),
        "(otolitos no leidos ese anyo; el panel saldra vacio).")
    }
  }

  df <- df[!is.na(df$Age) & !is.na(df$Lngt), ]
  if (!is.null(maxlength)) df <- df[df$Lngt < maxlength, ]

  if (nrow(df) == 0) {
    stop("Sin registros con Age y Lngt validos tras los filtros ",
         "(revisa camp/years/quarter/aphia/maxlength, y el aviso de anyos con Age=NA si ha salido).")
  }

  df$Year <- as.factor(df$Year)
  n_years <- length(unique(df$Year))

  # --- 5. Layout de paneles en funcion del numero de anyos --------------------
  ncolp <- min(ncol_max, n_years)
  nrowp <- ceiling(n_years / ncolp)

  # --- 6. Grupos por madurez ---------------------------------------------------
  grp <- NULL
  if (isTRUE(maturity) && "Maturity" %in% names(df)) grp <- df$Maturity > 61

  # --- 7. Grafico lattice -------------------------------------------------------
  pl <- lattice::xyplot(
    Lngt ~ Age | Year,
    data     = df,
    groups   = grp,
    pch      = 21,
    cex      = cex,
    auto.key = !is.null(grp),
    layout   = c(ncolp, nrowp),
    as.table = as_table,
    scales   = scales,
    xlab     = "Age",
    ylab     = ylab_txt,
    ...
  )

  print(pl)
  invisible(pl)
  if (out.dat) return(df)
}
