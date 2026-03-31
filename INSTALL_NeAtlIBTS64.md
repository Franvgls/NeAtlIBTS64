# NeAtlIBTS64 — Guía de instalación

**Paquete R para el IBTSWG (ICES) — Atlántico Nordeste**  
Migración a R 4.4+ (64 bits) del paquete original NeAtlIBTS (R 3.6.3, 32 bits).

---

## Requisitos previos

| Requisito | Versión mínima | Notas |
|-----------|---------------|-------|
| R | 4.4.0 | 64 bits. Descargar en https://cran.r-project.org |
| RTools | 4.4 | Necesario para compilar paquetes desde fuente en Windows. Descargar en https://cran.r-project.org/bin/windows/Rtools/ |
| RStudio | Cualquiera reciente | Recomendado. https://posit.co/download/rstudio-desktop/ |

> **Nota sobre disco**: si el disco C: tiene poco espacio, instalar R y las librerías en D: o la unidad disponible. En RStudio: `Tools > Global Options > Packages > Default library path`.

---

## Paso 1 — Instalar dependencias

Ejecutar en la consola de R **antes** de instalar el paquete:

```r
install.packages(c(
  "devtools",
  "remotes",
  "foreign",
  "sf",
  "sp",
  "maps",
  "mapdata",
  "icesDatras",
  "dplyr",
  "geosphere",
  "lubridate",
  "suncalc",
  "worrms",
  "grDevices"
))
```

Si algún paquete da error de compilación, probar con:

```r
install.packages("nombre_paquete", type = "binary")
```

---

## Paso 2 — Instalar NeAtlIBTS64 desde GitHub

```r
remotes::install_github("Franvgls/NeAtlIBTS64")
```

Si da problemas de autenticación o timeout, alternativa con devtools:

```r
devtools::install_github("Franvgls/NeAtlIBTS64")
```

Para instalar una versión o rama concreta:

```r
remotes::install_github("Franvgls/NeAtlIBTS64", ref = "main")
```

---

## Paso 3 — Instalar CampR64 desde GitHub (si se necesita)

```r
remotes::install_github("Franvgls/CampR64")
```

---

## Paso 4 — Verificar la instalación

```r
library(NeAtlIBTS64)

# Comprobar que los datasets del paquete están disponibles
data(IBTSsurvs)
head(IBTSsurvs)

# Comprobar versión de mapdata (debe ser 2.3.x)
packageVersion("mapdata")

# El namespace de mapdata estará vacío — es normal
ls(asNamespace("mapdata"))   # devuelve character(0)
```

---

## Problema conocido: mapdata en R 4.4+

`maps::map("worldHires", ...)` falla en R 4.4 porque `mapdata` 2.3.1 tiene el namespace vacío. El paquete incluye la solución internamente (asignación temporal al globalenv con `on.exit()`), pero si se usa `maps::map()` directamente fuera del paquete puede fallar.

**Solución manual si se necesita fuera del paquete:**

```r
.e <- new.env(parent = emptyenv())
utils::data("worldHiresMapEnv", package = "mapdata", envir = .e)
assign("worldHiresMapEnv", .e$worldHiresMapEnv, envir = globalenv())
on.exit(suppressWarnings(rm("worldHiresMapEnv", envir = globalenv())), add = TRUE)

maps::map("worldHires", xlim = c(-18, 3), ylim = c(35, 62))
```

---

## Shapefiles necesarios

Las funciones de mapa requieren shapefiles en la carpeta `inst/shapes/` del paquete. Si se instala desde GitHub estos se incluyen automáticamente.

Para verificar que están disponibles:

```r
list.files(system.file("shapes", package = "NeAtlIBTS64"))
```

Deben aparecer al menos: `ices_div`, `100m`, `bathy_geb`, `SWC_Q1`, `SCOROC`, `NI_IBTS`, `IGFS`, `Porcupine`, `CGFS_stratum`, `EVHOE`, `Sp_North.WGS84`, `PT_IBTS_2015`, `Sp_Cadiz`.

---

## Uso rápido

### Ciclo de trabajo habitual

```r
library(NeAtlIBTS64)

# 1. Resumen de campañas del año
ibts25 <- IBTSSurveySummary(2025)
ibts25$summary

# 2. Diagrama de Gantt
IBTSGantt(ibts25, 2025, IBTSsurvs = IBTSsurvs)

# 3. Mapa de posiciones de lances
IBTSNeAtl_map64(load = FALSE, newdev = TRUE)
IBTSPositMap(ibts25, 2025, quarters = c(3, 4), IBTSsurvs = IBTSsurvs, add = TRUE)

# 4. CPUE por especie (requiere tabla de especies)
spcodes <- data.frame(
  Code        = c("HKE",             "COD"),
  WoRMSCode   = c(126484,            126436),
  Common      = c("European hake",   "Atlantic cod"),
  LengthSplit = c(20,                23),
  stringsAsFactors = FALSE
)

# EVHOE requiere timeout ampliado por volumen de datos
options(timeout = 300)
cpue25_evhoe <- IBTSGetSpeciesCPUE(2025, species = spcodes,
                                    surveys = list(list(surv = "EVHOE", q = 4)),
                                    HHdata = ibts25)
options(timeout = 60)

# Resto de campañas
cpue25 <- IBTSGetSpeciesCPUE(2025, species = spcodes,
                              quarters = c(3, 4), HHdata = ibts25)

# Unir EVHOE al resto
cpue25 <- rbind(cpue25, cpue25_evhoe)

# 5. Mapa de distribución
MapIBTS64(cpue25, esp = "HKE", year = 2025,
          heading = "Hake <20 cm", dato = 1, NS = FALSE,
          IBTSsurvs = IBTSsurvs,
          metafile = TRUE, nfile = "HKE_small_2025",
          png_w = 1450, png_h = 2400, png_res = 150, pt = 18)
```

### Generar PNG con dos paneles

```r
png("posiciones_2025.png", width = 2400, height = 2000, pointsize = 18)
par(mfrow = c(1, 2))

IBTSNeAtl_map64(load = FALSE, newdev = FALSE, leg = FALSE)
IBTSPositMap(ibts25, 2025, quarters = 1, IBTSsurvs = IBTSsurvs, add = TRUE)
title("Q1 2025", font.main = 2)

IBTSNeAtl_map64(load = FALSE, newdev = FALSE, leg = FALSE)
IBTSPositMap(ibts25, 2025, quarters = c(3, 4), IBTSsurvs = IBTSsurvs, add = TRUE)
title("Q3+Q4 2025", font.main = 2)

dev.off()
```

---

## Desarrollo local (para modificar el paquete)

```r
# Cargar en sesión sin instalar (ciclo de desarrollo)
devtools::load_all("d:/FVG/GitHubRs/NeAtlIBTS64")

# Regenerar documentación desde roxygen
devtools::document("d:/FVG/GitHubRs/NeAtlIBTS64")

# Consultar ayuda de una función
?IBTSGetSpeciesCPUE

# Check completo
devtools::check("d:/FVG/GitHubRs/NeAtlIBTS64")

# Instalar definitivamente
devtools::install("d:/FVG/GitHubRs/NeAtlIBTS64")
```

> Al crear el proyecto en **Claude**, no conectar el repositorio GitHub completo — peta por tamaño. Usar solo el documento de contexto + archivos .R individuales de las funciones.

---

## Diagnóstico

```r
# Ver archivos con caracteres non-ASCII (deben ser 0 tras la limpieza)
for (f in list.files("d:/FVG/GitHubRs/NeAtlIBTS64/R",
                     pattern = "\\.R$", full.names = TRUE)) {
  out <- capture.output(tools::showNonASCIIfile(f))
  if (length(out) > 0) cat(basename(f), "\n")
}

# Verificar disponibilidad de una campaña en DATRAS
icesDatras::getSurveyYearQuarterList("EVHOE", 2025)

# Cerrar todos los dispositivos gráficos
graphics.off()
```

---

*NeAtlIBTS64 — Francisco Velasco, IEO Santander — IBTSWG/ICES*
