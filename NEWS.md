# NeAtlIBTS64 0.0.5

## Funciones nuevas

* `SpeciesHLperYear()`: resumen de datos HL de DATRAS por especie con
  rangos de talla, número de medidas, capturas escaladas y número de
  lances. Útil para explorar disponibilidad de datos de tallas por
  survey/año/quarter.
* `GetAlkDTR.NeAtl64()`: descarga datos CA de DATRAS para una especie,
  survey, año y trimestre, y los formatea listos para construir una ALK.
* `GrafAlk.NeAtl64()`: representación gráfica de la clave talla-edad
  como barras apiladas de proporciones por edad, alimentada con datos
  de DATRAS. Título automático con nombre científico desde WoRMS.

## Mejoras

* `SpeciesCAperYear()`: nueva salida por defecto (`out = "summary"`) con
  data.frame por especie incluyendo N total, N con edad leída, rango de
  tallas y rango de edades. Opción `out = "matrix"` para mantener el
  comportamiento legacy.

## Limpieza

* Eliminado `MapLengths_nc()`: versión obsoleta que leía CSVs locales,
  reemplazada por `MapLengths()` que tira directamente de DATRAS.
* `%>%` registrado a nivel de paquete vía `usethis::use_pipe()`, ya no
  hay que importarlo en cada función.
* Ejemplos de funciones que requieren acceso a DATRAS o datos locales
  envueltos en `\dontrun{}` (gearPlot*, qcHauls*, SplitLengths*,
  SurveyMap.IBTS*, MapLengths, getDatras2).

# NeAtlIBTS64 0.0.6

## Funciones nuevas
* `AuditAlkCAMP_DATRAS()`: compara otolitos por talla entre el CAMP
  local (via GetAlk.camp64) y DATRAS, detectando discrepancias y
  marcando tallas con datos imputados (flag 99/100 IEO).

## Mejoras
* `GetAlkDTR.NeAtl64()`: guarda n_por_talla como atributo antes de
  calcular proporciones, permitiendo llamadas anidadas directas dentro
  de GrafAlk.NeAtl64 sin perder el n encima de las barras.
* `GrafAlk.NeAtl64()`: acepta ahora dos formatos de entrada — formato
  agregado (talla, sexo, E0..Eplus+, salida de GetAlkDTR.NeAtl64) y
  formato individual (LngtClasscm, Age, CA records crudos).
