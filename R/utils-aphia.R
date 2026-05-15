.aphia_cache <- new.env(parent = emptyenv())

aphia_to_name <- function(aphia) {
  key <- as.character(aphia)
  if (!exists(key, envir = .aphia_cache)) {
    nm <- tryCatch(
      unname(unlist(worrms::wm_id2name_(as.numeric(aphia)))),
      error = function(e) NA_character_
    )
    assign(key, nm, envir = .aphia_cache)
  }
  get(key, envir = .aphia_cache)
}
