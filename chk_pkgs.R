libdir <- file.path(Sys.getenv("USERPROFILE"), "Documents", "R", "win-library", "4.5")
.libPaths(c(libdir, .libPaths()))
cat(.libPaths(), sep="\n")
for (p in c("icesDatras","dplyr","devtools")) cat(p, requireNamespace(p, quietly=TRUE), "\n")