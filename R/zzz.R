# Emacs: -*- r -*- vim: ft=r 
.First.lib <- function (lib, pkg) {
    library.dynam("rsprng", pkg, lib)
}
