# Emacs: -*- r -*- vim: ft=r 
# $Id: sprng.R,v 1.3 2002/04/23 00:14:32 nali Exp $
#
# wrapper functions for SPRNG (Scalable Parallel Random Number Generator)
# library

init.sprng <- function (nstream, streamno,
                        seed = 0,
                        kindprng = "default", 
                        para = 0) {
    if (streamno >= nstream || streamno < 0) {
        stop ("streamno must be from 0 to nstream - 1.")
    }
    if (!is.character(kindprng) || length (kindprng) > 1) {
        stop ("kindprng' must be a character string of length 1.")
    }
    if (!is.na (pmatch (kindprng, "default"))) {
        kindprng <- "LFG"
    }
    kind <- pmatch (kindprng, c ("LFG", "LCG", "LCG64",
                                 "CMRG", "MLFG", "PMLCG")) - 1
    if (is.na (kind)) {
        stop(paste("'", kindprng, "' is not a valid choice", sep = ""))
    }
    .Call ("r_init_sprng", as.integer (kind), as.integer (streamno),
           as.integer (nstream), as.integer (seed), as.integer (para))
    if (exists (".Random.seed") && .Random.seed[1] == 5) {
        # Leftover from have used user defined rng and forgot
        # to switch back after last run
        set.seed (0)
    }
    RNGkind ("user")
}

pack.sprng <- function () {
    .Call ("r_pack_sprng")
}

unpack.sprng <- function (rngstate) {
    invisible (.Call ("r_unpack_sprng", as.integer (rngstate)))
}

free.sprng <- function (kind.old = c("Marsaglia-Multicarry",
                                     "Kinderman-Ramage")) {
    ## restore old RNG kind
    RNGkind (kind.old[1], kind.old[2])
    invisible (.Call ("r_free_sprng"))
}

spawn.new.sprng <- function (nstream,
                             seed = 0,
                             kindprng = "default", 
                             para = 0) {
    if (!is.character(kindprng) || length (kindprng) > 1) {
        stop ("kindprng' must be a character string of length 1.")
    }
    if (!is.na (pmatch (kindprng, "default"))) {
        kindprng <- "LFG"
    }
    kind <- pmatch (kindprng, c ("LFG", "LCG", "LCG64",
                                 "CMRG", "MLFG", "PMLCG")) - 1
    if (is.na (kind)) {
        stop(paste("'", kindprng, "' is not a valid choice", sep = ""))
    }
    newstreams <- .Call ("r_spawn_new_sprng", as.integer (kind), 
                        as.integer (nstream), as.integer (seed), 
                        as.integer (para))
    matrix (unlist (newstreams), ncol = length (newstreams))
}

spawn.sprng <- function (nspawn) {
    newstreams <- .Call ("r_spawn_sprng", as.integer (nspawn))
    matrix (unlist (newstreams), ncol = length (newstreams))
}

type.sprng <- function ()
    .Call ("r_type_sprng")
