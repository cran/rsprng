/*
 * Wrapper to core functions of sprng.
 * $Id: sprng_core.c,v 1.3 2002/04/23 00:14:32 nali Exp $
 *
 **/

#include <R.h>
#include <Rdefines.h>
#include <R_ext/Random.h>

#include "sprng.h"

#include <string.h>


/*
 *  This struct is used to retrieve "rng_type" from the rng specific
 * "struct rngen". RNGs have different definations for "struct rngen",
 *  however, its first field must be the integer "rng_type"
 *
 *  It is defined in sprng.c but not in sprng.h
 */
struct rngen
{
    int rng_type;
};

static int *streamid = 0;
static double rn;

SEXP r_init_sprng (SEXP sexp_gtype,
                   SEXP sexp_streamno,
                   SEXP sexp_nstreams,
                   SEXP sexp_seed,
                   SEXP sexp_param)
{
    int gtype    = INTEGER (sexp_gtype)[0];
    int streamno = INTEGER (sexp_streamno)[0];
    int nstreams = INTEGER (sexp_nstreams)[0];
    int seed     = INTEGER (sexp_seed)[0];
    int param    = INTEGER (sexp_param)[0];

    streamid = init_sprng (gtype, streamno, nstreams, seed, param);
    return R_NilValue;
}

SEXP r_pack_sprng ()
{
    char *rng_buffer;
    int s = 0;
    int i;
    SEXP sexp_buffer;
    if (streamid) {
        s = pack_sprng (streamid, &rng_buffer);
        PROTECT (sexp_buffer = allocVector (INTSXP, s));
        for (i = 0; i < s; ++i) {
            INTEGER(sexp_buffer)[i] = (int) rng_buffer[i];
        }
        UNPROTECT (1);
        return sexp_buffer;
    } else {
        return R_NilValue;
    }
}

SEXP r_unpack_sprng (SEXP sexp_packed_stream)
{
    SEXP sexp_oldrng_buffer;
    char *rng_buffer;
    int s = length (sexp_packed_stream);
    int i;

    rng_buffer = (char *) R_alloc (s, sizeof (char));
    for (i = 0; i < s; ++i) {
        rng_buffer[i] = (char) INTEGER (sexp_packed_stream)[i];
    }
    if (streamid) {
        sexp_oldrng_buffer = r_pack_sprng ();
        free_sprng (streamid);
    }
    streamid = unpack_sprng (rng_buffer);

    if (streamid) {
        return sexp_oldrng_buffer;
    } else {
        return R_NilValue;
    }
}

SEXP r_free_sprng ()
{
    SEXP sexp_oldrng_buffer;
    int nstream;
    if (streamid) {
        sexp_oldrng_buffer = r_pack_sprng ();
        nstream = free_sprng (streamid);
        streamid = 0;
        return sexp_oldrng_buffer;
    } else {
        return R_NilValue;
    }
}

SEXP r_spawn_new_sprng (SEXP sexp_gtype,
                        SEXP sexp_nstreams,
                        SEXP sexp_seed,
                        SEXP sexp_param)
{
    int gtype    = INTEGER (sexp_gtype)[0];
    int nstreams = INTEGER (sexp_nstreams)[0];
    int seed     = INTEGER (sexp_seed)[0];
    int param    = INTEGER (sexp_param)[0];
    int  *savedstream = streamid;
    int i;
    SEXP sexp_spawned_streams;
    PROTECT (sexp_spawned_streams = allocVector (VECSXP, nstreams));
    for (i = 0; i < nstreams; ++i) {
        streamid = init_sprng (gtype, i, nstreams, seed, param);
        SET_VECTOR_ELT (sexp_spawned_streams, i, r_pack_sprng ());
        free_sprng (streamid);
    }
    UNPROTECT (1);
    streamid = savedstream;
    return sexp_spawned_streams;
}

SEXP r_spawn_sprng (SEXP sexp_nspawned)
{
    int **newstreams;
    int *savedstream = streamid;
    int i;
    int nspawned = INTEGER (sexp_nspawned)[0];
    SEXP sexp_spawned_streams;

    if (streamid) {
        nspawned = spawn_sprng (streamid, nspawned, &newstreams);
    } else {
        error ("No active sprng avaiable!");
        return R_NilValue;
    }

    PROTECT (sexp_spawned_streams = allocVector (VECSXP, nspawned));
    for (i = 0; i < nspawned; ++i) {
        streamid = newstreams[i];
        SET_VECTOR_ELT (sexp_spawned_streams, i, r_pack_sprng ());
        free_sprng (streamid);
    }
    UNPROTECT (1);
    streamid = savedstream;
    return sexp_spawned_streams;
}

SEXP r_type_sprng ()
{
    int gtype = ((struct rngen *) streamid)->rng_type;
    char *type;
    switch (gtype) {
    case SPRNG_LFG :
        type = "LFG";
        break;
    case SPRNG_LCG :
        type = "LCG";
        break;
    case SPRNG_LCG64 :
        type = "LCF64";
        break;
    case SPRNG_CMRG :
        type = "CMRG";
        break;
    case SPRNG_MLFG :
        type = "MLFG";
        break;
    case SPRNG_PMLCG :
        type = "PMLCG";
        break;
    default :
        break;
    }
    return mkString (type);
}

double *user_unif_rand ()
{
    rn = sprng (streamid);
    return &rn;
}

