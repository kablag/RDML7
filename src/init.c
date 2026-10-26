#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>

extern SEXP C_pcrd_decrypt(SEXP, SEXP);
extern SEXP C_qstd_find_curve(SEXP, SEXP, SEXP, SEXP);
extern SEXP C_qstd_string_candidates(SEXP, SEXP, SEXP);

static const R_CallMethodDef call_methods[] = {
    {"C_pcrd_decrypt", (DL_FUNC) &C_pcrd_decrypt, 2},
    {"C_qstd_find_curve", (DL_FUNC) &C_qstd_find_curve, 4},
    {"C_qstd_string_candidates", (DL_FUNC) &C_qstd_string_candidates, 3},
    {NULL, NULL, 0}
};

void R_init_RDML7(DllInfo *dll) {
    R_registerRoutines(dll, NULL, call_methods, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
