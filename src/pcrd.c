#include <R.h>
#include <Rinternals.h>
#include <stdint.h>

static uint32_t crc_table[256];
static int crc_table_ready = 0;

static void init_crc_table(void) {
    uint32_t i, j, crc;
    if (crc_table_ready) return;
    for (i = 0; i < 256; ++i) {
        crc = i;
        for (j = 0; j < 8; ++j)
            crc = (crc >> 1) ^ ((crc & 1) ? UINT32_C(0xedb88320) : 0);
        crc_table[i] = crc;
    }
    crc_table_ready = 1;
}

static uint32_t crc_byte(uint32_t crc, unsigned char value) {
    return (crc >> 8) ^ crc_table[(crc ^ value) & 0xff];
}

static void update_keys(uint32_t keys[3], unsigned char value) {
    keys[0] = crc_byte(keys[0], value);
    keys[1] = (keys[1] + (keys[0] & 0xff)) * UINT32_C(134775813) + 1;
    keys[2] = crc_byte(keys[2], (unsigned char)(keys[1] >> 24));
}

SEXP C_pcrd_decrypt(SEXP data, SEXP password) {
    R_xlen_t i, size;
    uint32_t keys[3] = {
        UINT32_C(0x12345678), UINT32_C(0x23456789), UINT32_C(0x34567890)
    };
    const unsigned char *input, *pwd;
    unsigned char *output, value;
    uint32_t temp;
    SEXP result;

    if (TYPEOF(data) != RAWSXP)
        error("`data` must be a raw vector");
    if (TYPEOF(password) != STRSXP || XLENGTH(password) != 1)
        error("`password` must be one string");

    init_crc_table();
    pwd = (const unsigned char *) CHAR(STRING_ELT(password, 0));
    while (*pwd) update_keys(keys, *pwd++);

    size = XLENGTH(data);
    result = PROTECT(allocVector(RAWSXP, size));
    input = RAW(data);
    output = RAW(result);
    for (i = 0; i < size; ++i) {
        temp = keys[2] | 2;
        value = input[i] ^ (unsigned char)(((temp * (temp ^ 1)) >> 8) & 0xff);
        output[i] = value;
        update_keys(keys, value);
    }
    UNPROTECT(1);
    return result;
}

static uint32_t uint32_le(const unsigned char *x) {
    return ((uint32_t)x[0]) | ((uint32_t)x[1] << 8) |
        ((uint32_t)x[2] << 16) | ((uint32_t)x[3] << 24);
}

SEXP C_qstd_find_curve(SEXP data, SEXP from, SEXP well_count, SEXP dye_count) {
    const unsigned char *x;
    R_xlen_t size, offset, last, i;
    uint32_t wells, dyes, cycles;
    SEXP result;

    if (TYPEOF(data) != RAWSXP) error("`data` must be a raw vector");
    x = RAW(data);
    size = XLENGTH(data);
    offset = (R_xlen_t) asReal(from);
    wells = (uint32_t) asInteger(well_count);
    dyes = (uint32_t) asInteger(dye_count);
    last = size > 32 ? size - 32 : 0;

    for (; offset <= last; ++offset) {
        if (uint32_le(x + offset) != 1 ||
            uint32_le(x + offset + 4) != wells ||
            uint32_le(x + offset + 8) != dyes)
            continue;
        cycles = uint32_le(x + offset + 12);
        if (cycles < 1 || cycles > 500 || offset + 16 + 4 * cycles > size)
            continue;
        for (i = 0; i < cycles; ++i)
            if (uint32_le(x + offset + 16 + 4 * i) != i + 1) break;
        if (i == cycles) {
            result = PROTECT(allocVector(REALSXP, 2));
            REAL(result)[0] = (double) offset;
            REAL(result)[1] = (double) cycles;
            UNPROTECT(1);
            return result;
        }
    }
    return R_NilValue;
}

SEXP C_qstd_string_candidates(SEXP data, SEXP from, SEXP to) {
    const unsigned char *x;
    R_xlen_t size, start, end, offset, count = 0, i = 0;
    uint32_t chars;
    SEXP result, offsets, lengths;

    if (TYPEOF(data) != RAWSXP) error("`data` must be a raw vector");
    x = RAW(data);
    size = XLENGTH(data);
    start = (R_xlen_t) asReal(from);
    end = (R_xlen_t) asReal(to);
    if (end > size - 4) end = size - 4;
    for (offset = start; offset <= end; ++offset) {
        chars = uint32_le(x + offset);
        if (chars >= 1 && chars <= 256 && offset + 4 + 2 * chars <= size)
            ++count;
    }
    result = PROTECT(allocVector(VECSXP, 2));
    offsets = PROTECT(allocVector(INTSXP, count));
    lengths = PROTECT(allocVector(INTSXP, count));
    for (offset = start; offset <= end; ++offset) {
        chars = uint32_le(x + offset);
        if (chars >= 1 && chars <= 256 && offset + 4 + 2 * chars <= size) {
            INTEGER(offsets)[i] = (int) offset;
            INTEGER(lengths)[i] = (int) chars;
            ++i;
        }
    }
    SET_VECTOR_ELT(result, 0, offsets);
    SET_VECTOR_ELT(result, 1, lengths);
    UNPROTECT(3);
    return result;
}
