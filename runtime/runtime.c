/*
 * Yorum Runtime Library
 *
 * This minimal C runtime provides the bridge between Yorum's LLVM IR output
 * and the operating system. The LLVM IR emitted by the Yorum compiler already
 * includes built-in print functions that call printf/puts directly, so this
 * file serves as documentation and as a linkable runtime for extended features.
 *
 * Compile:
 *   clang -c runtime.c -o runtime.o
 *
 * Link with Yorum-compiled object files:
 *   clang program.o runtime.o -o program
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>

/* ── Memory management ─────────────────────────────────────── */

void* yorum_alloc(int64_t size) {
    void* ptr = malloc((size_t)size);
    if (!ptr) {
        fprintf(stderr, "yorum: out of memory (requested %lld bytes)\n", (long long)size);
        abort();
    }
    return ptr;
}

void yorum_free(void* ptr) {
    free(ptr);
}

/* ── Panic handler ─────────────────────────────────────────── */

void yorum_panic(const char* message) {
    fprintf(stderr, "yorum panic: %s\n", message);
    abort();
}

/* ── Contract violation ────────────────────────────────────── */

void yorum_contract_violation(const char* kind, const char* func_name) {
    fprintf(stderr, "yorum: %s contract violated in function '%s'\n", kind, func_name);
    abort();
}
