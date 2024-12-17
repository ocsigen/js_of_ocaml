/*
Primitives implemented by the WASI libc. Use 'dune build @recompile-libc'
to update libc.wasm.

clang -O2  --target=wasm32-wasi --sysroot=/path/to/wasi-libc/sysroot -nodefaultlibs -lc libc.c -o libc.wasm
*/

#include <stdio.h>
#include <math.h>
#include <stdlib.h>
#include <time.h>
#include <string.h>

__attribute__((export_name("cos")))
double libc_cos (double x) {
  return cos(x);
}

__attribute__((export_name("sin")))
double libc_sin (double x) {
  return sin(x);
}

__attribute__((export_name("tan")))
double libc_tan (double x) {
  return tan(x);
}

__attribute__((export_name("acos")))
double libc_acos (double x) {
  return acos(x);
}

__attribute__((export_name("asin")))
double libc_asin (double x) {
  return asin(x);
}

__attribute__((export_name("atan")))
double libc_atan (double x) {
  return atan(x);
}

__attribute__((export_name("cosh")))
double libc_cosh (double x) {
  return cosh(x);
}

__attribute__((export_name("sinh")))
double libc_sinh (double x) {
  return sinh(x);
}

__attribute__((export_name("tanh")))
double libc_tanh (double x) {
  return tanh(x);
}

__attribute__((export_name("acosh")))
double libc_acosh (double x) {
  return acosh(x);
}

__attribute__((export_name("asinh")))
double libc_asinh (double x) {
  return asinh(x);
}

__attribute__((export_name("atanh")))
double libc_atanh (double x) {
  return atanh(x);
}

__attribute__((export_name("cbrt")))
double libc_cbrt (double x) {
  return cbrt(x);
}

__attribute__((export_name("exp")))
double libc_exp (double x) {
  return exp(x);
}

__attribute__((export_name("expm1")))
double libc_expm1 (double x) {
  return expm1(x);
}

__attribute__((export_name("log")))
double libc_log (double x) {
  return log(x);
}

__attribute__((export_name("log1p")))
double libc_log1p (double x) {
  return log1p(x);
}

__attribute__((export_name("log2")))
double libc_log2 (double x) {
  return log2(x);
}

__attribute__((export_name("log10")))
double libc_log10 (double x) {
  return log10(x);
}

__attribute__((export_name("atan2")))
double libc_atan2 (double x, double y) {
  return atan2(x, y);
}

__attribute__((export_name("hypot")))
double libc_hypot (double x, double y) {
  return hypot(x, y);
}

__attribute__((export_name("pow")))
double libc_pow (double x, double y) {
  return pow(x, y);
}

__attribute__((export_name("fmod")))
double libc_fmod (double x, double y) {
  return fmod(x, y);
}

__attribute__((export_name("strtod")))
double libc_strtod (const char * buf, char ** end) {
  return strtod(buf, end);
}

__attribute__((export_name("format_float")))
int format_float (char * buf, size_t len, const char * fmt, double f) {
  return snprintf(buf, len, fmt, f);
}

__attribute__((export_name("malloc")))
void * libc_malloc (size_t len) {
  return malloc(len);
}

__attribute__((export_name("free")))
void libc_free (void * ptr) {
  return free(ptr);
}

__attribute__((export_name("strlen")))
size_t libc_strlen (const char * s) {
  return strlen(s);
}


__attribute__((export_name("gmtime")))
struct tm * libc_gmtime (const time_t * timep) {
  return gmtime(timep);
}

__attribute__((export_name("localtime")))
struct tm * libc_localtime (const time_t * timep) {
  return localtime(timep);
}

__attribute__((export_name("mktime")))
time_t libc_mktime(struct tm *tm) {
  return mktime(tm);
}

__attribute__((import_module("OCaml"), import_name("_initialize")))
void start(void);

int main () {
  start();
}
