#ifndef __SYLIB_H_
#define __SYLIB_H_

#include <stdarg.h>
#include <stdio.h>
#include <sys/time.h>
/* Input & output functions */
int getint(), getch(), getarray(int a[]);
void putint(int a), putch(int a), putarray(int n, int a[]);

/* Timing function implementation */
extern struct timeval _sysy_start, _sysy_end;
#define _SYSY_N 1024
extern int _sysy_l1[_SYSY_N], _sysy_l2[_SYSY_N];
extern int _sysy_h[_SYSY_N], _sysy_m[_SYSY_N], _sysy_s[_SYSY_N], _sysy_us[_SYSY_N];
extern int _sysy_idx;
__attribute((constructor)) void before_main();
__attribute((destructor)) void after_main();
void _sysy_starttime(int lineno);
void _sysy_stoptime(int lineno);
void starttime();
void stoptime();

#endif
