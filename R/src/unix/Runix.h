#include <R_ext/RStartup.h>

extern "C"
void fpu_setup(Rboolean);	/* ./sys-unix.cpp */

void Rstd_read_history(const char *s);

void Rstd_Suicide(const char *s);
void Rstd_ShowMessage(const char *s);
int  Rstd_ReadConsole(const char *prompt, unsigned char *buf, size_t len,
		      int addtohistory);
void Rstd_WriteConsole(const char *buf, int len);
void Rstd_WriteConsoleEx(const char *buf, int len, int otype);
void Rstd_ResetConsole(void);
void Rstd_FlushConsole(void);
void Rstd_ClearerrConsole(void);
void Rstd_Busy(int which);
NORET void Rstd_CleanUp(SA_TYPE saveact, int status, int runLast);
int  Rstd_ShowFiles(int nfile, const char **file, const char **headers,
		    const char *wtitle, bool del, const char *pager);
size_t  Rstd_ChooseFile(int _new, char *buf, size_t len);
void Rstd_loadhistory(SEXP call, SEXP op, SEXP args, SEXP env);
void Rstd_savehistory(SEXP call, SEXP op, SEXP args, SEXP env);
void Rstd_addhistory(SEXP call, SEXP op, SEXP args, SEXP env);

void R_load_X11_shlib(void);
