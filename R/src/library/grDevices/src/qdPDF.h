#include <R.h>
#include <R_ext/QuartzDevice.h>

#ifdef __cplusplus
extern "C"
#endif
QuartzDesc_t QuartzPDF_DeviceCreate(void *dd, QuartzFunctions_t *fn, QuartzParameters_t *par);

