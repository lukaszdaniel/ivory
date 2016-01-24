/* Automatically generated from all.nw using noweb */
#include "survS.h"
void agsurv5(Sint *n2, Sint *nvar2, Sint *dd, double *x1, double *x2,
		double *xsum, double *xsum2, double *sum1, double *sum2, double *xbar) {
	double temp;
	int i, j, k, kk;
	double d;
	int n, nvar;

	n = n2[0];
	nvar = nvar2[0];

	for (int i = 0; i < n; i++) {
		d = dd[i];
		if (d == 1) {
			temp = 1 / x1[i];
			sum1[i] = temp;
			sum2[i] = temp * temp;
			for (int k = 0; k < nvar; k++)
				xbar[i + n * k] = xsum[i + n * k] * temp * temp;
		} else {
			temp = 1 / x1[i];
			for (int j = 0; j < d; j++) {
				temp = 1 / (x1[i] - x2[i] * j / d);
				sum1[i] += temp / d;
				sum2[i] += temp * temp / d;
				for (int k = 0; k < nvar; k++) {
					kk = i + n * k;
					xbar[kk] += ((xsum[kk] - xsum2[kk] * j / d) * temp * temp) / d;
				}
			}
		}
	}
}
