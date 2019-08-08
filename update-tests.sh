#!/bin/bash

source conf-names.sh

cmd="meld"

if test "$2" = "cp"; then
 cmd="cp"
fi;

cd $IVORY/tests

#base tests
if [[ "$1" == "core" ]] || [[ "$1" == "all" ]]; then
`$cmd any-all.Rout		any-all.Rout.save`
`$cmd arith.Rout		arith.Rout.save`
`$cmd arith-true.Rout		arith-true.Rout.save`
#`$cmd array-subset.Rout		array-subset.Rout.save`
`$cmd complex.Rout		complex.Rout.save`
`$cmd datasets.Rout		datasets.Rout.save`
`$cmd datetime.Rout		datetime.Rout.save`
`$cmd datetime2.Rout		datetime2.Rout.save`
`$cmd demos.Rout		demos.Rout.save`
`$cmd d-p-q-r-tests.Rout	d-p-q-r-tests.Rout.save`
`$cmd eval-etc.Rout		eval-etc.Rout.save`
`$cmd eval-etc-2.Rout		eval-etc-2.Rout.save`
`$cmd iec60559.Rout		iec60559.Rout.save`
`$cmd internet.Rout		internet.Rout.save`
`$cmd isas-tests.Rout		isas-tests.Rout.save`
`$cmd lapack.Rout		lapack.Rout.save`
`$cmd lm-tests.Rout		lm-tests.Rout.save`
`$cmd method-dispatch.Rout	method-dispatch.Rout.save`
`$cmd ok-errors.Rout		ok-errors.Rout.save`
#`$cmd packages.Rout		packages.Rout.save`
`$cmd print-tests.Rout		print-tests.Rout.save`
`$cmd p-r-random-tests.Rout	p-r-random-tests.Rout.save`
`$cmd reg-examples3.Rout	reg-examples3.Rout.save`
`$cmd reg-IO.Rout		reg-IO.Rout.save`
`$cmd reg-IO2.Rout		reg-IO2.Rout.save`
#`$cmd reg-plot.Rout		reg-plot.Rout.save`
 if [[ "$2" == "cp" ]]; then
 `$cmd reg-plot.pdf		reg-plot.pdf.save`
 `$cmd reg-plot-latin1.pdf	reg-plot-latin1.pdf.save`
 fi;
`$cmd reg-S4.Rout		reg-S4.Rout.save`
`$cmd reg-tests-2.Rout		reg-tests-2.Rout.save`
`$cmd reg-tests-3.Rout		reg-tests-3.Rout.save`
`$cmd R-intro.Rout		R-intro.Rout.save`
`$cmd simple-true.Rout		simple-true.Rout.save`
#`$cmd testit-Ex.R		testit-Ex.R.save`
#`$cmd testit.html		testit.html.save`
#`$cmd testit.txt		testit.txt.save`
#`$cmd testit.tex		testit.tex.save`
#`$cmd test-system.Rout		test-system.Rout.save`
#`$cmd ver20.html		ver20.html.save`
#`$cmd ver20-Ex.R		ver20-Ex.R.save`
#`$cmd ver20.tex			ver20.tex.save`
#`$cmd ver20.txt			ver20.txt.save`

#Examples
cd Examples
`$cmd datasets-Ex.Rout	datasets-Ex.Rout.save`
`$cmd graphics-Ex.Rout	graphics-Ex.Rout.save`
`$cmd grDevices-Ex.Rout	grDevices-Ex.Rout.save`
`$cmd grid-Ex.Rout	grid-Ex.Rout.save`
`$cmd splines-Ex.Rout	splines-Ex.Rout.save`
`$cmd stats4-Ex.Rout	stats4-Ex.Rout.save`
`$cmd stats-Ex.Rout	stats-Ex.Rout.save`
`$cmd tools-Ex.Rout	tools-Ex.Rout.save`
cd ..

`$cmd grDevices.Rcheck/tests/ps-tests.Rout	../src/library/grDevices/tests/ps-tests.Rout.save`
`$cmd grDevices.Rcheck/tests/xfig-tests.Rout	../src/library/grDevices/tests/xfig-tests.Rout.save`

`$cmd stats.Rcheck/tests/glm.Rout	../src/library/stats/tests/glm.Rout.save`
`$cmd stats.Rcheck/tests/ks-test.Rout	../src/library/stats/tests/ks-test.Rout.save`
`$cmd stats.Rcheck/tests/nls.Rout	../src/library/stats/tests/nls.Rout.save`
`$cmd stats.Rcheck/tests/simulate.Rout	../src/library/stats/tests/simulate.Rout.save`
`$cmd stats.Rcheck/tests/bandwidth.Rout	../src/library/stats/tests/bandwidth.Rout.save`

`$cmd grid.Rcheck/tests/testls.Rout	../src/library/grid/tests/testls.Rout.save`


`$cmd parallel.Rcheck/tests/multicore2.Rout	../src/library/parallel/tests/multicore2.Rout.save`
`$cmd parallel.Rcheck/tests/snow2.Rout		../src/library/parallel/tests/snow2.Rout.save`
fi;




if [[ "$1" == "cluster" ]] || [[ "$1" == "all" ]]; then
#cluster
`$cmd cluster.Rcheck/tests/agnes-ex.Rout			../../$RECIVORY/cluster/tests/agnes-ex.Rout.save`
`$cmd cluster.Rcheck/tests/daisy-ex.Rout			../../$RECIVORY/cluster/tests/daisy-ex.Rout.save`
`$cmd cluster.Rcheck/tests/clara.Rout			../../$RECIVORY/cluster/tests/clara.Rout.save`
`$cmd cluster.Rcheck/tests/clara-NAs.Rout		../../$RECIVORY/cluster/tests/clara-NAs.Rout.save`
`$cmd cluster.Rcheck/tests/clusplot-out.Rout		../../$RECIVORY/cluster/tests/clusplot-out.Rout.save`
`$cmd cluster.Rcheck/tests/diana-ex.Rout			../../$RECIVORY/cluster/tests/diana-ex.Rout.save`
`$cmd cluster.Rcheck/tests/ellipsoid-ex.Rout		../../$RECIVORY/cluster/tests/ellipsoid-ex.Rout.save`
`$cmd cluster.Rcheck/tests/mona.Rout			../../$RECIVORY/cluster/tests/mona.Rout.save`
`$cmd cluster.Rcheck/tests/pam.Rout			../../$RECIVORY/cluster/tests/pam.Rout.save`
`$cmd cluster.Rcheck/tests/silhouette-default.Rout	../../$RECIVORY/cluster/tests/silhouette-default.Rout.save`
fi;




if [[ "$1" == "foreign" ]] || [[ "$1" == "all" ]]; then
`$cmd	foreign.Rcheck/tests/arff.Rout			../../$RECIVORY/foreign/tests/arff.Rout.save`
`$cmd	foreign.Rcheck/tests/octave.Rout		../../$RECIVORY/foreign/tests/octave.Rout.save`
`$cmd	foreign.Rcheck/tests/stata.Rout			../../$RECIVORY/foreign/tests/stata.Rout.save`
`$cmd	foreign.Rcheck/tests/xport.Rout			../../$RECIVORY/foreign/tests/xport.Rout.save`
`$cmd	foreign.Rcheck/tests/minitab.Rout		../../$RECIVORY/foreign/tests/minitab.Rout.save`
`$cmd	foreign.Rcheck/tests/S3.Rout			../../$RECIVORY/foreign/tests/S3.Rout.save`
`$cmd	foreign.Rcheck/tests/testEmpty.Rout		../../$RECIVORY/foreign/tests/testEmpty.Rout.save`
`$cmd	foreign.Rcheck/tests/mval_bug.Rout		../../$RECIVORY/foreign/tests/mval_bug.Rout.save`
`$cmd	foreign.Rcheck/tests/spss.Rout			../../$RECIVORY/foreign/tests/spss.Rout.save`
`$cmd	foreign.Rcheck/tests/writeForeignSPSS.Rout	../../$RECIVORY/foreign/tests/writeForeignSPSS.Rout.save`
fi;


if [[ "$1" == "boot" ]] || [[ "$1" == "all" ]]; then
`$cmd boot.Rcheck/boot-Ex.Rout	../../$RECIVORY/boot/tests/Examples/boot-Ex.Rout.save`
fi;


if [[ "$1" == "MASS" ]] || [[ "$1" == "all" ]]; then
`$cmd MASS.Rcheck/tests/fitdistr.Rout	../../$RECIVORY/MASS/tests/fitdistr.Rout.save`
`$cmd MASS.Rcheck/tests/regression.Rout	../../$RECIVORY/MASS/tests/regression.Rout.save`
`$cmd MASS.Rcheck/MASS-Ex.Rout	../../$RECIVORY/MASS/tests/Examples/MASS-Ex.Rout.save`
fi;

if [[ "$1" == "Matrix" ]] || [[ "$1" == "all" ]]; then
`$cmd Matrix.Rcheck/tests/bind.Rout	../../$RECIVORY/Matrix/tests/bind.Rout.save`
`$cmd Matrix.Rcheck/tests/indexing.Rout	../../$RECIVORY/Matrix/tests/indexing.Rout.save`
fi;

if [[ "$1" == "nlme" ]] || [[ "$1" == "all" ]]; then
`$cmd nlme.Rcheck/tests/coef.Rout	../../$RECIVORY/nlme/tests/coef.Rout.save`
`$cmd nlme.Rcheck/tests/lme.Rout		../../$RECIVORY/nlme/tests/lme.Rout.save`
`$cmd nlme.Rcheck/tests/missing.Rout	../../$RECIVORY/nlme/tests/missing.Rout.save`
fi;


if [[ "$1" == "rpart" ]] || [[ "$1" == "all" ]]; then
`$cmd	rpart.Rcheck/tests/backticks.Rout	../../$RECIVORY/rpart/tests/backticks.Rout.save`
`$cmd	rpart.Rcheck/tests/cost.Rout		../../$RECIVORY/rpart/tests/cost.Rout.save`
`$cmd	rpart.Rcheck/tests/cptest.Rout		../../$RECIVORY/rpart/tests/cptest.Rout.save`
`$cmd	rpart.Rcheck/tests/priors.Rout		../../$RECIVORY/rpart/tests/priors.Rout.save`
`$cmd	rpart.Rcheck/tests/rescale.Rout		../../$RECIVORY/rpart/tests/rescale.Rout.save`
`$cmd	rpart.Rcheck/rpart-Ex.Rout		../../$RECIVORY/rpart/tests/Examples/rpart-Ex.Rout.save`
`$cmd	rpart.Rcheck/tests/testall.Rout		../../$RECIVORY/rpart/tests/testall.Rout.save`
`$cmd	rpart.Rcheck/tests/treble2.Rout		../../$RECIVORY/rpart/tests/treble2.Rout.save`
`$cmd	rpart.Rcheck/tests/treble3.Rout		../../$RECIVORY/rpart/tests/treble3.Rout.save`
`$cmd	rpart.Rcheck/tests/treble4.Rout		../../$RECIVORY/rpart/tests/treble4.Rout.save`
`$cmd	rpart.Rcheck/tests/treble.Rout		../../$RECIVORY/rpart/tests/treble.Rout.save`
`$cmd	rpart.Rcheck/tests/usersplits.Rout	../../$RECIVORY/rpart/tests/usersplits.Rout.save`
`$cmd	rpart.Rcheck/tests/xpred1.Rout		../../$RECIVORY/rpart/tests/xpred1.Rout.save`
`$cmd	rpart.Rcheck/tests/xpred2.Rout		../../$RECIVORY/rpart/tests/xpred2.Rout.save`
fi;


if [[ "$1" == "survival" ]] || [[ "$1" == "all" ]]; then
`$cmd	survival.Rcheck/tests/aareg.Rout		../../$RECIVORY/survival/tests/aareg.Rout.save`
`$cmd	survival.Rcheck/tests/anova.Rout		../../$RECIVORY/survival/tests/anova.Rout.save`
`$cmd	survival.Rcheck/tests/bladder.Rout		../../$RECIVORY/survival/tests/bladder.Rout.save`
`$cmd	survival.Rcheck/tests/book1.Rout		../../$RECIVORY/survival/tests/book1.Rout.save`
`$cmd	survival.Rcheck/tests/book2.Rout		../../$RECIVORY/survival/tests/book2.Rout.save`
`$cmd	survival.Rcheck/tests/book3.Rout		../../$RECIVORY/survival/tests/book3.Rout.save`
`$cmd	survival.Rcheck/tests/book4.Rout		../../$RECIVORY/survival/tests/book4.Rout.save`
`$cmd	survival.Rcheck/tests/book5.Rout		../../$RECIVORY/survival/tests/book5.Rout.save`
`$cmd	survival.Rcheck/tests/book6.Rout		../../$RECIVORY/survival/tests/book6.Rout.save`
`$cmd	survival.Rcheck/tests/book7.Rout		../../$RECIVORY/survival/tests/book7.Rout.save`
`$cmd	survival.Rcheck/tests/cancer.Rout		../../$RECIVORY/survival/tests/cancer.Rout.save`
`$cmd	survival.Rcheck/tests/clogit.Rout		../../$RECIVORY/survival/tests/clogit.Rout.save`
`$cmd	survival.Rcheck/tests/concordance2.Rout		../../$RECIVORY/survival/tests/concordance2.Rout.save`
`$cmd	survival.Rcheck/tests/concordance3.Rout		../../$RECIVORY/survival/tests/concordance3.Rout.save`
`$cmd	survival.Rcheck/tests/concordance.Rout		../../$RECIVORY/survival/tests/concordance.Rout.save`
`$cmd	survival.Rcheck/tests/counting.Rout		../../$RECIVORY/survival/tests/counting.Rout.save`
`$cmd	survival.Rcheck/tests/coxsurv2.Rout		../../$RECIVORY/survival/tests/coxsurv2.Rout.save`
`$cmd	survival.Rcheck/tests/coxsurv3.Rout		../../$RECIVORY/survival/tests/coxsurv3.Rout.save`
`$cmd	survival.Rcheck/tests/coxsurv4.Rout		../../$RECIVORY/survival/tests/coxsurv4.Rout.save`
`$cmd	survival.Rcheck/tests/coxsurv.Rout		../../$RECIVORY/survival/tests/coxsurv.Rout.save`
`$cmd	survival.Rcheck/tests/detail.Rout		../../$RECIVORY/survival/tests/detail.Rout.save`
`$cmd	survival.Rcheck/tests/difftest.Rout		../../$RECIVORY/survival/tests/difftest.Rout.save`
`$cmd	survival.Rcheck/tests/doaml.Rout		../../$RECIVORY/survival/tests/doaml.Rout.save`
`$cmd	survival.Rcheck/tests/doweight.Rout		../../$RECIVORY/survival/tests/doweight.Rout.save`
`$cmd	survival.Rcheck/tests/expected2.Rout		../../$RECIVORY/survival/tests/expected2.Rout.save`
`$cmd	survival.Rcheck/tests/expected.Rout		../../$RECIVORY/survival/tests/expected.Rout.save`
`$cmd	survival.Rcheck/tests/factor2.Rout		../../$RECIVORY/survival/tests/factor2.Rout.save`
`$cmd	survival.Rcheck/tests/factor.Rout		../../$RECIVORY/survival/tests/factor.Rout.save`
`$cmd	survival.Rcheck/tests/finegray.Rout		../../$RECIVORY/survival/tests/finegray.Rout.save`
`$cmd	survival.Rcheck/tests/frailty.Rout		../../$RECIVORY/survival/tests/frailty.Rout.save`
`$cmd	survival.Rcheck/tests/frank.Rout		../../$RECIVORY/survival/tests/frank.Rout.save`
`$cmd	survival.Rcheck/tests/fr_cancer.Rout		../../$RECIVORY/survival/tests/fr_cancer.Rout.save`
`$cmd	survival.Rcheck/tests/fr_kidney.Rout		../../$RECIVORY/survival/tests/fr_kidney.Rout.save`
`$cmd	survival.Rcheck/tests/fr_lung.Rout		../../$RECIVORY/survival/tests/fr_lung.Rout.save`
`$cmd	survival.Rcheck/tests/fr_ovarian.Rout		../../$RECIVORY/survival/tests/fr_ovarian.Rout.save`
`$cmd	survival.Rcheck/tests/fr_rat1.Rout		../../$RECIVORY/survival/tests/fr_rat1.Rout.save`
`$cmd	survival.Rcheck/tests/fr_rat2.Rout		../../$RECIVORY/survival/tests/fr_rat2.Rout.save`
`$cmd	survival.Rcheck/tests/fr_resid.Rout		../../$RECIVORY/survival/tests/fr_resid.Rout.save`
`$cmd	survival.Rcheck/tests/fr_simple.Rout		../../$RECIVORY/survival/tests/fr_simple.Rout.save`
`$cmd	survival.Rcheck/tests/infcox.Rout		../../$RECIVORY/survival/tests/infcox.Rout.save`
`$cmd	survival.Rcheck/tests/jasa.Rout			../../$RECIVORY/survival/tests/jasa.Rout.save`
`$cmd	survival.Rcheck/tests/model.matrix.Rout		../../$RECIVORY/survival/tests/model.matrix.Rout.save`
`$cmd	survival.Rcheck/tests/mstate.Rout		../../$RECIVORY/survival/tests/mstate.save`
`$cmd	survival.Rcheck/tests/nested.Rout		../../$RECIVORY/survival/tests/nested.Rout.save`
`$cmd	survival.Rcheck/tests/ovarian.Rout		../../$RECIVORY/survival/tests/ovarian.Rout.save`
`$cmd	survival.Rcheck/tests/prednew.Rout		../../$RECIVORY/survival/tests/prednew.Rout.save`
`$cmd	survival.Rcheck/tests/pspline.Rout		../../$RECIVORY/survival/tests/pspline.Rout.save`
`$cmd	survival.Rcheck/tests/pyear.Rout		../../$RECIVORY/survival/tests/pyear.Rout.save`
`$cmd	survival.Rcheck/tests/quantile.Rout		../../$RECIVORY/survival/tests/quantile.Rout.save`
`$cmd	survival.Rcheck/tests/ratetable.Rout		../../$RECIVORY/survival/tests/ratetable.Rout.save`
#`$cmd	survival.Rcheck/tests/r_capacitor.Rout		../../$RECIVORY/survival/tests/r_capacitor.Rout.save`
`$cmd	survival.Rcheck/tests/r_donnell.Rout		../../$RECIVORY/survival/tests/r_donnell.Rout.save`
`$cmd	survival.Rcheck/tests/r_lung.Rout		../../$RECIVORY/survival/tests/r_lung.Rout.save`
`$cmd	survival.Rcheck/tests/r_peterson.Rout		../../$RECIVORY/survival/tests/r_peterson.Rout.save`
`$cmd	survival.Rcheck/tests/r_resid.Rout		../../$RECIVORY/survival/tests/r_resid.Rout.save`
`$cmd	survival.Rcheck/tests/r_sas.Rout		../../$RECIVORY/survival/tests/r_sas.Rout.save`
`$cmd	survival.Rcheck/tests/r_scale.Rout		../../$RECIVORY/survival/tests/r_scale.Rout.save`
`$cmd	survival.Rcheck/tests/r_stanford.Rout		../../$RECIVORY/survival/tests/r_stanford.Rout.save`
`$cmd	survival.Rcheck/tests/r_strata.Rout		../../$RECIVORY/survival/tests/r_strata.Rout.save`
`$cmd	survival.Rcheck/tests/r_tdist.Rout		../../$RECIVORY/survival/tests/r_tdist.Rout.save`
`$cmd	survival.Rcheck/tests/r_user.Rout		../../$RECIVORY/survival/tests/r_user.Rout.save`
`$cmd	survival.Rcheck/tests/singtest.Rout		../../$RECIVORY/survival/tests/singtest.Rout.save`
`$cmd	survival.Rcheck/tests/strata2.Rout		../../$RECIVORY/survival/tests/strata2.Rout.save`
`$cmd	survival.Rcheck/tests/stratatest.Rout		../../$RECIVORY/survival/tests/stratatest.Rout.save`
`$cmd	survival.Rcheck/tests/summary_survfit.Rout	../../$RECIVORY/survival/tests/summary_survfit.Rout.save`
`$cmd	survival.Rcheck/tests/survfit1.Rout		../../$RECIVORY/survival/tests/survfit1.Rout.save`
`$cmd	survival.Rcheck/tests/survfit2.Rout		../../$RECIVORY/survival/tests/survfit2.Rout.save`
`$cmd	survival.Rcheck/tests/survreg2.Rout		../../$RECIVORY/survival/tests/survreg2.Rout.save`
`$cmd	survival.Rcheck/tests/surv.Rout			../../$RECIVORY/survival/tests/surv.Rout.save`
`$cmd	survival.Rcheck/tests/survSplit.Rout		../../$RECIVORY/survival/tests/survSplit.Rout.save`
`$cmd	survival.Rcheck/tests/survtest.Rout		../../$RECIVORY/survival/tests/survtest.Rout.save`
`$cmd	survival.Rcheck/tests/testci2.Rout		../../$RECIVORY/survival/tests/testci2.Rout.save`
`$cmd	survival.Rcheck/tests/testci.Rout		../../$RECIVORY/survival/tests/testci.Rout.save`
`$cmd	survival.Rcheck/tests/testnull.Rout		../../$RECIVORY/survival/tests/testnull.Rout.save`
`$cmd	survival.Rcheck/tests/testreg.Rout		../../$RECIVORY/survival/tests/testreg.Rout.save`
`$cmd	survival.Rcheck/tests/tiedtime.Rout		../../$RECIVORY/survival/tests/tiedtime.Rout.save`
`$cmd	survival.Rcheck/tests/tmerge2.Rout		../../$RECIVORY/survival/tests/tmerge2.Rout.save`
`$cmd	survival.Rcheck/tests/tmerge3.Rout		../../$RECIVORY/survival/tests/tmerge3.Rout.save`
`$cmd	survival.Rcheck/tests/tmerge.Rout		../../$RECIVORY/survival/tests/tmerge.Rout.save`
`$cmd	survival.Rcheck/tests/tt.Rout			../../$RECIVORY/survival/tests/tt.Rout.save`
`$cmd	survival.Rcheck/tests/turnbull.Rout		../../$RECIVORY/survival/tests/turnbull.Rout.save`
`$cmd	survival.Rcheck/tests/yates0.Rout		../../$RECIVORY/survival/tests/yates0.Rout.save`
`$cmd	survival.Rcheck/tests/yates1.Rout		../../$RECIVORY/survival/tests/yates1.Rout.save`
#`$cmd	survival.Rcheck/tests/rounding.Rout		../../$RECIVORY/survival/tests/rounding.Rout.save`
#`$cmd	survival.Rcheck/tests/mrtest.Rout		../../$RECIVORY/survival/tests/mrtest.Rout.save`
fi;
