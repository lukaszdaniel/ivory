��    L      |  e   �      p  �   q  7   �     1     M  '   e  3   �      �  9   �  ?     5   \  ?   �  $   �  M   �  &   E	  )   l	     �	  C   �	  @   �	  $   :
  !   _
      �
  8   �
  J   �
  -   &  4   T  V   �  <   �       L   �  2   �  ;     6   Y  B   �  T   �  G   (  E   p  K   �  2     >   5  v   t  ,   �  ;     1   T     �  4   �  8   �  7     0   H  8   y  -   �  5   �  )     M   @  6   �  0   �     �          1     D     [     u     �  %   �  B   �  "     7   7      o     �  /   �  1   �  \   �  F   \  #   �  2   �  :   �  ~  5  �   �  9   5     o     �  *   �  J   �  !     B   @  E   �  :   �  J     )   O  R   y  *   �  -   �  "   %  I   H  C   �  )   �  )      '   *  D   R  Y   �  D   �  F   6  [   }  ?   �  �     R   �  >   �  <   4  C   q  C   �  a   �  L   [   B   �   X   �   <   D!  E   �!  z   �!  3   B"  ?   v"  4   �"     �"  2   #  9   ;#  ;   u#  <   �#  ?   �#  0   .$  @   _$  )   �$  U   �$  5    %  8   V%  ,   �%     �%     �%     �%     &     -&     K&  -   k&  F   �&  /   �&  ?   '  "   P'     s'  0   �'  2   �'  y   �'  G   d(  &   �(  <   �(  K   )     F   B          5      1   G   .   @          9             )   %             '       	   <       $   4         ,                  ;       H   A          J          !      -          :   &   >      ?   8   3   7      E   /       0             L       *   D   
                             +   #      C   (      "   6      =   I   2                                K          %d observation (%s) has *only* NAs --> omit them for clustering! %d observations (%s ...) have *only* NAs --> omit them for clustering! %s has constant columns %s; these are standardized to 0 %s has invalid column names %s must be in 1:ncol(x) %s must contain column names or numbers 'A' must be p x p  cov-matrix defining an ellipsoid 'B' has to be a positive integer 'dmatrix' is not a dissimilarity matrix compatible to 'x' 'iniMem.p' must be a nonnegative n * k matrix with rowSums == 1 'k' (number of clusters) must be in {1,2, .., n/2 -1} 'm', a membership matrix, must be nonnegative with rowSums == 1 'maxit' must be non-negative integer 'medoids' must be NULL or vector of %d distinct indices in {1,2, .., n}, n=%d 'memb.exp' must be a finite number > 1 'par.method' must be of length 1, 3, or 4 'samples' should be at least 1 'sampsize' = %d should not be larger than the number of objects, %d 'sampsize' should be at least %d = max(2, 1+ number of clusters) 'weights' must be of length p (or 1) 'x' must be numeric  n x p matrix 'x' must only have integer codes >>>>> funny case in clusplot.default() -- please report! All variables must be binary (e.g., a factor with 2 levels, both present). Cannot keep data when 'x' is a dissimilarity! Distances must be result of dist or a square matrix. Each of the random samples contains objects between which no distance can be computed. FANNY algorithm has not converged in 'maxit' = %d iterations For each of the %d samples, at least one object was found which could not be assigned to a cluster (because of missing values). Missing values were displaced by the median of the corresponding variable(s) NA values in the dissimilarity matrix not allowed. Need either a dissimilarity 'dist' or diss.matrix 'dmatrix' No clustering performed, NA's in dissimilarity matrix. No clustering performed, NAs in the computed dissimilarity matrix. No clustering performed, a variable was found with all non missing values identical. No clustering performed, all variables have at least one missing value. No clustering performed, an object was found with all values missing. No clustering performed, found variable with more than half values missing. No valid silhouette information (#{clusters} =? 1) Number of clusters 'k' must be in {1,2, .., n-1}; hence n >= 2 Observation %s has *only* NAs --> omit it for clustering Observations %s have *only* NAs --> omit them for clustering! The clustering vector is of incorrect length The number of cluster should be at least 1 and at most n-1. algorithm possibly not converged in %d iterations ambiguous clustering method at least one binary variable has more than 2 levels. at least one binary variable has not 2 different levels. at least one binary variable has values not in {0,1,NA} binary variable(s) %s treated as interval scaled clustering 'x' and dissimilarity 'dist' are incompatible computed some negative or all 0 probabilities ellipsoidPoints() not yet implemented for p >= 3 dim. error from .C(cl_pam, *): invalid medID's full silhouette is only available for results of 'clara(*, keep.data = TRUE)' have %d observations, but not more than %d are allowed index has to be a function or a list of function invalid %s; must be named list invalid 'silhouette' object invalid 'spaceH0': invalid 'twins' object invalid clustering method invalid partition object invalid silhouette structure invalid type %s for column numbers %s mona() needs at least p >= 2 variables (in current implementation) need at least 2 objects to cluster no diss nor data found, nor the original argument of %s no points without missing values omitting NAs one or more objects contain only missing values one or more variables contain only missing values setting 'logical' variable %s to type 'asymm' setting 'logical' variables %s to type 'asymm' the memberships are all very close to 1/k. Maybe decrease 'memb.exp' ? the square matrix is not symmetric. when 'medoids.x' is FALSE, 'keep.data' must be too with mixed variables, metric "gower" is used automatically Project-Id-Version: R-cluster 2.0.8
Report-Msgid-Bugs-To: bugs.r-project.org
PO-Revision-Date: 
Last-Translator: Daniele Medri <dmedri@gmail.com>
Language-Team: Italian https://github.com/dmedri/R-italian-lang
Language: it
MIME-Version: 1.0
Content-Type: text/plain; charset=UTF-8
Content-Transfer-Encoding: 8bit
Plural-Forms: nplurals=2; plural=(n != 1);
X-Generator: Poedit 2.2.1
 %d osservazione (%s) ha *solo* NA --> omettetela dall'analisi! %d osservazioni (%s) hanno *solo* NA --> omettetele dall'analisi! %s ha colonne costanti %s; queste sono standardizzate a 0 %s ha nomi colonna non validi %s dev'essere in 1:ncol(x) %s deve contenere numeri o nomi di colonna 'A' dev'essere una matrice di covarianza p x p che definisce un ellissoide 'B' dev'essere un intero positivo 'dmatrix' non è una matrice di dissomiglianza compatibile con 'x' 'iniMem.p' dev'essere una matrice non-negativa n * k con rowSums == 1 'k' (il numero di cluster) dev'essere in {1,2, .., n/2 -1} 'm', una matrice di appartenenza, dev'essere non negativa con rowSums == 1 'maxit' dev'essere un intero non negativo 'medoids' dev'essere NULL o un vettore di %d indici distinti in {1,2, .., n}, n=%d 'memb.exp' dev'essere un numero finito > 1 'par.method' dev'essere di lunghezza 1, 3 o 4 'samples' dovrebbe essere almeno 1 'sampsize' = %d non dovrebbe essere più grande del numero di oggetti, %d 'sampsize' dovrebbe essere almeno %d = max(2, 1+ numero di cluster) 'weights' dev'essere di lunghezza p (o 1) 'x' dev'essere una matrice numerica n x p 'x' deve avere unicamente codici interi >>>>> caso insolito in clusplot.default() -- per piacere, riportalo! Tutte le variabili devono essere binarie (es. un fattore a 2 livelli, entrambi presenti). Non è possibile conservare i dati quando 'x' è una dissomiglianza! Le distanze devono essere il risultato di una matrice dist o quadrata. Ognuno dei campioni casuali contiene oggetti tra i quali non si possono calcolare distanze. L'algoritmo FANNY senza convergenza con 'maxit' = %d iterazioni Per ognuno dei %d campioni, è stato trovato almeno un oggetto che non può essere assegnato ad un cluster (a causa di valori mancanti). I valori mancanti sono stati spostati dalla mediana delle variabili corrispondenti I valori NA non sono ammessi in una matrice di dissimilarità. Necessaria una differenza "dist" o una diss.matrix "dmatrix" Nessun cluster generato, valori NA nella matrice di dissomiglianza. Nessun cluster generato, valori NA nella matrice di dissomiglianza. Nessun cluster generato, una variabile è stata trovata con tutti i valori non mancanti identici. Nessun cluster generato, tutte le variabili hanno almeno un valore mancante. Nessun cluster generato, un oggetto aveva tutti i valori mancanti. Nessun cluster generato, trovata una variabile con più della metà dei valori mancanti. Nessuna informazione valida di silhouette (#{clusters} =? 1) Il numero di cluster 'k' dev'essere in {1,2, .., n-1}; perciò n >= 2 L'osservazione %s ha *solo* NA --> omettetela dall'analisi Le osservazioni %s hanno *solo* NA --> omettetele dall'analisi! Il vettore di clustering è di lunghezza incorretta Il numerod i cluster dovrebbe essere almeno 1 e al massimo n-1. l'algoritmo potrebbe non convergere in %d iterazioni metodo di clustering ambiguo almeno una variabile binaria ha più di 2 livelli. almeno una variabile binaria non ha 2 livelli differenti. almeno una variabile binaria ha valori esterni a {0, 1, NA} variabili binarie %s trattate come intervallo ridimensionato il clustering 'x' e le dissimilarità 'dist' sono incompatibili calcolate alcune probabilità negative o tutte 0 ellipsoidPoints() non ancora implementato per p >= 3 dimensioni. errore in .C(cl_pam, *): medID non valido la silhouette piena è disponibile solo per risultati di 'clara(*, keep.data = TRUE)' hanno %d osservazioni, ma non più di %d sono ammesse l'indice dev'essere una funzione o una lista di funzioni %s non valido; dev'essere una lista nominata oggetto 'silhouette' non valido 'spaceH0' non valido: oggetto 'twins' non valido metodo di clustering non valido oggetto partizione non valido struttura silhouette non valida tipo %s non valido per i numeri di colonna %s mona() richiede almeno p >= 2 variabili (nell'attuale implementazione) richiede almeno 2 oggetti per l'analisi cluster nessun diss o dato trovato, neppure l'argomento originale di %s nessun punto senza valori mancanti si omettono gli NA uno o più oggetti contiene solo valori mancanti una o più variabili contiene solo valori mancanti configurazione della variabile 'logical' %s nel tipo 'asymm' configurazione delle variabili 'logical' %s nel tipo 'asymm' le appartenenze sono tutte molto vicine a 1/k. Decrementare 'memb.exp'? la matrice quadrata non è simmetrica. quando 'medoids.x' è FALSE, lo dev'essere anche 'keep.data' con variabili miste, la metrica "gower" è utilizzata in maniera automatica 