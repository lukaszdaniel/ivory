��    (      \  5   �      p     q  /   �  ,   �     �       M     ;   d  9   �     �  -   �       +   3     _     z  3   �     �  6   �          /  )   D  4   n     �  "   �     �  1   �  8   &  6   _  4   �  4   �  .      "   /  &   R     y  #   �  %   �     �     �     	  '   	  �  G	  !   5  <   W  6   �  %   �     �  m     B   {  A   �        A        \  -   {  (   �     �  :   �  #   &  N   J  -   �     �  1   �  B        U  %   o     �  A   �  @   �  :   4  0   o  =   �  :   �  8     #   R     v  0   �  ,   �  ,   �           >  5   L              #          &         "      %                                	       $              !                               (                        '                   
           - read select %d children:   read_child_ci(%d) - read length returned %lld
 'mcexit' can only be used in a child process 'mcexit()' function failed 'mckill' failed Error while shutting down parallel: unable to terminate some child processes
 GetLogicalProcessorInformation is not supported on this OS. WARNING: child %d was to be removed but it doesn't exist
 allocation failure cannot wait for child %d as it does not exist child %d does not exist child %d is waiting for permission to exit
 child %d: 'mcexit' called
 child %d: exiting
 child process %d got SIGUSR1; child_exit_status=%d
 child process %d started
 content to send must be RAW, use serialize() if needed descriptors must be integers error '%s' in select file descriptor is too large for select() in reading processor information, probable cause: %d invalid '%s' argument invalid CPU affinity specification memory allocation error only children can send data to the master process only the master process can send data to a child process parent[%d] created pipes: comm (%d->%d), sir (%d->%d)
 read_child(%d) - pid is not in the list of children
 read_child_ci(%d) - read %lld at %lld returned %lld
 requested CPU set is too large for this system retrieving CPU affinity set failed there is no pipe to the master process unable to create a pipe unable to fork, possible reason: %s unable to terminate child process: %s unable to terminate child: %s what must be a raw vector write error write error, closing pipe to the master Project-Id-Version: R 2.16.0
Report-Msgid-Bugs-To: 
PO-Revision-Date: 
Last-Translator: Łukasz Daniel <lukasz.daniel@gmail.com>
Language-Team: Łukasz Daniel <lukasz.daniel@gmail.com>
Language: pl_PL
MIME-Version: 1.0
Content-Type: text/plain; charset=UTF-8
Content-Transfer-Encoding: 8bit
na-Revision-Date: 2012-05-29 07:55+0100
Plural-Forms: nplurals=3; plural=(n==1 ? 0 : n%10>=2 && n%10<=4 && (n%100<10 || n%100>=20) ? 1 : 2);
X-Poedit-SourceCharset: iso-8859-1
X-Generator: Poedit 2.0.6
  - przeczytaj wybrane %d dzieci:   read_child_ci(%d) - przeczytana długość zwróciła %lld
 'mcexit' może być użyty jedynie w potomnym procesie funkcja 'mcexit()' nie powiodła się 'mckill' nie powiodło się Błąd podczas wyłączania równoległych procesów: nie można zakończyć niektórych procesów potomnych
 GetLogicalProcessorInformation nie jest wspierany na tym systemie. OSTRZEŻENIE: dziecko %d miało być usunięte, ale nie istnieje
 niepowodzenie przydziału nie można czekać na potomny proces %d ponieważ on nie istnieje potomny proces %d nie istnieje dziecko %d oczekuje na pozwolenie by wyjść
 dziecko %d: 'mcexit' zostało wywołane
 dziecko %d: wychodzenie
 proces potomny %d otrzymał SIGUSR1; child_exit_status=%d
 proces potomny %d rozpoczął się
 zawartość do wysłania musi być PUSTA, użyj 'serialize()' jeśli konieczne deskryptory muszą być liczbami całkowitymi błąd '%s' przy wyborze opis pliku jest zbyt duży dla funkcji 'select()' podczas czytania informacji o procesorze, prawdopodobny powód: %d niepoprawny argument '%s' niepoprawne określenie koligacji CPU błąd przydziału pamięci tylko potomne procesy mogą przesłać dane do procesu głównego tylko główny proces może przesłać dane do potomnego procesu parent[%d] utworzył kanały: comm (%d->%d), sir (%d->%d)
 read_child(%d) - pid nie jest na liście dzieci
  read_child_ci(%d) - przeczytane %lld w %lld zwróciło %lld
 zażądane ustawienie CPU jest zbyt duże dla tego systemu pozyskiwanie ustawienia koligacji CPU nie powiodło się nie ma potoku do procesu głównego nie można utworzyć potoku nie można rozdzielić, prawdopodobny powód: %s nie można zakończyć potomnego procesu: %s nie można zakończyć potomnego procesu: %s 'what' musi być pustym wektorem błąd zapisu błąd zapisu, zamykanie potoku do procesu głównego 