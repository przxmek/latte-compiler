# latte-compiler
> Autor: Przemysław Kuczyński (p.kuczynski@student.uw.edu.pl)

## Wymagania techniczne (z treści projektu zaliczeniowego)

1. Projekt powinien być oddany w postaci spakowanego archiwum TAR
   (.tar.gz lub .tgz)
2. W korzeniu projektu muszą się znajdować co najmniej:
    * Plik tekstowy **README** opisujący szczegóły kompilacji i uruchamiania
      programu, używane narzędzia i biblioteki, zaimplementowane rozszerzenia,
      strukturę katalogów projektu, ewentualnie odnośniki do bardziej
      szczegółowej dokumentacji.
    * Plik **Makefile** pozwalający na zbudowanie programu.
    * katalog **src** zawierający wyłącznie pliki źródłowe projektu (plus
      ewentualnie dostarczony przez nas plik Latte.cf); pliki pomocnicze takie
       jak biblioteki itp powinny być umieszczone w inych katalogach.
3. Program musi się kompilować na students poleceniem **make**
   (które oczywiście może wywoływać inne programy).
4. Wszelkie używane biblioteki (poza biblioteką standardową używanego jezyka
   programowania) muszą być opisane w README
5. Po zbudowaniu kompilatora, w korzeniu musi się znajdować plik wykonywalny
   o nazwie **latc** (może być skryptem uruchamiającym inne programy)
6. Kompilator musi akceptować wszystkie programy testowe z katalogu **good**
   i odrzucać ze stosownym komunikatem wszystkie programy z katalogu **bad**.
   Komunikaty o błędach muszą umnożliwiać lokalizację błędu (przez numer linii
   lub kontekst). Dla rozszerzeń musi akceptować wszystkie programy z
   odpowiednich podkatalogów **extension**. Uruchomienie poprawnego programu
   testowego ma dawać wyjście takie jak w odpowiednim pliku **.output**
   (dla wejścia zawartego w odpowiednim pliku **.input**, o ile istnieje)
7. Gdy kompilator akceptuje program, musi wypisać w pierwszej linii stderr
   napis OK ("OK\n"). Dalsze linie i stdout - dowolne. Kompilator musi się
   w takiej sytuacji zakończyć z kodem powrotu 0.
8. Gdy kompilator odrzuca program musi wypisać w pierwszej linii stderr napis
   ERROR ("ERROR\n"). Dalsze linie powinny zawierać stosowne informacje
   o błędach. Kompilator musi się w takiej sytuacji zakończyć z kodem powrotu
   różnym od 0.
9. Rowiązania wymagające stosowania niestandardowego oprogramowania proszę
   uzgadniać ze sprawdzającymi. Rozwiązania ściągające w trakcie kompilacji
   duże ilości danych z sieci nie są mile widziane.

## Generacja kodu LLVM (z treści projektu zaliczeniowego)

1. Po wykonaniu `make` w korzeniu projektu ma się znajdować program wykonywalny
   `latc_llvm`
2. Wykonanie `latc_llvm foo/bar/baz.lat` dla poprawnego programu wejściowego
   **baz.lat** ma stworzyć pliki **baz.ll** (kod LLVM) oraz **baz.bc** (bitkod
   LLVM wykonywalny przy uzyciu `lli`) w katalogu **foo/bar**.
3. Ewentualne funkcje biblioteczne (**printInt** etc.) należy umieścić w pliku
   **runtime.bc** w katalogu **lib** (dodatkowo proszę zamieścić jego źródło)

## Generacja kodu asemblera (z treści projektu zaliczeniowego)

1. Po wykonaniu `make` w korzeniu projektu ma się znajdować program wykonywalny
   **latc_ARCH** gdzie ARCH to x86 lub x86_64
2. Wykonanie `latc_ARCH foo/bar/baz.lat` dla poprawnego programu wejściowego
   **baz.lat** ma stworzyć pliki **baz.s** (kod asemblera) oraz
   wykonywalny **baz** w katalogu **foo/bar**.
3. Ewentualne funkcje biblioteczne (**printInt** etc.) należy umieścić w pliku
   **runtime.o** w katalogu **lib** (dodatkowo proszę zamieścić jego źródło)

## Kompilacja

```bash
$ make
```

## Uruchamianie programu

## Używane narzędzia i biblioteki

## Zaimplementowane rozszerzenia

## Struktura katalogów projektu

## Ewentualne odnośniki do bardziej szczegółowej dokumentacji
