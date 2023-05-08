# TOAST
Język imperatywny wzorowany na Latte.

### Syntax
Inspirowany C, bez specjalnych zmian.

### Typy
Trzy standardowe: `int`, `string`, `bool`.

### Funkcje
Rekurencyjne, także zagnieżdżone, statyczne wiązane.
```
int inner() {
    return 2;
}
int outer() {
    int inner() {
        return 5;
    }
    return inner();
}
```
Powyższa funkcja outer() zwróci 5.
W języku Toast każda funkcja musi zwracać wartość.

### Wykonanie
Język wykonuje instrukcje od góry do dołu, nie jest wymagana funkcja main() jako punkt startowy programu.

### Arytmetyka
Standardowo dopuszczalne operacje, w tym dzielenie i modulo z wyłapywaniem błędów czasu wykonania.

### Output
Do wypisywania na standardowe wyjście służy wbudowana funkcja `Print` przyjmująca jako argument dowolne wyrażenie jednego z wbudowanych typów.

### Komentarze
Dopuszczalne są trzy rodzjae
```
# komentarz
// komentarz
/* ko
mentarz */
```
### Break, continue
Działają standardowo.

### Sposób uruchomienia
Należy w folderze wiktor_grzankowski wykonać komendę `make`, a następnie interpreter może wczytać program z pliku `./interpreter program` lub "z palca" jako string czy też z pomocą cat `cat program | ./interpreter`.

### Tabelka Cech
    na 15 punktów:
   	 1. trzy typy wartości: int, bool, string
   	 2. Literały, arytmetyka, porównania
   	 3. Zmienne, operacja przypisania
   	 4. Jawne wypisywanie wartości na wyjście
   	 5. While, if
   	 6. Funkcje, procedury (bez zagnieżdżania), rekurencja
   	 7. Przekazywanie parametrów przez zmienną i przez wartość
    na 20 punktów:
   	 8. Przesłanianie identyfikatorów ze statycznym wiązaniem
   	 9. Obsługa błędów wykonania
   	 10. Funkcje przyjmujące i zwracające wartość
    na 30 punktów:
   	 11. Break, Continue
   	 12. Funkcje zagnieżdżone ze statycznym wiązaniem

W ostatecznej wersji rozważane jest dodanie statycznego typowania oraz dowolnie zagnieżdżonych krotek.

Obecna szacowana wartość projektu to 23 punkty.