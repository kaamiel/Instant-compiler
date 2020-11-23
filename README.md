# Instant compiler

Program w języku Instant składa się z ciągu instrukcji rozdzielonych średnikami.

Instrukcje są dwojakiego rodzaju:

* wyrażenie – wypisuje obliczoną wartość wyrażenia na standardowe wyjście,
* przypisanie postaci `zmienna = wyrażenie` – przypisuje wartość wyrażenia na zmienną po lewej stronie; nic nie wypisuje.

Wyrażenia składają się z literałów całkowitych nieujemnych, zmiennych i operacji arytmetycznych. Kolejność obliczenia argumentów operatora nie jest określona.

Składnia w formacie BNFC

~~~
Prog. Program ::= [Stmt] ;
SAss. Stmt ::= Ident "=" Exp;
SExp. Stmt ::= Exp ;
separator Stmt ";" ;

ExpAdd.            Exp1   ::= Exp2 "+"  Exp1 ;
ExpSub.            Exp2   ::= Exp2 "-"  Exp3 ;
ExpMul.            Exp3   ::= Exp3 "*"  Exp4 ;
ExpDiv.            Exp3   ::= Exp3 "/"  Exp4 ;
ExpLit.            Exp4   ::= Integer ;
ExpVar.            Exp4   ::= Ident ;
coercions Exp 4;
~~~

**Uwaga:**

* dodawanie wiąże **w prawo**
* przyjmujemy, że dodawanie i mnożenie są przemienne, ale nie są łączne.

Zadanie polega na napisaniu kompilatora dla języka Instant do JVM i LLVM.

W tym zadaniu wygenerowany kod powinien wykonywać wszystkie wyspecyfikowane operacje. Nie jest zatem na przykład dozwolone zastapienie wyrazenia `2+3` przez stałą `5`, pominiecie przypisań na nieużywane zmienne itp.

Jedynym dozwolonym, a nawet pożądanym usprawnieniem jest wybór takiej kolejności obliczania podwyrażeń aby zminimalizować potrzebny rozmiar stosu JVM. W każdym wypadku potrzebny rozmiar stosu musi być obliczony i zadeklarowany. Podobnie należy obliczyć i zadeklarować liczbę wykorzystywanych zmiennych lokalnych.

Kompilacja i uruchamianie
------------

Po zbudowaniu kompilatora (poleceniem `make`), w korzeniu będą znajdować się pliki wykonywalne `insc_jvm` oraz `insc_llvm`.

Wykonanie `insc_jvm foo/bar/baz.ins` dla poprawnego programu wejściowego `baz.ins` stworzy pliki `baz.j` (kod Jasmin) oraz `baz.class` w katalogu `foo/bar`.

Wykonanie `insc_llvm foo/bar/baz.ins` dla poprawnego programu wejściowego `baz.ins` stworzy pliki `baz.ll` (tekstowy kod LLVM) oraz `baz.bc` (bitkod LLVM wykonywalny przy uzyciu `lli`) w katalogu `foo/bar`.

W katalogu `tests/` znajdują się proste testy poprawnościowe, można je uruchomić skryptem `test.sh`.
