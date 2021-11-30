# Analiza podatkov s programom R - 2021/22

Vzorčni repozitorij za projekt pri predmetu APPR v študijskem letu 2021/22. 

## Število potovanj na prebivalca v Evropski uniji v odvisnosti od različnih dejavnikov
Primerjala bom število potovanj na prebivalca v odvisnosti od povprečne plače, BDP-ja, brzposelnosti ter v odvisnosti od spola in starosti.

časovna razsežnost je od leta 2009 (2012, odvisno od podatkov) do 2019, krajevna pa so članice EU (izjema so podatki z minimalno plačo, kjer niso zajete vse države).
Za podatke sem vzela le potovanja iz osebnih razlogov in ne službenih.

Prva tabela bo imela podatke o številu prebivalcev in številu potovanj. Iz njih bom naredila indeks, ki bo povedal povprečno število potovanj na osebo.

Druga tabela bo imela podatke o BDP-ju, brezposelnosti in povprečnih plačah.

Tretja tabela bo imela podatke o spolu in starosti.

Svoj indeks iz prve tabele bi primerjala z gospodarskimi(2.tabela) in demografskimi(3.tabela) značilnostmi in ugotavljala njihov vpliv na indeks.

Viri podatkov:

Število prebivalcev(CSV):
https://www.worldbank.org/en/home

Povprečna letna plača(CSV):
https://www.oecd.org

Brezposelnost, število potovanj (skupaj,starost,spol), BDP (HTML):
https://ec.europa.eu/eurostat/web/main/data/database




## Program

Glavni program in poročilo se nahajata v datoteki `projekt.Rmd`.
Ko ga prevedemo, se izvedejo programi, ki ustrezajo drugi, tretji in četrti fazi projekta:

* obdelava, uvoz in čiščenje podatkov: `uvoz/uvoz.r`
* analiza in vizualizacija podatkov: `vizualizacija/vizualizacija.r`
* napredna analiza podatkov: `analiza/analiza.r`

Vnaprej pripravljene funkcije se nahajajo v datotekah v mapi `lib/`.
Potrebne knjižnice so v datoteki `lib/libraries.r`
Podatkovni viri so v mapi `podatki/`.
Zemljevidi v obliki SHP, ki jih program pobere,
se shranijo v mapo `../zemljevidi/` (torej izven mape projekta).
