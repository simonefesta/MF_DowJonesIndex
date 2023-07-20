# MF_DowJonesIndex

Progetto per il corso di Mercati Finanziari, analisi del Dow Jones Index.

###### Linguaggio utilizzato: R

## Svolgimento dell'analisi

#### Indici di riferimento

Si è studiato l'indice Dow Jones, prelevando i dati da Yahoo Finance.
Tale sito mantiene *due versioni* di questo indice:

- [Dow Jones Industrial Average (^DJI) Charts, Data &amp; News - Yahoo Finance](https://finance.yahoo.com/quote/%5EDJI?p=%5EDJI)
  
- [1/100 DOW JONES INDUSTRIAL AVER (^DJX) Charts, Data &amp; News - Yahoo Finance](https://finance.yahoo.com/quote/%5EDJX/options?p=%5EDJX&straddle=true&date=1694736000)
  

Il primo (*DJI*) permette di visionare i *dati storici* ma NON le *opzioni*.
Il secondo (*DJX*) permette di visionare le *opzioni* ma NON i *storici*.

#### Prelievo dei dati

- Dall'indice DJI sono stati prelevati i dati storici dal *15/07/2020* al *15/07/2023*.
  In particolare, è stato realizzato un plot contenente *<data, close_value>*
  per ogni riga dei dati storici.
  
- Dall'indice DJX sono state prelevate giornalmente le opzioni, sia in versione *call* sia in versione *put*. Di tale indice è stato prelevato anche il *valore iniziale del titolo*, dato di partenza per la generazione del *Lattice plot* finale.
  

#### Generazione di dati utili

- Con i dati storici del DJI è stato prodotto, giorno per giorno, un *tasso di rendimento logaritmico*, espresso come $ln(\frac{S_{n}}{S_{n-1}})$. Questi dati sono stati renderizzati su un grafico, oltre che salvati in file.csv
  
- Con le opzioni del DJX è stato tracciato, in funzione ad ogni *strike* trovato, l'andamento nel tempo del *lastCallPrice*, ovvero l'evoluzione dell'ultimo prezzo di Call.
  

#### Calcolo di variabilità e rendimenti

Partendo dai rendimenti logaritmici ottenuti, sono stati calcolate la *media campionaria* e *variabilità campionaria*, mediante funzioni apposite fornite da R.

Successivamente, dal sito https://treasurydirect.gov/GA-FI/FedInvest/selectSecurityPriceDate *FedInvest*, è stato prelevato giorno per giorno il file.csv contentente gli andamenti dei vari CUSIP. 
In particolare, viene analizzato il 
*CUSIP 912796CQ0*, con scadenza *14 settembre 2023*.

- *rendimento giornaliero* = $\frac{100-sellValue}{sellValue}$
  
- *rendimento annuale* = $(1+ \;rendimentoGiornaliero)^\frac{365.2425}{giorni \;maturità}$
  
- *rendimento continuamente composto* = $\frac{ln(1+rendimentoAnnuale)}{giorni \;maturità}$
  

NB: *giorni maturità* è dato dalla differenza tra il *14/09/2023* e la data in cui è stato prodotto/scaricato il file di FedInvest.

#### Calibrazione del modello

Per la calibrazione, si utilizza la media del *rendimento continuamente composto*, ovvero la somma dei valori ottenuti giorno dopo giorno diviso il numero di giorni in cui è stato calcolato. 
Per completare la calibrazione, partendo dal *rendimento continuamente composto*, che chiameremo *r*, troviamo:

- $u = exp(\sigma*\Delta t)$
  
- $d = exp(- \sigma*\Delta t)$
  
- $\tilde{p} = \frac{1 + r - d}{u - d}$
  
- $\tilde{q} = \frac{u-(1+r)}{u - d}$
  
- $S_0 = 338.77$ (valore iniziale del DJX)
  

NB:

- $\sigma \doteq $ variabilità calcolata precedentemente
  
- $\Delta t \doteq$ dimensione intervallo di osservazione.
