# DOW JONES INDEX ANALYSIS

###################### Prelievo dei dati da Yahoo Finance###############

# Calcolo le date di inizio e fine per l'intervallo di tre anni, a partire dal 2023-07-15
start_date <- "2020-07-15"
end_date <- "2023-07-15"

# Formatto le date nel formato richiesto dall'URL
start_timestamp <- as.POSIXct(start_date)
end_timestamp <- as.POSIXct(end_date)

# Costruisco l'URL del file CSV utilizzando le date di inizio e fine
url <- paste0("https://query1.finance.yahoo.com/v7/finance/download/%5EDJI?period1=",
              as.numeric(start_timestamp), "&period2=",
              as.numeric(end_timestamp), "&interval=1d&events=history")

# Imposto il percorso di destinazione per il salvataggio del file CSV

# Imposto la directory di lavoro sulla directory attuale (ovvero la cartella "dowjones")
current_file <- normalizePath(rstudioapi::getSourceEditorContext()$path)
current_dir <- dirname(current_file)



# Specifica il nome del file con la data corrente (salvo i dati in dowjones-<dataodierna>)
nome_file <- paste("dowjones_", format(Sys.Date(), "%d-%m-%Y"), ".csv", sep = "")



# Specifica il percorso relativo alla sottocartella (questo csv lo salvo nella sottocartella "data")
datafolder <- "data"

#specifico quindi il path completo del file, che parte dalla directory attuale, va in "data", e salva nel csv precedentemente dichiarato.
file_path <- file.path(current_dir, datafolder, nome_file) 
# Scarica il file CSV dell'indice Dow Jones degli ultimi tre anni
download.file(url, destfile = file_path, method = "auto")

# Leggi il file CSV e visualizza i dati
dow_data <- read.csv(file_path)
head(dow_data)

# Leggi il file CSV esistente
data <- read.csv(file_path)

# Aggiungi la colonna "indice riga"
data$index <- 1:nrow(data)

# Riorganizza l'ordine delle colonne
data <- data[, c("index", names(data)[-ncol(data)])]

# Salva il file CSV modificato
write.csv(data, file_path, row.names = FALSE)

###################### Grafico del valore "Close" degli ultimi 3 anni#####################################
# Leggi il file CSV con i dati dell'indice Dow Jones
dow_data <- read.csv(file_path)

# Converti la colonna delle date in formato di data
dow_data$Date <- as.Date(dow_data$Date)

# Crea un nuovo grafico con titolo
plot(dow_data$Date, dow_data$Close, type = "l", col = "blue", xlab = "Data", ylab = "Close Value",
     main = "Andamento dell'indice Dow Jones ^DJI")


###################### Rendimenti giornalieri (grafico + csv)#####################################################

#RENDIMENTI GIORNALIERI
# Leggi il file CSV, specificando che la prima riga contiene i nomi delle colonne e l'ultima riga non deve essere inclusa
data <- read.csv(file_path, header = TRUE)
head(data)

# Rimuovi l'ultima riga vuota
data <- data[-nrow(data), ]

# Converti la colonna "Date" in formato data
data$Date <- as.Date(data$Date)

# Calcola i rendimenti giornalieri
returns <- c(0, diff(data$Close) / lag(data$Close, default = data$Close[1]))

# Aggiungi i rendimenti giornalieri come colonna al dataframe
data$DailyReturn <- returns[1:nrow(data)]
print(data$DailyReturn)

# Crea un grafico dei rendimenti giornalieri
plot(data$Date, data$DailyReturn, type = "l",col = "purple", xlab = "Data", ylab = "Rendimento giornaliero", main = "Rendimento giornaliero")

# Aggiungi una griglia al grafico
grid()

# Aggiungi una legenda
legend("bottom", legend = "Rendimento giornaliero", lty = 1, col = "purple")
#SALVO QUESTI DATI IN UN CSV

# Creazione del dataframe dei rendimenti giornalieri
returns_data <- data.frame(data$Date, data$DailyReturn)

# Rinomina le colonne del dataframe
colnames(returns_data) <- c("data", "rendimento giornaliero")

# Salva il dataframe nel file "rendimenti.csv"

# Specifica il nome del file con la data corrente (salvo i dati in dowjones-<dataodierna>)
rendimenti <- paste("rendimentigiornalieri_", format(Sys.Date(), "%d-%m-%Y"), ".csv", sep = "")

# Specifica il percorso relativo alla sottocartella (questo csv lo salvo nella sottocartella "data")
datafolder <- "data"

#specifico quindi il path completo del file, che parte dalla directory attuale, va in "data", e salva nel csv precedentemente dichiarato.
rendimenti_path <- file.path(current_dir, datafolder, rendimenti) 
write.csv(returns_data, rendimenti_path, row.names = TRUE)

###################### Rendimenti giornalieri Logaritmici (grafico + csv)#####################################################

#RENDIMENTI GIORNALIERI Logaritmici

# Leggi il file CSV, specificando che la prima riga contiene i nomi delle colonne e l'ultima riga non deve essere inclusa
data_log <- read.csv(file_path, header = TRUE)
head(data_log)



# Converti la colonna "Date" in formato data
data_log$Date <- as.Date(data_log$Date)


data_log$Log_Return <- c(0, log(data_log$Close[-1] / lag(data_log$Close[-nrow(data_log)])))

# Crea un grafico dei rendimenti giornalieri
plot(data_log$Date, data_log$Log_Return, type = "l",col = "green", xlab = "Data", ylab = "Rendimento giornaliero log", main = "Rendimento giornaliero log")

# Aggiungi una griglia al grafico
grid()



# Aggiungi una legenda
legend("bottom", legend = "Rendimento giornaliero log", lty = 1, col = "green")
#SALVO QUESTI DATI IN UN CSV


# Creazione del dataframe dei rendimenti giornalieri
returns_data_log <- data.frame(data_log$Date, data_log$Log_Return)
print(returns_data_log)
# Rinomina le colonne del dataframe
colnames(returns_data_log) <- c("data", "rendimento giornaliero log")

# Salva il dataframe nel file "rendimenti.csv"

# Specifica il nome del file con la data corrente (salvo i dati in dowjones-<dataodierna>)
rendimenti_log <- paste("rendimentigiornalieriLOG_", format(Sys.Date(), "%d-%m-%Y"), ".csv", sep = "")

# Specifica il percorso relativo alla sottocartella (questo csv lo salvo nella sottocartella "data")
datafolder <- "data"

#specifico quindi il path completo del file, che parte dalla directory attuale, va in "data", e salva nel csv precedentemente dichiarato.
rendimenti_log_path <- file.path(current_dir, datafolder, rendimenti_log) 
write.csv(returns_data_log, rendimenti_log_path, row.names = TRUE)

###################### PARTE 2: JDX INDEX OPTIONS #####################################################
library(quantmod)
# Il nome della variabile contenente i dati include il giorno corrente in cui li scarico.
# Ottengo la data odierna
data_odierna <- format(Sys.Date(), "%Y-%m-%d")

# Crea il nome della variabile con la data odierna
DJX_Opt <- paste0("DJX_Opt_", data_odierna)

start_value <- 338.77 #valore iniziale dell'osservazione, data 14/07/23 (ok anche per il 15/07, mercati chiusi)

DJX_Opt <- getOptionChain("^DJX", Exp="2023-09-15", src='yahoo')
class(DJX_Opt)
length(DJX_Opt)
show(DJX_Opt[[1]])
class(DJX_Opt[[1]])
nrow(DJX_Opt[[1]])
show(DJX_Opt[[2]])
class(DJX_Opt[[2]])
nrow(DJX_Opt[[2]])
show(DJX_Opt[[1]]$Strike)
show(DJX_Opt[[2]]$Strike)
Strike <- sort(union(DJX_Opt[[1]]$Strike, DJX_Opt[[2]]$Strike))
show(Strike)
length(Strike)
Call_Indx <- sapply(Strike, function(x) which(DJX_Opt[[1]]$Strike==x)[1])
Put_Indx <- sapply(Strike, function(x) which(DJX_Opt[[2]]$Strike==x)[1])
DJX_Opt_df <- data.frame(Indx=1:length(Strike),
                                      Call_ContractID=DJX_Opt[[1]]$ContractID[Call_Indx], 
                                      Call_Bid=DJX_Opt[[1]]$Bid[Call_Indx],
                                      Call_Ask=DJX_Opt[[1]]$Ask[Call_Indx],
                                      Call_Vol=DJX_Opt[[1]]$Vol[Call_Indx],
                                      Call_OI=DJX_Opt[[1]]$OI[Call_Indx],
                                      Call_PrChg=DJX_Opt[[1]]$Chg[Call_Indx],
                                      Call_PrChgPct=DJX_Opt[[1]]$ChgPct[Call_Indx],
                                      Call_LastTrTime=DJX_Opt[[1]]$LastTradeTime[Call_Indx],
                                      Call_LastPr=DJX_Opt[[1]]$Last[Call_Indx],
                                      Call_ImplVol=DJX_Opt[[1]]$IV[Call_Indx],
                                      Call_ITM=DJX_Opt[[1]]$ITM[Call_Indx],
                                      Strike=Strike,
                                      Put_ITM=DJX_Opt[[2]]$ITM[Put_Indx],
                                      Put_ImplVol=DJX_Opt[[2]]$IV[Put_Indx],
                                      Put_LastPr=DJX_Opt[[2]]$Last[Put_Indx],
                                      Put_LastTrTime=DJX_Opt[[2]]$LastTradeTime[Put_Indx],
                                      Put_PrChgPct=DJX_Opt[[2]]$ChgPct[Put_Indx],
                                      Put_PrChg=DJX_Opt[[2]]$Chg[Put_Indx],
                                      Put_OI=DJX_Opt[[2]]$OI[Put_Indx],
                                      Put_Vol=DJX_Opt[[2]]$Vol[Put_Indx],
                                      Put_Ask=DJX_Opt[[2]]$Ask[Put_Indx],
                                      Put_Bid=DJX_Opt[[2]]$Bid[Put_Indx],
                                      Put_ContractID=DJX_Opt[[2]]$ContractID[Put_Indx])
  head(DJX_Opt_df,10)                                   
  tail(DJX_Opt_df,10)
  #prendo la directory corrente, e poi vado nella sottodirectory "options".
  current_file <- normalizePath(rstudioapi::getSourceEditorContext()$path)
  current_dir <- dirname(current_file)
  datafolder2 <- "options"
  
  file_path <- file.path(current_dir, datafolder2, paste0("DJX_Opt_", data_odierna,".csv"))
  print(file_path)
  write.csv(DJX_Opt_df, file_path)
  
  #requires install.packages("dplyr")

  
  
  
  #Plot delle opzioni
  library(tidyverse)

  #mi metto nella sottocartella "options"
  options_path <- file.path(current_dir,datafolder2)
  #print(options_path)
  option_files <- sort(list.files(path = options_path, pattern = "DJX_Opt_", full.names = TRUE))
  # Inizializza un nuovo dataframe vuoto
  index_day<-0
  #print(index_day)
  strike_values <- c()
  
  # Ottenere la dimensione della lista "option_files"
  dimensione_lista <- length(option_files)
  
  # Stampa la dimensione della lista "option_files"
  #print(dimensione_lista)
  

 # new_data <- data.frame(matrix(NA, nrow = 0, ncol = dimensione_lista))
  #new_data <-data.frame()
  
  # Loop attraverso i file CSV
  for (file in option_files) {
    
    # Estrai la parte variabile (data) dal nome del file utilizzando substr
    nome_file <- basename(file)
    posizione_trattino <- max(gregexpr("_", nome_file)[[1]])
    parte_variabile <- substr(nome_file, posizione_trattino + 1, nchar(nome_file) - 4)
    
    # Converti la parte variabile in un valore "data" e aggiorna la variabile data_variabile
    data_variabile <- as.Date(parte_variabile)
    #print(data_variabile)
    
    # Leggi i dati dal file CSV
    dati <- read.csv(file)
   # print(file)
    
    # Rimuovi le righe con Call_LastPr uguale a NA
    dati <- dati %>% filter(!is.na(Call_LastPr))
    
    if (index_day == 0) {
      col_name <- "Strike"
      strike_values <<- c(strike_values, dati[[col_name]])
      } 
    
        call_last_pr_values <- c()
        col_name_2 <- "Call_LastPr"
      for (val in strike_values){  
        filtered_rows <- dati %>% filter(.data[[col_name]] == val)
        if (nrow(filtered_rows) > 0) {
          call_last_pr_values <- c(call_last_pr_values, filtered_rows[[col_name_2]])
        } else {
          # Se non ci sono righe corrispondenti, aggiungi un NA
          call_last_pr_values <- c(call_last_pr_values, NA)
        }
        

      }
        print(call_last_pr_values)
        
      if (index_day == 0){
        new_data <- data.frame(call_last_pr_values)
      } else{
        
        
        #print(call_last_pr_values)
        
        #new_data[index_day] <-call_last_pr_values 
        new_data <-cbind(new_data,call_last_pr_values)
       }
  
        index_day <- index_day + 1
        
        write_csv(new_data, strike_file)
      }
        
    }
    
    
   
    
  

  
  print(new_data)
  # Creazione del grafico
  plot(1, 1, type = "n", xlim = c(1, ncol(new_data)), ylim = c(min(new_data), max(new_data)), 
       xlab = "Giorni dall'osservazione iniziale", ylab = "Andamento delle call a partire dallo strike")
  
  # Aggiunta delle linee per ogni riga del dataframe
  for (i in 1:nrow(new_data)) {
    lines(1:ncol(new_data), new_data[i,], col = i)
  }
  

  

  
########Calcolo di devianza, rendimento medio giornaliero, annuale, prendendolo dai csv del CUSIP########
  # Calcola la deviazione standard dei rendimenti logaritmici
  data_log <- read.csv(rendimenti_log_path, header = TRUE)
  head(data_log$rendimento.giornaliero.log)
  variabilita <- sd(data_log$rendimento.giornaliero.log)
  rendimento_medio <- mean(data_log$rendimento.giornaliero.log)
  
  # Stampa il risultato
  print(variabilita)
  print(rendimento_medio)
  
  #calcolo del rendimento privo di rischio per CUSIP 912796CQ0 con scadenza 14 settembre 2023
  #l'idea è: ho file vari di nome securityprice_[dataDownload] li apro uno ad uno in ordine, prelevo il cusip associato e la data, e li salvo in un nuovo csv dove ho l'andamento di quel CUSIP.
  
  library(dplyr)
  fedinvest_dir <-file.path(current_dir, "fedinvest")
  maturity_date<- as.Date("2023-09-14")
  
  
  preleva_e_aggiungi_csv <- function(direttorio, parte_fissa_nome, nuovo_csv) {
    # Ottieni la lista dei nomi dei file CSV nel direttorio specificato
    files <- sort(list.files(path = direttorio, pattern = parte_fissa_nome, full.names = TRUE))

    # Inizializza un nuovo dataframe vuoto
    nuovo_dataframe <- data.frame()
    
    # Loop attraverso i file CSV
    for (file in files) {
      
      # Estrai la parte variabile (data) dal nome del file utilizzando substr
      nome_file <- basename(file)
      posizione_trattino <- max(gregexpr("_", nome_file)[[1]])
      parte_variabile <- substr(nome_file, posizione_trattino + 1, nchar(nome_file) - 4)
      
      # Converti la parte variabile in un valore "data" e aggiorna la variabile data_variabile
      data_variabile <- as.Date(parte_variabile)
      #print(data_variabile)

      # Leggi i dati dal file CSV
      dati <- read.csv(file)
      #print(file)
      
      # Seleziona la riga con il valore "912796CQ0" nel primo campo
      riga_x <- dati %>% filter(dati[, 1] == "912796CQ0")
      
      # Se la riga con "x" è stata trovata, aggiungi il valore del primo campo e la data alla nuova dataframe
      if (nrow(riga_x) > 0) {
       # print(riga_x[,7])
        r_no_risk_daily <- (100-riga_x[,7])/riga_x[,7] #rendimento giornaliero
        time_to_maturity_days <-as.numeric(maturity_date-data_variabile)
        r_no_risk_year <- (1+r_no_risk_daily)^(365.2425/time_to_maturity_days)-1
        print(r_no_risk_year)
        
        nuovo_dataframe <- rbind(nuovo_dataframe, c(riga_x[, 7],as.character(data_variabile), r_no_risk_daily,r_no_risk_year)) #prendo la settima colonna, ovvero valore SELL (cusip). as character perchè sennò in csv non interpreta bene.
      }
    }
    
    # Assegna nomi alle colonne
   colnames(nuovo_dataframe) <- c("Sell_value", "data_osservazione","r_norisk_daily","r_norisk_annual")
    nuovo_csv <- file.path(fedinvest_dir,nuovo_csv)
    
    # Scrivi il nuovo dataframe nel nuovo file CSV
    write.csv(nuovo_dataframe, nuovo_csv, row.names = FALSE)
  }
  
  # Esempio di utilizzo della funzione
  preleva_e_aggiungi_csv(direttorio = fedinvest_dir, parte_fissa_nome = "securityprice_", nuovo_csv = "cusipLife.csv")
  
  
########Calcolo tasso d'interesse non rischioso continuamente composto########
  