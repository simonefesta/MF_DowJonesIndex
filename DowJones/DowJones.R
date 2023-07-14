# DOW JONES INDEX ANALYSIS
library(rstudioapi)

###################### Prelievo dei dati da Yahoo Finance###############

# Calcolo le date di inizio e fine per l'intervallo di tre anni
start_date <- as.Date(Sys.Date() - 3*365)
end_date <- Sys.Date()

# Formato le date nel formato richiesto dall'URL
start_timestamp <- as.POSIXct(start_date)
end_timestamp <- as.POSIXct(end_date)

# Costruisco l'URL del file CSV utilizzando le date di inizio e fine
url <- paste0("https://query1.finance.yahoo.com/v7/finance/download/%5EDJI?period1=",
              as.numeric(start_timestamp), "&period2=",
              as.numeric(end_timestamp), "&interval=1d&events=history")

# Imposto il percorso di destinazione per il salvataggio del file CSV

# Imposto la directory di lavoro sulla directory attuale (ovvero la cartella "dowjones")
current_file <- normalizePath(rstudioapi::getSourceEditorContext()$path)
print(current_file)
current_dir <- dirname(current_file)
print(current_dir)


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

#lines(dow_data$Date, dow_data$High, col = "green", lty = "dashed")
#lines(dow_data$Date, dow_data$Low, col = "red")

# Aggiungi una legenda
#legend("topleft", legend = c("Open", "High", "Low"), col = c("blue", "green", "red"), lty = c("solid", "dashed", "solid"))

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

###################### TODO