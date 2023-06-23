# Calcola le date di inizio e fine per l'intervallo di tre anni
start_date <- as.Date(Sys.Date() - 3*365)
end_date <- Sys.Date()

# Formatta le date nel formato richiesto dall'URL
start_timestamp <- as.POSIXct(start_date)
end_timestamp <- as.POSIXct(end_date)

# Costruisci l'URL del file CSV utilizzando le date di inizio e fine
url <- paste0("https://query1.finance.yahoo.com/v7/finance/download/%5EDJI?period1=",
              as.numeric(start_timestamp), "&period2=",
              as.numeric(end_timestamp), "&interval=1d&events=history")

# Imposta il percorso di destinazione per il salvataggio del file CSV
file_path <- "/Users/festinho/Progetti Uni/MF_DowJonesIndex/data/dow_jones.csv"

# Scarica il file CSV dell'indice Dow Jones degli ultimi tre anni
download.file(url, destfile = file_path, method = "auto")

# Leggi il file CSV e visualizza i dati
dow_data <- read.csv(file_path)
head(dow_data)