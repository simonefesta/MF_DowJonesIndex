# DOW JONES INDEX ANALYSIS
###### Libraries######
library(timeDate)
library(timeSeries)
library(fBasics)
library(fOptions)
library(tibble)
library("data.table")
library(dplyr)
library(quantmod)
library(reshape2)
library(tidyverse)
library(ggplot2)
library(gridExtra)
library(cowplot)
library(urca)
library(moments)
library(lmtest)
library(latticeExtra)

###### Paths #####

# Imposto la directory di lavoro sulla directory attuale (ovvero la cartella "dowjones")
current_file <- normalizePath(rstudioapi::getSourceEditorContext()$path)
current_dir <- dirname(current_file)

# Specifica il percorso relativo alla sottocartella (questo csv lo salvo nella sottocartella "data")
datafolder <- "data"
optionsfolder <- "options"
fedinvestfolder <- "fedinvest"



###### Prelievo dei dati del Dow Jones Index da Yahoo Finance + grafico ###############

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



# Specifica il nome del file con la data corrente (salvo i dati in dowjones-<dataodierna>)
dow_jones_file <- paste("dowjones_", format(Sys.Date(), "%d-%m-%Y"), ".csv", sep = "")



#specifico quindi il path completo del file, che parte dalla directory attuale, va in "data", e salva nel csv precedentemente dichiarato.
dow_jones_path <- file.path(current_dir, datafolder, dow_jones_file) 

# Scarica il file CSV dell'indice Dow Jones degli ultimi tre anni
download.file(url, destfile = dow_jones_path, method = "auto")

# Leggi il file CSV e visualizza i dati
dow_data <- read.csv(dow_jones_path)
head(dow_data)


# Aggiungi la colonna "indice riga"
dow_data$index <- 1:nrow(dow_data)

# Riorganizza l'ordine delle colonne
dow_data <- dow_data[, c("index", names(dow_data)[-ncol(dow_data)])]

# Salva il file CSV modificato
write.csv(dow_data, dow_jones_path, row.names = FALSE)


# Converti la colonna delle date in formato di data
dow_data$Date <- as.Date(dow_data$Date)

# Crea un nuovo grafico con titolo
plot(dow_data$Date, dow_data$Close, type = "l", col = "blue", xlab = "Data", ylab = "Close Value",
     main = "Andamento dell'indice Dow Jones ^DJX")



###### Rendimenti giornalieri Logaritmici (grafico + csv)#####################################################


#Utilizzo "dow_data" prodotto sopra, calcolo il rendimento logaritmico


dow_data$Log_Return <-  c(0, diff(log(dow_data$Close)))

# Crea un grafico dei rendimenti giornalieri
plot(dow_data$Date, dow_data$Log_Return, type = "l",col = "black", xlab = "Data", ylab = "Rendimento giornaliero log", main = "Rendimento giornaliero log")

# Aggiungi una griglia al grafico
grid()

#Salvo questi dati in un csv, associandoli prima ad un dataframe contenente la coppia <data, rendimento giornaliero logaritmico>

# Creazione del dataframe dei rendimenti giornalieri
returns_data_log <- data.frame(dow_data$Date, dow_data$Log_Return)

head(returns_data_log)

# Rinomina le colonne del dataframe
colnames(returns_data_log) <- c("data", "rendimento giornaliero log")

# Salva il dataframe nel file "rendimenti.csv"

# Specifica il nome del file con la data corrente (salvo i dati in dowjones-<dataodierna>)
rendimenti_log_file <- paste("rendimentigiornalieriLOG_", format(Sys.Date(), "%d-%m-%Y"), ".csv", sep = "")


#specifico quindi il path completo del file, che parte dalla directory attuale, va in "data", e salva nel csv precedentemente dichiarato.
rendimenti_log_path <- file.path(current_dir, datafolder, rendimenti_log_file)

write.csv(returns_data_log, rendimenti_log_path, row.names = TRUE)

###### Prelievo delle opzioni di DJX #####

# Il nome della variabile contenente i dati include il giorno corrente in cui li scarico.
# Ottengo la data odierna
data_odierna <- format(Sys.Date(), "%Y-%m-%d")

# Crea il nome della variabile con la data odierna
DJX_Opt <- paste0("DJX_Opt_", data_odierna)


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
  
  
  #salvo il tutto nella sottodirectory "options"
  opt_path <- file.path(current_dir, optionsfolder, paste0("DJX_Opt_", data_odierna,".csv"))
  head(opt_path)
  write.csv(DJX_Opt_df, opt_path)
  
###### Plot dei vari strike appena scaricati ####
  

  #mi metto nella sottocartella "options" e prelevo tutti i file posseduti contenenti le options di JDX
  options_path <- file.path(current_dir,optionsfolder)
  option_files <- sort(list.files(path = options_path, pattern = "DJX_Opt_", full.names = TRUE))
  
  # index day è associato al "giorno" dell'osservazione, strike_values conterrà gli strike che osserverò
  index_day<-0
  strike_values <- c()

  
  
  strike_file <- file.path(current_dir,optionsfolder, "StrikeStory.csv") #qui scriverò la "storia" dei vari strike
  
  # Loop attraverso i file CSV
  for (file in option_files) {
    
    # Estrai la parte variabile (data) dal nome del file utilizzando substr
    nome_file <- basename(file)
    posizione_trattino <- max(gregexpr("_", nome_file)[[1]])
    parte_variabile <- substr(nome_file, posizione_trattino + 1, nchar(nome_file) - 4)
    
    # Converti la parte variabile in un valore "data" e aggiorna la variabile data_variabile
    data_variabile <- as.Date(parte_variabile)

    # Leggi i dati dal file CSV
    dati <- read.csv(file)

    # Rimuovi le righe con Call_LastPr uguale a NA
    dati <- dati %>% filter(!is.na(Call_LastPr))
    
    if (index_day == 0) {
                         col_name <- "Strike"
                         strike_values <<- c(strike_values, dati[[col_name]]) #prelevo tutti gli strike non nulli
      } #adesso prendo i "last price Call"
    
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

      if (index_day == 0){ # se index_day == 0, vuol dire che sto cercando gli strike, allora sono all'inizio, quindi creo lo strike_frame
        
        strike_frame <- data.frame(call_last_pr_values)
      
        } else{ #lo strike_frame già esiste, devo solo aggiornarlo
      
               strike_frame <-cbind(strike_frame,call_last_pr_values)
              }
  
        index_day <- index_day + 1 #aggiorno l'indice dei giorni passati
        
        write_csv(strike_frame, strike_file)
      }
        

   varianze <- apply(strike_frame, 1, sd)
   print(varianze)
   clean_strike_frame <- subset(strike_frame, varianze >0.4)
   print(clean_strike_frame)

   
  # Grafico di tutti gli strike in funzione di lastCallPrice di un dataset ########
   
   
  plot(1, 1, type = "n", xlim = c(0,ncol(strike_frame)), ylim = c(min(strike_frame), max(strike_frame)), 
       xlab = "Giorni dall'osservazione iniziale", ylab = "LastCallPrice", 
       main = "Evoluzione del lastCallPrice in funzione di tutti gli strike")
  

  # Aggiunta delle linee per ogni riga del dataframe
  for (i in 1:nrow(strike_frame)) {
    lines(0:(ncol(strike_frame)-1), strike_frame[i,], col = i)
  }
   
   legend("topright", legend = strike_values, col = 1:length(strike_values), lty = 1, pch = 16, title = "Strikes associated",xpd = TRUE,cex = 0.55)
   

  
  
  # Grafico degli strike in funzione di lastCallPrice di un dataset (only traded)#######
  
  # Questa serie di comandi mi permettono di avere sul grafico gli Strike di riferimento che osservo.
   
  strike_clean_read <- read.csv(file)
  strike_clean_total <- strike_clean_read$Strike
  # Trova il numero totale di colonne nel dataframe "clean_strike_frame"
  num_colonne <- ncol(clean_strike_frame)
  # Estrai la colonna "LastPr" dal dataframe "clean_strike_frame" (considerando l'ultima colonna)
  ultima_colonna <- clean_strike_frame[, num_colonne]
  # Filtra le colonne "Strike" in base alla presenza dell'elemento "LastPr" nel vettore "clean_strike_frame_strikes"
  strike_legend <- strike_clean_total[strike_clean_read$Call_LastPr %in% clean_strike_frame[, num_colonne]]

  

    plot(1, 1, type = "n", xlim = c(0,ncol(clean_strike_frame)), ylim = c(min(clean_strike_frame), max(clean_strike_frame)), 
       xlab = "Giorni dall'osservazione iniziale", ylab = "LastCallPrice", 
       main = "Evoluzione del lastCallPrice in funzione dei soli strike variati")
  
    
      # Aggiunta delle linee per ogni riga del dataframe
  for (i in 1:nrow(clean_strike_frame)) {
    lines(0:(ncol(clean_strike_frame)-1), clean_strike_frame[i,], col = i)
  }
   
    legend("topleft", legend = strike_legend, col = 1:length(strike_legend), lty = 1, pch = 16, 
           title = "Strikes associated", cex = 0.55)
    
  

  
###### Calcolo di devianza, rendimento medio giornaliero, annuale, prendendolo dai csv del CUSIP########
  
  #NOTA: manualmente recarsi su https://treasurydirect.gov/GA-FI/FedInvest/selectSecurityPriceDate, scaricare il csv,
  #      metterlo nella cartella "fedinvest", e aggiungere la data al nome nello stesso formato dei file presenti,
  #      ovvero DJX_Opt_[year]-[month]-[currentday]
  
  # Calcolo la deviazione standard dei rendimenti logaritmici dell'ultimo report
  
  data_log <- read.csv(rendimenti_log_path, header = TRUE)
  head(data_log$rendimento.giornaliero.log) #se faccio anteprima di data_log, vediamo come rendimento.giornaliero.log è il nome della colonna).
  
  variabilita <- sd(data_log$rendimento.giornaliero.log)
  rendimento_medio <- mean(data_log$rendimento.giornaliero.log)
  
  # Stampa il risultato
  print(variabilita)
  print(rendimento_medio)
  
  #calcolo del rendimento privo di rischio per CUSIP 912796CQ0 con scadenza 14 settembre 2023
  #l'idea è: ho file vari di nome securityprice_[dataDownload] li apro uno ad uno in ordine, prelevo il cusip associato e la data, e li salvo in un nuovo csv dove ho l'andamento di quel CUSIP.
  
  fedinvest_dir <-file.path(current_dir, fedinvestfolder)
  
  maturity_date<- as.Date("2023-09-14")
  
  # Inizializza un nuovo dataframe vuoto
  rendimenti_df <- data.frame()
  
  rendimenti_famiglia_csv <- function(folder, parte_fissa_nome, rendimenti_csv_file_output) {
    
    # Ottieni la lista dei nomi dei file CSV nella folder specificato
    files <- sort(list.files(path = folder, pattern = parte_fissa_nome, full.names = TRUE))

    
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
     
      
      # Seleziona la riga con il valore "912796CQ0" nel primo campo
      riga_x <- dati %>% filter(dati[, 1] == "912796CQ0")
      
      # Se la riga con "x" è stata trovata, aggiungi il valore del primo campo e la data al dataframe
      
      if (nrow(riga_x) > 0) {
       
        r_no_risk_daily <- (100-riga_x[,7])/riga_x[,7] #rendimento giornaliero
        
        time_to_maturity_days <-as.numeric(maturity_date-data_variabile) #tempo alla maturità
        
        r_no_risk_year <- (1+r_no_risk_daily)^(365.2425/time_to_maturity_days)-1 #rendimento annuale
        
        print(r_no_risk_year)
        
        r_composite <- log(1+r_no_risk_year)/time_to_maturity_days #forse devo dividere per 1, non per maturity days
        
        print(r_composite) #Nota: in rbind uso "<<-" e non "<-" per salvare tali dati nel dataframe globalmente
        
        rendimenti_df <<- rbind(rendimenti_df, c(riga_x[, 7],as.character(data_variabile),time_to_maturity_days, r_no_risk_daily,r_no_risk_year, r_composite)) #prendo la settima colonna, ovvero valore SELL (cusip). as character perchè sennò in csv non viene riconosciuto.
        

      }
    }
    
    # Assegna nomi alle colonne
    colnames(rendimenti_df) <<- c("Sell_value", "data_osservazione","days_to_maturity","r_norisk_daily","r_norisk_annual","r_composite")
    rendimenti_csv_file_output <- file.path(fedinvest_dir,rendimenti_csv_file_output)
    print(rendimenti_df)
    
    # Scrivi il nuovo dataframe nel nuovo file CSV
    write.csv(rendimenti_df, rendimenti_csv_file_output, row.names = FALSE)
  }
  
  # Richiamo della funzione
  rendimenti_famiglia_csv(folder = fedinvest_dir, parte_fissa_nome = "securityprice_", rendimenti_csv_file = "cusipLife.csv")
  
###### Calibrazione & Lattice Plot  ########

  
  # Model Setting ----------------------------------------------------------------

  # converto il rendimento continuamente composto e i giorni alla maturità in valori numerici (nel csv sono salvati come char)
  rendimenti_df$r_composite <- as.numeric(rendimenti_df$r_composite)
  rendimenti_df$days_to_maturity <- as.numeric(rendimenti_df$days_to_maturity)
  

  #deltaT <- as.numeric(maturity_date-data_variabile) #tempo alla maturità, a partire dall'ultima osservazione disponibile.
  deltaT <-1 #anche se tratto opzioni europee, le sto plottando su un grafico americano giornaliero, quindi sto vedendo l'andamento giornaliero, ovvero ampiezza 1 giorno.
  r <- mean(rendimenti_df$r_composite)
  u <- exp(variabilita*sqrt(deltaT))
  d <- exp(-variabilita*sqrt(deltaT))
  p <- (1 + r - d)/(u-d)
  q <- (u - (1+r))/(u-d) 
  S_0 <-  345.85 #338.77
  print(strike_legend)
  chosed_strike <-1 #è il l'i-esimo strike osservabile da strike legend. quindi se voglio il primo, metto 1, il secondo 2 etc...
  K <- strike_legend[chosed_strike]
  N <- 5
  
  #https://www.investing.com/indices/1-100-dow-jones-industrial-average-historical-data reference to DJX story
  
  
  # stock values (K not used, è stata impostata a 0) ------------------------------------------------------------
  S <- matrix(NA, nrow=N+1, ncol = N+1)
  S[1,1] <- S_0
  show(S)
  
  # First Procedure
  for(n in 1:N){
    for(k in 0:n){S[k+1,n+1] <- S_0*u^(n-k)*d^k}
  }
  show(S)
  
  BinomialTreePlot(S)
  
  # Second Procedure
  S <- matrix(NA, nrow=N+1, ncol = N+1)
  S[1,1] <- S_0
  for(n in 1:N){
    for(k in 0:n){S[n+1,k+1] <- round(S_0*u^k*d^(n-k),3)}
  }
  show(S)
  S_df <- as.data.frame(S)

  S_tb <- setDT(S_df)   
  class(S_tb)
  #head(S_tb)
  
  # library(reshape2)
  S_rsh_df <- melt(S_tb, na.rm=FALSE)
  show(S_rsh_df[1:20,])
  
  # We add an Index identifying variable to the data frame S_rsh_df
  # library(tidyverse)
  # library(dplyr)
  S_mod_rsh_df <- subset(S_rsh_df, select = -variable)
  S_mod_rsh_df <- rename(S_mod_rsh_df, S_value = value)
  #head(S_mod_rsh_df,15)
  S_mod_rsh_df <- add_column(S_mod_rsh_df, Index=rep(0:(nrow(S_df)-1), times=ncol(S_df)), .before="S_value")
  #head(S_mod_rsh_df,15)
  # We are finally in a position to draw a draft plot of the price lattice
  # library(ggplot2)
  
  Data_df <- S_mod_rsh_df
  title_content <- bquote(atop("University of Roma \"Tor Vergata\" - Corso di Metodi Probabilistici e Statistici per i Mercati Finanziari", 
                               "Example of Lattice Plot for DJX Index in CRR Model"))
  subtitle_content <- bquote(paste("market periods N = ", .(N), ", risk free rate r = ", .(r), ", up factor u = ",.(u), ", down factor d = ",.(d), ", risk neutral probability distribution (p,q) = (",.(p),",",.(q),")."))
  caption_content <- "Author: Simone Festa, mat. 0320408"
  y_breaks_num <- 4
  y_margin <- 0 #was 5
  y_breaks_low <- floor(min(Data_df$S_value, na.rm =TRUE))-y_margin
  y_breaks_up <- ceiling(max(Data_df$S_value, na.rm =TRUE))+y_margin
  y_breaks <- seq(from=y_breaks_low, to=y_breaks_up, length.out=y_breaks_num)
  y_labs <- format(y_breaks, scientific=FALSE)
  K <- 0
  y_lims <- c((y_breaks_low-K*y_margin), (y_breaks_up+K*y_margin))
  y_name <- bquote("stock values")
  y1_txt <- bquote("stock values")
  y2_txt <- bquote("call current payoffs")
  leg_labs <- c(y1_txt)
  leg_vals <- c("y1_txt"="black")
  leg_sort <- c("y1_txt")
  S_lattice_sp <- ggplot(Data_df, aes(Index, S_value)) + 
    geom_point(na.rm = TRUE, colour="black") +
    geom_text(aes(label=round(S_value,3), colour="y1_txt"), hjust=1.0, vjust=-0.7, na.rm = TRUE) + 
    ggtitle(title_content) +
    labs(subtitle=subtitle_content, caption=caption_content) +
    xlab("time") + 
    scale_y_continuous(name=y_name, breaks=y_breaks, labels=NULL, limits=y_lims,
                       sec.axis = sec_axis(~., breaks=y_breaks, labels=y_labs)) +
    scale_colour_manual(name="Legend", labels=leg_labs, values=leg_vals, breaks=leg_sort) +
    theme(plot.title=element_text(hjust=0.5), plot.subtitle=element_text(hjust=0.5),
          axis.text.x = element_text(angle=0, vjust=1),
          legend.key.width = unit(0.80,"cm"), legend.position="bottom")
  plot(S_lattice_sp)
  
  # Comparazione previsione - indice reale#######
  
  #requires install.packages("latticeExtra", repos="http://R-Forge.R-project.org")

  # Ho prelevato da https://www.investing.com/indices/1-100-dow-jones-industrial-average-historical-data i dati reali per confrontarli, dal 17/07/2023
  x<-0:5
  y<-c(345.85,349.52,350.61,352.25,352.28,354.11)
  index_real_evolution <- data.frame(x =x, y = y)
  index_prevision_evolution <- layer_data(S_lattice_sp, 1) #per incompatibilità grafica, scarico i dati del lattice e li metto in un altro grafico su cui ci posso lavorare.
  print(index_prevision_evolution)
  comparison_plot <- xyplot(y ~ x, data = index_prevision_evolution, type = "p", col = "black", pch = 16,
                          main = "Evoluzione del DJX rispetto le previsioni", xlab = "Giorni dall'osservazione", ylab = "DJX value",
                          panel = function(x, y, ...) {
                            panel.xyplot(x, y, ...)
                            panel.text(x = x, y = y, labels = y, pos = 4, offset = 0.5, cex = 0.8, col = "black")
                            panel.points(index_real_evolution$x, index_real_evolution$y, col = "red", pch = 16)
                            panel.lines(index_real_evolution$x, index_real_evolution$y, col = "red")
                            panel.text(x = index_real_evolution$x, y = index_real_evolution$y, labels = index_real_evolution$y, pos = 4, offset = 0.5, cex = 0.8, col = "red")
                      
                             })
  print(comparison_plot)
  
  # Stock values, call current payoffs######
  
  # Still assume K=S_0=100 and consider an American call option we have
  K <- strike_legend[chosed_strike] #345.85
  print(strike_legend)
  print(K)
  ACP <- matrix(NA, nrow=N+1, ncol = N+1)
  ACP[1,1] <- 0
  for(n in 1:N){
    for(k in 0:N){ACP[n+1,k+1] <- round(max(S[n+1,k+1]-K,0),3)}
  }
  show(ACP)
  
  ACP_df <- as.data.frame(ACP)
  # library("data.table")
  ACP_tb <- setDT(ACP_df)   
  class(ACP_tb)
  head(ACP_tb)
  # library(reshape2)
  ACP_rsh_df <- melt(ACP_tb, na.rm=FALSE)
  show(ACP_rsh_df[1:20,])
  ACP_mod_rsh_df <- subset(ACP_rsh_df, select = -variable)
  ACP_mod_rsh_df <- rename(ACP_mod_rsh_df, ACP_value=value)
  show(ACP_mod_rsh_df[1:20,])
  ACP_mod_rsh_df <- add_column(ACP_mod_rsh_df, Index=rep(0:(nrow(S_df)-1), times=ncol(ACP_df)), .before="ACP_value")
  show(ACP_mod_rsh_df[1:20,])
  ACP_mod_rsh_df <- add_column(ACP_mod_rsh_df, S_value=S_mod_rsh_df$S_value, .before="ACP_value")
  show(ACP_mod_rsh_df[1:20,])
  
  Data_df <- ACP_mod_rsh_df
  length <- N
  title_content <- bquote(atop("University of Roma \"Tor Vergata\" - Corso di Metodi Probabilistici e Statistici per i Mercati Finanziari", 
                               "Example of Lattice Plot for American Call Option - Current Payoffs in CRR Model"))
  subtitle_content <- bquote(paste("market periods N = ", .(N), ", risk free rate r = ", .(r), ", up factor u = ",.(u), ", down factor d = ",.(d), ", risk neutral probability distribution (p,q) = (",.(p),",",.(q),"), exercise price K = ",.(K),"."))
  caption_content <- "Author: Simone Festa, mat 032040"
  y_breaks_num <- 4 #before 4
  y_margin <- 0 #before 5
  y_breaks_low <- floor(min(Data_df$S_value, na.rm =TRUE))-y_margin
  y_breaks_up <- ceiling(max(Data_df$S_value, na.rm =TRUE))+y_margin
  y_breaks <- seq(from=y_breaks_low, to=y_breaks_up, length.out=y_breaks_num)
  y_labs <- format(y_breaks, scientific=FALSE)
  #K <- 0
  y_lims <- c((y_breaks_low-K*y_margin), (y_breaks_up+K*y_margin))
  y_name <- bquote("stock values")
  y1_txt <- bquote("stock values")
  y2_txt <- bquote("call current payoffs")
  leg_labs <- c(y1_txt, y2_txt)
  leg_vals <- c("y1_txt"="black", "y2_txt"="red")
  leg_sort <- c("y1_txt", "y2_txt")
  S_ACP_lattice_sp <- ggplot(Data_df, aes(Index, S_value, group=factor(Index))) + 
    geom_point(na.rm = TRUE, colour="black") +
    geom_text(aes(label=round(S_value,3), colour="y1_txt"), hjust=1.0, vjust=-0.7, na.rm = TRUE) + 
    geom_text(aes(label=round(ACP_value,3), colour="y2_txt"), hjust=1.0, vjust=1.3, na.rm = TRUE) + 
    ggtitle(title_content) +
    labs(subtitle=subtitle_content, caption=caption_content) +
    xlab("time") + 
    scale_y_continuous(name=y_name, breaks=y_breaks, labels=NULL, limits=y_lims,
                       sec.axis = sec_axis(~., breaks=y_breaks, labels=y_labs)) +
    scale_colour_manual(name="Legend", labels=leg_labs, values=leg_vals, breaks=leg_sort) +
    theme(plot.title=element_text(hjust=0.5), plot.subtitle=element_text(hjust=0.5),
          axis.text.x = element_text(angle=0, vjust=1),
          legend.key.width = unit(0.80,"cm"), legend.position="bottom")
  plot(S_ACP_lattice_sp)
  #
  # Stock values, call current payoffs, call expected payoffs####
  
  AC_EP <- matrix(NA, nrow=N+1, ncol = N+1)
  AC_EP[N+1,] <- ACP[N+1,]
  for(n in N:1){
    for(k in 0:n){AC_EP[n,k] <- round((1/(1+r))*(q*AC_EP[n+1,k]+p*AC_EP[n+1,k+1]),3)}
  }
  #show(AC_EP)
  
  AC_EP_df <- as.data.frame(AC_EP)
  # library("data.table")
  AC_EP_tb <- setDT(AC_EP_df)   
  class(AC_EP_tb)
  head(AC_EP_tb)
  # library(reshape2)
  AC_EP_rsh_df <- melt(AC_EP_tb, na.rm=FALSE)
  show(AC_EP_rsh_df[1:20,])
  AC_EP_mod_rsh_df <- subset(AC_EP_rsh_df, select = -variable)
  AC_EP_mod_rsh_df <- rename(AC_EP_mod_rsh_df, AC_EP_value=value)
  AC_EP_mod_rsh_df <- add_column(AC_EP_mod_rsh_df, Index=rep(0:(nrow(AC_EP_df)-1), times=ncol(AC_EP_df)), .before="AC_EP_value")
  #show(AC_EP_mod_rsh_df[1:20,])
  
  ACP_mod_rsh_df <- add_column(ACP_mod_rsh_df, AC_EP_value=AC_EP_mod_rsh_df$AC_EP_value, .after="ACP_value")
  #show(ACP_mod_rsh_df[1:20,])
  
  # library(ggplot2)
  Data_df <- ACP_mod_rsh_df
  length <- N
  title_content <- bquote(atop("University of Roma \"Tor Vergata\" - Corso di Metodi Probabilistici e Statistici per i Mercati Finanziari", 
                               "Example of Lattice Plot for American Call Option - Current Payoffs and Expected Payoffs in CRR Model"))
  subtitle_content <- bquote(paste("market periods N = ", .(N), ", risk free rate r = ", .(r), ", up factor u = ",.(u), ", down factor d = ",.(d), ", risk neutral probability distribution (p,q) = (",.(p),",",.(q),"), exercise price K = ",.(K),"."))
  caption_content <- "Author: Simone Festa, mat. 0320408"
  y_breaks_num <- 4
  y_margin <- 0 #was 4
  y_breaks_low <- floor(min(Data_df$S_value, na.rm =TRUE))-y_margin
  y_breaks_up <- ceiling(max(Data_df$S_value, na.rm =TRUE))+y_margin
  y_breaks <- seq(from=y_breaks_low, to=y_breaks_up, length.out=y_breaks_num)
  y_labs <- format(y_breaks, scientific=FALSE)
  strike_legend[chosed_strike]
  y_lims <- c((y_breaks_low-K*y_margin), (y_breaks_up+K*y_margin))
  y_name <- bquote("stock values")
  y1_txt <- bquote("stock values")
  y2_txt <- bquote("call current payoffs")
  y3_txt <- bquote("call expected payoffs")
  leg_labs <- c(y1_txt, y2_txt, y3_txt)
  leg_vals <- c("y1_txt"="black", "y2_txt"="red", "y3_txt"="blue")
  leg_sort <- c("y1_txt", "y2_txt", "y3_txt")
  S_ACP_AC_EP_lattice_sp <- ggplot(Data_df, aes(Index, S_value)) + 
    geom_point(na.rm = TRUE, colour="black") +
    geom_text(aes(label=round(S_value,3), colour="y1_txt"), hjust=1.0, vjust=-0.7, na.rm = TRUE) + 
    geom_text(aes(label=round(ACP_value,3), colour="y2_txt"), hjust=1.0, vjust=1.3, na.rm = TRUE) + 
    geom_text(aes(label=round(AC_EP_value,3), colour="y3_txt"), hjust=-0.2, vjust=1.3, na.rm = TRUE) + 
    ggtitle(title_content) +
    labs(subtitle=subtitle_content, caption=caption_content) +
    xlab("time") + 
    scale_y_continuous(name=y_name, breaks=y_breaks, labels=NULL, limits=y_lims,
                       sec.axis = sec_axis(~., breaks=y_breaks, labels=y_labs)) +
    scale_colour_manual(name="Legend", labels=leg_labs, values=leg_vals, breaks=leg_sort) +
    theme(plot.title=element_text(hjust=0.5), plot.subtitle=element_text(hjust=0.5),
          axis.text.x = element_text(angle=0, vjust=1),
          legend.key.width = unit(0.80,"cm"), legend.position="bottom")
  plot(S_ACP_AC_EP_lattice_sp)
  
  ACP_mod_rsh_df <- add_column(ACP_mod_rsh_df, ACMV_value=pmax(ACP_mod_rsh_df$ACP_value, ACP_mod_rsh_df$AC_EP_value, na.rm=TRUE), .after="AC_EP_value")
  #show(AP_PO_mod_rsh_df[1:20,])
  
  # Stock values, call current payoffs, call expected payoffs, call market value#####
  
  K <- strike_legend[chosed_strike]  
  
  Data_df <- ACP_mod_rsh_df
  length <- N
  title_content <- bquote(atop("University of Roma \"Tor Vergata\" - Corso di Metodi Probabilistici e Statistici per i Mercati Finanziari", 
                               "Example of Lattice Plot for American Call Option - Current Payoffs, Expected Payoffs, and Market Values in CRR Model"))
  subtitle_content <- bquote(paste("market periods N = ", .(N), ", risk free rate r = ", .(r), ", up factor u = ",.(u), ", down factor d = ",.(d), ", risk neutral probability distribution (p,q) = (",.(p),",",.(q),"), exercise price K = ",.(K),"."))
  caption_content <- "Author: Simone Festa, mat. 0320408"
  y_breaks_num <- 4 #original 4
  y_margin <- 0 #original 5
  y_breaks_low <- floor(min(Data_df$S_value, na.rm =TRUE))-y_margin
  y_breaks_up <- ceiling(max(Data_df$S_value, na.rm =TRUE))+y_margin
  y_breaks <- seq(from=y_breaks_low, to=y_breaks_up, length.out=y_breaks_num)
  y_labs <- format(y_breaks, scientific=FALSE)
  #K <- 0 
  y_lims <- c((y_breaks_low-K*y_margin), (y_breaks_up+K*y_margin))
  y_name <- bquote("stock values")
  y1_txt <- bquote("stock values")
  y2_txt <- bquote("call current payoffs")
  y3_txt <- bquote("call expected payoffs")
  y4_txt <- bquote("call market values")
  leg_labs <- c(y1_txt, y2_txt, y3_txt, y4_txt)
  leg_vals <- c("y1_txt"="black", "y2_txt"="red", "y3_txt"="blue", "y4_txt"="magenta")
  leg_sort <- c("y1_txt", "y2_txt", "y3_txt", "y4_txt")
  S_ACP_AC_EP_ACMV_lattice_sp <- ggplot(Data_df, aes(Index, S_value)) + 
    geom_point(na.rm = TRUE, colour="black") +
    geom_text(aes(label=round(S_value,3), colour="y1_txt"), hjust=1.0, vjust=-0.7, na.rm=TRUE) + 
    geom_text(aes(label=round(ACP_value,3), colour="y2_txt"), hjust=1.0, vjust=1.3, na.rm=TRUE) + 
    geom_text(aes(label=round(AC_EP_value,3), colour="y3_txt"), hjust=-0.2, vjust=1.3, na.rm=TRUE) + 
    geom_text(aes(label=round(ACMV_value,3), colour="y4_txt"), hjust=-0.2, vjust=-0.7, na.rm = TRUE) + 
    ggtitle(title_content) +
    labs(subtitle=subtitle_content, caption=caption_content) +
    xlab("time") + 
    scale_y_continuous(name=y_name, breaks=y_breaks, labels=NULL, limits=y_lims,
                       sec.axis = sec_axis(~., breaks=y_breaks, labels=y_labs)) +
    scale_colour_manual(name="Legend", labels=leg_labs, values=leg_vals, breaks=leg_sort) +
    theme(plot.title=element_text(hjust=0.5), plot.subtitle=element_text(hjust=0.5),
          axis.text.x = element_text(angle=0, vjust=1),
          legend.key.width = unit(0.80,"cm"), legend.position="bottom")
  plot(S_ACP_AC_EP_ACMV_lattice_sp)
  
  
 
##### Osservazioni finali######
  
  # Confronto tra payoff immediato tra DJX e strike K#######
  K <- strike_legend[chosed_strike]
  print(K)
  lattice_df <-ACP_mod_rsh_df
  lattice_df <- na.omit(lattice_df)
  
  # Creazione del grafico con etichette dei valori delle colonne "ACP_value", "AC_EP_value" e "ACMV_value"
  grafico_lattice <- ggplot(lattice_df, aes(x = Index, y = S_value)) +
    geom_point() +
    geom_text(aes(label = S_value), nudge_x = -0.2, nudge_y = +0.35, color = "black") +
    
    geom_text(aes(label = ACP_value), nudge_x = -0.2, nudge_y = -0.35, color = "red") +
    #geom_text(aes(label = AC_EP_value), nudge_x = +0.1, nudge_y = -0.25, color = "blue") +
    #geom_text(aes(label = ACMV_value), nudge_x = +0.1, nudge_y = +0.25, color = "magenta") +
    labs(title = "Confronto predizione - real data", x = "Giorni dall'osservazione", y = "Valore DJX") +
    theme_classic() +   guides(color = FALSE)
    index_real_evolution <- index_real_evolution %>%
    mutate(Payoff_immediato = y - K)
    print(index_real_evolution)
    
  call_evolution_about_chosed_strike <- clean_strike_frame[chosed_strike,]
  print(call_evolution_about_chosed_strike)
  for (i in 1:6){ 
   print(y[i])
    index_real_evolution$payoff_c0[i] <- y[i] - K- call_evolution_about_chosed_strike[i]
    if (index_real_evolution$payoff_c0[i] <=0 ){
      index_real_evolution$payoff_c0[i] <- 0
      
    }
  }
  
  print(index_real_evolution)
  
  

  grafico_confronto <- grafico_lattice +
    geom_line(data = index_real_evolution, aes(x = x, y = y), color = "#230fd6", linewidth = 1) +
    geom_point(data = index_real_evolution, aes(x = x, y = y), color = "#230fd6", size = 1.5) +
    geom_text(data = index_real_evolution, aes(x = x, y = y, label = round(Payoff_immediato, 3)), color = "#fc4103", nudge_y = -1.5)+
    geom_text(data = index_real_evolution, aes(x = x, y = y, label = round(y, 3)), color = "#190d82", vjust = 2) +
    geom_text(data = index_real_evolution, aes(x = x, y = y, label = payoff_c0), color = "violet", nudge_y = -2.2)
    
  
  
  # Stampa il grafico con i nuovi punti e le etichette "Payoff"
  plot(grafico_confronto)

  
  
  # Put - Call Parity#####
  
  #Copio in nuovo df le triple <callLP,putLP,strike> solo se nessuno dei tre è NA.
  
  # Converti le colonne "Call_LastTrTime" e "Put_LastTrTime" in formato data senza l'ora
  DJX_PCP_df <- DJX_Opt_df %>%
    mutate(Call_LastTrDate = as.Date(Call_LastTrTime),
           Put_LastTrDate = as.Date(Put_LastTrTime))

  DJX_PCP_df <- DJX_PCP_df %>% #max due giorni di differenza tra trade call e put
    filter(abs(difftime(Call_LastTrDate, Put_LastTrDate, units = "days")) <= 2)

  
  DJX_PCP_df <- DJX_PCP_df %>%
    select(Strike, Call_LastPr, Put_LastPr) #già ordinate
  print(DJX_PCP_df)
  

  S <- 355.20 #è S0, lo devo prendere dal sito investing.com il giorno che esamino la put call parity, poichè ovviamente cambia.
  DJX_PCP_df$r_stimato <- DJX_PCP_df$Strike / (DJX_PCP_df$Put_LastPr - DJX_PCP_df$Call_LastPr + S) - 1
  
  print(DJX_PCP_df)
  
  
  r_mean <-mean(DJX_PCP_df$r_stimato)
  print(r_mean)
  
  DJX_PCP_df$deltaParity<- (DJX_PCP_df$Call_LastPr - DJX_PCP_df$Put_LastPr) - S + DJX_PCP_df$Strike/(1+r_mean)
  print(DJX_PCP_df)
  print (mean(DJX_PCP_df$deltaParity))
  DJX_PCP_df <- subset(DJX_PCP_df, Strike != 345)
  print(DJX_PCP_df)
  
  
  # Stima il modello di regressione lineare
  model <- lm(r_stimato ~ Strike, data = DJX_PCP_df)
  
  # Estrai i coefficienti stimati del modello
  intercept <- coef(model)[1]
  slope <- coef(model)[2]
  
  grafico <- ggplot(DJX_PCP_df, aes(x = Strike, y = r_stimato)) +
    geom_point() +
    geom_abline(intercept = intercept, slope = slope, color = "red") +
    geom_text(aes(label = sprintf("(%.2f, %.6f)", Strike, r_stimato)), hjust = 0.5, vjust = 1.7, size = 3) +
    labs(title = "Retta di regressione per il tasso di rendimento", x = "Strike", y = "r") +
    theme_classic()

  
  plot(grafico)
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
