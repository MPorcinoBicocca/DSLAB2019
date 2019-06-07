data_2010 <- read.csv("data_2010.csv") #dati sdg 2010
data_2011 <- read.csv("data_2011.csv") #dati sdg 2011
data_2012 <- read.csv("data_2012.csv") #dati sdg 2012
data_2013 <- read.csv("data_2013.csv") #dati sdg 2013
data_2014 <- read.csv("data_2014.csv") #dati sdg 2014
data_2015 <- read.csv("data_2015.csv") #dati sdg 2015
data_2016 <- read.csv("data_2016.csv") #dati sdg 2016

##### Suddivisione delle variabili per i diversi sdg #####

sdg1 <- c("poverty", "home_warm", "dwelling_condition")
sdg4 <- c("edu_pre_scuola","edu_0_2_18_24","edu_0_2_25_34","edu_3_4_18_24","edu_3_4_25_34","edu_5_8_18_24","edu_5_8_25_34","drop_18_24")
sdg8 <- c("real_gdp_pro_capite","dmc","unemployment_rate","young_people_not_employed","killed_at_work_perc")
sdg9 <- c("ghg_emissions", "rd_expenditure", "mobile_subs", "fixed_telephone_subs", "fixed_broadband_subs", "internet_use")

##### Creazione di una lista con le lista contenente le liste create in precedenza #####

sdgs <- list(sdg1, sdg4, sdg8, sdg9)

##### Creazione della lista di nomi che verranno assegnati ai risultati dell'svdper ogni sdg #####

nomi <- c("sdg1","sdg4","sdg8","sdg9")

##### Funzione apply_svd che:
#1) per ogni sdg applica l'svd e concatena i risultati al dataset di partenza
#2) applica l'svd ai valori trovati al punto 1 e concatena il risultato al dataset di partenza

apply_svd <- function(dataset, lista){
  plot_list <- list()
  for(i in 1:4){
    tmp <- dataset[,lista[[i]]]
    tmp.svd <- svd(tmp)
    tmp_ris <- (tmp.svd$u*(-1))[,1]
    tmp_ris1<-data.frame(tmp_ris)
    colnames(tmp_ris1)<-c(nomi[i])
    dataset <- cbind(dataset, tmp_ris1)
    
  }
  temp = dataset[,nomi]
  temp.svd <- svd(temp)
  temp_ris <- abs(temp.svd$u[,1]*temp.svd$d[1])
  temp_ris1<-data.frame(temp_ris)
  colnames(temp_ris1) <- "svd"
  dataset <- cbind(dataset, temp_ris1)
  return(dataset)
}

##### Applicazione della funzione apply_svd ai dataset di ogni anno #####

data_2010.svd <- apply_svd(data_2010, sdgs)
data_2011.svd <- apply_svd(data_2011, sdgs)
data_2012.svd <- apply_svd(data_2012, sdgs)
data_2013.svd <- apply_svd(data_2013, sdgs)
data_2014.svd <- apply_svd(data_2014, sdgs)
data_2015.svd <- apply_svd(data_2015, sdgs)
data_2016.svd <- apply_svd(data_2016, sdgs)

##### Creazione del dataset con i valori finali per ogni anno e per ogni paese #####

finale <- cbind(data_2010.svd[,c("country", "svd")], data_2011.svd[,"svd"], data_2012.svd[,"svd"], data_2013.svd[,"svd"], data_2014.svd[,"svd"], data_2015.svd[,"svd"], data_2016.svd[,"svd"])
colnames(finale) <- c("country", "svd_2010", "svd_2011", "svd_2012", "svd_2013", "svd_2014", "svd_2015", "svd_2016")

