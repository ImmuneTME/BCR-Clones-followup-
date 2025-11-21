library(tidyverse)

#los archivos necesarios se componen del ID_Clon y su frecuencia
#1.- repertorio completo en PRE
#2.- repertorio completo en POST

setwd("E:/BCR/RE_1/")

for (n in 1:10000) {
  
  #1.-Lectura de los 2 grupos de archivos y almacenarlos en variables
  #clonesPRE
  Ruta_General_summary= "E:/BCR/Clone ID 2.0/PRE_exp/"
  Ruta_Pre_1_3 = "-1.clone_freq.csv"
  Ruta_Pre_2_3 = paste(Ruta_General_summary, n, Ruta_Pre_1_3, sep = "")
  if(!file.exists(Ruta_Pre_2_3)) {next}
  
  clonesPRE=read.csv(Ruta_Pre_2_3, header = TRUE, row.names = NULL, col.names = c("ID_Clon", "Frequency_PRE"))
  
  #clonesPOST 
  Ruta_General_summary= "E:/BCR/Clone ID 2.0/POST_exp/"
  Ruta_Pre_1_4 = "-2.clone_freq.csv"
  Ruta_Pre_2_4 = paste(Ruta_General_summary, n, Ruta_Pre_1_4, sep = "")
  if(!file.exists(Ruta_Pre_2_4)) {next}
  
  clonesPOST=read.csv(Ruta_Pre_2_4, header = TRUE, row.names = NULL, col.names = c("ID_Clon", "Frequency_POST"))
  
  #2.- Hacer dataframes de ID_clones con sus frecuencias pre/post segun vamos filtrando
  #unimos los clones de pre y post
  check_matches <- merge(clonesPRE, clonesPOST, by = 1, all = TRUE) 
  #donde hay dato de frecuencia del clon en pre y en post son los clones compartidos
  #estudiamos si se contraen o se expanden los clones y guardamos el listado de clones compartidos
  check_matches[complete.cases(check_matches), ] -> shared_clones
  shared_clones <- shared_clones %>%
    mutate(freq_change = ifelse(Frequency_PRE > Frequency_POST, 0, 1))
  
  Archivo_1 = paste(n, ".shared_all.csv", sep = "")
  Archivo_2 = paste("E:/BCR/RE_1/BULK/shared_bulk/", 
                    Archivo_1, sep = "")
  write.csv(shared_clones, Archivo_2, row.names = FALSE)
  
  #seleccionas los que nos estan en pre pero si en post, los emergentes
  check_matches[!complete.cases(check_matches$Frequency_PRE), ] -> emerging_clones
  select(emerging_clones, -Frequency_PRE) -> emerging_clones
  
  Archivo_3 = paste(n,  ".emerging_all.csv", sep = "")
  Archivo_4 = paste("E:/BCR/RE_1/BULK/emerging_bulk/", 
                    Archivo_3, sep = "")
  write.csv(emerging_clones, Archivo_4, row.names = FALSE )
  
  #seleccionas los que nos estan en post pero si en pre, los perdidos (borrados)
  check_matches[!complete.cases(check_matches$Frequency_POST), ] -> lost_clones
  select(lost_clones, -Frequency_POST) -> lost_clones
  
  Archivo_5 = paste(n, ".lost_all.csv", sep = "")
  Archivo_6 = paste("E:/BCR/RE_1/BULK/lost_bulk/", 
                    Archivo_5, sep = "")
  write.csv(lost_clones, Archivo_6, row.names = FALSE )
  
  #3.- Calcular espacios clonales resultantes en un dataframe
  
  #primero calculamos el numero de clones en cada categoria
  n_pre <- nrow(clonesPRE)
  n_post <- nrow(clonesPOST)
  n_clones_shared <- nrow(shared_clones)
  #FRECUENCIA PRE > FRECUENCIA POST: CONTRAEN = 0
  #FRECUENCIA PRE < FRECUENCIA POST: EXPANDEN = 1
  n_contracted <- nrow(shared_clones[shared_clones$freq_change == 0,])
  n_expanded <- nrow(shared_clones[shared_clones$freq_change == 1,])
  
  n_lost_clones <- nrow(lost_clones)
  n_clones_emerging <- nrow(emerging_clones)
  
  #obtenemos y guardamos el dataframe con todas las variables
  data.frame(
    cs_shared_pre = sum(shared_clones$Frequency_PRE),
    cs_shared_post = sum(shared_clones$Frequency_POST),
    n_clones_shared = n_clones_shared,
    percent_shared_pre = (n_clones_shared/nrow(clonesPRE))*100,
    percent_shared_post = (n_clones_shared/nrow(clonesPOST))*100,
    n_contracted = n_contracted,
    n_expanded = n_expanded,
    
    percent_contracted = (n_contracted/n_clones_shared)*100,
    percent_expanded = (n_expanded/n_clones_shared)*100,
    
    percent_contracted_pre = (n_contracted/n_pre)*100,
    percent_expanded_pre = (n_expanded/n_pre)*100,
    percent_contracted_post = (n_contracted/n_post)*100,
    percent_expanded_post = (n_expanded/n_post)*100,
    
    cs_con_pre = sum(shared_clones$Frequency_PRE[shared_clones$freq_change == 0]),
    cs_exp_pre = sum(shared_clones$Frequency_PRE[shared_clones$freq_change == 1]),
    cs_con_post = sum(shared_clones$Frequency_POST[shared_clones$freq_change == 0]),
    cs_exp_post = sum(shared_clones$Frequency_POST[shared_clones$freq_change == 1]),
    
    cs_lost_clones = sum(lost_clones$Frequency_PRE),
    n_lost_clones = n_lost_clones,
    percent_lost_clones = (n_lost_clones/nrow(clonesPRE))*100,
    
    cs_emerging_clones = sum(emerging_clones$Frequency_POST),
    n_clones_emerging = n_clones_emerging,
    percent_emerging_clones = (n_clones_emerging/nrow(clonesPOST))*100
  ) -> emerging_clonal_spaces
  
  Archivo_7 = paste(n, ".emerging_all_spaces.csv", sep = "")
  Archivo_8 = paste("E:/BCR/RE_1/BULK/clonal_spaces_bulk/", 
                    Archivo_7, sep = "")
  write.csv(emerging_clonal_spaces, Archivo_8, row.names = FALSE )
  
}


#unimos en un unico dataframe la informacion de los pacientes
folder_path <- "E:/BCR/RE_1/BULK/clonal_spaces_bulk/"
csv_files <- list.files(folder_path, pattern = "\\.csv$", full.names = TRUE)
emerging_clonal_spaces_experimental <- data.frame()
for(file in csv_files){
  data <- read.csv(file)
  row_name <- substr(basename(file), 1, 4)
  row.names(data) <- row_name
  emerging_clonal_spaces_experimental <- rbind(emerging_clonal_spaces_experimental, data)
}
write.csv(emerging_clonal_spaces_experimental, "PBMCs_bulk_emerging_cs.csv", row.names = TRUE, col.names=TRUE)

#comprobaciones
#debe ser true
nrow(clonesPRE) == n_clones_shared + n_lost_clones 
nrow(clonesPOST) == n_clones_shared + n_clones_emerging
emerging_clonal_spaces$percent_expanded + emerging_clonal_spaces$percent_contracted == 100
emerging_clonal_spaces$percent_emerging_clones + emerging_clonal_spaces$percent_shared_post == 100
emerging_clonal_spaces$percent_lost_clones + emerging_clonal_spaces$percent_shared_pre == 100
#debe ser ~ 1
emerging_clonal_spaces$cs_shared_pre + emerging_clonal_spaces$cs_lost_clones -> cs_pre
emerging_clonal_spaces$cs_shared_post + emerging_clonal_spaces$cs_emerging_clones -> cs_post


