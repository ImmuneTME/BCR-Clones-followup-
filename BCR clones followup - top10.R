library(tidyverse)
library(plyr)
library(openxlsx)

#se abren los archivos donde [1] = ID_CLon; [2] = Frecuency
#para estudiar el repertorio teniendo en cuenta el top 10 los archivos a abrir, en orden, son:
#1.- top 10 en pre -> top10PRE
#2.- top 10 en post -> top10POST
#3.- todo el repertorio en pre -> clonesPRE
#4.- todo el repertorio en post -> clonesPOST


for (n in 1002:2105) {
  #1.-Lectura de los archivos y almacenarlos en variables
  #top10PRE
  Ruta_General_toppre= "E:/BCR/TOP 10 2.0/TOP 10 lists/TOP 10 list PRE exp/"
  Ruta_Pre_1_1 = "-1.top10_list.csv"
  Ruta_Pre_2_1 = paste(Ruta_General_toppre, n, Ruta_Pre_1_1, sep = "")
  if(!file.exists(Ruta_Pre_2_1)) {next}
  
  top10PRE=read.csv(Ruta_Pre_2_1, header = TRUE, row.names = NULL, col.names = c("ID_Clon", "Frequency_PRE"))
  
  #top10POST
  Ruta_General_toppost= "E:/BCR/TOP 10 2.0/TOP 10 lists/TOP 10 list POST exp/"
  Ruta_Pre_1_2 = "-2.top10_list.csv"
  Ruta_Pre_2_2 = paste(Ruta_General_toppost, n, Ruta_Pre_1_2, sep = "")
  if(!file.exists(Ruta_Pre_2_2)) {next}
  
  top10POST=read.csv(Ruta_Pre_2_2, header = TRUE, row.names = NULL, col.names = c("ID_Clon", "Frequency_POST"))
  
  #clonesPRE
  Ruta_General_summary= "E:/BCR/Clone ID 2.0/PRE_exp/"
  Ruta_Pre_1_3 = "-1.clone_freq.csv"
  Ruta_Pre_2_3 = paste(Ruta_General_summary, n, Ruta_Pre_1_3, sep = "")
  if(!file.exists(Ruta_Pre_2_3)) {next}
  
  clonesPRE=read.csv(Ruta_Pre_2_3, header = TRUE, row.names = NULL, col.names = c("ID_Clon", "Frequency_PRE_all"))
  
  #clonesPOST
  Ruta_General_summary= "E:/BCR/Clone ID 2.0/POST_exp/"
  Ruta_Pre_1_4 = "-2.clone_freq.csv"
  Ruta_Pre_2_4 = paste(Ruta_General_summary, n, Ruta_Pre_1_4, sep = "")
  if(!file.exists(Ruta_Pre_2_4)) {next}
  
  clonesPOST=read.csv(Ruta_Pre_2_4, header = TRUE, row.names = NULL, col.names = c("ID_Clon", "Frequency_POST_all"))
  
  #2.- Hacer dataframes de ID_clones con sus frecuencias pre/post segun vamos filtrando y guardarlos
  #vemos los que coinciden del top 10, conservando todas las entradas 
  top10pre_matches <- merge(top10PRE, top10POST, 
                            by = 1, all = TRUE) 
  #nos quedamos con los que estan en post pero no en pre
  top10pre_matches[!complete.cases(top10pre_matches$Frequency_PRE), ] -> checkdf
  #nos quedamos con el top 10 compartido entre pre y post y la frecuencia de dichos clones en ambos repertorios
  top10pre_matches[complete.cases(top10pre_matches), ] -> top10shared
  
  #comprobamos el cambio de la frecuencia de esos clones en su paso de pre a post
  # 0 si en pre ocupan mas que en post; 1 si en post ocupan mas que en pre
  top10shared <- top10shared %>%
    mutate(freq_change = ifelse(Frequency_PRE > Frequency_POST, 0, 1))
  #calculo del fold change del cambio de frecuencia
  top10shared$Frequency_POST/top10shared$Frequency_PRE -> top10shared$FC
  #clasificar el cambio en al menos duplicacion o reduccion a la mitad y no cambio
  top10shared$change <- ifelse(top10shared$FC >= 2, "doubled", ifelse(top10shared$FC <= 0.5, "halfed", "unchanged"))
  
  #guardamos la lista de clones del top 10 shared
  Archivo_1 = paste(n, "-1", ".top10shared.csv", sep = "")
  Archivo_2 = paste("E:/BCR/RE_1/SPECIFICS/shared_top10/", 
                    Archivo_1, sep = "")
  write.csv(top10shared, Archivo_2, row.names = FALSE)
  
  #junto el top 10 que solo esta en post con el repertorio completo en pre
  allclones_matches <- merge(checkdf, clonesPRE, 
                             by = 1, all = TRUE) 
  #los que no coinciden seran clones emergentes del top 10
  #los que si coincidan son clones promoted (ascendidos), no estan en el top 10 en pre pero si en el resto del repertorio y aparecen en el top 10 en post
  allclones_matches[complete.cases(allclones_matches$Frequency_POST), ] -> check2df
  allclones_matches[!complete.cases(allclones_matches$Frequency_PRE_all), ] -> emerging_clones  
  select(emerging_clones, -Frequency_PRE, -Frequency_PRE_all) -> top10emerging
  
  #guardamos los clones emergentes en el top 10
  Archivo_3 = paste(n, "-1", ".top10emerging.csv", sep = "")
  Archivo_4 = paste("E:/BCR/RE_1/SPECIFICS/emerging_top10/", 
                    Archivo_3, sep = "")
  write.csv(top10emerging, Archivo_4, row.names = FALSE )
  
  #ahora nos quedamos con los promoted
  allclones_matches[complete.cases(allclones_matches$Frequency_PRE_all, allclones_matches$Frequency_POST),] -> non_top10shared1
  select(non_top10shared1, -Frequency_PRE) -> non_top10shared1
  
  #vemos los cambios en las frecuencias en el paso de pre a post
  non_top10shared <- non_top10shared1 %>%
    mutate(freq_change = ifelse(Frequency_PRE_all > Frequency_POST, 0, 1))
  non_top10shared$Frequency_POST/non_top10shared$Frequency_PRE_all -> non_top10shared$FC
  non_top10shared$change <- ifelse(non_top10shared$FC >= 2, "doubled", ifelse(non_top10shared$FC <= 0.5, "halfed", "unchanged"))
  
  #guardamos la lista de clones ascendidos
  Archivo_5 = paste(n, "-1", ".top10promoted.csv", sep = "")
  Archivo_6 = paste("E:/BCR/RE_1/SPECIFICS/promoted_top10/", 
                    Archivo_5, sep = "")
  write.csv(non_top10shared, Archivo_6, row.names = FALSE )
  
  #al total de clones de pre le quitamos su top10
  anti_join(clonesPRE, top10PRE, by = "ID_Clon") -> PRE_restante
  colnames(PRE_restante) <- c("ID_Clon", "Frequency_PRE_rest")
  
  #al total de clones de post le quitamos su top10
  anti_join(clonesPOST, top10POST, by = "ID_Clon") -> POST_restante
  colnames(POST_restante) <- c("ID_Clon", "Frequency_POST_rest")
  
  #a veces no hay clones compartidos en el top 10 de pre y el top 10 de post, lo que detiene el bucle mas adelante
  #por ello hacemos esto: si no hay de esos clones nos quedamos con el top 10 de pre
  #si los hay, al top 10 de pre le quitamos los compartidos con post
  #esto lo usaremos para ver el top 10 degradado y el top 10 borrado
  # Check if top10shared is empty
  if (nrow(top10shared) == 0) {
    thing <- top10PRE
  } else {
    thing <- anti_join(top10PRE, top10shared, by = "ID_Clon")
  }
  
  #nos quedamos con top 10 perdido: top 10 no compartido (pre)
  anti_join(top10PRE, top10shared, by = "ID_Clon") -> top10perdido
  Archivo_7 = paste(n, "-1", ".top10lost.csv", sep = "")
  Archivo_8 = paste("E:/BCR/RE_1/SPECIFICS/lost_top10/", 
                    Archivo_7, sep = "")
  write.csv(top10perdido, Archivo_8, row.names = FALSE )
  
  #nos quedamos con el nuevo top 10: top 10 no compartido (post)
  anti_join(top10POST, top10shared, by = "ID_Clon") -> top10nuevo
  Archivo_9 = paste(n, "-1", ".top10new.csv", sep = "")
  Archivo_10 = paste("E:/BCR/RE_1/SPECIFICS/new_top10/", 
                     Archivo_9, sep = "")
  write.csv(top10nuevo, Archivo_10, row.names = FALSE )
  
  #aqui usamos la construccion de antes que tiene en cuenta si el top 10 compartido esta vacio
  #nos quedamos con los clones del top 10 pre que en post aparecen fuera del top 10
  merge(thing, POST_restante, by = "ID_Clon") -> top10degradado1
  #estudiamos sus cambios en frecuencia y lo guardamos
  top10degradado <- top10degradado1 %>%
    mutate(freq_change = ifelse(Frequency_PRE > Frequency_POST_rest, 0, 1))
  top10degradado$Frequency_POST_rest/top10degradado$Frequency_PRE -> top10degradado$FC
  top10degradado$change <- ifelse(top10degradado$FC >= 2, "doubled", ifelse(top10degradado$FC <= 0.5, "halfed", "unchanged"))
  Archivo_11 = paste(n, "-1", ".top10demoted.csv", sep = "")
  Archivo_12 = paste("E:/BCR/RE_1/SPECIFICS/demoted_top10/", 
                     Archivo_11, sep = "")
  write.csv(top10degradado, Archivo_12, row.names = FALSE )
  
  #los top 10 no compartidos ni en el top 10 ni en el resto del repertorio de post son los borrados 
  anti_join(thing, POST_restante, by = "ID_Clon") -> top10borrado
  Archivo_13 = paste(n, "-1", ".top10erased.csv", sep = "")
  Archivo_14 = paste("E:/BCR/RE_1/SPECIFICS/erased_top10/", 
                     Archivo_13, sep = "")
  write.csv(top10borrado, Archivo_14, row.names = FALSE )
  
  #estudiamos los que se comparten entre pre y post y siempre estan fuera del top 10
  merge(PRE_restante, POST_restante, by = "ID_Clon") -> otros_shared_fixed1
  otros_shared_fixed <- otros_shared_fixed1 %>%
    mutate(freq_change = ifelse(Frequency_PRE_rest > Frequency_POST_rest, 0, 1))
  otros_shared_fixed$Frequency_POST_rest/otros_shared_fixed$Frequency_PRE_rest -> otros_shared_fixed$FC
  otros_shared_fixed$change <- ifelse(otros_shared_fixed$FC >= 2, "doubled", ifelse(otros_shared_fixed$FC <= 0.5, "halfed", "unchanged"))
  Archivo_15 = paste(n, "-1", ".other_shared_fixed.csv", sep = "")
  Archivo_16 = paste("E:/BCR/RE_1/SPECIFICS/other_shared_fixed_top10/", 
                     Archivo_15, sep = "")
  write.csv(otros_shared_fixed, Archivo_16, row.names = FALSE )
  
  #guardamos y estudiamos los cambios en frecuencia de los clones ascendidos en pre
  #los clones que en pre estan en el resto del repertorio y en post en el top10 (en pre)
  non_top10shared1 -> other_promoted1
  c("ID_Clon", "Frequency_POST_rest", "Frequency_PRE_rest") -> colnames(other_promoted1)
  other_promoted <- other_promoted1 %>%
    mutate(freq_change = ifelse(Frequency_PRE_rest > Frequency_POST_rest, 0, 1))
  other_promoted$Frequency_POST_rest/other_promoted$Frequency_PRE_rest -> other_promoted$FC
  other_promoted$change <- ifelse(other_promoted$FC >= 2, "doubled", ifelse(other_promoted$FC <= 0.5, "halfed", "unchanged"))
  Archivo_17 = paste(n, "-1", ".other_promoted.csv", sep = "")
  Archivo_18 = paste("E:/BCR/RE_1/SPECIFICS/other_promoted_top10/", 
                     Archivo_17, sep = "")
  write.csv(other_promoted, Archivo_18, row.names = FALSE )
  
  #guardamos y estudiamos los cambios en frecuencia de los clones degradados en post
  #clones que en pre estan en el top 10 y en post en el resto del repertorio (en post) 
  top10degradado1 -> other_demoted1
  c("ID_Clon", "Frequency_PRE_rest", "Frequency_POST_rest") -> colnames(other_demoted1)
  other_demoted <- other_demoted1 %>%
    mutate(freq_change = ifelse(Frequency_PRE_rest > Frequency_POST_rest, 0, 1))
  other_demoted$Frequency_POST_rest/other_demoted$Frequency_PRE_rest -> other_demoted$FC
  other_demoted$change <- ifelse(other_demoted$FC >= 2, "doubled", ifelse(other_demoted$FC <= 0.5, "halfed", "unchanged"))
  Archivo_19 = paste(n, "-1", ".other_demoted.csv", sep = "")
  Archivo_20 = paste("E:/BCR/RE_1/SPECIFICS/other_demoted_top10/", 
                     Archivo_19, sep = "")
  write.csv(other_demoted, Archivo_20, row.names = FALSE )
  
  #unimos los clones fijos y no fijos que se comparten fuera del top 10 en pre y en post
  #other shared fixed + other promoted (pre) / other demoted (post)
  join(otros_shared_fixed, other_promoted, type = "full") -> other_shared_pre
  join(otros_shared_fixed, other_demoted, type = "full") -> other_shared_post
  #al repertorio no top 10 le quitamos lo compartido no top 10 y obtenemos otros borrados (pre) y otros emergentes (post)
  otros_emergentes <- anti_join(POST_restante, other_shared_post, by = "ID_Clon")
  otros_borrados <- anti_join(PRE_restante, other_shared_pre, by = "ID_Clon")
  
  #calculamos el numero de clones de cada categoria para poder obtener las metricas de cs y % 
  n_pre <- nrow(clonesPRE)
  n_post <- nrow(clonesPOST)
  
  n_top10shared <- nrow(top10shared)
  n_con_top10shared <- nrow(top10shared[top10shared$freq_change == 0,])
  n_exp_top10shared <- nrow(top10shared[top10shared$freq_change == 1,])
  n_doubled_top10shared <- nrow(top10shared[top10shared$change == "doubled",])
  n_halfed_top10shared <- nrow(top10shared[top10shared$change == "halfed",])
  n_unchanged_top10shared <- nrow(top10shared[top10shared$change == "unchanged",])
  
  n_top10degradado <- nrow(top10degradado)
  n_con_top10degradado <- nrow(top10degradado[top10degradado$freq_change == 0,])
  n_exp_top10degradado <- nrow(top10degradado[top10degradado$freq_change == 1,])
  n_doubled_top10degradado <- nrow(top10degradado[top10degradado$change == "doubled",])
  n_halfed_top10degradado <- nrow(top10degradado[top10degradado$change == "halfed",])
  n_unchanged_top10degradado <- nrow(top10degradado[top10degradado$change == "unchanged",])
  
  n_top10borrado <- nrow(top10borrado)
  n_non_top10shared <- nrow(non_top10shared)
  n_top10perdido <- nrow(top10perdido)
  n_top10nuevo <- nrow(top10nuevo) 
  n_top10emerging <- nrow(top10emerging)
  
  n_top10promoted <- nrow(non_top10shared)
  n_con_top10promoted <- nrow(non_top10shared[non_top10shared$freq_change == 0,])
  n_exp_top10promoted <- nrow(non_top10shared[non_top10shared$freq_change == 1,])
  n_doubled_top10promoted <- nrow(non_top10shared[non_top10shared$change == "doubled",])
  n_halfed_top10promoted <- nrow(non_top10shared[non_top10shared$change == "halfed",])
  n_unchanged_top10promoted <- nrow(non_top10shared[non_top10shared$change == "unchanged",])
  
  n_pre_restante <- nrow(PRE_restante)
  n_post_restante <- nrow(POST_restante) 
  
  n_otros_borrados <- nrow(otros_borrados)
  n_otros_emergentes <- nrow(otros_emergentes)
  n_otros_shared_pre <- nrow(other_shared_pre)
  n_otros_shared_post <-nrow(other_shared_post)
  
  n_otros_shared_fixed <- nrow(otros_shared_fixed)
  n_con_otros_shared_fixed <- nrow(otros_shared_fixed [otros_shared_fixed$freq_change == 0,])
  n_exp_otros_shared_fixed  <- nrow(otros_shared_fixed [otros_shared_fixed$freq_change == 1,])
  n_doubled_otros_shared_fixed <- nrow(otros_shared_fixed[otros_shared_fixed$change == "doubled",])
  n_halfed_otros_shared_fixed <- nrow(otros_shared_fixed[otros_shared_fixed$change == "halfed",])
  n_unchanged_otros_shared_fixed <- nrow(otros_shared_fixed[otros_shared_fixed$change == "unchanged",])
  
  n_other_promoted <- nrow(other_promoted)
  n_con_other_promoted <- nrow(other_promoted [other_promoted$freq_change == 0,])
  n_exp_other_promoted  <- nrow(other_promoted [other_promoted$freq_change == 1,])
  n_doubled_other_promoted <- nrow(other_promoted[other_promoted$change == "doubled",])
  n_halfed_other_promoted <- nrow(other_promoted[other_promoted$change == "halfed",])
  n_unchanged_other_promoted <- nrow(other_promoted[other_promoted$change == "unchanged",])
  
  n_other_demoted <- nrow(other_demoted)
  n_con_other_demoted <- nrow(other_demoted [other_demoted$freq_change == 0,])
  n_exp_other_demoted <- nrow(other_demoted [other_demoted$freq_change == 1,])
  n_doubled_other_demoted <- nrow(other_demoted[other_demoted$change == "doubled",])
  n_halfed_other_demoted <- nrow(other_demoted[other_demoted$change == "halfed",])
  n_unchanged_other_demoted <- nrow(other_demoted[other_demoted$change == "unchanged",])
  
  n_other_shared_pre <- nrow(other_shared_pre)
  n_other_shared_post <- nrow(other_shared_post)
  
  #calculos de las metricas
  #nota: los porcentajes SIEMPRE se refieren al total del repertorio 
  data.frame(
    cs_top10shared_pre = sum(top10shared$Frequency_PRE),
    n_top10shared = nrow(top10shared),
    percent_top10shared_pre = (n_top10shared/n_pre)*100,
    cs_top10shared_post = sum(top10shared$Frequency_POST),
    percent_top10shared_post = (n_top10shared/n_post)*100,
    
    n_con_top10shared = n_con_top10shared,
    n_exp_top10shared = n_exp_top10shared,
    percent_con_top10shared_pre =  (n_con_top10shared/n_pre)*100,
    percent_exp_top10shared_pre = (n_exp_top10shared/n_pre)*100,
    percent_con_top10shared_post =  (n_con_top10shared/n_post)*100,
    percent_exp_top10shared_post = (n_exp_top10shared/n_post)*100,  
    
    n_doubled_top10shared = n_doubled_top10shared,
    n_halfed_top10shared = n_halfed_top10shared,
    n_unchanged_top10shared = n_unchanged_top10shared,
    percent_doubled_top10shared_pre = (n_doubled_top10shared/n_pre)*100,
    percent_halfed_top10shared_pre = (n_halfed_top10shared/n_pre)*100,
    percent_unchanged_top10shared_pre = (n_unchanged_top10shared/n_pre)*100,
    percent_doubled_top10shared_post = (n_doubled_top10shared/n_post)*100,
    percent_halfed_top10shared_post = (n_halfed_top10shared/n_post)*100,
    percent_unchanged_top10shared_post = (n_unchanged_top10shared/n_post)*100,
    
    cs_top10shared_con_pre = sum(top10shared$Frequency_PRE[top10shared$freq_change == 0]),
    cs_top10shared_exp_pre = sum(top10shared$Frequency_PRE[top10shared$freq_change == 1]),
    cs_top10shared_con_post = sum(top10shared$Frequency_POST[top10shared$freq_change == 0]),
    cs_top10shared_exp_post = sum(top10shared$Frequency_POST[top10shared$freq_change == 1]),
    
    cs_top10shared_doubled_pre = sum(top10shared$Frequency_PRE[top10shared$change == "doubled"]),
    cs_top10shared_halfed_pre = sum(top10shared$Frequency_PRE[top10shared$change == "halfed"]),
    cs_top10shared_unchanged_pre = sum(top10shared$Frequency_PRE[top10shared$change == "unchanged"]),
    cs_top10shared_doubled_post = sum(top10shared$Frequency_POST[top10shared$change == "doubled"]),
    cs_top10shared_halfed_post = sum(top10shared$Frequency_POST[top10shared$change == "halfed"]),
    cs_top10shared_unchanged_post = sum(top10shared$Frequency_POST[top10shared$change == "unchanged"]),
    
    FC_top10shared = median(top10shared$FC),
    
    cs_top10nuevo = sum(top10nuevo$Frequency_POST),
    n_top10nuevo = nrow(top10nuevo),
    percent_top10nuevo = (n_top10nuevo/nrow(clonesPOST))*100,
    
    cs_top10perdido = sum(top10perdido$Frequency_PRE),
    n_top10perdido = nrow(top10perdido),
    percent_top10perdido = (n_top10perdido/nrow(clonesPRE))*100,
    
    cs_top10emergente = sum(top10emerging$Frequency_POST),
    n_top10emerging = n_top10emerging,
    percent_top10emerging = (n_top10emerging/n_post)*100,
    
    cs_top10degradado = sum(top10degradado$Frequency_PRE),
    n_top10degradado = nrow(top10degradado),
    percent_top10degradado = (n_top10degradado/nrow(clonesPRE))*100,
    
    n_con_top10degradado = n_con_top10degradado,
    n_exp_top10degradado = n_exp_top10degradado,
    percent_con_top10degradado =  (n_con_top10degradado/n_pre)*100,
    percent_exp_top10degradado = (n_exp_top10degradado/n_pre)*100,
    
    n_doubled_top10degradado = n_doubled_top10degradado,
    n_halfed_top10degradado = n_halfed_top10degradado,
    n_unchanged_top10degradado = n_unchanged_top10degradado,
    percent_doubled_top10degradado = (n_doubled_top10degradado/n_pre)*100,
    percent_halfed_top10degradado = (n_halfed_top10degradado/n_pre)*100,
    percent_unchanged_top10degradado = (n_unchanged_top10degradado/n_pre)*100,
    
    cs_top10degradado_con = sum(top10degradado$Frequency_PRE[top10degradado$freq_change == 0]),
    cs_top10degradado_exp = sum(top10degradado$Frequency_PRE[top10degradado$freq_change == 1]),
    
    cs_top10degradado_doubled = sum(top10degradado$Frequency_PRE[top10degradado$change == "doubled"]),
    cs_top10degradado_halfed = sum(top10degradado$Frequency_PRE[top10degradado$change == "halfed"]),
    cs_top10degradado_unchanged = sum(top10degradado$Frequency_PRE[top10degradado$change == "unchanged"]),
    
    FC_top10degradado = median(top10degradado$FC),
    
    cs_top10borrado = sum(top10borrado$Frequency_PRE),
    n_top10borrado = n_top10borrado,
    percent_top10borrado = (n_top10borrado/n_pre)*100,
    
    cs_top10promoted = sum(non_top10shared$Frequency_POST),
    n_top10promoted = n_top10promoted,
    percent_top10promoted = (n_top10promoted/n_post)*100,
    
    n_con_top10promoted = n_con_top10promoted,
    n_exp_top10promoted = n_exp_top10promoted,
    percent_con_top10promoted =  (n_con_top10promoted/n_post)*100,
    percent_exp_top10promoted = (n_exp_top10promoted/n_post)*100,  
    
    n_doubled_top10promoted = n_doubled_top10promoted,
    n_halfed_top10promoted = n_halfed_top10promoted,
    n_unchanged_top10promoted = n_unchanged_top10promoted,
    percent_doubled_top10promoted = (n_doubled_top10promoted/n_post)*100,
    percent_halfed_top10promoted = (n_halfed_top10promoted/n_post)*100,
    percent_unchanged_top10promoted = (n_unchanged_top10promoted/n_post)*100,
    
    cs_top10promoted_con = sum(non_top10shared$Frequency_POST[non_top10shared$freq_change == 0]),
    cs_top10promoted_exp = sum(non_top10shared$Frequency_POST[non_top10shared$freq_change == 1]),
    
    cs_top10promoted_doubled = sum(non_top10shared$Frequency_POST[non_top10shared$change == "doubled"]),
    cs_top10promoted_halfed = sum(non_top10shared$Frequency_POST[non_top10shared$change == "halfed"]),
    cs_top10promoted_unchanged = sum(non_top10shared$Frequency_POST[non_top10shared$change == "unchanged"]),
    
    FC_top10promoted = median(non_top10shared$FC),
    
    cs_PRE_restante = sum(PRE_restante$Frequency_PRE_rest),
    n_PRE_restante = nrow(PRE_restante),
    cs_POST_restante = sum(POST_restante$Frequency_POST_rest),
    n_POST_restante = nrow(POST_restante),
    
    cs_otros_emergentes = sum(otros_emergentes$Frequency_POST_rest),
    n_otros_emergentes = nrow(otros_emergentes),
    percent_otros_emergentes = (n_otros_emergentes/nrow(clonesPOST))*100,
    
    cs_otros_borrados = sum(otros_borrados$Frequency_PRE_rest),
    n_otros_borrados = n_otros_borrados,
    percent_otros_borrados = (n_otros_borrados/n_pre)*100,
    
    cs_otros_shared_pre = sum(other_shared_pre$Frequency_PRE_rest),
    n_otros_shared_pre = n_otros_shared_pre,
    percent_otros_shared_pre = (n_otros_shared_pre/n_pre)*100,
    cs_otros_shared_post = sum(other_shared_post$Frequency_POST_rest),
    n_otros_shared_post = n_otros_shared_post,
    percent_otros_shared_post = sum(n_otros_shared_post/n_post)*100,
    
    cs_otros_shared_fixed_pre = sum(otros_shared_fixed$Frequency_PRE_rest),
    n_otros_shared_fixed = n_otros_shared_fixed,
    percent_otros_shared_fixed_pre = (n_otros_shared_fixed/n_pre)*100,
    cs_other_shared_fixed_post = sum(otros_shared_fixed$Frequency_POST_rest),
    percent_otros_shared_fixed_post = (n_otros_shared_fixed/n_post)*100,
    
    n_con_otros_shared_fixed = n_con_otros_shared_fixed,
    n_exp_otros_shared_fixed = n_exp_otros_shared_fixed,
    percent_con_otros_shared_fixed_pre =  (n_con_otros_shared_fixed/n_pre)*100,
    percent_exp_otros_shared_fixed_pre = (n_exp_otros_shared_fixed/n_pre)*100,
    percent_con_otros_shared_fixed_post =  (n_con_otros_shared_fixed/n_post)*100,
    percent_exp_otros_shared_fixed_post = (n_exp_otros_shared_fixed/n_post)*100,  
    
    n_doubled_otros_shared_fixed = n_doubled_otros_shared_fixed,
    n_halfed_otros_shared_fixed = n_halfed_otros_shared_fixed,
    n_unchanged_otros_shared_fixed = n_unchanged_otros_shared_fixed,
    percent_doubled_otros_shared_fixed_pre = (n_doubled_otros_shared_fixed/n_pre)*100,
    percent_halfed_otros_shared_fixed_pre = (n_halfed_otros_shared_fixed/n_pre)*100,
    percent_unchanged_otros_shared_fixed_pre = (n_unchanged_otros_shared_fixed/n_pre)*100,
    percent_doubled_otros_shared_fixed_post = (n_doubled_otros_shared_fixed/n_post)*100,
    percent_halfed_otros_shared_fixed_post = (n_halfed_otros_shared_fixed/n_post)*100,
    percent_unchanged_otros_shared_fixed_post = (n_unchanged_otros_shared_fixed/n_post)*100,
    
    cs_otros_shared_fixed_con_pre = sum(otros_shared_fixed$Frequency_PRE_rest[otros_shared_fixed$freq_change == 0]),
    cs_otros_shared_fixed_exp_pre = sum(otros_shared_fixed$Frequency_PRE_rest[otros_shared_fixed$freq_change == 1]),
    cs_otros_shared_fixed_con_post = sum(otros_shared_fixed$Frequency_POST_rest[otros_shared_fixed$freq_change == 0]),
    cs_otros_shared_fixed_exp_post = sum(otros_shared_fixed$Frequency_POST_rest[otros_shared_fixed$freq_change == 1]),
    
    cs_otros_shared_fixed_doubled_pre = sum(otros_shared_fixed$Frequency_PRE_rest[otros_shared_fixed$change == "doubled"]),
    cs_otros_shared_fixed_halfed_pre = sum(otros_shared_fixed$Frequency_PRE_rest[otros_shared_fixed$change == "halfed"]),
    cs_otros_shared_fixed_unchanged_pre = sum(otros_shared_fixed$Frequency_PRE_rest[otros_shared_fixed$change == "unchanged"]),
    cs_otros_shared_fixed_doubled_post = sum(otros_shared_fixed$Frequency_POST_rest[otros_shared_fixed$change == "doubled"]),
    cs_otros_shared_fixed_halfed_post = sum(otros_shared_fixed$Frequency_POST_rest[otros_shared_fixed$change == "halfed"]),
    cs_otros_shared_fixed_unchanged_post = sum(otros_shared_fixed$Frequency_POST_rest[otros_shared_fixed$change == "unchanged"]),
    
    FC_otros_shared_fixed = median(otros_shared_fixed$FC),
    
    cs_other_promoted = sum(other_promoted$Frequency_PRE_rest),
    n_other_promoted = n_other_promoted,
    percent_other_promoted = (n_other_promoted/n_pre)*100,
    
    n_con_other_promoted = n_con_other_promoted,
    n_exp_other_promoted = n_exp_other_promoted,
    percent_con_other_promoted =  (n_con_other_promoted/n_pre)*100,
    percent_exp_other_promoted = (n_exp_other_promoted/n_pre)*100,
    
    n_doubled_other_promoted = n_doubled_other_promoted,
    n_halfed_other_promoted = n_halfed_other_promoted,
    n_unchanged_other_promoted = n_unchanged_other_promoted,
    percent_doubled_other_promoted = (n_doubled_other_promoted/n_pre)*100,
    percent_halfed_other_promoted = (n_halfed_other_promoted/n_pre)*100,
    percent_unchanged_other_promoted = (n_unchanged_other_promoted/n_pre)*100,
    
    cs_other_promoted_con = sum(other_promoted$Frequency_PRE_rest[other_promoted$freq_change == 0]),
    cs_other_promoted_exp = sum(other_promoted$Frequency_PRE_rest[other_promoted$freq_change == 1]),
    
    cs_other_promoted_doubled = sum(other_promoted$Frequency_PRE_rest[other_promoted$change == "doubled"]),
    cs_other_promoted_halfed = sum(other_promoted$Frequency_PRE_rest[other_promoted$change == "halfed"]),
    cs_other_promoted_unchanged= sum(other_promoted$Frequency_PRE_rest[other_promoted$change == "unchanged"]),
    
    FC_other_promoted = median(other_promoted$FC),
    
    cs_other_demoted = sum(other_demoted$Frequency_POST_rest),
    n_other_demoted = n_other_demoted,
    percent_other_demoted = (n_other_demoted/n_post)*100,
    
    n_con_other_demoted = n_con_other_demoted,
    n_exp_other_demoted = n_exp_other_demoted,
    percent_con_other_demoted =  (n_con_other_demoted/n_post)*100,
    percent_exp_other_demoted = (n_exp_other_demoted/n_post)*100,  
    
    n_doubled_other_demoted = n_doubled_other_demoted,
    n_halfed_other_demoted = n_halfed_other_demoted,
    n_unchanged_other_demoted = n_unchanged_other_demoted,
    percent_doubled_other_demoted = (n_doubled_other_demoted/n_post)*100,
    percent_halfed_other_demoted = (n_halfed_other_demoted/n_post)*100,
    percent_unchanged_other_demoted = (n_unchanged_other_demoted/n_post)*100,
    
    cs_other_demoted_con = sum(other_demoted$Frequency_POST_rest[other_demoted$freq_change == 0]),
    cs_other_demoted_exp = sum(other_demoted$Frequency_POST_rest[other_demoted$freq_change == 1]),
    
    cs_other_demoted_doubled = sum(other_demoted$Frequency_POST_rest[other_demoted$change == "doubled"]),
    cs_other_demoted_halfed = sum(other_demoted$Frequency_POST_rest[other_demoted$change == "halfed"]),
    cs_other_demoted_unchanged = sum(other_demoted$Frequency_POST_rest[other_demoted$change == "unchanged"]),
    
    FC_other_demoted = median(other_demoted$FC)
    
  ) -> new_metrics
  Archivo_21 = paste(n, "-1", ".emerging_top10_spaces.csv", sep = "")
  Archivo_22 = paste("E:/BCR/RE_1/SPECIFICS/clonal_spaces_top10/", 
                     Archivo_21, sep = "")
  write.csv(new_metrics, Archivo_22, row.names = FALSE )
}


#ya una vez calculadas las metricas para cada paciente, unimos la informacion de todos los pacientes en un unico dataframe
#rbind clonal_spaces info
folder_path <- "E:/BCR/RE_1/SPECIFICS/clonal_spaces_top10/"
csv_files <- list.files(folder_path, pattern = "\\.csv$", full.names = TRUE)
emerging_clonal_spaces_experimental <- data.frame()
for(file in csv_files){
  data <- read.csv(file)
  row_name <- substr(basename(file), 1, 4)
  row.names(data) <- row_name
  emerging_clonal_spaces_experimental <- rbind(emerging_clonal_spaces_experimental, data)
}
write.csv(emerging_clonal_spaces_experimental, "PBMCs_top10_emerging_cs.csv", row.names = TRUE, col.names=TRUE)


#comprobaciones de que los calculos son correctos
n_top10shared + n_top10degradado + n_top10borrado
n_top10shared + n_top10perdido  
n_otros_borrados + n_otros_shared_pre
n_otros_borrados + n_otros_shared_fixed + n_other_promoted
n_pre_restante
n_pre

cs_otros_borrados + cs_other_shared_fixed_pre + cs_other_promoted + cs_top10shared_pre + cs_top10degradado_PRE + cs_top10borrado
percent_otros_borrados + percent_other_shared_fixed_pre + percent_other_promoted + percent_top10shared_pre + percent_top10degradado_PRE + percent_top10borrado

n_otros_borrados + n_doubled_otros_shared_fixed + n_halfed_otros_shared_fixed + n_unchanged_otros_shared_fixed + n_doubled_other_promoted + n_halfed_other_promoted + n_unchanged_other_promoted + n_doubled_top10shared + n_halfed_top10shared + n_unchanged_top10shared + n_doubled_top10degradado + n_halfed_top10degradado + n_unchanged_top10degradado + n_top10borrado
new_metrics$cs_otros_borrados + new_metrics$cs_otros_shared_fixed_doubled_pre+ new_metrics$cs_otros_shared_fixed_halfed_pre + new_metrics$cs_otros_shared_fixed_unchanged_pre + new_metrics$cs_other_promoted_doubled + new_metrics$cs_other_promoted_halfed + new_metrics$cs_other_promoted_unchanged + new_metrics$cs_top10shared_doubled_pre + new_metrics$cs_top10shared_halfed_pre + new_metrics$cs_top10shared_unchanged_pre + new_metrics$cs_top10degradado_doubled+ new_metrics$cs_top10degradado_halfed + new_metrics$cs_top10degradado_unchanged + new_metrics$cs_top10borrado
new_metrics$percent_otros_borrados + new_metrics$percent_exp_otros_shared_fixed_pre + new_metrics$percent_con_otros_shared_fixed_pre + new_metrics$percent_exp_other_promoted+ new_metrics$percent_con_other_promoted + new_metrics$percent_exp_top10shared_pre + new_metrics$percent_con_top10shared_pre + new_metrics$percent_exp_top10degradado + new_metrics$percent_con_top10degradado + new_metrics$percent_top10borrado

n_otros_borrados + n_exp_otros_shared_fixed + n_con_otros_shared_fixed + n_exp_other_promoted + n_con_other_promoted + n_exp_top10shared + n_con_top10shared + n_exp_top10degradado + n_con_top10degradado + n_top10borrado
new_metrics$cs_otros_borrados + new_metrics$cs_otros_shared_fixed_exp_pre+ new_metrics$cs_otros_shared_fixed_con_pre + new_metrics$cs_other_promoted_exp + new_metrics$cs_other_promoted_con + new_metrics$cs_top10shared_exp_pre + new_metrics$cs_top10shared_con_pre + new_metrics$cs_top10degradado_exp+ new_metrics$cs_top10degradado_con + new_metrics$cs_top10borrado
new_metrics$percent_otros_borrados + new_metrics$percent_doubled_otros_shared_fixed_pre + new_metrics$percent_halfed_otros_shared_fixed_pre + new_metrics$percent_unchanged_otros_shared_fixed_pre + new_metrics$percent_doubled_other_promoted+ new_metrics$percent_halfed_other_promoted + new_metrics$percent_unchanged_other_promoted + new_metrics$percent_doubled_top10shared_pre + new_metrics$percent_halfed_top10shared_pre + new_metrics$percent_unchanged_top10shared_pre + new_metrics$percent_doubled_top10degradado + new_metrics$percent_halfed_top10degradado + new_metrics$percent_unchanged_top10degradado + new_metrics$percent_top10borrado


n_top10shared + n_non_top10shared + n_top10emerging
n_top10shared + n_top10nuevo
n_otros_shared_fixed + n_other_demoted + n_otros_emergentes
n_otros_shared_post + n_otros_emergentes  
n_post_restante
n_post

cs_other_shared_fixed_post + cs_other_demoted + cs_otros_emergentes + cs_top10shared_post + cs_top10promoted + cs_top10emergente
percent_other_shared_fixed_post + percent_other_demoted + percent_otros_emergentes + percent_top10shared_post + percent_top10promoted + percent_top10emerging 

n_doubled_otros_shared_fixed + n_halfed_otros_shared_fixed + n_unchanged_otros_shared_fixed + n_doubled_other_demoted + n_halfed_other_demoted + n_unchanged_other_demoted + n_otros_emergentes + n_doubled_top10shared + n_halfed_top10shared + n_unchanged_top10shared + n_doubled_top10promoted + n_halfed_top10promoted + n_unchanged_top10promoted + n_top10emerging
new_metrics$cs_otros_shared_fixed_doubled_post + new_metrics$cs_otros_shared_fixed_halfed_post + new_metrics$cs_otros_shared_fixed_unchanged_post + new_metrics$cs_other_demoted_doubled + new_metrics$cs_other_demoted_halfed + new_metrics$cs_other_demoted_unchanged + new_metrics$cs_otros_emergentes + new_metrics$cs_top10shared_doubled_post + new_metrics$cs_top10shared_halfed_post + new_metrics$cs_top10shared_unchanged_post + new_metrics$cs_top10promoted_doubled + new_metrics$cs_top10promoted_halfed + new_metrics$cs_top10promoted_unchanged + new_metrics$cs_top10emergente
new_metrics$percent_doubled_otros_shared_fixed_post + new_metrics$percent_halfed_otros_shared_fixed_post + new_metrics$percent_unchanged_otros_shared_fixed_post + new_metrics$percent_doubled_other_demoted + new_metrics$percent_halfed_other_demoted + new_metrics$percent_unchanged_other_demoted + new_metrics$percent_otros_emergentes + new_metrics$percent_doubled_top10shared_post + new_metrics$percent_halfed_top10shared_post + new_metrics$percent_unchanged_top10shared_post + new_metrics$percent_doubled_top10promoted + new_metrics$percent_halfed_top10promoted + new_metrics$percent_unchanged_top10promoted + new_metrics$percent_top10emerging

n_exp_otros_shared_fixed + n_con_otros_shared_fixed + n_exp_other_demoted + n_con_other_demoted +  n_otros_emergentes + n_exp_top10shared + n_con_top10shared + n_exp_top10promoted + n_con_top10promoted + n_top10emerging
new_metrics$cs_otros_shared_fixed_exp_post + new_metrics$cs_otros_shared_fixed_con_post + new_metrics$cs_other_demoted_exp + new_metrics$cs_other_demoted_con + new_metrics$cs_otros_emergentes + new_metrics$cs_top10shared_exp_post + new_metrics$cs_top10shared_con_post + new_metrics$cs_top10promoted_exp + new_metrics$cs_top10promoted_con + new_metrics$cs_top10emergente
new_metrics$percent_exp_otros_shared_fixed_post + new_metrics$percent_con_otros_shared_fixed_post + new_metrics$percent_exp_other_demoted + new_metrics$percent_con_other_demoted + new_metrics$percent_otros_emergentes + new_metrics$percent_exp_top10shared_post + new_metrics$percent_con_top10shared_post + new_metrics$percent_exp_top10promoted + new_metrics$percent_con_top10promoted + new_metrics$percent_top10emerging

