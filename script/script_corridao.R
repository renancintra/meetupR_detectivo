library(tidyr)
library(dplyr)
library(poLCA)
library(janitor)
library(stringr)
library(purrr)

db <-readRDS('data\\amostra_renan.rds') 


db_polca<-
  db %>% 
  as_tibble() %>% 
  dplyr::select(
    NU_INSCRICAO,
    NU_IDADE,                # idade
    SG_UF_RESIDENCIA,        # Unidade da Federacao
    TP_SEXO,                 #  Sexo
    TP_COR_RACA ,            # cor/raca
    TP_DEPENDENCIA_ADM_ESC,  # Tipo escola: federal, estadual, municipal, privada
    TP_LOCALIZACAO_ESC,      # localizacao escola: urbana, rural
    Q005,                    # qte pessoas morando na residencia
    Q012,                    # ter geladeira na residencia
    Q015,                    # Maquina de secar roupa
    Q020,                    # aparelho de DVD
    NU_NOTA_MT) %>% 
  filter(!is.na(NU_NOTA_MT)) %>% 
  mutate(
    flag_fraude_sim = case_when(NU_NOTA_MT >= quantile(NU_NOTA_MT, 0.99) ~ 1,
                                TRUE ~ 0),
    TP_COR_RACA = TP_COR_RACA + 1
    
  ) %>% 
  mutate_if(is.character,
            funs(as.numeric(as.factor(.)))
  )


formula <- cbind( 
  NU_IDADE,
  SG_UF_RESIDENCIA,
  TP_SEXO,
  TP_COR_RACA,
  TP_DEPENDENCIA_ADM_ESC,
  TP_LOCALIZACAO_ESC,
  Q005,
  Q012,
  Q015,
  Q020
)~1

otimizacao <- function(x){
  set.seed(1234)
  bic <- poLCA(formula, db_polca, nclass = x, verbose = F)$bic 
  k   <- x
  
  return(c(k, bic))
  
}

opt_k_list <- (do.call('rbind', lapply(2:10, otimizacao)))
opt_k <- opt_k_list[which.min(opt_k_list[,2]),1]

set.seed(1234)
lc_model <- poLCA(formula, 
                  db_polca, 
                  nclass = opt_k, 
                  verbose = FALSE,
                  na.rm = FALSE  )

prob_classes <- lc_model$posterior


db_cla_model_outros<-dplyr::as_tibble(data.frame(db_polca,
                                                 classes = prob_classes))

# fraudes conhecidas
fraude_conh <-
  db_cla_model_outros %>% 
  filter(flag_fraude_sim == 1) %>% 
  dplyr::select(NU_INSCRICAO) %>% 
  pull()

probs<-
  db_cla_model_outros %>% 
  dplyr::select(starts_with('class')) %>% 
  data.frame()

row.names(probs) <- db_cla_model_outros$NU_INSCRICAO



rank_deteccao <-function(fraude_enc  = fraude_enc, 
                         db_polca    = db_polca){
  
  set.seed(1311) #semente com o dia do meetup
  eucl_dist <-
    probs %>% 
    dist(. ,method = 'euclidean') %>% 
    as.matrix() %>% 
    as_tibble(rownames = "nu_insc") %>% 
    clean_names() %>% 
    filter( nu_insc == fraude_enc) %>% 
    tidyr::pivot_longer(.,
                        cols = starts_with('x'),
                        names_to = "nu_insc_2",
                        values_to = 'distance') %>% 
    arrange((distance)) %>% 
    mutate( nu_insc_2 = stringr::str_remove(string = nu_insc_2, pattern = 'x'),
            nu_insc_2 = as.numeric(nu_insc_2)) %>% 
    left_join(
      db_polca %>% 
        dplyr::select(NU_INSCRICAO,
                      flag_fraude_sim),
      by = c("nu_insc_2"= "NU_INSCRICAO")
    ) %>% 
    filter( nu_insc_2 != fraude_enc) %>% 
    mutate( ranking = seq(1: nrow(.))) %>% 
    mutate( random = runif(nrow(.), 0,1)) %>% 
    arrange( random) %>% 
    mutate(ranking_random = seq(1: nrow(.)))
  
  
  lista_cla_result_top10<- 
    eucl_dist %>% 
    filter(ranking <= 10 ) %>% 
    summarize( sum(flag_fraude_sim)) %>% 
    pull()
  
  lista_random_result_top10 <- 
    eucl_dist %>%
    filter(ranking_random <= 10 ) %>% 
    summarize( sum(flag_fraude_sim)) %>%
    pull()
  
  lista_cla_result_top20 <- 
    eucl_dist %>% 
    filter(ranking <= 20 ) %>% 
    summarize( sum(flag_fraude_sim)) %>%
    pull()
  
  lista_random_result_top20 <- eucl_dist %>% 
    filter(ranking_random <= 20 ) %>% 
    summarize( sum(flag_fraude_sim)) %>%
    pull()
  
  passo1_cla <- eucl_dist %>%
    filter(flag_fraude_sim == 1 ) %>% 
    summarize( min(ranking)) %>%
    pull()
  
  passo1_random <- eucl_dist %>% 
    filter(flag_fraude_sim == 1 ) %>% 
    summarize( min(ranking_random)) %>% 
    pull()
  
  passo2_cla <- eucl_dist %>% 
    filter(flag_fraude_sim == 1) %>% 
    filter(ranking != min(ranking)) %>% 
    summarize( min(ranking)) %>% 
    pull()
  
  passo2_random <- eucl_dist %>% 
    filter(flag_fraude_sim == 1) %>% 
    filter(ranking_random != min(ranking_random) ) %>%
    summarize( min(ranking_random)) %>%
    pull()
  
  
  return(data.frame(cbind(fraude_enc,
                          lista_cla_result_top10,
                          lista_random_result_top10,
                          lista_cla_result_top20,
                          lista_random_result_top20,
                          passo1_cla,
                          passo1_random,
                          passo2_cla,
                          passo2_random))
  )
}

tabela_resultados <- map_df(.x = fraude_conh, .f = rank_deteccao, db_polca) %>% 
  as_tibble() 

tabela_resultados %>% 
  summarize(
    
    cla_med_top10  = mean(lista_cla_result_top10),
    rand_med_top10 = mean(lista_random_result_top10),
    
    cla_med_top20  = mean(lista_cla_result_top20),
    rand_med_top20 = mean(lista_random_result_top20),
    
   
    cla_acu_top10 = sum(lista_cla_result_top10 > 0),
    rand_acu_top10 = sum(lista_random_result_top10> 0),
    
    
    cla_acu_top20  = sum(lista_cla_result_top20>0),
    rand_acu_top20 = sum(lista_random_result_top20>0),
    
    cla_passo1_q1    = quantile(passo1_cla , 0.25),
    rand_passo1_q1  = quantile(passo1_random, 0.25) ,
    
    cla_passo1_med   = median(passo1_cla),
    rand_passo1_med  = median(passo1_random) ,
    
    cla_passo1_q3   = quantile(passo1_cla , 0.75),
    rand_passo1_q3  = quantile(passo1_random, 0.75) 
    
  ) %>% 
  t() %>%
  as.data.frame() %>% 
  mutate(aux     = row.names(.),
         tipos   = if_else( grepl("cla", aux), "cla", 'random'),
         result  = str_remove(aux, "cla_|rand_")) %>% 
  as_tibble() %>% 
  dplyr::select(-aux) %>% 
  tidyr::pivot_wider(names_from  = tipos,
                     values_from = V1 )
