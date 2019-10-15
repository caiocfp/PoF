

library(dplyr)
library(NbClust)
library(lmtest)
library(normtest)
library(ordinal)
library(MASS)

###########################################################################

#!diagnostics off
desp12 <- (read.csv("export_pof_tabela_despesa_12.csv",
                           header = T,
                           sep = ",",
                           dec = ".",
                           stringsAsFactors = T))


names(desp12) <- c("tipo_registro",
                   "cod_uf",
                   "n_sequencial",
                   "dv_sequencial",
                   "n_domicilio",
                   "n_uc",
                   "estr_geograf",
                   "f_exp_desenho_amostral",
                   "f_exp_ajust_estimativas",
                   "n_quadro",
                   "cod_item",
                   "forma_aquisicao",
                   "valor_despesa_aquisao",
                   "mes_ultima_despesa",
                   "n_meses",
                   "fator_anualizacao",
                   "deflator_fator",
                   "valor_despesa_defl",
                   "valor_despesa_anualizado_defl",
                   "cod_imputacao",
                   "renda_mone_mensal_uc",
                   "renda_N_mone_mensal_uc",
                   "renda_total_mensal_uc",
                   "cod_local_aquisicao")





###############número de indivíduos###########################

length(unique(desp12$n_sequencial))

unique(desp12$mes_ultima_despesa)

##############agrupar a base por indivíduos###################

Pof   <- desp12 %>%
         group_by(renda_mone_mensal_uc) %>%
         summarise(despesa = sum(as.numeric(valor_despesa_aquisao)))


names(Pof) <- c("renda",
                "despesa")
          
 
summary(Pof$despesa[Pof$despesa > 1000])
length(Pof$despesa[Pof$despesa > 1000])
 
summary(Pof$renda)
summary(Pof$despesa)

length(Pof$renda[Pof$renda > 5000])

length(Pof$despesa[Pof$despesa > 1000])



Pof1 <- Pof %>%
       subset(renda <= 5000)

Pof1  <- Pof1 %>%
        subset(despesa <= 1000)  


summary(Pof1)
##############gráfico###########################################

attach(Pof1)

        plot(Pof1$renda,
             Pof1$despesa)
        
  par(mfrow = c(1,2))      
        
        hist(renda)
        hist(despesa)

summary(renda)                

################cluster-ward#################################
distPof <- dist(Pof1)

HCTR <- hclust(distPof,
               method = "ward.D2")   

plot(HCTR)

HCTR1 <- NbClust(Pof1,
                method = "ward.D2",
                index = "duda")


HCTR1$Best.nc


###############cluster-kMEANS##################################

rendaN_G1 <- scale(Pof1$renda)
despesaN_G1 <- scale(Pof1$despesa)
Pof1_Norm <- cbind(rendaN_G1,
                   despesaN_G1)


CKM  <-  kmeans(Pof1_Norm,4)

##############proporção dos grupos no kmeans####################

length(CKM$cluster[CKM$cluster == "1"])/length(CKM$cluster)
length(CKM$cluster[CKM$cluster == "2"])/length(CKM$cluster)
length(CKM$cluster[CKM$cluster == "3"])/length(CKM$cluster)
length(CKM$cluster[CKM$cluster == "4"])/length(CKM$cluster)



#############grupos-até renda média###########################

Pof1 <- cbind(Pof1,
              CKM$cluster)

names(Pof1) <- c("renda",
                 "despesa",
                 "cluster")


grupoK1.1  <- Pof1 %>%
              subset(cluster == "1")

grupoK1.2  <- Pof1 %>%
              subset(cluster == "2")

grupoK1.2  <- Pof1 %>%
              subset(cluster == "2")

grupoK1.3  <- Pof1 %>%
              subset(cluster == "3")

grupoK1.4  <- Pof1 %>%
              subset(cluster == "4")

par(mfrow = c(2,2))

 plot(grupoK1.1$renda,
      grupoK1.1$despesa,
      title = "grupo1")
          
 plot(grupoK1.2$renda,
      grupoK1.2$despesa,
      grupo = "grupo2")
 
 plot(grupoK1.3$renda,
      grupoK1.3$despesa,
      grupo = "grupo3")
 
 plot(grupoK1.4$renda,
      grupoK1.4$despesa,
      grupo = "grupo4")
 
 
########base por credito###############################
 
#!diagnostics off
           Pof2   <- desp12 %>%
           subset(forma_aquisicao == "5"|
                    forma_aquisicao == "6")
 

 
table(Pof2$cod_item) 

length(desp12$mes_ultima_despesa[desp12$mes_ultima_despesa != 0])

######separação por código##############################


length(Pof2$cod_item[Pof2$renda_mone_mensal_uc <= 2000])

table(Pof2$cod_item[Pof2$renda_mone_mensal_uc <= 2000])


table(Pof2$cod_item[between(Pof2$renda_mone_mensal_uc,
                             2000,
                             4000)])   

table(Pof2$cod_item[between(Pof2$renda_mone_mensal_uc,
                            4000,
                            6000)])

table(Pof2$cod_item[Pof2$renda_mone_mensal_uc >= 6000])
#######por n_quadro##################################### 

 unique(Pof2$cod_item)
 
 
length(Pof2$n_quadro[Pof2$n_quadro == "10"]) 
length(Pof2$n_quadro[Pof2$n_quadro == "11"])
length(Pof2$n_quadro[Pof2$n_quadro == "12"])

table(Pof2$cod_item[Pof2$n_quadro == "11"])


Pof2  <- Pof2 %>% 
         subset(n_quadro == "11")


Pof2  <- Pof2 %>%
         group_by(renda_mone_mensal_uc) %>%
         summarise(despesa = sum(as.numeric(valor_despesa_aquisao)))

#!diagnostics off

names(Pof2) <- c("renda",
                 "despesa")



###########gráficos iniciais####################### 

#!diagnostics off

par(mfrow = c(1,1))
 
attach(Pof2)
plot(renda,
     despesa) 



summary(Pof2)


############outliers##############################

length(renda[renda > 4620])
length(renda[renda > 6000])
length(renda[renda > 10000])

length(despesa[despesa > 1798])
length(despesa[despesa > 5000])
length(despesa[despesa > 10000])

Pof2 <- Pof2 %>%
        subset(despesa <= 10000)

Pof2 <- Pof2 %>%
        subset(renda <= 10000)

summary(Pof2)

plot(Pof2$renda,
     Pof2$despesa)

##########Regressão##############################

#!diagnostics off

Reg <- lm((Pof2$despesa) ~ 
          (Pof2$renda),
          data = Pof2)


summary(Reg)
coeftest(Reg)
ajb.norm.test(Reg$residuals)

plot((Pof2$renda),
     (Pof2$despesa),
     ylab = "renda",
     xlab = "despesa")

abline(Reg,
       col = "red")


##############Logit-mês-aquisicao#####################################

#############pesquisa no 12º mês###################

Pof12m  <- desp12 %>%
          subset(n_meses != 0)

hist(Pof12m$valor_despesa_aquisao)

summary(Pof12m$valor_despesa_aquisao)

plot(Pof12m$valor_despesa_aquisao)


Pof12m  <-   Pof12m %>% subset(valor_despesa_aquisao <= 10000)

summary(Pof12m$valor_despesa_aquisao)

plot(Pof12m$renda_mone_mensal_uc,
      Pof12m$valor_despesa_aquisao,
      xlab = "renda",
      ylab = "despesa")

#####################

tempo <- as.factor(Pof12m$mes_ultima_despesa)

gasto <- Pof12m$valor_despesa_aquisao

renda <- Pof12m$renda_mone_mensal_uc

Log1 <- cbind(tempo,
              gasto)

class(Log1$tempo)

#########Logit1####################################

glm.fit <- clm(tempo ~
                 gasto,
                data = Pof12m)

summary(glm.fit)
coeftest(glm.fit)


#######Logit2####################################


glm.fit2 <- clm(tempo ~
                   gasto +
                   round(renda),
                   data = Pof12m)

glm.fit2a <- polr(tempo ~
                  gasto +
                  round(renda),
                data = Pof12m)




summary(renda)
length(gasto[!NA])


summary(glm.fit2)
coeftest(glm.fit2)

glm.fit2$coefficients


 fit2 <- as.data.frame(glm.fit2a$fitted.values)  


fit2 <- as.data.frame(glm.fit2$model)

summary(fit2)
#######Probabilidades#############################

plot(glm.fit$fitted.values)

fit <- as.data.frame(glm.fit$fitted.values)

summary(fit)

cor(gasto,Pof12m$renda_mone_mensal_uc)

##############Logit-mês-aquisicao#####################################

######crédito-ou-não###############################

credito <- ifelse(Pof12m$forma_aquisicao == "6"|
                  Pof12m$forma_aquisicao == "5",1,0)

length((credito[credito == "1"]))

credito <- as.factor(credito)

glm.fit3 <- glm(credito ~
                    gasto +
                    round(renda),
                  data = Pof12m,
                  family = "binomial")

summary(glm.fit3)


#######cesta de produtos##############################################

Pof1 <- desp12 %>%
        subset(valor_despesa_aquisao != 	999999.99)



############Grupos por quadro########


#############Aluguel, impostos e outras taxas de domicílios################

Pof1_10 <- Pof1 %>%
           subset(n_quadro == 10)


Pof1_10  <-  Pof1_10 %>%
             within.data.frame(Nome <- (ifelse(cod_item == 101, "aluguel do imovel",
                               ifelse(cod_item == 201, "prestacao do alugel",
                               ifelse(cod_item == 301, "aluguel de garagem",
                               ifelse(cod_item == 401, "condominio",
                               ifelse(cod_item == 501, "Iptu",
                               ifelse(cod_item == 601, "adicionais",
                               ifelse(cod_item == 603, "multa do aluguel",
                               ifelse(cod_item == 701, "adicionais de prestacao",
                               ifelse(cod_item == 702, "juros de prestacao",
                               ifelse(cod_item == 801, "adicionais de aluguel de garagem",
                               ifelse(cod_item == 901, "adicionais de condominio",
                               ifelse(cod_item == 1001, "adicionais de iptu",
                               ifelse(cod_item == 1002 , "juros iptu",
                               ifelse(cod_item == 1003, "multa de iptu",
                               ifelse(cod_item == 1101, "iptr",
                               ifelse(cod_item == 1102, "iptr",
                               ifelse(cod_item == 1201, "spu",
                               ifelse(cod_item == 1301, "incra",
                               ifelse(cod_item == 1401, "adicionais do iptr", NA)))))))))))))))))))))
 
                                      
                                                                                                             
#############################

#!diagnostics off

Pof1_11 <- Pof1 %>%
           subset(n_quadro == 11)


Pof1_11  <-  Pof1_11 %>%
  within.data.frame(Nome <- ifelse(cod_item ==	101,	"AZULEJO E PISO",
                                ifelse(cod_item ==	102,	"PISO",
ifelse(cod_item ==	103,	"RODAPE (EXCETO DE MADEIRA)",
ifelse(cod_item ==	104,	"AZULEJO",
ifelse(cod_item ==	105,	"LAJOTA",
ifelse(cod_item ==	106,	"CERAMICA",
ifelse(cod_item ==	201,	"APARELHOS SANITARIOS (PIA, VASO, BIDE, BANHEIRA, ETC.)",
ifelse(cod_item ==	202,	"BIDE",
ifelse(cod_item ==	203,	"CONJUNTO SANITARIO",
ifelse(cod_item ==	204,	"VASO SANITARIO",
ifelse(cod_item ==	205,	"BANHEIRA",
ifelse(cod_item ==	206,	"PECA PARA SUPORTE DE PAPEL HIGIENICO",
ifelse(cod_item ==	207,	"SABONETEIRA (EXCETO DE PLASTICO)",
ifelse(cod_item ==	208,	"PIA",
ifelse(cod_item ==	301,	"JANELA E PORTA",
ifelse(cod_item ==	302,	"PORTA",
ifelse(cod_item ==	304,	"JANELA",
ifelse(cod_item ==	401,	"MADEIRA E TACO",
ifelse(cod_item ==	402,	"TACO",
ifelse(cod_item ==	403,	"CAIBRO",
ifelse(cod_item ==	404,	"RIPA",
ifelse(cod_item ==	405,	"SARRAFO",
ifelse(cod_item ==	406,	"MADEIRA",
ifelse(cod_item ==	407,	"ALIZAR",
ifelse(cod_item ==	501,	"CIMENTO", cod_item))))))))))))))))))))))))))
       


Pof1_11  <-  Pof1_11 %>%
  within.data.frame(Nome <-       
ifelse(cod_item ==	502,	"CIMENTO BRANCO",
ifelse(cod_item ==	503,	"CIMENTCOLA OU CIMENTOCOLA",
ifelse(cod_item ==	601,	"TIJOLO",
ifelse(cod_item ==	602,	"BLOCO DE CERAMICA",
ifelse(cod_item ==	701,	"VIDRO",
ifelse(cod_item ==	801,	"TINTA",
ifelse(cod_item ==	802,	"VERNIZ",
ifelse(cod_item ==	901,	"MATERIAL DE PINTURA EM GERAL",
ifelse(cod_item ==	902,	"MASSA CORRIDA PARA PAREDE",
ifelse(cod_item ==	903,	"CAL",
ifelse(cod_item ==	904,	"CAL VIRGEM",
ifelse(cod_item ==	905,	"ROLO DE PINTURA",
ifelse(cod_item ==	906,	"LIXA DE MADEIRA",
ifelse(cod_item ==	907,	"LIXA DE FERRO",
ifelse(cod_item ==	1001,	"TORNEIRA, CANO E MATERIAL HIDRAULICO EM GERAL",
ifelse(cod_item ==	1002,	"CANO",
ifelse(cod_item ==	1003,	"MATERIAL HIDRAULICO",
ifelse(cod_item ==	1101,	"FERRAGENS (FECHADURA, FERRO, ARAME, PREGO, ETC.)",
ifelse(cod_item ==	1102,	"FECHADURA",
ifelse(cod_item ==	1103,	"FERRO",
ifelse(cod_item ==	1104,	"ARAME",
ifelse(cod_item ==	1105,	"PREGO"	,
ifelse(cod_item ==	1106,	"CADEADO",
ifelse(cod_item ==	1107,	"RALO DE ESGOTAMENTO DE AGUA",
ifelse(cod_item ==	1108,	"TELA DE ARAME",
ifelse(cod_item ==	1109,	"TRILHO METALICO", Nome)))))))))))))))))))))))))))
       
       
       
Pof1_11  <-  Pof1_11 %>%
  within.data.frame(Nome <-         
ifelse(cod_item ==	1110,	"ROLAMENTO PARA PORTA",
ifelse(cod_item ==	1201,	"FIO E MATERIAL ELETRICO EM GERAL	",
ifelse(cod_item ==	1202,	"MATERIAL PARA INSTALACAO ELETRICA",
ifelse(cod_item ==	1203,	"FIO PARA INSTALACAO ELETRICA",
ifelse(cod_item ==	1301,	"MAO-DE-OBRA (PEDREIRO, MARCENEIRO, ELETRICISTA, PINTOR, ETC.)",
ifelse(cod_item ==	1302,	"PEDREIRO"	,
ifelse(cod_item ==	1303,	"MARCENEIRO"	,
ifelse(cod_item ==	1304,	"ELETRICISTA",
ifelse(cod_item ==	1305,	"PINTOR",
ifelse(cod_item ==	1306,	"VIDRACEIRO",
ifelse(cod_item ==	1401,	"DEDETIZACAO, DESRATIZACAO, DESINSETIZACAO, ETC.",
ifelse(cod_item ==	1402,	"DESRATIZACAO",
ifelse(cod_item ==	1403,	"EXTERMINIO DE INSETOS",
ifelse(cod_item ==	1404,	"DESINSETIZACAO",
ifelse(cod_item ==	1501,	"VITRIFICACAO (SINTECO, POLIURETANO, ETC.)",
ifelse(cod_item ==	1502,	"SINTECO"	,
ifelse(cod_item ==	1503,	"POLIURETAN	",
ifelse(cod_item ==	1601,	"REFORMA OU OBRA POR EMPREITADA",
ifelse(cod_item ==	1602,	"CONSTRUCAO POR EMPREITADA",
ifelse(cod_item ==	1603,	"EMPREITEIRO",
ifelse(cod_item ==	1604,	"EMPREITADA",
ifelse(cod_item ==	1605,	"OBRA POR EMPREITADA",
ifelse(cod_item ==	1606,	"TERRAPLANAGEM (EMPREITADA)", Nome))))))))))))))))))))))))
       
       
Pof1_11  <-  Pof1_11 %>%
  within.data.frame(Nome <-          
ifelse(cod_item ==	1607,	"SERVICO DE TERRAPLANAGEM (EMPREITADA)",
ifelse(cod_item ==	1701,	"LICENCA PARA OBRA, PLANTA E DESEMBARACO DE DOCUMENTACAO",
ifelse(cod_item ==	1702,	"PLANTA PARA OBRA",
ifelse(cod_item ==	1703,	"DESEMBARACO DE DOCUMENTACAO PARA OBRA",
ifelse(cod_item ==	1704,	"LICENCA PARA OBRA",
ifelse(cod_item ==	1801,	"BOIA DE CAIXA DE AGUA",
ifelse(cod_item ==	1901,	"ALUGUEL DE CARRETO PARA MATERIAL DE CONSTRUCAO",
ifelse(cod_item ==	1902,	"FRETE DE MATERIAL DE CONSTRUCAO",
ifelse(cod_item ==	1903,	"ALUGUEL DE CACAMBA PARA RETIRAR ENTULHOS",
ifelse(cod_item ==	1904,	"RETIRADA DE ENTULHO",
ifelse(cod_item ==	1905,	"CARRETO DE MATERIAL DE CONSTRUCAO",
ifelse(cod_item ==	1906,	"ALUGUEL DE MATERIAL DE CONSTRUCAO",
ifelse(cod_item ==	2001,	"CAIXA DE CORREIO",
ifelse(cod_item ==	2002,	"CAIXA DE CORRESPONDENCIA",
ifelse(cod_item ==	2003,	"CAIXA DE AR CONDICIONADO",
ifelse(cod_item ==	2101,	"LAJE PRE-MOLDADA",
ifelse(cod_item ==	2201,	"AREIA, TERRA, BARRO, SAIBRO, ETC.",
ifelse(cod_item ==	2202,	"ARENOSO",
ifelse(cod_item ==	2203,	"BARRO"	,
ifelse(cod_item ==	2204,	"SAIBRO",
ifelse(cod_item ==	2205,	"CAULIM",
ifelse(cod_item ==	2206,	"TERRA",
ifelse(cod_item ==	2207,	"ATERRO",
ifelse(cod_item ==	2208,	"CASCALHO", Nome)))))))))))))))))))))))))


Pof1_11  <-  Pof1_11 %>%
  within.data.frame(Nome <- 
ifelse(cod_item ==	2301,	"PEDRAS (BRITADA, MARROADA, PARALELEPIPEDO, ETC.)",
ifelse(cod_item ==	2302,	"PEDRA BRITADA",
ifelse(cod_item ==	2303,	"BRITA",
ifelse(cod_item ==	2304,	"PO DE PEDRA",
ifelse(cod_item ==	2305,	"PEDRA MARROADA",
ifelse(cod_item ==	2306,	"PEDRA DE ALICERCE",
ifelse(cod_item ==	2307,	"ASFALTO"	,
ifelse(cod_item ==	2401,	"PORTAO"	,
ifelse(cod_item ==	2402,	"PORTAO DE FERRO",
ifelse(cod_item ==	2403,	"PORTAO ELETRICO",
ifelse(cod_item ==	2501,	"INSS DE OBRA",
ifelse(cod_item ==	2502,	"INPS DE OBRA",
ifelse(cod_item ==	2601,	"BASCULANTE",
ifelse(cod_item ==	2701,	"COLA (EXCETO DURPOX)",
ifelse(cod_item ==	2702,	"COLA DE MADEIRA",
ifelse(cod_item ==	2703,	"COLA SUPER BOND",
ifelse(cod_item ==	2704,	"SUPER BOND",
ifelse(cod_item ==	2705,	"COLA DE SAPATEIRO",
ifelse(cod_item ==	2801,	"ARGAMASSA",
ifelse(cod_item ==	2802,	"REJUNTE",
ifelse(cod_item ==	2901,	"TELHAS (BARRO, AMIANTO, FIBRA, ALUMINIO, ETC.)",Nome))))))))))))))))))))))
       
       
Pof1_11  <-  Pof1_11 %>%
  within.data.frame(Nome <-        
ifelse(cod_item ==	2902,	"TELHA DE BARRO",
ifelse(cod_item ==	2903,	"TELHA DE AMIANTO"	,
ifelse(cod_item ==	2904,	"TELHA DE FIBRA",
ifelse(cod_item ==	2905,	"TELHA DE ALUMINIO",
ifelse(cod_item ==	3001,	"SOLEIRA E PARAPEITO DE JANELA DE QUALQUER MATERIAL",
ifelse(cod_item ==	3002,	"PARAPEITO DE JANELA DE QUALQUER MATERIAL",
ifelse(cod_item ==	3101,	"PEDRA MARMORE PARA PIA",
ifelse(cod_item ==	3102,	"MARMORE (PEDRA) PARA PIA",
ifelse(cod_item ==	3103,	"BALCAO DE MARMORE PARA PIA",
ifelse(cod_item ==	3104,	"BALCAO DE PIA",
ifelse(cod_item ==	3105,	"BALCAO DE COZINHA",
ifelse(cod_item ==	3106,	"GRANITO (PEDRA) PARA PIA",
ifelse(cod_item ==	3107,	"BALCAO DE GRANITO PARA PIA",
ifelse(cod_item ==	3108,	"GRANITO",
ifelse(cod_item ==	3201,	"CAIXA DE AGUA DE QUALQUER MATERIAL",
ifelse(cod_item ==	3202,	"TAMPA DE CAIXA DE AGUA",
ifelse(cod_item ==	3203,	"CAIXA DE AGUA",
ifelse(cod_item ==	3301,	"GRADE DE FERRO",
ifelse(cod_item ==	3401,	"BLOCO DE CIMENTO",
ifelse(cod_item ==	3402,	"BLOCKET (BLOCO DE CIMENTO)", Nome)))))))))))))))))))))
       
       
Pof1_11  <-  Pof1_11 %>%
  within.data.frame(Nome <-          
ifelse(cod_item ==	3501,	"BOX DE BANHEIRO DE QUALQUER MATERIAL",
ifelse(cod_item ==	3601,	"TANQUE DE QUALQUER MATERIAL",
ifelse(cod_item ==	3701,	"PORTAL",
ifelse(cod_item ==	3801,	"CAIXA DE LUZ (PADRAO)",
ifelse(cod_item ==	3901,	"CAIXA DE DESCARGA",
ifelse(cod_item ==	4001,	"GESSO",
ifelse(cod_item ==	4101,	"ESPELHO DE BANHEIRO",
ifelse(cod_item ==	4201,	"MANILHA",
ifelse(cod_item ==	4301,	"PLACA, ANEL, VIGA, PALANQUE, ETC. DE CIMENTO OU CONCRETO",
ifelse(cod_item ==	4302,	"ANEL DE CIMENTO OU CONCRETO",
ifelse(cod_item ==	4303,	"VIGA DE CIMENTO OU CONCRETO",
ifelse(cod_item ==	4304,	"PALANQUE DE CIMENTO OU CONCRETO",
ifelse(cod_item ==	4305,	"PLACA DE CIMENTO OU CONCRETO",
ifelse(cod_item ==	4401,	"CAIXA DE REGISTRO DE AGUA",
ifelse(cod_item ==	4501,	"MATERIAL PLASTICO PARA COBERTURA",
ifelse(cod_item ==	4502,	"TOLDO PLASTICO",
ifelse(cod_item ==	4503,	"PLASTICO (MATERIAL PARA COBERTURA)",
ifelse(cod_item ==	4504,	"TOLDO DE LONA",
ifelse(cod_item ==	4505,	"LONA"	, Nome))))))))))))))))))))
       
Pof1_11  <-  Pof1_11 %>%
  within.data.frame(Nome <-           
ifelse(cod_item ==	4601,	"DUREPOX"	,
ifelse(cod_item ==	4602,	"EPOX (COLA)",
ifelse(cod_item ==	4701,	"CUBA PARA PIA (QUALQUER MATERIAL)",
ifelse(cod_item ==	4702,	"PIA (CUBA) DE QUALQUER MATERIAL",
ifelse(cod_item ==  4801,	"cALHA",
ifelse(cod_item ==  4901,	"ESQUADRIA",
ifelse(cod_item ==  5001,	"FORMICA (FOLHA)",
ifelse(cod_item ==  5002,	"FOLHA DE FORMICA",
ifelse(cod_item ==  5101,	"POSTE",
ifelse(cod_item ==  5102,	"POSTE DE MADEIRA",
ifelse(cod_item ==  5103,	"POSTE METALICO",
ifelse(cod_item ==  5104,	"POSTE DE CIMENTO",
ifelse(cod_item ==  5201,	"CUPINICIDA", Nome))))))))))))))



Pof1_11  <-  Pof1_11 %>%
  within.data.frame(Nome <-  
ifelse(cod_item == 5202,	"VENENO CONTRA CUPIM (CUPINICIDA)",
ifelse(cod_item == 5301,	"PRODUTOS PARA PISCINA",
ifelse(cod_item == 5302,	"CLORO PARA PISCINA",
ifelse(cod_item == 5303,	"SULFATO DE ALUMINIO PARA PISCINA",
ifelse(cod_item == 5401,	"PAPEL DE PAREDE",
ifelse(cod_item == 5501,	"ESCADA CARACOL",
ifelse(cod_item == 5601,	"IMPERMEABILIZANTE"	,
ifelse(cod_item == 5602,	"MASSA DE VEDACAO DE TELHA",
ifelse(cod_item == 5701,	"PALHA PARA COBERTURA",
ifelse(cod_item == 5702,	"SAPE PARA COBERTURA",
ifelse(cod_item == 5801,	"ALUGUEL DE TERRENO PARA SEPULTURA",
ifelse(cod_item == 5901,	"MASSA DE FIXACAO E VEDACAO DE VIDRO",
ifelse(cod_item == 6001,	"CASA DE MADEIRA USADA",
ifelse(cod_item == 6101,	"BANDEIRA DE JANELA",
ifelse(cod_item == 6201,	"CONCRETO USINADO", Nome))))))))))))))))

Pof1_11  <-  Pof1_11 %>%
  within.data.frame(Nome <- 
ifelse(cod_item == 6301,	"CIPO DE AMARRAR",
ifelse(cod_item == 6401,	"TETO SOLAR",
ifelse(cod_item == 6501,	"ALUGUEL DE MAQUINAS",
ifelse(cod_item == 6601,	"FOSSA SEPTICA	"	,
ifelse(cod_item == 6701,	"FIXOTAC",
ifelse(cod_item == 6901,	"LATA VAZIA (USADA EM CONSTRUCAO)",
ifelse(cod_item == 6902,	"BALDE PARA CONSTRUCAO",
ifelse(cod_item == 7001,	"RUFO DE TELHADO",
ifelse(cod_item == 7101,	"OXIDO DE FERRO (PARA PISO DE CERAMICA)",
ifelse(cod_item == 7201,	"ESTACA PARA CERCA (QUALQUER MATERIAL)",
ifelse(cod_item == 7301,	"LIXEIRA METALICA",
ifelse(cod_item == 7401,	"MASSA PARA REJUNTE",
ifelse(cod_item == 7501,	"CHAPA METALICA",                                                                                                                   
ifelse(cod_item == 7601,	"ACESSORIOS PARA BANHEIRO",
ifelse(cod_item == 7701,	"FORRO PARA COBERTURA",
ifelse(cod_item == 7702,	"FORRO DE MADEIRA",
ifelse(cod_item == 7703,	"FORRO DE GESSO",
ifelse(cod_item == 7704,	"FORRO DE PVC",
ifelse(cod_item == 7705,	"FORRO PARA COBERTURA INDETERMINADO",
ifelse(cod_item == 7801,	"ALUGUEL DE LIXADEIRA ELETRICA",
ifelse(cod_item == 7901,	"MOTOR ELETRICO PARA PORTAO",
ifelse(cod_item == 8001,	"CONSTRUCAO DE JAZIGO",
ifelse(cod_item == 8101,	"REFORMA DE JAZIGO",
ifelse(cod_item == 8201,	"LAPIDE DE JAZIGO",
ifelse(cod_item == 8202,	"LAPIDE",
ifelse(cod_item == 8301,	"GRAVACAO EM JAZIGO",
ifelse(cod_item == 8401,	"GAVETA DE JAZIGO",
ifelse(cod_item == 8501,	"AQUISICAO DE SEPULTURA	"	,
ifelse(cod_item == 8502,	"AQUISICAO DE TERRENO PARA SEPULTURA",
ifelse(cod_item == 8601,	"MADEIRA E TACO DE MADEIRA CERTIFICADA (SELO FSC)",
ifelse(cod_item == 8602,	"TACO DE MADEIRA CERTIFICADA (SELO FSC)",
ifelse(cod_item == 8603,	"CAIBRO DE MADEIRA CERTIFICADA (SELO FSC)",
ifelse(cod_item == 8604,	"RIPA DE MADEIRA CERTIFICADA (SELO FSC)",
ifelse(cod_item == 8605,	"SARRAFO DE MADEIRA CERTIFICADA (SELO FSC)",
ifelse(cod_item == 8606,	"MADEIRA COM CERTIFICADO (SELO FSC)",
ifelse(cod_item == 8701,	"CARPETE",
ifelse(cod_item == 8801,  "COMBOGO CERAMICA VAZADA",
ifelse(cod_item == 8901,  "LAREIRA",
ifelse(cod_item == 99901, "AGREGADO", Nome))))))))))))))))))))))))))))))))))))))))




################grupo 11 - compras com crédito##################                                                                                                                                                                                             

Pof1_11a <- Pof1_11 %>%
            subset(forma_aquisicao == "5"|
                     forma_aquisicao == "6")



Pof1_11a <- Pof1_11a %>% 
            within.data.frame(per <- percent(valor_despesa_aquisao/sum(valor_despesa_aquisao)))


Pof1_11b <- Pof1_11a %>%
            group_by(Nome) %>%
            summarise(total_per = sum(per))


###############grupo 12 - Outros itens do Domicílio principal com serviços público, privados e habitação#####

Pof1_12  <- Pof1 %>%
            subset(n_quadro == 12)

 table((Pof1_12$cod_item))
 
 Pof1_12  <-  Pof1_12 %>%
   within.data.frame(Nome <- 
 ifelse(cod_item == 	101, "AQUISICAO DO IMOVEL A VISTA (VALOR PAGO, IMPOSTO DE TRANSMISSAO, ETC.)",
 ifelse(cod_item == 	102, "IMPOSTO DE TRANSMISSAO DO IMOVEL (AQUISICAO A VISTA)",
 ifelse(cod_item == 	103, "AQUISICAO DO IMOVEL A VISTA (VALOR PAGO)",
 ifelse(cod_item == 	201, "AQUISICAO DO IMOVEL A PRAZO (ENTRADA, PARCELAS, TAXAS, IMPOSTOS, ETC.)",
 ifelse(cod_item == 	202, "TAXAS PARA AQUISICAO DO IMOVEL A PRAZO",
 ifelse(cod_item == 	204, "PARCELAS PARA AQUISICAO DO IMOVEL A PRAZO",
 ifelse(cod_item == 	205, "ENTRADA PARA AQUISICAO DO IMOVEL A PRAZO",
 ifelse(cod_item == 	301, "LOCACAO DO IMOVEL (CONTRATO, DEPOSITOS DE LOCACAO E CONSERVACAO, ETC.)",
 ifelse(cod_item == 	302, "CONTRATO DE LOCACAO DO IMOVEL",
 ifelse(cod_item == 	303, "DEPOSITOS DE LOCACAO DO IMOVEL",
 ifelse(cod_item == 	305, "SEGURO FIADOR",
 ifelse(cod_item == 	401, "MUDANCA", cod_item)))))))))))))
 
 
 
 
 
 Pof1_12  <-  Pof1_12 %>%
   within.data.frame(Nome <- 
 ifelse(cod_item == 	501, "SEGUROS SOBRE O IMOVEL (INCENDIO, ROUBO, ETC.)",
 ifelse(cod_item == 	502, "SEGURO CONTRA ROUBO DO IMOVEL",
 ifelse(cod_item == 	503, "SEGURO CONTRA INCENDIO DO IMOVEL",
 ifelse(cod_item == 	601, "TAXAS (COLETA DE LIXO, PREVENCAO E EXTINCAO DE INCENDIO, SEGURANCA, ETC.)",
 ifelse(cod_item == 	602, "TAXA DE COLETA DE LIXO",
 ifelse(cod_item == 	603, "TAXA DE PREVENCAO E EXTINCAO DE INCENDIO",
 ifelse(cod_item == 	604, "TAXA DE SEGURANCA",
 ifelse(cod_item == 	605, "TAXA DE ILUMINACAO PUBLICA",
 ifelse(cod_item == 	606, "TAXA DE CONSERVACAO URBANA",
 ifelse(cod_item == 	607, "TAXA DE LIXO",
 ifelse(cod_item == 	608, "TAXA DE ASFALTAMENTO",
 ifelse(cod_item == 	609, "TAXA DE INSTALACAO DE ESGOTO",
 ifelse(cod_item == 	610, "TAXA DE TRATAMENTO DE ESGOTO",
 ifelse(cod_item == 	701, "TAXA DE OCUPACAO DO IMOVEL",
 ifelse(cod_item == 	801, "TAXA DE INSTALACAO DE RELOGIO DE AGUA",
 ifelse(cod_item == 	802, "TAXA DE INSTALACAO DE HIDROMETRO", Nome)))))))))))))))))
 
 
 
Pof1_12  <-  Pof1_12 %>%
within.data.frame(Nome <-
ifelse(cod_item == 	804	, "MANUTENCAO DE HIDROMETRO",
ifelse(cod_item == 	901	, "TAXA DE TRANSFERENCIA DE TELEFONE",
ifelse(cod_item == 	1001, "TAXA DE INSTALACAO DE INTERFONE,TELEFONE",
ifelse(cod_item == 	1002, "TAXA DE INSTALACAO DE INTERFONE",
ifelse(cod_item == 	1003, "TAXA DE INSTALACAO DE TELEFONE",
ifelse(cod_item == 	1101, "TAXA DE MANUTENCAO DE ELEVADOR",
ifelse(cod_item == 	1201, "TAXA DE LIGACAO DE LUZ",
ifelse(cod_item == 	1202, "TAXA DE RELIGACAO DE LUZ",
ifelse(cod_item == 	1301, "TAXA DE INSTALACAO DE POSTE DE ENERGIA ELETRICA",
ifelse(cod_item == 	1501, "TAXA DE VERIFICACAO DE VAZAMENTO DE AGUA",
ifelse(cod_item == 	1601, "TAXA DE INSTALACAO DE CENTRAL DE GAS",
ifelse(cod_item == 	1701, "TAXA DE UTILIZACAO E MANUTENCAO DE BOMBA DE AGUA",
ifelse(cod_item == 	2001, "TAXA DE INSTALACAO DE TELEVISAO POR ASSINATURA",
ifelse(cod_item == 	2101, "TAXA DE ADESAO DE TELEVISAO POR ASSINATURA",
ifelse(cod_item == 	2301, "TAXA EXTRA DE CONDOMINIO (REFORMA E MANUTENCAO)	",       
ifelse(cod_item == 	2201, "TAXA DE INSTALACAO DE INTERNET", Nome)))))))))))))))))


Pof1_12  <-  Pof1_12 %>%
within.data.frame(Nome <-
ifelse(cod_item == 	2302	, "	TAXA DE REFORMA DE PREDIO (EXTRA DE CONDOMINIO)	",
ifelse(cod_item == 	2303	, "	TAXA DE MANUTENCAO DE PREDIO (EXTRA DE CONDOMINIO)	",
ifelse(cod_item == 	2304	, "	TAXA EXTRA DE FUNDO DE RESERVA (CONDOMINIO)	",
ifelse(cod_item == 	2401	, "	CERTIFICADO DE CADASTRO DE IMOVEL	",
ifelse(cod_item == 	2501	, "	VIGILANCIA ELETRONICA	",
ifelse(cod_item == 	2601	, "	VALOR DO IMOVEL ADQUIRIDO EM PRIMEIRA LOCACAO A VISTA	",
ifelse(cod_item == 	2701	, "	VALOR DO IMOVEL ADQUIRIDO EM PRIMEIRA LOCACAO A PRAZO	",
ifelse(cod_item == 	2801	, "	VALOR DO IMOVEL ADQUIRIDO USADO A VISTA	",
ifelse(cod_item == 	2901	, "	VALOR DO IMOVEL ADQUIRIDO USADO A PRAZO	",
ifelse(cod_item == 	3001	, "	TAXA DE RELIGACAO DE TELEFONE	",
ifelse(cod_item == 	3101	, "	PARCELAMENTO DE DIVIDA DE CONTA DE AGUA	",
ifelse(cod_item == 	3201	, "	PARCELAMENTO DE DIVIDA DE ENERGIA ELETRICA	",
ifelse(cod_item == 	3301	, "	TAXA DE CAIXA POSTAL DE CORREIO	",
ifelse(cod_item == 	3401	, "	MULTA DE VIOLACAO DE AGUA E ENERGIA ELETRICA	",
ifelse(cod_item == 	3501	, "	SEGURO CONTRA DANOS DE ELETRODOMESTICOS DA COMPANHIA ELETRICA	",
ifelse(cod_item == 	99901	, "	AGREGADO	", Nome)))))))))))))))))
                                                                                                                



##################grupo 13 - aluguel de aparelhos e utilidades de uso de doméstico

unique(Pof1$cod_item[Pof1$n_quadro == 13])                                                                                                                                    


length(Pof1$n_quadro[Pof1$n_quadro == 13])                                                                                                              

Pof1_12  <-  Pof1_12 %>%
within.data.frame(Nome <-
ifelse(cod_item ==  101	, "	AR CONDICIONADO (ALUGUEL)	",
ifelse(cod_item == 	201	, "	TELEFONE RESIDENCIAL (ALUGUEL)	",
ifelse(cod_item == 	301	, "	TELEVISAO (ALUGUEL)	",
ifelse(cod_item == 	501	, "	FREEZER (ALUGUEL)	",
ifelse(cod_item == 	901	, "	SOM (ALUGUEL)	",
ifelse(cod_item == 	1001	, "	MICROCOMPUTADOR (ALUGUEL)	",
ifelse(cod_item == 	1101	, "	FILMADORA (ALUGUEL)	",
ifelse(cod_item == 	1301	, "	VIDEOGAME (ALUGUEL)	",
ifelse(cod_item == 	1401	, "	KARAOKE OU VIDEOKE (ALUGUEL)	",
ifelse(cod_item == 	1601	, "	CORTADOR DE GRAMA (ALUGUEL)	",
ifelse(cod_item == 	1701	, "	DVD (ALUGUEL)	",
ifelse(cod_item == 	1901	, "	BATERIA (ALUGUEL)	",
ifelse(cod_item == 	2001	, "	JOGO DE LUZ (ALUGUEL)	",
ifelse(cod_item == 	2101	, "	FILTRO DE AGUA ELETRICO (ALUGUEL)	",
ifelse(cod_item == 	2201	, "	MAQUINA DE LAVAR PEDRAS (ALUGUEL)	", cod_item))))))))))))))))



####################Pof em 90 dias################################

#!diagnostics off
desp90 <- (read.csv("export_pof_tabela_despesa_90_dias.csv",
                    header = T,
                    sep = ",",
                    dec = ".",
                    stringsAsFactors = T))


names(desp90) <- c("tipo_registro",
                     "cod_uf",
                     "n_sequencial",
                     "dv_sequencial",
                     "n_domicilio",
                     "n_uc",
                     "estr_geograf",
                     "f_exp_desenho_amostral",
                     "f_exp_ajust_estimativas",
                     "n_quadro",
                     "cod_item",
                     "forma_aquisicao",
                     "valor_despesa_aquisao",
                     "fator_anualizacao",
                     "deflator_fator",
                     "valor_despesa_defl",
                     "valor_despesa_anualizado_defl",
                     "cod_imputacao",
                     "renda_mone_mensal_uc",
                     "renda_N_mone_mensal_uc",
                     "renda_total_mensal_uc",
                     "quantidade_item",
                     "cod_unidade_medida",
                     "cod_peso_volume",
                     "qtd_final",
                     "cos_imputacao_qtd",
                     "cod_local_aquisicao")

                                                                                                         

##############QUADROS-DESPESA90######

unique(desp90$n_quadro)
table(desp90$n_quadro)

names(desp90)



#########grupo 6 serviços e taxas do domicílios principais###

Pof2_6  <- desp90 %>%
           subset(n_quadro == 6)


table(Pof2_6$cod_item)

Pof2_6  <-  Pof2_6 %>%
within.data.frame(Nome <-
ifelse(cod_item == 	101	, "AGUA E ESGOTO (M3)",
ifelse(cod_item == 	201	, "ENERGIA ELETRICA (KWH)",
ifelse(cod_item == 	301	, "GAS ENCANADO (M3)",
ifelse(cod_item == 	401	, "TELEFONE RESIDENCIAL",
ifelse(cod_item == 	501	, "ACESSO A INTERNET (PROVEDOR, A CABO, COMUNICACAO VIA SATELITE, ETC.)",
ifelse(cod_item == 	502	, "INTERNET BANDA LARGA",
ifelse(cod_item == 	503	, "PROVEDOR DE ACESSO A INTERNET DISCADA",
ifelse(cod_item == 	504	, "INTERNET SEM FIO",
ifelse(cod_item == 	505	, "INTERNET VIA SATELITE",
ifelse(cod_item == 	506	, "INTERNET VIA RADIO",
ifelse(cod_item == 	701	, "TV POR ASSINATURA (MENSALIDADE/PACOTE)",
ifelse(cod_item == 	801	, "TELEFONE FIXO, CELULAR E INTERNET (PACOTE)",
ifelse(cod_item == 	901	, "TELEFONE FIXO E INTERNET (PACOTE)",
ifelse(cod_item == 	1001, "TELEFONE FIXO E CELULAR (PACOTE)",
ifelse(cod_item == 	1101, "TELEFONE FIXO E CELULAR (PACOTE)",
ifelse(cod_item == 	1201, "TELEFONE VIRTUAL - MENSAGEM (MENSALIDADE, ASSINATURA, ALUGUEL)",
ifelse(cod_item == 	1202, "PAGERS (MENSALIDADE, ASSINATURA, ALUGUEL)",
ifelse(cod_item == 	1203, "MOBI (MENSALIDADE, ASSINATURA, ALUGUEL)",
ifelse(cod_item == 	1204, "BIP (MENSALIDADE, ASSINATURA, ALUGUEL)",
ifelse(cod_item == 	1301, "INTERNET E CELULAR (PACOTE)",
ifelse(cod_item == 	1401, "TELEFONE FIXO, INTERNET E TV POR ASSINATURA (PACOTE)",
ifelse(cod_item == 	1501, "TELEFONE FIXO E TV POR ASSINATURA (PACOTE)",
ifelse(cod_item == 	99901,"AGREGADO	", cod_item)))))))))))))))))))))))) 




                                                                                                              
                                                                                                                                                   
                                                                                                                                                   
                                                                                                                                                   

#########grupo 7 Aquisição de combustíveis domésticos e outros do domicílio principal###

Pof2_7  <- desp90 %>%
  subset(n_quadro == 7)


table(Pof2_7$cod_item)


Pof2_7  <-  Pof2_7 %>%
  within.data.frame(Nome <-
ifelse(cod_item == 	101	,"	GAS DE BOTIJAO (COMBUSTIVEL DOMESTICO)	",
ifelse(cod_item == 	102	,"	GAS DE BOTIJAO (KG) (COMBUSTIVEL DOMESTICO)	",
ifelse(cod_item == 	103	,"	GAS DE BUJAO (COMBUSTIVEL DOMESTICO)	",
ifelse(cod_item == 	104	,"	GAS DE BUJAO (KG) (COMBUSTIVEL DOMESTICO)	",
ifelse(cod_item == 	201	,"	AGUA PARA USO GERAL	",
ifelse(cod_item == 	202	,"	AQUISICAO DE AGUA (USO GERAL) (LITROS)	",
ifelse(cod_item == 	203	,"	PIPA DE AGUA (LITROS)	",
ifelse(cod_item == 	204	,"	GARRAFAO DE AGUA (LITROS)	",
ifelse(cod_item == 	301	,"	RECARGA DE BATERIA (ILUMINACAO DOMICILIAR)	",
ifelse(cod_item == 	401	,"	GAS DE BUJAO PARA ILUMINACAO	",
ifelse(cod_item == 	402	,"	GAS DE BUJAO (KG) (PARA ILUMINACAO)	",
ifelse(cod_item == 	403	,"	GAS DE BOTIJAO (PARA ILUMINACAO)	",
ifelse(cod_item == 	501	,"	QUEROSENE (COMBUSTIVEL DOMESTICO)	",
ifelse(cod_item == 	502	,"	QUEROSENE (LITRO) (COMBUSTIVEL DOMESTICO)	",
ifelse(cod_item == 	601	,"	PO DE SERRA (COMBUSTIVEL DOMESTICO)	",
ifelse(cod_item == 	701	,"	ALCOOL (COMBUSTIVEL DOMESTICO)	",
ifelse(cod_item == 	702	,"	ALCOOL (LITRO) (COMBUSTIVEL DOMESTICO)	",
ifelse(cod_item == 	801	,"	OLEO DIESEL (COMBUSTIVEL DOMESTICO)	",
ifelse(cod_item == 	802	,"	OLEO DIESEL (LITRO) (COMBUSTIVEL DOMESTICO)	",
ifelse(cod_item == 	901	,"	GASOLINA (COMBUSTIVEL DOMESTICO)	",
ifelse(cod_item == 	902	,"	GASOLINA (LITRO) (COMBUSTIVEL DOMESTICO)	",
ifelse(cod_item == 	1001	,"	CARVAO PARA USO DOMESTICO	",
ifelse(cod_item == 	1101	,"	LENHA PARA USO DOMESTICO	", cod_item))))))))))))))))))))))))     


#########grupo 8 Aquisição de combustíveis domésticos e outros do domicílio principal###

Pof2_8  <- desp90 %>%
  subset(n_quadro == 8)
  
  
table(Pof2_8$cod_item)  


Pof2_8  <-  Pof2_8 %>%
within.data.frame(Nome <- 
ifelse(cod_item ==	101,	"AZULEJO E PISO",
ifelse(cod_item ==	102,	"PISO",
ifelse(cod_item ==	103,	"RODAPE (EXCETO DE MADEIRA)",
ifelse(cod_item ==	104,	"AZULEJO",
ifelse(cod_item ==	105,	"LAJOTA",
ifelse(cod_item ==	106,	"CERAMICA",
ifelse(cod_item ==	201,	"APARELHOS SANITARIOS (PIA, VASO, BIDE, BANHEIRA, ETC.)",
ifelse(cod_item ==	202,	"BIDE",
ifelse(cod_item ==	203,	"CONJUNTO SANITARIO",
ifelse(cod_item ==	204,	"VASO SANITARIO",
ifelse(cod_item ==	205,	"BANHEIRA",
ifelse(cod_item ==	206,	"PECA PARA SUPORTE DE PAPEL HIGIENICO",
ifelse(cod_item ==	207,	"SABONETEIRA (EXCETO DE PLASTICO)",
ifelse(cod_item ==	208,	"PIA",
ifelse(cod_item ==	301,	"JANELA E PORTA",
ifelse(cod_item ==	302,	"PORTA",
ifelse(cod_item ==	304,	"JANELA",
ifelse(cod_item ==	401,	"MADEIRA E TACO",
ifelse(cod_item ==	402,	"TACO",
ifelse(cod_item ==	403,	"CAIBRO",
ifelse(cod_item ==	404,	"RIPA",
ifelse(cod_item ==	405,	"SARRAFO",
ifelse(cod_item ==	406,	"MADEIRA",
ifelse(cod_item ==	407,	"ALIZAR",
ifelse(cod_item ==	501,	"CIMENTO", cod_item))))))))))))))))))))))))))



Pof2_8  <-  Pof2_8 %>%
within.data.frame(Nome <-       
ifelse(cod_item ==	502,	"CIMENTO BRANCO",
ifelse(cod_item ==	503,	"CIMENTCOLA OU CIMENTOCOLA",
ifelse(cod_item ==	601,	"TIJOLO",
ifelse(cod_item ==	602,	"BLOCO DE CERAMICA",
ifelse(cod_item ==	701,	"VIDRO",
ifelse(cod_item ==	801,	"TINTA",
ifelse(cod_item ==	802,	"VERNIZ",
ifelse(cod_item ==	901,	"MATERIAL DE PINTURA EM GERAL",
ifelse(cod_item ==	902,	"MASSA CORRIDA PARA PAREDE",
ifelse(cod_item ==	903,	"CAL",
ifelse(cod_item ==	904,	"CAL VIRGEM",
ifelse(cod_item ==	905,	"ROLO DE PINTURA",
ifelse(cod_item ==	906,	"LIXA DE MADEIRA",
ifelse(cod_item ==	907,	"LIXA DE FERRO",
ifelse(cod_item ==	1001,	"TORNEIRA, CANO E MATERIAL HIDRAULICO EM GERAL",
ifelse(cod_item ==	1002,	"CANO",
ifelse(cod_item ==	1003,	"MATERIAL HIDRAULICO",
ifelse(cod_item ==	1101,	"FERRAGENS (FECHADURA, FERRO, ARAME, PREGO, ETC.)",
ifelse(cod_item ==	1102,	"FECHADURA",
ifelse(cod_item ==	1103,	"FERRO",
ifelse(cod_item ==	1104,	"ARAME",
ifelse(cod_item ==	1105,	"PREGO"	,
ifelse(cod_item ==	1107,	"RALO DE ESGOTAMENTO DE AGUA",
ifelse(cod_item ==	1108,	"TELA DE ARAME",
ifelse(cod_item ==	1109,	"TRILHO METALICO",
ifelse(cod_item ==	1106,	"CADEADO",  Nome)))))))))))))))))))))))))))




Pof2_8  <-  Pof2_8 %>%
within.data.frame(Nome <-         
ifelse(cod_item ==	1110,	"ROLAMENTO PARA PORTA",
ifelse(cod_item ==	1201,	"FIO E MATERIAL ELETRICO EM GERAL	",
ifelse(cod_item ==	1202,	"MATERIAL PARA INSTALACAO ELETRICA",
ifelse(cod_item ==	1203,	"FIO PARA INSTALACAO ELETRICA",
ifelse(cod_item ==	1301,	"MAO-DE-OBRA (PEDREIRO, MARCENEIRO, ELETRICISTA, PINTOR, ETC.)",
ifelse(cod_item ==	1302,	"PEDREIRO",
ifelse(cod_item ==	1303,	"MARCENEIRO",
ifelse(cod_item ==	1304,	"ELETRICISTA",
ifelse(cod_item ==	1305,	"PINTOR",
ifelse(cod_item ==	1306,	"VIDRACEIRO",
ifelse(cod_item ==	1401,	"DEDETIZACAO, DESRATIZACAO, DESINSETIZACAO, ETC.",
ifelse(cod_item ==	1402,	"DESRATIZACAO",
ifelse(cod_item ==	1403,	"EXTERMINIO DE INSETOS",
ifelse(cod_item ==	1404,	"DESINSETIZACAO",
ifelse(cod_item ==	1501,	"VITRIFICACAO (SINTECO, POLIURETANO, ETC.)",
ifelse(cod_item ==	1502,	"SINTECO",
ifelse(cod_item ==	1503,	"POLIURETAN",
ifelse(cod_item ==	1601,	"REFORMA OU OBRA POR EMPREITADA",
ifelse(cod_item ==	1602,	"CONSTRUCAO POR EMPREITADA",
ifelse(cod_item ==	1603,	"EMPREITEIRO",
ifelse(cod_item ==	1604,	"EMPREITADA",
ifelse(cod_item ==	1605,	"OBRA POR EMPREITADA",
ifelse(cod_item ==	1606,	"TERRAPLANAGEM (EMPREITADA)", Nome))))))))))))))))))))))))                                                                                                                                                    

       

Pof2_8  <-  Pof2_8 %>%
within.data.frame(Nome <-          
ifelse(cod_item ==	1607,	"SERVICO DE TERRAPLANAGEM (EMPREITADA)",
ifelse(cod_item ==	1701,	"LICENCA PARA OBRA, PLANTA E DESEMBARACO DE DOCUMENTACAO",
ifelse(cod_item ==	1702,	"PLANTA PARA OBRA",
ifelse(cod_item ==	1703,	"DESEMBARACO DE DOCUMENTACAO PARA OBRA",
ifelse(cod_item ==	1704,	"LICENCA PARA OBRA",
ifelse(cod_item ==	1801,	"BOIA DE CAIXA DE AGUA",
ifelse(cod_item ==	1901,	"ALUGUEL DE CARRETO PARA MATERIAL DE CONSTRUCAO",
ifelse(cod_item ==	1902,	"FRETE DE MATERIAL DE CONSTRUCAO",
ifelse(cod_item ==	1903,	"ALUGUEL DE CACAMBA PARA RETIRAR ENTULHOS",
ifelse(cod_item ==	1904,	"RETIRADA DE ENTULHO",
ifelse(cod_item ==	1905,	"CARRETO DE MATERIAL DE CONSTRUCAO",
ifelse(cod_item ==	1906,	"ALUGUEL DE MATERIAL DE CONSTRUCAO",
ifelse(cod_item ==	2001,	"CAIXA DE CORREIO",
ifelse(cod_item ==	2002,	"CAIXA DE CORRESPONDENCIA",
ifelse(cod_item ==	2003,	"CAIXA DE AR CONDICIONADO",
ifelse(cod_item ==	2101,	"LAJE PRE-MOLDADA",
ifelse(cod_item ==	2201,	"AREIA, TERRA, BARRO, SAIBRO, ETC.",
ifelse(cod_item ==	2202,	"ARENOSO",
ifelse(cod_item ==	2203,	"BARRO"	,
ifelse(cod_item ==	2204,	"SAIBRO",
ifelse(cod_item ==	2205,	"CAULIM",
ifelse(cod_item ==	2206,	"TERRA",
ifelse(cod_item ==	2207,	"ATERRO",
ifelse(cod_item ==	2208,	"CASCALHO", Nome)))))))))))))))))))))))))








Pof2_8  <-  Pof2_8 %>%
within.data.frame(Nome <- 
ifelse(cod_item ==	2301,	"PEDRAS (BRITADA, MARROADA, PARALELEPIPEDO, ETC.)",
ifelse(cod_item ==	2302,	"PEDRA BRITADA",
ifelse(cod_item ==	2303,	"BRITA",
ifelse(cod_item ==	2304,	"PO DE PEDRA",
ifelse(cod_item ==	2305,	"PEDRA MARROADA",
ifelse(cod_item ==	2306,	"PEDRA DE ALICERCE",
ifelse(cod_item ==	2307,	"ASFALTO"	,
ifelse(cod_item ==	2401,	"PORTAO"	,
ifelse(cod_item ==	2402,	"PORTAO DE FERRO",
ifelse(cod_item ==	2403,	"PORTAO ELETRICO",
ifelse(cod_item ==	2501,	"INSS DE OBRA",
ifelse(cod_item ==	2502,	"INPS DE OBRA",
ifelse(cod_item ==	2601,	"BASCULANTE",
ifelse(cod_item ==	2701,	"COLA (EXCETO DURPOX)",
ifelse(cod_item ==	2702,	"COLA DE MADEIRA",
ifelse(cod_item ==	2703,	"COLA SUPER BOND",
ifelse(cod_item ==	2901,	"TELHAS (BARRO, AMIANTO, FIBRA, ALUMINIO, ETC.)", Nome))))))))))))))))))



Pof1_11  <-  Pof1_11 %>%
within.data.frame(Nome <-        
ifelse(cod_item ==	2902,	"TELHA DE BARRO",
ifelse(cod_item ==	2903,	"TELHA DE AMIANTO"	,
ifelse(cod_item ==	2904,	"TELHA DE FIBRA",
ifelse(cod_item ==	2905,	"TELHA DE ALUMINIO",
ifelse(cod_item ==	3001,	"SOLEIRA E PARAPEITO DE JANELA DE QUALQUER MATERIAL",
ifelse(cod_item ==	3002,	"PARAPEITO DE JANELA DE QUALQUER MATERIAL",
ifelse(cod_item ==	3101,	"PEDRA MARMORE PARA PIA",
ifelse(cod_item ==	3102,	"MARMORE (PEDRA) PARA PIA",
ifelse(cod_item ==	3103,	"BALCAO DE MARMORE PARA PIA",
ifelse(cod_item ==	3104,	"BALCAO DE PIA",
ifelse(cod_item ==	3105,	"BALCAO DE COZINHA",
ifelse(cod_item ==	3106,	"GRANITO (PEDRA) PARA PIA",
ifelse(cod_item ==	3107,	"BALCAO DE GRANITO PARA PIA",
ifelse(cod_item ==	3108,	"GRANITO",
ifelse(cod_item ==	3109,	"CAIXA DE DESCARGA",       
ifelse(cod_item ==	3201,	"CAIXA DE AGUA DE QUALQUER MATERIAL",
ifelse(cod_item ==	3202,	"TAMPA DE CAIXA DE AGUA",
ifelse(cod_item ==	3203,	"CAIXA DE AGUA",
ifelse(cod_item ==	3301,	"GRADE DE FERRO",
ifelse(cod_item ==	3401,	"BLOCO DE CIMENTO",
ifelse(cod_item ==	3402,	"BLOCKET (BLOCO DE CIMENTO)", Nome))))))))))))))))))))))                                                                                                                                      


Pof1_11  <-  Pof1_11 %>%
within.data.frame(Nome <-          
ifelse(cod_item ==	3501,	"BOX DE BANHEIRO DE QUALQUER MATERIAL",
ifelse(cod_item ==	3601,	"TANQUE DE QUALQUER MATERIAL",
ifelse(cod_item ==	3701,	"PORTAL",
ifelse(cod_item ==	3801,	"CAIXA DE LUZ (PADRAO)",
ifelse(cod_item ==	3901,	"CAIXA DE DESCARGA",
ifelse(cod_item ==	4001,	"GESSO",
ifelse(cod_item ==	4101,	"ESPELHO DE BANHEIRO",
ifelse(cod_item ==	4201,	"MANILHA",
ifelse(cod_item ==	4301,	"PLACA, ANEL, VIGA, PALANQUE, ETC. DE CIMENTO OU CONCRETO",
ifelse(cod_item ==	4302,	"ANEL DE CIMENTO OU CONCRETO",
ifelse(cod_item ==	4303,	"VIGA DE CIMENTO OU CONCRETO",
ifelse(cod_item ==	4304,	"PALANQUE DE CIMENTO OU CONCRETO",
ifelse(cod_item ==	4305,	"PLACA DE CIMENTO OU CONCRETO",
ifelse(cod_item ==	4401,	"CAIXA DE REGISTRO DE AGUA",
ifelse(cod_item ==	4501,	"MATERIAL PLASTICO PARA COBERTURA",
ifelse(cod_item ==	4502,	"TOLDO PLASTICO",
ifelse(cod_item ==	4503,	"PLASTICO (MATERIAL PARA COBERTURA)",
ifelse(cod_item ==	4504,	"TOLDO DE LONA",
ifelse(cod_item ==	4505,	"LONA"	, Nome))))))))))))))))))))



Pof2_8  <-  Pof2_8 %>%
within.data.frame(Nome <-           
ifelse(cod_item ==	4601,	"DUREPOX"	,
ifelse(cod_item ==	4602,	"EPOX (COLA)",
ifelse(cod_item ==	4701,	"CUBA PARA PIA (QUALQUER MATERIAL)",
ifelse(cod_item ==	4702,	"PIA (CUBA) DE QUALQUER MATERIAL",
ifelse(cod_item ==  4801,	"cALHA",
ifelse(cod_item ==  4901,	"ESQUADRIA",
ifelse(cod_item ==  5001,	"FORMICA (FOLHA)",
ifelse(cod_item ==  5002,	"FOLHA DE FORMICA",
ifelse(cod_item ==  5101,	"POSTE",
ifelse(cod_item ==  5102,	"POSTE DE MADEIRA",
ifelse(cod_item ==  5103,	"POSTE METALICO",
ifelse(cod_item ==  5104,	"POSTE DE CIMENTO",
ifelse(cod_item ==  5201,	"CUPINICIDA", Nome))))))))))))))



Pof2_8  <-  Pof2_8 %>%
within.data.frame(Nome <- 
ifelse(cod_item ==  5101,	"POSTE",                                                                                   
ifelse(cod_item ==  5102,	"POSTE DE MADEIRA",                                                                                           
ifelse(cod_item ==  5103, "POSTE METALICO",                                                                                                
ifelse(cod_item ==  5104,	"POSTE DE CIMENTO",                                                                                                          
ifelse(cod_item ==  5201,	"CUPINICIDA", Nome))))))






Pof2_8  <-  Pof2_8 %>%
within.data.frame(Nome <-  
ifelse(cod_item == 5202,	"VENENO CONTRA CUPIM (CUPINICIDA)",
ifelse(cod_item == 5301,	"PRODUTOS PARA PISCINA",
ifelse(cod_item == 5302,	"CLORO PARA PISCINA",
ifelse(cod_item == 5303,	"SULFATO DE ALUMINIO PARA PISCINA",
ifelse(cod_item == 5401,	"PAPEL DE PAREDE",
ifelse(cod_item == 5501,	"ESCADA CARACOL",
ifelse(cod_item == 5601,	"IMPERMEABILIZANTE"	,
ifelse(cod_item == 5602,	"MASSA DE VEDACAO DE TELHA",
ifelse(cod_item == 5701,	"PALHA PARA COBERTURA",
ifelse(cod_item == 5702,	"SAPE PARA COBERTURA",
ifelse(cod_item == 5801,	"ALUGUEL DE TERRENO PARA SEPULTURA",
ifelse(cod_item == 5901,	"MASSA DE FIXACAO E VEDACAO DE VIDRO",
ifelse(cod_item == 6001,	"CASA DE MADEIRA USADA",
ifelse(cod_item == 6101,	"BANDEIRA DE JANELA",
ifelse(cod_item == 6201,	"CONCRETO USINADO", Nome))))))))))))))))                                                                              

       
       
       
       
Pof2_8  <-  Pof2_8 %>%
within.data.frame(Nome <- 
ifelse(cod_item == 6301,	"CIPO DE AMARRAR",
ifelse(cod_item == 6401,	"TETO SOLAR",
ifelse(cod_item == 6501,	"ALUGUEL DE MAQUINAS",
ifelse(cod_item == 6601,	"FOSSA SEPTICA	"	,
ifelse(cod_item == 6701,	"FIXOTAC",
ifelse(cod_item == 6901,	"LATA VAZIA (USADA EM CONSTRUCAO)",
ifelse(cod_item == 6902,	"BALDE PARA CONSTRUCAO",
ifelse(cod_item == 7001,	"RUFO DE TELHADO",
ifelse(cod_item == 7101,	"OXIDO DE FERRO (PARA PISO DE CERAMICA)",
ifelse(cod_item == 7201,	"ESTACA PARA CERCA (QUALQUER MATERIAL)",
ifelse(cod_item == 7301,	"LIXEIRA METALICA",
ifelse(cod_item == 7401,	"MASSA PARA REJUNTE",
ifelse(cod_item == 7501,	"CHAPA METALICA",                                                                                                                   
ifelse(cod_item == 7601,	"ACESSORIOS PARA BANHEIRO",
ifelse(cod_item == 7701,	"FORRO PARA COBERTURA",
ifelse(cod_item == 7702,	"FORRO DE MADEIRA",
ifelse(cod_item == 7703,	"FORRO DE GESSO",
ifelse(cod_item == 7704,	"FORRO DE PVC",
ifelse(cod_item == 7705,	"FORRO PARA COBERTURA INDETERMINADO",
ifelse(cod_item == 7801,	"ALUGUEL DE LIXADEIRA ELETRICA",
ifelse(cod_item == 7901,	"MOTOR ELETRICO PARA PORTAO",
ifelse(cod_item == 8001,	"CONSTRUCAO DE JAZIGO",
ifelse(cod_item == 8101,	"REFORMA DE JAZIGO",
ifelse(cod_item == 8201,	"LAPIDE DE JAZIGO",
ifelse(cod_item == 8202,	"LAPIDE",
ifelse(cod_item == 8301,	"GRAVACAO EM JAZIGO",
ifelse(cod_item == 8401,	"GAVETA DE JAZIGO",
ifelse(cod_item == 8501,	"AQUISICAO DE SEPULTURA	"	,
ifelse(cod_item == 8502,	"AQUISICAO DE TERRENO PARA SEPULTURA",
ifelse(cod_item == 8601,	"MADEIRA E TACO DE MADEIRA CERTIFICADA (SELO FSC)",
ifelse(cod_item == 8602,	"TACO DE MADEIRA CERTIFICADA (SELO FSC)",
ifelse(cod_item == 8603,	"CAIBRO DE MADEIRA CERTIFICADA (SELO FSC)",
ifelse(cod_item == 8604,	"RIPA DE MADEIRA CERTIFICADA (SELO FSC)",
ifelse(cod_item == 8605,	"SARRAFO DE MADEIRA CERTIFICADA (SELO FSC)",
ifelse(cod_item == 8606,	"MADEIRA COM CERTIFICADO (SELO FSC)",
ifelse(cod_item == 8701,	"CARPETE",
ifelse(cod_item == 8801,  "COMBOGO CERAMICA VAZADA",
ifelse(cod_item == 8901,  "LAREIRA",
ifelse(cod_item == 99901, "AGREGADO", Nome))))))))))))))))))))))))))))))))))))))))


#########Grupo 9 Consertos e manutenção de móveis, aparelhos, máquinas e utensílios de uso domésticos

Pof2_9  <- desp90 %>%
  subset(n_quadro == 9)


table(Pof2_9$cod_item)



Pof2_9  <-  Pof2_9 %>%
within.data.frame(Nome <- 
ifelse(cod_item == 	101	,"	CONSERTO DE MOVEIS (PECA+MAO-DE-OBRA)	",
ifelse(cod_item == 	102	,"	REFORMA DE MOVEIS (PECA+MAO-DE-OBRA)	",
ifelse(cod_item == 	201	,"	CONSERTO DE MOVEIS (PECA)	",
ifelse(cod_item == 	202	,"	REFORMA DE MOVEIS (PECA)	",
ifelse(cod_item == 	301	,"	CONSERTO DE MOVEIS (MAO-DE-OBRA)	",
ifelse(cod_item == 	302	,"	REFORMA DE MOVEIS (MAO-DE-OBRA)	",
ifelse(cod_item == 	401	,"	MANUTENCAO DE MOVEIS	",
ifelse(cod_item == 	501	,"	CONSERTO DE ASPIRADOR DE PO (PECA+MAO-DE-OBRA)	",
ifelse(cod_item == 	601	,"	CONSERTO DE ASPIRADOR DE PO (PECA)	",
ifelse(cod_item == 	701	,"	CONSERTO DE ASPIRADOR DE PO (MAO-DE-OBRA)	",
ifelse(cod_item == 	801	,"	MANUTENCAO DE APARELHOS DOMESTICOS	",
ifelse(cod_item == 	901	,"	CONSERTO DE GRILL (PECA+MAO-DE-OBRA)	",
ifelse(cod_item == 	1001,"	CONSERTO DE GRILL (PECA)	",
ifelse(cod_item == 	1101,"	CONSERTO DE GRILL (MAO-DE-OBRA)	", cod_item)))))))))))))))

Pof2_9  <-  Pof2_9 %>%
within.data.frame(Nome <-
ifelse(cod_item == 	1201	,"	MANUTENCAO DE UTENSILIOS DOMESTICOS NAO-ELETRICOS	",
ifelse(cod_item == 	1301	,"	MANUTENCAO DE UTENSILIOS DOMESTICOS ELETRICOS	",
ifelse(cod_item == 	1401	,"	CONSERTO DE FERRO ELETRICO (PECA+MAO-DE-OBRA)	",
ifelse(cod_item == 	1501	,"	CONSERTO DE FERRO ELETRICO (PECA)	",
ifelse(cod_item == 	1601	,"	CONSERTO DE FERRO ELETRICO (MAO-DE-OBRA)	",
ifelse(cod_item == 	1701	,"	CONSERTO DE ANTENA PARABOLICA (PECA+MAO-DE-OBRA)	",
ifelse(cod_item == 	1801	,"	CONSERTO DE ANTENA PARABOLICA (PECA)	",
ifelse(cod_item == 	1901	,"	CONSERTO DE ANTENA PARABOLICA (MAO-DE-OBRA)	",
ifelse(cod_item == 	2001	,"	CONSERTO DE TELEFONE (PECA+MAO-DE-OBRA)	",
ifelse(cod_item == 	2101	,"	CONSERTO DE TELEFONE (PECA)	",
ifelse(cod_item == 	2201	,"	CONSERTO DE TELEFONE (MAO-DE-OBRA)	",
ifelse(cod_item == 	2301	,"	CONSERTO DE PERSIANA (PECA+MAO-DE-OBRA)	",
ifelse(cod_item == 	2401	,"	CONSERTO DE PERSIANA (PECA)	", Nome))))))))))))))



Pof2_9  <-  Pof2_9 %>%
within.data.frame(Nome <-
ifelse(cod_item == 	2501	,"	CONSERTO DE PERSIANA (MAO-DE-OBRA)	",
ifelse(cod_item == 	2601	,"	CONSERTO DE RECEPTOR DE ANTENA PARABOLICA (PECA+MAO-DE-OBRA)	",
ifelse(cod_item == 	2701	,"	CONSERTO DE RECEPTOR DE ANTENA PARABOLICA (PECA)	",
ifelse(cod_item == 	2801	,"	CONSERTO DE RECEPTOR DE ANTENA PARABOLICA (MAO-DE-OBRA)	",
ifelse(cod_item == 	2901	,"	MONTAGEM DE MOVEIS	",
ifelse(cod_item == 	3001	,"	CONSERTO DE GELADEIRA (PECA+MAO-DE-OBRA)	",
ifelse(cod_item == 	3101	,"	CONSERTO DE GELADEIRA (PECA)	",
ifelse(cod_item == 	3201	,"	CONSERTO DE GELADEIRA (MAO-DE-OBRA)	",
ifelse(cod_item == 	3301	,"	CONSERTO DE FREEZER (PECA+MAO DE OBRA)	",
ifelse(cod_item == 	3401	,"	CONSERTO DE FREEZER (PECA)	",
ifelse(cod_item == 	3501	,"	CONSERTO DE FREEZER (MAO DE OBRA)	",
ifelse(cod_item == 	3601	,"	CONSERTO DE TELEVISAO (PECA+MAO-DE-OBRA)	",
ifelse(cod_item == 	3701	,"	CONSERTO DE TELEVISAO (PECA)	", Nome))))))))))))))
                                                                                           


Pof2_9  <-  Pof2_9 %>%
within.data.frame(Nome <-
ifelse(cod_item == 	3801	,"	CONSERTO DE TELEVISAO (MAO-DE-OBRA)	",
ifelse(cod_item == 	4001	,"	CONSERTO DE TELAO (PECA)	",
ifelse(cod_item == 	4101	,"	CONSERTO DE TELAO (MAO-DE-OBRA)	",
ifelse(cod_item == 	4201	,"	CONSERTO DE APARELHO SONORO (PECA+MAO-DE-OBRA)	",
ifelse(cod_item == 	4301	,"	CONSERTO DE APARELHO SONORO (PECA)	",
ifelse(cod_item == 	4401	,"	CONSERTO DE APARELHO SONORO (MAO-DE-OBRA)	",
ifelse(cod_item == 	4501	,"	CONSERTO DE RADIO (PECA+MAO-DE-OBRA)	",
ifelse(cod_item == 	4601	,"	CONSERTO DE RADIO (PECA)	",
ifelse(cod_item == 	4701	,"	CONSERTO DE RADIO (MAO+DE+OBRA)	",
ifelse(cod_item == 	4801	,"	CONSERTO DE RADIO-RELOGIO (PECA+MAO-DE-OBRA)	",
ifelse(cod_item == 	4901	,"	CONSERTO DE RADIO-RELOGIO (PECA)	",
ifelse(cod_item == 	5001	,"	CONSERTO DE RADIO-RELOGIO (MAO-DEOBRA)	",
ifelse(cod_item == 	5101	,"	CONSERTO DE VIDEOCASSETE (PECA+MAO-DE-OBRA)	",
ifelse(cod_item == 	5201	,"	CONSERTO DE VIDEOCASSETE (PECA)	",
ifelse(cod_item == 	5301	,"	CONSERTO DE VIDEOCASSETE (MAO-DE-OBRA)	", Nome))))))))))))))))




Pof2_9  <-  Pof2_9 %>%
within.data.frame(Nome <-
ifelse(cod_item == 	5401	,"	CONSERTO DE MAQUINA DE LAVAR ROUPA (PECA+MAO-DE-OBRA)	",
ifelse(cod_item == 	5501	,"	CONSERTO DE MAQUINA DE LAVAR ROUPA (PECA)	",
ifelse(cod_item == 	5601	,"	CONSERTO DE MAQUINA DE LAVAR ROUPA (MAO-DE-OBRA)	",
ifelse(cod_item == 	5701	,"	CONSERTO DE MAQUINA DE SECAR ROUPA (PECA+MAO-DE-OBRA)	",
ifelse(cod_item == 	5801	,"	CONSERTO DE MAQUINA DE SECAR ROUPA (PECA)	",
ifelse(cod_item == 	5901	,"	CONSERTO DE MAQUINA DE SECAR ROUPA (MAO-DE-OBRA)	",
ifelse(cod_item == 	6001	,"	CONSERTO DE FOGAO (PECA+MAO-DE-OBRA)	",
ifelse(cod_item == 	6101	,"	CONSERTO DE FOGAO (PECA)	",
ifelse(cod_item == 	6201	,"	CONSERTO DE FOGAO (MAO-DE-OBRA)	",
ifelse(cod_item == 	6301	,"	CONSERTO DE AR CONDICIONADO (PECA+MAO-DE-OBRA)	",
ifelse(cod_item == 	6401	,"	CONSERTO DE AR CONDICIONADO (PECA)	",
ifelse(cod_item == 	6501	,"	CONSERTO DE AR CONDICIONADO (MAO-DE-OBRA)	",
ifelse(cod_item == 	6601	,"	CONSERTO DE FURADEIRA ELETRICA (PECA+MAO-DE-OBRA)	",
ifelse(cod_item == 	6701	,"	CONSERTO DE FURADEIRA ELETRICA (PECA)	",
ifelse(cod_item == 	6801	,"	CONSERTO DE FURADEIRA ELETRICA (MAO-DE-OBRA)	",
ifelse(cod_item == 	6901	,"	CONSERTO DE ORGAO ELETRONICO (PECA+MAO-DE-OBRA)	",
ifelse(cod_item == 	7001	,"	CONSERTO DE ORGAO ELETRONICO (PECA)	",
ifelse(cod_item == 	7101	,"	CONSERTO DE ORGAO ELETRONICO (MAO-DE-OBRA)	",
ifelse(cod_item == 	7201	,"	CONSERTO DE SECRETARIA ELETRONICA (PECA+MAO-DE-OBRA)	",
ifelse(cod_item == 	7301	,"	CONSERTO DE SECRETARIA ELETRONICA (PECA)	",
ifelse(cod_item == 	7401	,"	CONSERTO DE SECRETARIA ELETRONICA (MAO-DE-OBRA)	", 
ifelse(cod_item == 	7501	,"	CONSERTO DE MAQUINA DE COSTURA (PECA+MAO-DE-OBRA)	",
ifelse(cod_item == 	7601	,"	CONSERTO DE MAQUINA DE COSTURA (PECA)	", Nome))))))))))))))))))))))))  

Pof2_9  <-  Pof2_9 %>%
within.data.frame(Nome <-
ifelse(cod_item == 	7701	,"	CONSERTO DE MAQUINA DE COSTURA (MAO-DE-OBRA)	",
ifelse(cod_item == 	7801	,"	CONSERTO DE MAQUINA DE OVERLOCK (PECA+MAO-DE-OBRA)	",
ifelse(cod_item == 	7901	,"	CONSERTO DE MAQUINA DE OVERLOCK (PECA)	",
ifelse(cod_item == 	8001	,"	CONSERTO DE MAQUINA DE OVERLOCK (MAO-DE-OBRA)	",
ifelse(cod_item == 	8101	,"	CONSERTO DE BATEDEIRA DE BOLO (PECA+MAO-DE-OBRA)	",
ifelse(cod_item == 	8201	,"	CONSERTO DE BATEDEIRA DE BOLO (PECA)	",
ifelse(cod_item == 	8301	,"	CONSERTO DE BATEDEIRA DE BOLO (MAO-DE-OBRA)	",
ifelse(cod_item == 	8401	,"	CONSERTO DE LIQUIDIFICADOR (PECA+MAO-DE-OBRA)	",
ifelse(cod_item == 	8501	,"	CONSERTO DE LIQUIDIFICADOR (PECA)	",
ifelse(cod_item == 	8601	,"	CONSERTO DE LIQUIDIFICADOR (MAO-DE-OBRA)	",
ifelse(cod_item == 	8701	,"	CONSERTO DE MULTIPROCESSADOR (PECA+MAO-DE-OBRA)	",
ifelse(cod_item == 	8801	,"	CONSERTO DE MULTIPROCESSADOR (PECA)	",
ifelse(cod_item == 	8901	,"	CONSERTO DE MULTIPROCESSADOR (MAO-DE-OBRA)	",
ifelse(cod_item == 	9001	,"	CONSERTO DE FORNO ELETRICO (PECA+MAO-DE-OBRA)	",
ifelse(cod_item == 	9101	,"	CONSERTO DE FORNO ELETRICO (PECA)	",
ifelse(cod_item == 	9201	,"	CONSERTO DE FORNO ELETRICO (MAO-DE-OBRA)	",
ifelse(cod_item == 	9301	,"	CONSERTO DE MAQUINA DE LAVAR LOUCAS (PECA+MAO-DE-OBRA)	",
ifelse(cod_item == 	9401	,"	CONSERTO DE MAQUINA DE LAVAR LOUCAS (PECA)	",
ifelse(cod_item == 	9501	,"	CONSERTO DE MAQUINA DE LAVAR LOUCAS (MAO-DE-OBRA)	",
ifelse(cod_item == 	9601	,"	CONSERTO DE RELOGIO DE MESA (PECA+MAO-DE-OBRA)	",
ifelse(cod_item == 	9701	,"	CONSERTO DE RELOGIO DE MESA (PECA)	",
ifelse(cod_item == 	9801	,"	CONSERTO DE RELOGIO DE MESA (MAO-DE-OBRA)	", Nome)))))))))))))))))))))))


############Despesa 90 - por Crédito#########################


Pof2_6a  <- Pof2_6 %>%
            subset(forma_aquisicao == "5"|
            forma_aquisicao == "6")


Pof2_7a  <- Pof2_7 %>%
  subset(forma_aquisicao == "5"|
           forma_aquisicao == "6")

Renda2_7a <- Pof2_7a$renda_mone_mensal_uc
Despesa2_7a  <- Pof2_7a$valor_despesa_aquisao  
Nome2_7a      <- Pof2_7a$Nome


Pof2_7aB <- cbind(
                  Renda2_7a,
                  Despesa2_7a,
                  Nome2_7a)



Pof2_8a  <- Pof2_8 %>%
  subset(forma_aquisicao == "5"|
           forma_aquisicao == "6")

Renda2_8a <- Pof2_8a$renda_mone_mensal_uc
Despesa2_8a  <- Pof2_8a$valor_despesa_aquisao  
Nome2_8a      <- Pof2_8a$Nome


Pof2_8aB <- cbind(
  Renda2_8a,
  Despesa2_8a,
  Nome2_8a)


Pof2_9a  <- Pof2_9 %>%
  subset(forma_aquisicao == "5"|
           forma_aquisicao == "6")

Renda2_9a <- Pof2_9a$renda_mone_mensal_uc
Despesa2_9a  <- Pof2_9a$valor_despesa_aquisao  
Nome2_9a      <- Pof2_9a$Nome


Pof2_9aB <- cbind(
  Renda2_9a,
  Despesa2_9a,
  Nome2_9a)


rm(Despesa2_7a)


########Import 90-crédito


desp90_C <- read.csv("pof_tabela_90_credito.csv",
                     header = T,
                     dec = ".",
                     sep = ";",
                     stringsAsFactors = T)

desp90_C <- desp90_C %>%
            subset(DESPESA != 	999999.99)


###################GRÁFICOS_90_CREDITO################

plot(desp90_C$RENDA,
     desp90_C$DESPESA,
     xlab = "renda",
     ylab = "despesa")

prod <- desp90_C %>%
        group_by(PRODUTO) %>%
        summarise(TOTAL = sum(DESPESA)) %>%
        within.data.frame(part <- (percent(TOTAL/sum(TOTAL))))
                           
