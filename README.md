# panel_corporate
########################################## Setar diretório ##############################
########################################################################################


setwd("C:/Users/Caio/Desktop/Arquivos Caio/Mestrado/Dissertação/dados/corporativo")


##

########################################vARIÁVEIS "PURAS"#################################
##########################################################################################

ativoTotal <- read.csv("ativ_tot.csv",
                       header = T,
                       sep = ";",
                       dec = ".",
                       na.strings = "-",
                       stringsAsFactors = T)

# Ativo Total no t-1#

ativoTotal_1 <- read.csv("ativ_tot-1.csv",
                       header = T,
                       sep = ";",
                       dec = ".",
                       na.strings = "-",
                       stringsAsFactors = T)
##

ativoCirc <- read.csv("atv_circ.csv",
                       header = T,
                       sep = ";",
                       dec = ".",
                       na.strings = "-",
                       stringsAsFactors = T)

caixaEquiv <- read.csv("caixa_eq.csv",
                      header = T,
                      sep = ";",
                      dec = ".",
                      na.strings = "-",
                      stringsAsFactors = T)

capex <- read.csv("capex.csv",
                     header = T,
                     sep = ";",
                     dec = ".",
                     na.strings = "-",
                     stringsAsFactors = T)

deprec <- read.csv("depr_amor.csv",
                   header = T,
                   sep = ";",
                   dec = ".",
                   na.strings = "-",
                   stringsAsFactors = T)




dividTotal <- read.csv("dvd_tot.csv",
              header = T,
              sep = ";",
              dec = ".",
              na.strings = "-",
              stringsAsFactors = T)


#Todas as variáveis se dão no 4 demonstrativo contábil, aqui, as observações acontecem no 1 demonstrativo#

Total1Daef <-  read.csv("div_tot_1daefa.csv",
                        header = T,
                        sep = ";",
                        dec = ".",
                        na.strings = "-",
                        stringsAsFactors = T)
##
ebit <- read.csv("ebit.csv",
                       header = T,
                       sep = ";",
                       dec = ".",
                       na.strings = "-",
                       stringsAsFactors = T)

imobil <- read.csv("imob.csv",
                   header = T,
                   sep = ";",
                   dec = ".",
                   na.strings = "-",
                   stringsAsFactors = T)

#imobilizado com 1 defasagem ----- indica ano t, mas refere-se à observação no t-1#


imobil_1 <- read.csv("imob_1.csv",
                   header = T,
                   sep = ";",
                   dec = ".",
                   na.strings = "-",
                   stringsAsFactors = T)


#descarta valores nulos e inferiores de imobilizado# 

imobilPos <- data.frame(imobil %>% 
                          if (IMOB > 0) {
                            IMOB = IMOB
                          } else {
                            IMOB = NA   
                          })



names(imobilPos)[names(imobilPos) == "IMOB"] <- "Kpositivo"


##



investCap <- read.csv("inv_k.csv",
                   header = T,
                   sep = ";",
                   dec = ".",
                   na.strings = "-",
                   stringsAsFactors = T)


investPatLiq <- read.csv("inv_PatLiq.csv",
                      header = T,
                      sep = ";",
                      dec = ".",
                      na.strings = "-",
                      stringsAsFactors = T)

passivCirc <- read.csv("psv_circ.csv",
                         header = T,
                         sep = ";",
                         dec = ".",
                         na.strings = "-",
                         stringsAsFactors = T)

 
receita <- read.csv("receita.csv",
                       header = T,
                       sep = ";",
                       dec = ".",
                       na.strings = "-",
                       stringsAsFactors = T)


valorMercado <- read.csv("valor_merc.csv",
                    header = T,
                    sep = ";",
                    dec = ".",
                    na.strings = "#DIV/0!",
                    stringsAsFactors = T)


##################################Construção de variáveis##################################
###########################################################################################

##cash-flow ---- normalizado pelo ativo total e pelo imobilizado(K)##

cash_flow <- c((ebit$EBIT + deprec$DEPR_AMOR)/ativoTotal$ATIVO_TOT)

cash_flowK <- merge(ebit, deprec,
                    by = c("FIRMA",
                           "COD_FIRMA",
                           "ANO",
                           "SETOR")) 

cash_flowK <- merge(cash_flowK, imobilPos,
                    by = c("FIRMA",
                           "COD_FIRMA",
                           "ANO",
                           "SETOR")) 

cash_flowK <- within.data.frame(cash_flowK,
                                cash_K <- (EBIT + DEPR_AMOR)/IMOB)

summary(cash_flowK$cash_K)





##debt_to_assets##

debt_to_assets <- c(dividTotal$DVD_TOT/ativoTotal$ATIVO_TOT)



summary(debt_to_assets)
####

##Investimento 1##


investRate1 <- merge(investCap,imobil_1,
                     by = c("FIRMA", "ANO", "COD_FIRMA", "SETOR"))                        
    
investRate1 <- within.data.frame(investRate1, taxaInvest <- INV_K/IMOB1)

summary(investRate1$taxaInvest)
                 
####                  
               
##Investimento 2##

investRate2 <- c((capex$CAPEX - deprec$DEPR_AMOR)/ativoTotal_1$ATV_TOT.1)

summary(investRate2)

##Taxa investimento 3##

investRate3 <- merge(capex,deprec,
                      by = c("COD_FIRMA",
                              "FIRMA",
                              "SETOR",
                              "ANO"))
                      

investRate3 <- merge(investRate3,imobil_1,
                     by = c("COD_FIRMA",
                            "FIRMA",
                            "SETOR",
                            "ANO"))

investRate3 <- within.data.frame(investRate3, taxaInvest <- (CAPEX - DEPR_AMOR)/IMOB1 )

summary(investRate3$taxaInvest)

##q-Tobyn##

q_tobyn <- c((valorMercado$VALOR_MERC
              + dividTotal$DVD_TOT
              + ativoCirc$ATV_CIRC
              - passivCirc$PSV_CIRC)/
               ativoTotal$ATIVO_TOT)

summary(q_tobyn)

##price-cost-margin##

PCM <- c(ebit$EBIT/receita$RECEITA)



##data frame Y = InvestRate3##
##acelerador de vendas##

investRate3 <- cbind(investRate3, receita$RECEITA)
investRate3 <- cbind(investRate3, imobil$IMOB )
investRate3 <- cbind(investRate3, q_tobyn)

names(investRate3)[names(investRate3) == "receita$RECEITA"] <- "vendas"
names(investRate3)[names(investRate3) == "imobil$IMOB"] <- "K"
names(investRate3)[names(investRate3) == "IMOB1"] <- "K-1"


investRate3 <- merge(investRate3,imobilPos,
                     by = c("COD_FIRMA",
                            "FIRMA",
                            "SETOR",
                            "ANO"))



investRate3 <- within.data.frame(investRate3, vendas_K <- (vendas/Kpositivo))


#defasagem das vendas#

investRate3 <- within.data.frame(investRate3, vendas_K1 <- lag(vendas_K, 1))
investRate3 <- within.data.frame(investRate3, vendas_K2 <- lag(vendas_K, 2))
investRate3 <- within.data.frame(investRate3, vendas_K3 <- lag(vendas_K, 3))

#número de observações#
sum(!is.na(investRate3$vendas_K))
sum(!is.na(investRate3$taxaInvest))
sum(!is.na(investRate3$q_tobyn))

##estatística descritiva##

  summary(investRate3 %>%
            select(vendas_K,
                    taxaInvest,
                   q_tobyn))
  
#####################################outliers#########################################
######################################################################################
  
#Taxa de Investimento#  

 ggplot(investRate3, 
                    aes(y = taxaInvest,
                        x = "Firma")) +
  geom_boxplot(outlier.color = "red")          

 
boxTaxa <- (boxplot(taxaInvest~ANO, 
            data = investRate3)) 

taxaOut <- data.frame(taxaInvest = as.numeric(c(boxTaxa$out)))
  
taxaOut <- merge.data.frame(investRate3, taxaOut,
                            by = "taxaInvest")               

##

#Vendas por capital#

ggplot(investRate3, 
       aes(y = vendas_K,
           x = "Firma")) +
  geom_boxplot(outlier.color = "red")  

boxVendas <- (boxplot(vendas_K~ANO, 
                    data = investRate3)) 

vendasOut <- data.frame(vendas_K = as.numeric(c(boxVendas$out)))

vendasOut <- merge.data.frame(investRate3, vendasOut,
                            by = "vendas_K")   
##

##q_tobyn##


ggplot(investRate3, 
       aes(y = q_tobyn,
           x = "Firma")) +
  geom_boxplot(outlier.color = "red")  

boxTobyn <- (boxplot(q_tobyn~ANO, 
                      data = investRate3)) 

TobynOut <- data.frame(q_tobyn = as.numeric(c(boxTobyn$out)))

TobynOut <- merge.data.frame(investRate3, TobynOut,
                              by = "q_tobyn")

#################Expurgar os outliers criando um painel com InvestRate3#################
########################################################################################

#################Expurgar os outliers criando um painel com InvestRate3#################
########################################################################################

#taxa de investimento#

i_Rate3_Outless <- merge.data.frame(investRate3,
                                    anti_join(investRate3,taxaOut,
                                                by = "taxaInvest"))
##

#vendas por capital#

i_Rate3_Outless <- merge.data.frame(i_Rate3_Outless,
                                    anti_join(i_Rate3_Outless,vendasOut,
                                              by = "vendas_K"))

##

#q_tobyn#


i_Rate3_Outless <- merge.data.frame(i_Rate3_Outless,
                                    anti_join(i_Rate3_Outless,TobynOut,
                                              by = "q_tobyn"))

################# modelo de painel##################################################
#####################################################################################


#instalar pacote#

install.packages("plm")
library(plm)

##


#setar o painel#


attach(i_Rate3_Outless)

Y <- cbind(as.numeric(taxaInvest))
X <- cbind(i_Rate3_Outless$vendas_K,
           i_Rate3_Outless$vendas_K1,
           i_Rate3_Outless$vendas_K2,
           i_Rate3_Outless$vendas_K3)
           


dadosIR3 <- pdata.frame(i_Rate3_Outless,
                     index = c("COD_FIRMA",
                             "ANO"))


##sumário##

summary(Y)
summary(X)


#modelo em painel fixo#

fixo3 <- plm(Y ~ X,
             data = dadosIR3,
             model = "within")


summary(fixo3)























#taxa de investimento#

i_Rate3_Outless <- merge.data.frame(investRate3,
                                    anti_join(investRate3,taxaOut,
                                                by = "taxaInvest"))
##

#vendas por capital#

i_Rate3_Outless <- merge.data.frame(i_Rate3_Outless,
                                    anti_join(i_Rate3_Outless,vendasOut,
                                              by = "vendas_K"))

##

#q_tobyn#


i_Rate3_Outless <- merge.data.frame(i_Rate3_Outless,
                                    anti_join(i_Rate3_Outless,TobynOut,
                                              by = "q_tobyn"))

