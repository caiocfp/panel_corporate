########################################## Setar diretório ##############################
########################################################################################


setwd("C:/Users/caio.falconi/Documents/Corp_ms")


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


#######Ativo Circulante################

ativoCirc <- read.csv("atv_circ.csv",
                       header = T,
                       sep = ";",
                       dec = ".",
                       na.strings = "-",
                       stringsAsFactors = T)

###########Caixa Equivalentes de Caixa##############


caixaEquiv <- read.csv("caixa_eq.csv",
                      header = T,
                      sep = ";",
                      dec = ".",
                      na.strings = "-",
                      stringsAsFactors = T)


#############Capex#####################################

capex <- read.csv("capex.csv",
                     header = T,
                     sep = ";",
                     dec = ".",
                     na.strings = "-",
                     stringsAsFactors = T)

###############Comex#####################

comex_R <- read.csv("comex_Real.csv",
                  header = T,
                  sep = ";",
                  dec = ".",
                  na.strings = "#VALOR!",
                  stringsAsFactors = T)

#############Depreciação##########################

deprec <- read.csv("depr_amor.csv",
                   header = T,
                   sep = ";",
                   dec = ".",
                   na.strings = "-",
                   stringsAsFactors = T)


############Dívida Total###########################

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

##################Ebit#######################################

ebit <- read.csv("ebit.csv",
                       header = T,
                       sep = ";",
                       dec = ".",
                       na.strings = "-",
                       stringsAsFactors = T)

#################imobilizado##############################

imobil <- read.csv("imob.csv",
                   header = T,
                   sep = ";",
                   dec = ".",
                   na.strings = "-",
                   stringsAsFactors = T)

#converte 0 em Na#

 imobil[,5][imobil[,5] == 0] <- NA

#imobilizado com 1 defasagem ----- indica ano t, mas refere-se à observação no t-1#


imobil_1 <- read.csv("imob_1.csv",
                   header = T,
                   sep = ";",
                   dec = ".",
                   na.strings = "-",
                   stringsAsFactors = T)

#converte 0 em Na#


imobil_1[,5][imobil_1[,5] == 0] <- NA


##

#################Investimento em Capital###################

investCap <- read.csv("inv_k.csv",
                   header = T,
                   sep = ";",
                   dec = ".",
                   na.strings = "-",
                   stringsAsFactors = T)


############Investimento por Patrimônio Líquido################


investPatLiq <- read.csv("inv_PatLiq.csv",
                      header = T,
                      sep = ";",
                      dec = ".",
                      na.strings = "-",
                      stringsAsFactors = T)

#############Passivo Circulante###############################

passivCirc <- read.csv("psv_circ.csv",
                         header = T,
                         sep = ";",
                         dec = ".",
                         na.strings = "-",
                         stringsAsFactors = T)

 
#RECEITA E DEFASAGENS#####################################################################

receita <- read.csv("receita.csv",
                       header = T,
                       sep = ";",
                       dec = ".",
                       na.strings = "-",
                       stringsAsFactors = T)


receita_1 <- read.csv("receita_1.csv",
                    header = T,
                    sep = ";",
                    dec = ".",
                    na.strings = "-",
                    stringsAsFactors = T)

receita_2 <- read.csv("receita_2.csv",
                      header = T,
                      sep = ";",
                      dec = ".",
                      na.strings = "-",
                      stringsAsFactors = T)

receita_3 <- read.csv("receita_3.csv",
                      header = T,
                      sep = ";",
                      dec = ".",
                      na.strings = "-",
                      stringsAsFactors = T)

receita <- merge.data.frame(receita, receita_1,
                           by = c(      "FIRMA",
                                      
                                        "ANO",
                                        "SETOR")) 

receita <- merge.data.frame(receita, receita_2,
                            by = c(      "FIRMA",
                                         
                                         "ANO",
                                         "SETOR")) 

receita <- merge.data.frame(receita, receita_3,
                            by = c(      "FIRMA",
                                      
                                         "ANO",
                                         "SETOR")) 
receita <- receita %>%
                   select(COD_FIRMA,
                          FIRMA,
                          SETOR,
                          ANO,
                          RECEITA,
                          RECEITA.1,
                          RECEITA.2,
                          RECEITA.3)


################Valor de Mercado##########################

valorMercado <- read.csv("valor_merc.csv",
                    header = T,
                    sep = ";",
                    dec = ".",
                    na.strings = "#DIV/0!",
                    stringsAsFactors = T)

##############Vendas Setor###################################

vendas_SETOR <- read.csv("vendas_setor_R.csv",
                         header = T,
                         sep = ";",
                         dec = ".",
                         na.strings = "#VALOR!",
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

cash_flowK <- merge(cash_flowK, imobil,
                    by = c("FIRMA",
                           "COD_FIRMA",
                           "ANO",
                           "SETOR")) 

cash_flowK <- within.data.frame(cash_flowK,
                                cash_K <- (EBIT + DEPR_AMOR)/IMOB)


cash_flowK <- cash_flowK %>%
              select(COD_FIRMA,
                     FIRMA,
                     SETOR,
                     ANO,
                     cash_K)



summary(cash_flowK$cash_K)


##############Cash Flow sem negativos################################


cashFlowPs <- data.frame(cash_flowK)

cashFlowPs[,5][cashFlowPs[,5] < 0] <- NA

summary(cashFlowPs)

sum(!is.na(cashFlowPs$cash_K))

##debt_to_assets##
debt_to_assets <- c(dividTotal$DVD_TOT/ativoTotal$ATIVO_TOT)



summary(debt_to_assets)
####


##Import rate##

Imp_R <- merge(vendas_SETOR,comex_R,
                by = c("FIRMA",
                       
                       "ANO",
                       "SETOR")) 

Imp_R <- within.data.frame(Imp_R,
                           Taxa_Imp <- (imp/(VENDAS_SETOR+imp)))

summary(Imp_R$Taxa_Imp)

##Investimento 1##


investRate1 <- merge(investCap,imobil_1,
                     by = c("FIRMA", "ANO", "COD_FIRMA", "SETOR"))                        
    
investRate1 <- within.data.frame(investRate1, taxaInvest <- INV_K/IMOB1)

investRate1 <- investRate1 %>%
         select(COD_FIRMA,
         FIRMA,
         SETOR,
         ANO,
         taxaInvest)

summary(investRate1$taxaInvest)
                 
####                  
               
##Investimento 2##

investRate2 <- merge(capex,deprec,
                     by = c("COD_FIRMA",
                            "FIRMA",
                            "SETOR",
                            "ANO"))




investRate2 <- merge(investRate2,ativoTotal_1,
                     by = c("COD_FIRMA",
                            "FIRMA",
                            "SETOR",
                            "ANO"))



investRate2 <- within.data.frame(investRate2, taxaInvest <- (CAPEX - DEPR_AMOR)/ATV_TOT.1)



investRate2 <- investRate2 %>%
  select(COD_FIRMA,
         FIRMA,
         SETOR,
         ANO,
         taxaInvest)

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

investRate3 <- investRate3 %>%
  select(
    FIRMA,
    SETOR,
    ANO,
    taxaInvest)                             


summary(investRate3$taxaInvest)

##q-Tobyn##

q_tobyn <- merge.data.frame(valorMercado, ativoTotal,
                            by = c("COD_FIRMA",
                                   "FIRMA",
                                   "SETOR",
                                   "ANO"))

q_tobyn <- merge.data.frame(q_tobyn, ativoCirc,
                           by = c("COD_FIRMA",
                                  "FIRMA",
                                  "SETOR",
                                  "ANO"))

q_tobyn <- merge.data.frame(q_tobyn, passivCirc,
                            by = c("COD_FIRMA",
                                   "FIRMA",
                                   "SETOR",
                                   "ANO"))

q_tobyn <- merge.data.frame(q_tobyn, dividTotal,
                            by = c("COD_FIRMA",
                                   "FIRMA",
                                   "SETOR",
                                   "ANO"))

attach(q_tobyn)

mean(VALOR_MERC)
mean()

q_tobyn <- within.data.frame(q_tobyn, 
                             q_tobyn <- c(
                                          as.numeric(
                                            (VALOR_MERC+(PSV_CIRC + DVD_TOT - ATV_CIRC))
                                            /
                                            (ATIVO_TOT)
                                            )
                                          )
                                       )
                                          



q_tobyn <- q_tobyn %>%
  select(
    FIRMA,
    SETOR,
    ANO,
    q_tobyn)      

class(q_tobyn$q_tobyn)

                             
sum(!is.na(q_tobyn$q_tobyn))

mean(q_tobyn$q_tobyn, na.rm = TRUE)

##price-cost-margin##

PCM <- merge.data.frame(ebit, receita,
                            by = c(
                                   "FIRMA",
                                   "SETOR",
                                   "ANO"))


PCM <- within.data.frame(PCM, pcm <- c(EBIT/RECEITA))

PCM <- PCM %>%
  select(
    FIRMA,
    SETOR,
    ANO,
    pcm)    



#Vendas por Capital (imobilizado)###########################################

vendas_K <- merge.data.frame(receita,imobil,
                             by = c(
                                    "FIRMA",
                                    "SETOR",
                                    "ANO"))

vendas_K <- within.data.frame(vendas_K, 
                              vendas_K <- c(
                                RECEITA/IMOB
                              )
                              )


            
  

vendas_K <- within.data.frame(vendas_K, 
                              vendas_K1 <- lag(vendas_K))

vendas_K <- within.data.frame(vendas_K, 
                              vendas_K2 <- lag(vendas_K1))


vendas_K <- within.data.frame(vendas_K, 
                              vendas_K3 <- lag(vendas_K2))

vendas_K <- vendas_K %>%
                      select(FIRMA,
                             SETOR,
                             ANO,
                             vendas_K,
                             vendas_K1,
                             vendas_K2,
                             vendas_K3)  


##################Vendas por Setor-LN################################

vendas_SETOR <- within.data.frame(vendas_SETOR,
                                  lnVendas <- c(log(VENDAS_SETOR_r)))

##data frame Y = InvestRate3############################################
##acelerador de vendas##



investRate3 <- merge.data.frame(investRate3,q_tobyn,
                                by = c(
                                       "FIRMA",
                                       "SETOR",
                                       "ANO"))


investRate3 <- merge(investRate3,vendas_K,
                     by = c(
                            "FIRMA",
                            "SETOR",
                            "ANO"))



investRate3 <- merge(investRate3,cashFlowPs,
                     by = c(
                       "FIRMA",
                       "SETOR",
                       "ANO"))

investRate3 <- merge(investRate3,vendas_SETOR,
                     by = c(
                       "FIRMA",
                       "SETOR",
                       "ANO"))

investRate3 <- merge(investRate3,PCM,
                     by = c(
                       "FIRMA",
                       "SETOR",
                       "ANO"))

investRate3 <- merge(investRate3,Imp_R,
                     by = c(
                       "FIRMA",
                       "SETOR",
                       "ANO"))

investRate3 <- cbind(investRate3,
                     debt_to_assets)


investRate3 <- investRate3 %>%
                   select( - COD_FIRMA)

investRate3 <- investRate3 %>%
  select( - ipca)
                            
investRate3 <- investRate3 %>%
  select( - VENDAS_SETOR)

investRate3 <- investRate3 %>%
  select( - VENDAS_SETOR_r)

investRate3 <- investRate3 %>%
  select( - exp)

investRate3 <- investRate3 %>%
  select( - imp)

investRate3 <- within.data.frame(investRate3,
                                 taxaInvest1 <- lag(taxaInvest))

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
                   q_tobyn,
                   lnVendas,
                   pcm,
                   Taxa_Imp))

################# modelo de painel##################################################
#####################################################################################


#instalar pacote#

install.packages("plm")
library(plm)

##


#setar o painel#


##raw##

attach(investRate3)

Y <- cbind(taxaInvest)

attach(investRate3)

X <- cbind(investRate3$Taxa_Imp,
           investRate3$taxaInvest1,
           investRate3$q_tobyn,
           investRate3$vendas_K,
           investRate3$vendas_K1,
           investRate3$vendas_K2,
           investRate3$vendas_K3,
           investRate3$lnVendas,
           investRate3$pcm)
  


       
           

dadosIR3 <- pdata.frame(investRate3,
                     index = c("FIRMA",
                               "ANO"
                               ))


##

## sem outliers##

attach(i_Rate3_Outless)


Y2 <- cbind(i_Rate3_Outless$taxaInvest)

X2 <- cbind(i_Rate3_Outless$q_tobyn,
            i_Rate3_Outless$vendas_K,
            i_Rate3_Outless$vendas_K1,
            i_Rate3_Outless$vendas_K2,
            i_Rate3_Outless$vendas_K3,
            i_Rate3_Outless$cash_K)

dadosIR3_Out <- pdata.frame(i_Rate3_Outless,
                            index = c("FIRMA",
                                      "ANO"))


##sumário##

summary(Y2)
summary(X2)


sum(!is.na(i_Rate3_Outless$vendas_K))
sum(!is.na(i_Rate3_Outless$taxaInvest))
sum(!is.na(i_Rate3_Outless$q_tobyn))

######################################################################################


 ######################################################################################



#modelo em painel fixo#

##raw##################################################################################

fixo3 <- plm(Y ~ X,
             data = dadosIR3,
             model = "within")


summary(fixo3)

##

##teste de efeito de tempo##

attach(dadosIR3)

fixo3_Temp <- plm(Y ~ X + factor(x = ANO),
                  data = dadosIR3,
                  index = c("FIRMA",
                            'ANO'),
                  model = "within")
summary(fixo3_Temp)              

###heteroscedasticidade##

library(lmtest)

bptest( Y ~ X + factor(x = FIRMA),
        data = dadosIR3,
        studentize = F)

#Estimação da matrix de covariança robusta#

coeftest(fixo3)

coeftest(fixo3, vcovHC)

coeftest(fixo3, 
         vcovHC(fixo3,
                method = "arellano"))
 



###heteroscedasticidade##

library(lmtest)

bptest( Y ~ X + factor(x = FIRMA),
        data = dadosIR3,
        studentize = F)

#Estimação da matrix de covariança robusta#



coeftest(fixo3, 
         vcovHC(fixo3,
                method = "arellano"))


coeftest(fixo3Out, 
         vcovHC(fixo3Out,
                method = "arellano"))

