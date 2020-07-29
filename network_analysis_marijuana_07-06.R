#Idade de primeiro uso de maconha e sintomas de impulsividade
#em usuários de crack que sofreram trauma infantil

library(haven)
banco_vanessa <-  read_sav("C:/Users/TB e Internet/Desktop/Análise de redes/Banco_03_06.sav")
banco <- banco_vanessa
banco <- as.data.frame(banco)

library(stats)
TRAUMA_alcool <- subset(banco, subset = (DROGA_PRINCIPAL == 3),
                   select = c("info_idade1uso_maconha", "atencao",
                              "instabi_cog",
                              "motor",
                              "perseveranca",
                              "auto_control",
                              "cog_complexity", "CTQ_PN",
                              "CTQ_PA",
                              "CTQ_EN",
                              "CTQ_EA",
                              "CTQ_SA"))

TRAUMA_clean <- na.omit(TRAUMA_alcool)
TRAUMA_clean$info_idade1uso_maconha <- scale(TRAUMA_clean$info_idade1uso_maconha)
TRAUMA_clean$instabi_cog <- scale(TRAUMA_clean$instabi_cog)
TRAUMA_clean$motor <- scale(TRAUMA_clean$motor)
TRAUMA_clean$perseveranca <- scale(TRAUMA_clean$perseveranca)
TRAUMA_clean$auto_control <- scale(TRAUMA_clean$auto_control)
TRAUMA_clean$cog_complexity <- scale(TRAUMA_clean$cog_complexity)
TRAUMA_clean$CTQ_PN <- scale(TRAUMA_clean$CTQ_PN)
TRAUMA_clean$CTQ_EN <- scale(TRAUMA_clean$CTQ_EN)
TRAUMA_clean$CTQ_EA <- scale(TRAUMA_clean$CTQ_EA)
TRAUMA_clean$CTQ_SA <- scale(TRAUMA_clean$CTQ_SA)
library(bootnet)
library(NetworkComparisonTest)
library(dplyr)
library(qgraph)
#net_AC13 <- banco_vanessa
#net_AC13 <- read_sav("C:/Users/TB e Internet/Desktop/Análise de redes/vanessa_redes/net_AC13.sav")
library(mgm)
library(lavaan)
library(glmnet)
alcool <- estimateNetwork(TRAUMA_clean, default = "EBICglasso")
plot(alcool)

crack <- estimateNetwork(TRAUMA_clean, default = "EBICglasso")
plot(crack)

crack_alcool <- estimateNetwork(TRAUMA_clean, default = "EBICglasso")
plot(crack_alcool)

library(doParallel)
registerDoParallel(cores=3)
foreach(i=1:3) %dopar% sqrt(i)
library(doParallel)  
no_cores <- detectCores() - 1  
registerDoParallel(cores=no_cores)  
cl <- makeCluster(no_cores)  

#comparando redes 
net_NCT <- NCT(alcool, crack, crack_alcool, it = 2500, test.edges = TRUE, edges = 'all', p.adjust.methods = "fdr", test.centrality = TRUE,
               centrality = c("strength", "betweenness", "closeness"))
stopCluster(cl) #parar o paralelo
net_NCT$glstrinv.real
net_NCT$glstrinv.sep
net_NCT$glstrinv.pval
# Estrutura da rede (diferenca)
net_NCT$glstrinv.real

# Estrutura da rede - valores de cada rede
net_NCT$glstrinv.sep

# Estrutura da rede (diferenca) p-value
net_NCT$glstrinv.pval

net_NCT$diffcen.pval
net_NCT$diffcen.real

pvals_df <- net_NCT$einv.pvals[order(net_NCT$einv.pvals$`p-value`), ]
View(pvals_df)

net_NCT$einv.pvals$`p-value`

library(qgraph)

centralityPlot(list("Crack" = model_net), include = c("Strength", "Betweenness", "Closeness"),
               labels = gen_long_labs, theme_bw = TRUE)

centralityPlot(list("Crack" = crack), include = c("Strength", "Betweenness", "Closeness"),
               theme_bw = TRUE)

centralityPlot(list("Alcool" = alcool), include = c("Strength", "Betweenness", "Closeness"),
               theme_bw = TRUE)
centralityPlot(list("Alcool+Crack" = crack_alcool), include = c("Strength", "Betweenness", "Closeness"),
               theme_bw = TRUE)

centralityPlot(list("Alcool" = alcool, "Crack" = crack, "Alcool + Crack"= crack_alcool), include = c("Strength", "Betweenness", "Closeness"), theme_bw = TRUE)

centralityPlot(crack, include = c("Strength", "Betweenness", "Closeness"))
centralityPlot(crack_alcool, include = c("Strength", "Betweenness", "Closeness"))
centralityPlot(alcool, include = c("Strength", "Betweenness", "Closeness"))

boot_case_crack <- bootnet(crack, nBoots = 1000, type = "case", nCores = 3, statistics = c("Strength", "Betweenness", "Closeness"))
boot_case_crack_alcool <- bootnet(crack_alcool, nBoots = 1000, type = "case", nCores = 3, statistics = c("Strength", "Betweenness", "Closeness"))
boot_case_alcool <- bootnet(alcool, nBoots = 1000, type = "case", nCores = 3, statistics = c("Strength", "Betweenness", "Closeness"))
# Bootstrap charts

plot(boot_case_crack, statistics = c("Strength", "Betweenness", "Closeness"))
plot(boot_case_crack_alcool, statistics = c("Strength", "Betweenness", "Closeness"))
plot(boot_case_alcool, statistics = c("Strength", "Betweenness", "Closeness"))

