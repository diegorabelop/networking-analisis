library(haven)
banco_vanessa <- read_sav("C:/Users/TB e Internet/Desktop/Análise de redes/banco_vanessa.sav")

banco <- na.omit(banco_vanessa)
# Load packages ----
library(bootnet)
library(NetworkComparisonTest)
library(dplyr)
library(qgraph)

# Splitting datasets ----
net <- scale(dt)

net0_index <- dt$Gender_at_birth == 0

net0 <- net[net0_index, ]
net0 <- net0[, -ncol(net0)]
dim(net0)

net1 <- net[!net0_index, ]
net1 <- net1[, -ncol(net1)]
dim(net1)

# Estimating networks ----
model_net <- estimateNetwork(net, default = c("EBICglasso"))
model_net0 <- estimateNetwork(net0, default = c("EBICglasso"))
model_net1 <- estimateNetwork(net1, default = c("EBICglasso"))


nodes_labs <- c("Mood", "Guilt", "Ins E", "Ins M", "Ins M", "Wk Ac", "Ret", "Agi",
                "An Pch", "An Som", "Som GI", "G Som", "Genit", "Hyp", "Ls W", "Cons")

long_labs <- c("Depressed Mood", "Feelings Of Guilt", "Insomnia: Early In The Night", "Insomnia: Middle Of The Night",
               "Insomnia: Early Hours Of The Morning", "Work And Activities", "Retardation", "Agitation", "Anxiety Psychic",
               "Anxiety Somatic", "Somatic Symptoms Gastro-intestinal", "General Somatic Symptoms", "Genital Symptoms",
               "Hypochondriasis", "Loss Of Weight", "Consciousness")

gen_nodes_labs <- c("Mood", "Suic", "Guilt", "Ins E", "Ins M", "Ins M", "Wk Ac", "Ret", "Agi",
                    "An Pch", "An Som", "Som GI", "G Som", "Genit", "Hyp", "Ls W", "Cons", "SId")

gen_long_labs <- c("Depressed Mood", "Suicide", "Feelings Of Guilt", "Insomnia: Early In The Night", "Insomnia: Middle Of The Night",
                   "Insomnia: Early Hours Of The Morning", "Work And Activities", "Retardation", "Agitation", "Anxiety Psychic",
                   "Anxiety Somatic", "Somatic Symptoms Gastro-intestinal", "General Somatic Symptoms", "Genital Symptoms",
                   "Hypochondriasis", "Loss Of Weight", "Consciousness", "Suicide Ideation")

plot(model_net)
plot(model_net0)
plot(model_net1)

plot(model_net, labels = gen_nodes_labs)
plot(model_net0, labels = nodes_labs)
plot(model_net1, labels = nodes_labs)

model_net0$labels <- nodes_labs
model_net1$labels <- nodes_labs

model_net0$nNode
length(nodes_labs)

Layout <- averageLayout(model_net0, model_net1)
layout(t(1:2))
plot(model_net0, layout = Layout)
plot(model_net1, layout = Layout)


# Comparing networks ----
net_NCT <- NCT(model_net0, model_net1, it = 2500, test.edges = TRUE, edges = 'all', p.adjust.methods = "fdr", test.centrality = TRUE,
               centrality = c("strength", "betweenness", "closeness"))

net_NCT$glstrinv.real
net_NCT$glstrinv.sep
net_NCT$glstrinv.pval

net_NCT$diffcen.pval
net_NCT$diffcen.real

pvals_df <- net_NCT$einv.pvals[order(net_NCT$einv.pvals$`p-value`), ]
View(pvals_df)

net_NCT$einv.pvals$`p-value`

library(qgraph)

centralityPlot(list("Ideation General Network" = model_net), include = c("Strength", "Betweenness", "Closeness"),
               labels = gen_long_labs, theme_bw = TRUE)


centralityPlot(list("SI-" = model_net0, "SI+" = model_net1), include = c("Strength", "Betweenness", "Closeness"),
               labels = long_labs, theme_bw = TRUE)

centralityPlot(model_net0, include = c("Strength", "Betweenness", "Closeness"))
centralityPlot(model_net1, include = c("Strength", "Betweenness", "Closeness"))

boot_case <- bootnet(model_net, nBoots = 1000, type = "case", nCores = 2, statistics = c("Strength", "Betweenness", "Closeness"))
boot0_case <- bootnet(model_net0, nBoots = 1000, type = "case", nCores = 2, statistics = c("Strength", "Betweenness", "Closeness"))
boot1_case <- bootnet(model_net1, nBoots = 1000, type = "case", nCores = 2, statistics = c("Strength", "Betweenness", "Closeness"))

boot <- bootnet(model_net, nBoots = 1000, nCores = 2)
boot0 <- bootnet(model_net0, nBoots = 1000, nCores = 2)
boot1 <- bootnet(model_net1, nBoots = 1000, nCores = 2)

# Bootstrap charts
plot(boot1)
plot(boot2)

plot(boot_case, statistics = c("Strength", "Betweenness", "Closeness"))
plot(boot0_case, statistics = c("Strength", "Betweenness", "Closeness"))
plot(boot1_case, statistics = c("Strength", "Betweenness", "Closeness"))
