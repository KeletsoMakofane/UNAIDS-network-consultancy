#remotes::install_github("KeletsoMakofane/networkepi", auth_token = "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", upgrade = "never")

library(networkepi)
library(tidygraph)
library(igraph)
library(tidyverse) 
library(lubridate)
library(foreign)


############################ FUNCTIONS ########################

`%ni%` <- negate(`%in%`)


############################ Data ########################
table_households <- read_csv("/Users/keletsomakofane/Documents/_data/AHRI/HSE-Households All.Release 2019-09/RD06-99 ACDIS HSE-H All/RD06-99 ACDIS HSE-H All.csv")


############################## File Directories ###########################################################
#root.data.directory is inherited from the environment calling this script
root.data.directory <- "/Users/keletsomakofane/Documents/_data/"
data.input.dir          <-  paste(root.data.directory, "AHRI/generated_files/", sep="")
ahri.latest.data        <-  paste(root.data.directory, "AHRI/ahri1/", sep = "")

data.conjugal.raw         <- read.dta(paste(ahri.latest.data, "RD01-09 ACDIS Conjugal Relationships.dta", sep=""))
data.hhhead.raw           <- read.dta(paste(ahri.latest.data, "RD01-10 ACDIS Household Head Relationships.dta", sep=""))
data.individual.raw       <- read_csv("/Users/keletsomakofane/Documents/_data/AHRI/ahri1/_individualsdownload/RD01-01 ACDIS Individuals/RD01-01 ACDIS Individuals.csv")

data.episodes.raw         <- read.dta("/Users/keletsomakofane/Documents/_data/AHRI/ahri1/_surveillanceepisodessociodemdownload/SurveillanceEpisodesSocioDem.dta")

#data.individual.extended  <- readRDS(paste(data.input.dir, "data_individual_extended.R", sep=""))



############################### Clean DATA ##################################################

data.conjugal <- data.conjugal.raw
data.hhhead <- data.hhhead.raw

data.indivdual.household <- data.episodes.raw %>%
  dplyr::rename(IIntId = IndividualId, HHIntId = HouseholdId) %>%
  mutate(year = year(EndDate)) %>%
  dplyr::select(IIntId, HHIntId, year) %>%
  group_by(IIntId, year) %>%
  summarize(HHIntId = last(HHIntId)) %>%
  arrange(year) %>%
  mutate(year = paste0("y", year)) %>%
  pivot_wider(names_from = year, values_from = HHIntId) %>%
  rename(name = IIntId) %>%
  mutate(name = as.character(name))

data.household.ses.indexes.pre <- data.episodes.raw %>%
  dplyr::rename(IIntId = IndividualId, HHIntId = HouseholdId) %>%
  mutate(year = year(EndDate)) %>%
  arrange(year) %>%
  dplyr::select(HHIntId, year, contains("Idx")) %>%
  group_by(HHIntId, year) %>%
  summarize(
    SEIdx = mean(SEIdx, na.rm=TRUE),
    DwellingIdx = mean(DwellingIdx, na.rm=TRUE),
    LifeStockIdx = mean(LifeStockIdx, na.rm=TRUE),
    PowerSupplyIdx = mean(PowerSupplyIdx, na.rm=TRUE),
    WaterSanitationIdx = mean(WaterSanitationIdx, na.rm=TRUE),
    ModerntAssetIdx = mean(ModerntAssetIdx, na.rm=TRUE),
    PerceivedStatusIdx = mean(PerceivedStatusIdx, na.rm=TRUE)
            ) 

data.household.se.index <- data.household.ses.indexes.pre %>%
  select(HHIntId, year, SEIdx) %>%
  mutate(year = paste0("SEIdx", year)) %>%
  pivot_wider(names_from = year, values_from = SEIdx)

data.household.dwelling.index <- data.household.ses.indexes.pre %>%
  select(HHIntId, year, DwellingIdx) %>%
  mutate(year = paste0("DwellingIdx", year)) %>%
  pivot_wider(names_from = year, values_from = DwellingIdx)

data.household.lifestock.index <- data.household.ses.indexes.pre %>%
  select(HHIntId, year, LifeStockIdx) %>%
  mutate(year = paste0("LifeStockIdx", year)) %>%
  pivot_wider(names_from = year, values_from = LifeStockIdx)

data.household.powersup.index <- data.household.ses.indexes.pre %>%
  select(HHIntId, year, PowerSupplyIdx) %>%
  mutate(year = paste0("PowerSupplyIdx", year)) %>%
  pivot_wider(names_from = year, values_from = PowerSupplyIdx)

data.household.watersan.index <- data.household.ses.indexes.pre %>%
  select(HHIntId, year, WaterSanitationIdx) %>%
  mutate(year = paste0("WaterSanitationIdx", year)) %>%
  pivot_wider(names_from = year, values_from = WaterSanitationIdx)

data.household.assets.index <- data.household.ses.indexes.pre %>%
  select(HHIntId, year, ModerntAssetIdx) %>%
  mutate(year = paste0("ModerntAssetIdx", year)) %>%
  pivot_wider(names_from = year, values_from = ModerntAssetIdx)

data.household.status.index <- data.household.ses.indexes.pre %>%
  select(HHIntId, year, PerceivedStatusIdx) %>%
  mutate(year = paste0("PerceivedStatusIdx", year)) %>%
  pivot_wider(names_from = year, values_from = PerceivedStatusIdx)

data.household.indices <- data.household.se.index %>%
  left_join(data.household.dwelling.index) %>%
  left_join(data.household.lifestock.index) %>%
  left_join(data.household.powersup.index) %>%
  left_join(data.household.watersan.index) %>%
  left_join(data.household.assets.index) %>%
  left_join(data.household.status.index) 


data.individual.hiv.pre        <- read_csv("/Users/keletsomakofane/Documents/_data/AHRI/hiv_surveillance_2020/RD05-99 ACDIS HIV All.csv")  %>%
  mutate(year =  year(VisitDate)) %>%
  arrange(VisitDate) %>%
  group_by(IIntId, year) %>%
  summarize(
    EverTestedHIV = last(EverTestedHIV),
    CD4Count = last(CD4Count),
    KnowsHIVStatus = last(KnowsHIVStatus)
  ) %>%
  mutate(EverTestedHIV = ifelse(EverTestedHIV == "00002", FALSE, ifelse(EverTestedHIV == "00001", TRUE, NA))) %>%
  mutate(KnowsHIVStatus = ifelse(KnowsHIVStatus == "00002", FALSE, ifelse(KnowsHIVStatus == "00001", TRUE, NA)))

data.individual.evertested <- data.individual.hiv.pre %>%
  select(IIntId, year, EverTestedHIV) %>%
  mutate(year = paste0("EverTestedHIV", year)) %>%
  pivot_wider(names_from = year, values_from = EverTestedHIV)

data.individual.knowshivstatus <- data.individual.hiv.pre %>%
  select(IIntId, year, KnowsHIVStatus) %>%
  mutate(year = paste0("KnowsHIVStatus", year)) %>%
  pivot_wider(names_from = year, values_from = KnowsHIVStatus)

data.individual.cd4 <- data.individual.hiv.pre %>%
  select(IIntId, year, CD4Count) %>%
  mutate(year = paste0("CD4Count", year)) %>%
  pivot_wider(names_from = year, values_from = CD4Count)

data.individual.hiv <- data.individual.evertested %>%
  left_join(data.individual.knowshivstatus) %>%
  left_join(data.individual.cd4)





data.individual <- data.episodes.raw %>%
  dplyr::rename(IIntId = IndividualId, HHIntId = HouseholdId) %>%
  arrange(IIntId, EndDate) %>%
  group_by(IIntId) %>%
  summarise(
    Sex = last(Sex), 
    DoB = last(DoB),
    MotherIntId = last(MotherId),
    FatherIntId = last(FatherID),
    SpouseId = last(SpouseId)) 
  

######################## CREATE INDIVIDUAL NETWORK #########################################

individual.mother.edges   <- data.individual %>% 
  dplyr::select(IIntId, MotherIntId) %>% 
  unique() %>% 
  mutate(type = "mother") %>%
  rename(from = IIntId, to = MotherIntId) %>%
  filter(complete.cases(.))

individual.father.edges   <- data.individual %>% 
  dplyr::select(IIntId, FatherIntId) %>% 
  unique() %>%  
  mutate(type = "father") %>%
  rename(from = IIntId, to = FatherIntId) %>%
  filter(complete.cases(.))

individual.spouse.edges   <- data.individual %>% 
  dplyr::select(IIntId, SpouseId) %>% 
  unique() %>%  
  mutate(type = "spouse") %>%
  rename(from = IIntId, to = SpouseId) %>%
  filter(complete.cases(.))

edge.info <- individual.mother.edges %>%
  rbind(individual.father.edges) %>%
  rbind(individual.spouse.edges) %>%
  mutate(to = as.character(to), from = as.character(from)) %>%
  data.frame(stringsAsFactors = FALSE)

vertex.info.pre <- data.individual %>%
  mutate(name = as.character(IIntId)) %>%
  select(name, Sex, DoB)


######## Extract Kin Co-residence Edges from HHHead data using bipartite network
edges.hhhead <- data.hhhead %>%
  filter((HHHRelationType %ni% c("Domestic Worker", "Tenant", "refused", "query", "default", "other", "don't know", "missing", "Unknown"))) %>%
  dplyr::select(IIntId, HHIntId) %>%
  mutate_all(as.character) %>%
  mutate(HHIntId = paste("house", HHIntId, sep=""))

graph.hhhead <- tbl_graph(edges = edges.hhhead,  directed = FALSE) 
V(graph.hhhead)$type <- (V(graph.hhhead)$name %in% edges.hhhead$IIntId)  # Is it a person or househole?

graph.hhhead.proj <- bipartite_projection(graph.hhhead, which=TRUE) %>%
  as_tbl_graph # project into person network



### Add Node IDs that are in edges data but not vertex info data to the vertex info data
all.vertices            <- c(edge.info$from, edge.info$to, vertex.info.pre$name) %>% as.character() %>% unique()

vertex.info <- data.frame(name = all.vertices, stringsAsFactors = FALSE) %>%
  left_join(vertex.info.pre, by="name") %>%
  data.frame(stringsAsFactors = FALSE)



######## Create kinship network objects 
kin.net.1.save <- tbl_graph(nodes = vertex.info, edges = edge.info,  directed = FALSE) %>%
  simplify() %>% 
  as_tbl_graph()

kin.net.2.save <- connect(kin.net.1.save, 2) %>% as_tbl_graph()

######## Create combination kinship+co-residence network objects. 

combo.net.2.save <- igraph::union(graph.hhhead.proj, kin.net.2.save) %>%
  simplify() %>%
  as_tbl_graph() %>%
  activate(edges) %>%
  select(from, to, type) %>%
  mutate(type = ifelse(is.na(type), "cores", type)) %>%
  activate(nodes)

working_individual_network <- combo.net.2.save %>%
  tidygraph::select(
    name, 
    Sex,
    DoB
    ) %>%
  left_join(data.indivdual.household) %>%
  left_join(data.individual.hiv)

saveRDS(working_individual_network, "working_individual_network.R")




######################## CREATE HOUSEHOLD NETWORK #########################################

household_graph_2018 <- working_individual_network %>%
  filter(!is.na(y2018)) %>%
  collapse_graph("y2018") 

saveRDS(household_graph_2018, "working_household_network_2018.R")


household_graph_2017 <- working_individual_network %>%
  filter(!is.na(y2017)) %>%
  collapse_graph("y2017") 

saveRDS(household_graph_2017, "working_household_network_2017.R")




analysis_data   <- read_csv("/Users/keletsomakofane/Documents/_data/AHRI/ahri1/_memberstatusobservationsdownload/RD01-11 ACDIS Member Status Observations/RD01-11 ACDIS Member Status Observations.csv") %>%
  mutate(year = year(VisitDate)) %>%
  filter(year > 2011) %>%
  #select(IIntId, VisitDate, HHIntId, year) %>%
  right_join(table_hiv, by=c("IIntId", "year")) %>%
  arrange(IIntId, VisitDate) %>%
  ungroup() %>%
  left_join(household_wealth, by = c("HHIntId", "year")) %>%
  mutate(HIVResult = as.numeric(HIVResult), KnowsHIVStatus = as.numeric(KnowsHIVStatus)) %>%
  filter(HIVResult %in% c(0,1)) %>%
  mutate(wealth = wealth / sd(wealth, na.rm = TRUE), network_wealth = network_wealth / sd(network_wealth, na.rm = TRUE)) %>%
  mutate(KnowsHIVStatus = ifelse(KnowsHIVStatus > 2, NA, KnowsHIVStatus)) %>%
  mutate(KnowsWhereToObtainART = ifelse(KnowsWhereToObtainART > 2, NA, KnowsWhereToObtainART)) 
  



saveRDS(analysis_data, "analysis_data.R")


with(analysis_data %>% filter(network_wealth_decile == "0-7"), ctable(wealth_quartile, HIVResult))
with(analysis_data %>% filter(network_wealth_decile == "7-10"), ctable(wealth_quartile, HIVResult))

ctable(wealth_quartile, network_wealth_decile)
ctable(wealth_quartile, HIVResult)
ctable(network_wealth_decile, HIVResult)

lm(HIVResult ~ network_wealth_decile + wealth + degree , data = analysis_data) %>% summary()
lm(KnowsHIVStatus ~ network_wealth_decile + wealth + degree , data = analysis_data) %>% summary()
