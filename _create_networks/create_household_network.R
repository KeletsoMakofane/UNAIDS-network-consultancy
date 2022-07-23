library(tidygraph)
library(igraph)
library(tidyverse) 
library(lubridate)
library(foreign)


############################ FUNCTIONS ########################

sort.edges <- function(x1,x2, result.index){
  x <- cbind(as.character(x1), as.character(x2))
  z <- t(apply(x, 1, function(g){g[order(g)]}))
  return(z[,result.index])
}

collapse.graph <- function(graph, attribute){
  #graph is an igraph with name attribute
  #attribute is a string - name of attribute we are collapsing over
  
  dictionary <- graph %>%
    vertex_attr() %>%
    bind_cols() %>%
    data.frame() %>%
    dplyr::select(attribute, "name")
  
  edges <- graph %>%
    as_edgelist() %>%
    data.frame() %>%
    mutate_all(as.character) %>%
    left_join(dictionary, by = c("X1" = "name")) %>%
    left_join(dictionary, by = c("X2" = "name"))
  
  names(edges) <- c("from.old", "to.old", "from", "to" )
  
  new.edges <- edges %>%
    mutate(to.sorted = sort.edges(to, from, 1), from.sorted = sort.edges(to, from, 2)) %>% 
    mutate(to = to.sorted, from = from.sorted ) %>%
    group_by(to, from) %>%
    summarize(weight = n()) %>%
    ungroup() %>%
    mutate(missing = is.na(to) | is.na(from)) %>%
    filter(!missing) %>%
    select(-missing)
  
  names(dictionary) <- c("attribute", "name")
  
  vertex.info <- dictionary %>%
    filter_all(function(x){!is.na(x)}) %>%
    group_by(attribute) %>%
    summarize(size = n()) 
  
  names(vertex.info) <- c("name", "size")
  
  result <- tbl_graph(nodes = vertex.info, edges = new.edges,  directed = FALSE)
}

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
data.individual.extended  <- readRDS(paste(data.input.dir, "data_individual_extended.R", sep=""))



############################### Clean DATA ##################################################

data.conjugal <- data.conjugal.raw
data.hhhead <- data.hhhead.raw

if (!testing.scripts){
  rm(list=c("data.conjugal.raw",
            "data.hhhead.raw"))
}

######################## Create Networks #########################################
#(these contain every entry in AHRI including HITS ineligible)


individual.mother.edges   <- data.individual.extended %>% 
  dplyr::select(IIntId, MotherIntId) %>% 
  unique() %>% 
  mutate(type = "mother") %>%
  rename(from = IIntId, to = MotherIntId)

individual.father.edges   <- data.individual.extended %>% 
  dplyr::select(IIntId, FatherIntId) %>% 
  unique() %>%  
  mutate(type = "father") %>%
  rename(from = IIntId, to = FatherIntId)

conjugal.edges            <- data.conjugal %>% 
  dplyr::select(FemalePartnerId, MalePartnerId) %>% 
  unique() %>%  
  mutate(type = "conjugal")  %>%
  rename(from = FemalePartnerId, to = MalePartnerId)

edge.info <- individual.mother.edges %>%
  rbind(individual.father.edges) %>%
  rbind(conjugal.edges) %>%
  filter(complete.cases(.)) %>%
  mutate(to = as.character(to), from = as.character(from)) %>%
  data.frame(stringsAsFactors = FALSE)

vertex.info.pre <- data.individual.extended %>%
  mutate(name = as.character(IIntId)) 


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

if (!testing.scripts){
  rm(list=c("individual.mother.edges",
            "individual.father.edges",
            "conjugal.edges",
            "data.hhhead",
            "graph.hhhead"))
}

### Add Node IDs that are in edges data but not vertex info data to the vertex info data
all.vertices            <- c(edge.info$from, edge.info$to, vertex.info.pre$name) %>% as.character() %>% unique()

vertex.info <- data.frame(name = all.vertices, stringsAsFactors = FALSE) %>%
  left_join(vertex.info.pre, by="name") %>%
  data.frame(stringsAsFactors = FALSE)

if(!testing.scripts){
  rm(vertex.info.pre)
}


######## Create kinship network objects 
kin.net.1.save <- tbl_graph(nodes = vertex.info, edges = edge.info,  directed = FALSE) %>%
  simplify() %>% 
  as_tbl_graph()
kin.net.2.save <- connect(kin.net.1.save, 2) %>% as_tbl_graph()

######## Create combination kinship+co-residence network objects. 

combo.net.2.save <- igraph::union(graph.hhhead.proj, kin.net.2.save) %>%
  simplify() %>%
  as_tbl_graph() %>%
  filter(!is.na(HHIntId))


table_households_spread <- table_households %>%
  mutate(year = year(VisitDate)) %>%
  filter(year > 2010) %>%
  mutate(DSRound = paste0("r", DSRound)) %>%
  select(HHIntId, DSRound, BDN) %>%
  pivot_wider(names_from = DSRound, values_from = BDN) 
  
graph_households <- collapse.graph(combo.net.2.save, "HHIntId") %>%
  mutate(HHIntId = name) %>%
  activate(nodes) %>%
  left_join(table_households_spread)
  

