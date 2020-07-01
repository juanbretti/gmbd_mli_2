## load libraries
library(dplyr)
library(networkD3)
library(tidyr)
# read in EU referendum results dataset
refresults <- read.csv("data/EU-referendum-result-data.csv")
# aggregate by region
results <- refresults %>% 
  dplyr::group_by(Region) %>% 
  dplyr::summarise(Remain = sum(Remain), Leave = sum(Leave))


# format in prep for sankey diagram
results <- tidyr::gather(results, result, vote, -Region)
# create nodes dataframe
regions <- unique(as.character(results$Region))
nodes <- data.frame(node = c(0:13), 
                    name = c(regions, "Leave", "Remain"))
#create links dataframe
results <- merge(results, nodes, by.x = "Region", by.y = "name")
results <- merge(results, nodes, by.x = "result", by.y = "name")
links <- results[ , c("node.x", "node.y", "vote")]
colnames(links) <- c("source", "target", "value")


# draw sankey network
networkD3::sankeyNetwork(Links = links, Nodes = nodes, 
                         Source = 'source', 
                         Target = 'target', 
                         Value = 'value', 
                         NodeID = 'name',
                         units = 'votes')



# https://stackoverflow.com/questions/9968433/sankey-diagrams-in-r
# Load package
library(networkD3)

# Load energy projection data
# Load energy projection data
URL <- paste0(
  "https://cdn.rawgit.com/christophergandrud/networkD3/",
  "master/JSONdata/energy.json")
Energy <- jsonlite::fromJSON(URL)
str(Energy)

range(Energy$links$source)

# Plot
sankeyNetwork(Links = Energy$links, Nodes = Energy$nodes, Source = "source",
              Target = "target", Value = "value", NodeID = "name",
              units = "TWh", fontSize = 12, nodeWidth = 30)

# https://plotly.com/r/sankey-diagram/
library(plotly)
library(rjson)
json_file <- "https://raw.githubusercontent.com/plotly/plotly.js/master/test/image/mocks/sankey_energy.json"
json_data <- fromJSON(paste(readLines(json_file), collapse=""))

