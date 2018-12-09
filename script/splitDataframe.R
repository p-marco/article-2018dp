df <- read.csv('../data/sentences.csv', header = T, stringsAsFactors = F)
dfs <- split(df, list(df$Corpus, df$L1)) # list of dfs
#dfss <- split(dfs, dfs$L1) # list of dfs
# use numbers as file names
lapply(names(dfs),
       function(x){write.csv(dfs[[x]], paste0("../data/partials/",x,".csv"),
                             row.names = FALSE)}) 