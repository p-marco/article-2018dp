library(dplyr)
library(udpipe)

# Define dataframe
# 
# df <- read.csv('../data/caseStudyDP.csv', header = T, stringsAsFactors = F)
#df <- read.csv('../data/sentences.csv', header = T, stringsAsFactors = F)

#during first time model download execute the below line too
# We do need to execute the next code before running udmodel_english

#model <- udpipe_download_model(language = "italian")



#udmodel_italian <- udpipe_load_model(file = "italian-ud-2.0-170801.udpipe")#

#df <- select(df, "content")
#txt <- as.character(df)
#x <- udpipe_annotate(udmodel_italian, x = txt, doc_id = seq_along(txt))#

##x <- data.frame(s)
##head(x)#
#
#

#### CONLL#

#cat(x$conllu, file = "myannotation.conllu")#
#



udmodel_italian <- udpipe_load_model(file = "italian-ud-2.0-170801.udpipe")

createConllu <- function(dataname){
df <- read.csv(paste("../data/",dataname,".csv",sep=""), header = T, stringsAsFactors = F)
df <- select(df, "content")
txt <- as.character(df)
x <- udpipe_annotate(udmodel_italian, x = txt, doc_id = seq_along(txt))

#x <- data.frame(s)
#head(x)



### CONLL

cat(x$conllu, file = paste("../data/out/",dataname,".conllu",sep=""))


}

createConllu("sentences")