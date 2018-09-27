library(dplyr)
library(tidyr)
library(ggplot2)
library(udpipe)






udmodel_italian <- udpipe_load_model(file = "italian-ud-2.0-170801.udpipe")





#### SET GLOBAL VARIABLES


data.in.path  	<- 	"../data/"
data.out.path  	<- 	"../data/out/"



# Define the corpora of the collection
czechit.ces 	<- 	"partials/CZECH-IT.ces"
czechit.slk 	<- 	"partials/CZECH-IT.slk"
merlin.ces 		<- 	"partials/MERLIN.ces"
merlin.slk 		<- 	"partials/MERLIN.slk"
valico.ces 		<- 	"partials/VALICO.ces"
valico.slk 		<- 	"partials/VALICO.slk"
sentences 		<- 	"sentences"




# Read the csv by var name
# call the dataframe as df(dataname), e.g.
# head(df(sentences))

df <- function(dataname){
	df <- read.csv(paste(data.in.path,dataname,'.csv', sep=""), header = T, stringsAsFactors = F)
	df <- as.data.frame(df) 
	return(df)
}



df.clean.corpus <- function(df){
	df <- select(df, "content")
	txt <- as.character(df)
	x <- udpipe_annotate(udmodel_italian, x = txt, doc_id = seq_along(txt))
	x  <- as.data.frame(x)
	# Remove punctuation tokens
	x.clean = subset(x, upos!="PUNCT")
	# Print the output
		# SHAME 1 : Needed to put out the count from the function
		# SHAME 2 : The output is reduced by 1 which represent the token of the header ('content')  
	as.numeric(sum(nrow(x.clean) -1))
}




cleanCorpus <- function(dataname){
	df <- select(dataname, "content")
	txt <- as.character(df)
	x <- udpipe_annotate(udmodel_italian, x = txt, doc_id = seq_along(txt))
	x  <- as.data.frame(x)
	# Remove punctuation tokens
	x.clean = subset(x, upos!="PUNCT")
	# Print the output
		# SHAME 1 : Needed to put out the count from the function
		# SHAME 2 : The output is reduced by 1 which represent the token of the header ('content')  
	as.numeric(sum(nrow(x.clean) -1))
}



#### FOLLOWING PART CAN BE REMOVED

df <- read.csv('data/sentences.csv', header = T, stringsAsFactors = F)




df.czechit.ces <- read.csv('data/partials/CZECH-IT.ces.csv', header = T, stringsAsFactors = F)
df.czechit.slk <- read.csv('data/partials/CZECH-IT.slk.csv', header = T, stringsAsFactors = F)

df.merlin.ces <- read.csv('data/partials/MERLIN.ces.csv', header = T, stringsAsFactors = F)
df.merlin.slk <- read.csv('data/partials/MERLIN.slk.csv', header = T, stringsAsFactors = F)
df.valico.ces <- read.csv('data/partials/VALICO.ces.csv', header = T, stringsAsFactors = F)
df.valico.slk <- read.csv('data/partials/VALICO.slk.csv', header = T, stringsAsFactors = F)

#################################################################






df <- select(df, "content")
txt <- as.character(df)
x <- udpipe_annotate(udmodel_italian, x = txt, doc_id = seq_along(txt))
library(data.table)
x <- as.data.table(x)
x <- subset(x, upos!="PUNCT")

x <- x[, pos_sequence := txt_nextgram(x = upos, n = 2), by = list(doc_id, sentence_id)]
# tail(sort(table(x$pos_sequence)))

# df.out <- as.data.table(x$pos_sequence, sep=",")

df.out <- as.data.table(sort(table(x$pos_sequence)), sep=",")




df.out <- df.out %>% separate(col = V1, into = c("A", "B"), sep = " ")

#data.table::fwrite(df.out, file = "data/out/pos.csv", sep = ",")

df.noun.begin  <- subset(df.out, A=='NOUN')
df.noun.end  <- subset(df.out, B=='NOUN')


#data.table::fwrite(df.noun.begin, file = "data/out/pos.noun.begin.csv", sep = ",")
#data.table::fwrite(df.noun.end, file = "data/out/pos.noun.end.csv", sep = ",")

#barplot(df.noun.begin$N, main="Noun+POS", names.arg=df.noun.begin$B)
#barplot(df.noun.end$N, main="POS+Noun", names.arg=df.noun.end$A)

df.noun.begin.clean  <- df.noun.begin[order(df.noun.begin$B), ]
df.noun.end.clean  <- df.noun.begin[order(df.noun.end$A), ]

comparison <- data.frame(df.noun.begin.clean$N,df.noun.end.clean$N) 

barplot(t(as.matrix(comparison)), beside=TRUE, names.arg = c(df.noun.begin$B))
 
