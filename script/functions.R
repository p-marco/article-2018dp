# 	---
# 	Marco Petolicchio, 2018
# 	R scripts for article on DP analysis
# 	---


# 	LOAD LIBRARIES
	library(data.table)
	library(dplyr)
	library(ggplot2)
	library(tidyr)
	library(udpipe)

# 	LOAD MODELS FOR NLP
	udmodel_italian	<- 	udpipe_load_model(file = "italian-ud-2.0-170801.udpipe")

# 	SET GLOBAL VARIABLES
	data.in.path  	<- 	"../data/"
	data.out.path  	<- 	"../data/out/"

# 	DEFINE THE CORPORA OF THE COLLECTION
	czechit.ces 	<- 	"partials/CZECH-IT.ces"
	czechit.slk 	<- 	"partials/CZECH-IT.slk"
	merlin.ces 		<- 	"partials/MERLIN.ces"
	merlin.slk 		<- 	"partials/MERLIN.slk"
	valico.ces 		<- 	"partials/VALICO.ces"
	valico.slk 		<- 	"partials/VALICO.slk"
	sentences 		<- 	"sentences"


# 	CREATE FUNCTIONS

	df <- function(dataname){
		# READ THE CSV BY VAR NAME
		# CALL THE DATAFRAME AS DF(DATANAME), E.G.
		# HEAD(DF(SENTENCES))
		df <- read.csv(paste(data.in.path,dataname,'.csv', sep=""), header = T, stringsAsFactors = F)
		df <- as.data.frame(df) 
		return(df)
	}

	df.clean.corpus <- function(dataname){
		# CALL THE WISHED DATA FRAME, 
		# PERFORM THE ANNOTATION AND CLEAN OUT 
		# THE PUNCTUATION TOKENS
		df(dataname)
		df <- select(df(dataname), "content")
		txt <- as.character(df)
		x <- udpipe_annotate(udmodel_italian, x = txt, doc_id = seq_along(txt))
		x  <- as.data.table(x)
		x.clean = subset(x, upos!="PUNCT")
	}

	df.clean.ngrams <- function(dataname, ngrams){
		# CALL THE CLEAN FUNCTION AND 
		# GENERATE THE NGRAMS WITH AN INDEX PASSED IN THE FUNCTION
		df.clean.corpus(df)
		x <- x[, pos_sequence := txt_nextgram(x = upos, n = ngrams), by = list(doc_id, sentence_id)]
	}

df.out <- as.data.table(sort(table(x$pos_sequence)), sep=",")




df.out <- df.out %>% separate(col = V1, into = c("A", "B"), sep = " ")


df.noun.begin  <- subset(df.out, A=='NOUN')
df.noun.end  <- subset(df.out, B=='NOUN')

df.noun.begin.clean  <- df.noun.begin[order(df.noun.begin$B), ]
df.noun.end.clean  <- df.noun.begin[order(df.noun.end$A), ]

comparison <- data.frame(df.noun.begin.clean$N,df.noun.end.clean$N) 

barplot(t(as.matrix(comparison)), beside=TRUE, names.arg = c(df.noun.begin$B))
 
