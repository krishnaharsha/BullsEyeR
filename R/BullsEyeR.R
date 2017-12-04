#'@title Topic Modelling for content curation @COGNIZANT ANALYTICS
#'@description 1.freqAnalysis()- returns dataframe with words and their frequencies after initial preprocessing sparsity control and TFIDF analysis is performed.we can pick some words from the high frequency list as custom stop words.2.createDTM()- repeats first step, now including the custom stop words as well, removes empty documents if any and returns a Document term matrix. This DTM is used for finding optimal number of topics for LDA modelling  using FindTopicsNumber() from 'ldatuning' package3.BullsEye()- Performs preprocessing with custom stop words,topic number from ldatuning and builds unigram topic model with/without stemming. Returns    a)a data frame with keywords/dominant words of each document along with the topic number assigned by LDA model.b)a list of zero length documents after preprocessing c)a data frame with top 20 terms in all the topics discovered by LDA
#'@parameters  ds is a character vector of text documents,spvar is a sparsity variable which defaults to 0.99,mystopwords is a character vector of custom stop words which defaults to NULL,tno is number of topics to be used to model text using LDA approach which defaults to 20,seedno is seed which defaults to 12345,stemvar is variable indicating stemming to be performed or not which defaults to '0' meaning no stemming
#'@return See Description

#' @export
freqAnalysis<-function(ds,spvar=0.99,seedno=12345,stemvar=0){


  #load packages
  library(tm)
  library(NLP)
  require(topicmodels)
  require(slam)
  require(Matrix)
  #preprocessing
  ds <- gsub("[^a-zA-Z]+", " ", ds)
  corp<- Corpus(VectorSource(ds));

  corp <- tm_map(corp, content_transformer(function(x) iconv(x, to='UTF-8', sub='byte')))
  corp <- tm_map(corp, tolower)

  corp <- tm_map(corp, content_transformer(tolower)) # convert all text to lower case
  corp <- tm_map(corp, removePunctuation)
  corp <- tm_map(corp, removeNumbers)
  corp <- tm_map(corp, removeWords, stopwords("SMART"))
  corp <- tm_map(corp, removeWords, stopwords("english"))
  if(stemvar==1){
    #saving corpus before stem for stem completion at the end
    corp.stemComp<-corp
    #preprocessing continued
    corp <- tm_map(corp, stemDocument, language = "english") ## Stemming the words
  }
  corp<-tm_map(corp,stripWhitespace)
  #row.names(corp)
  #corp<- tm_map(corp, PlainTextDocument)
  ################################### create a term document matrix #######################################
  # library(SnowballC)
  # library(RWeka)
  # BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
  # corp.tdm<- DocumentTermMatrix(corp, control = list(Tokenizer=BigramTokenizer,weight=weightTf))

  corp.tdm <- DocumentTermMatrix(corp)
  dim(corp.tdm)
  #######################################Controlling Sparse Terms #########################################



  corp.tdm.sp <- removeSparseTerms(corp.tdm, sparse=spvar)
  # ## Convert document term matrix to data frame

  corp.tdm.sp.t<-(corp.tdm.sp)

  ####################################### TF IDF as a preprocessing Step ##################################


  # transpose document term matrix, necessary for the next steps using mean term
  #frequency-inverse document frequency (tf-idf)
  #to select the vocabulary for topic modeling
  #corp.tdm.sp.t <- corp.tdm
  summary(col_sums(corp.tdm.sp.t))
  # calculate tf-idf values
  term_tfidf <- tapply(corp.tdm.sp.t$v/row_sums(corp.tdm.sp.t)[corp.tdm.sp.t$i], corp.tdm.sp.t$j,mean) * log2(nDocs(corp.tdm.sp.t)/col_sums(corp.tdm.sp.t>0))
  summary(term_tfidf)
  #plot(term_tfidf)
  df0 <- as.data.frame.table(term_tfidf)
  #fix(df0)
  #Reducing vocabulary using Tf-Idf scores
  # keep only those terms above lower quartile
  corp.tdm.sp.t.tdif <- corp.tdm.sp.t[,df0$Freq >=median(term_tfidf)]
  corp.tdm.sp.t.tdif <- corp.tdm.sp.t.tdif[row_sums(corp.tdm.sp.t) > 0, ]
  summary(col_sums(corp.tdm.sp.t.tdif))
  dim(corp.tdm)
  dim(corp.tdm.sp.t.tdif)
  #freq anal
  tdm<-corp.tdm.sp.t.tdif
  freq=colSums(as.matrix(tdm))
  #head(freq,10)

  #plot(sort(freq, decreasing = T),col="blue",main="Word TF-IDF frequencies", xlab="TF-IDF-based rank", ylab = "TF-IDF")
  #library(ggplot2)

  high.freq=tail(sort(freq),n=100000)
  hfdafr<-as.data.frame(high.freq)
  freqAnal<-cbind(rownames(hfdafr),hfdafr$high.freq)
  colnames(freqAnal)<-c("word","frequency")

  return(freqAnal)
}


#' @export
createDTM<-function(ds,spvar=0.99,myStopWords=NULL,stemvar=0){
  #load packages
  library(tm)
  library(NLP)
  require(topicmodels)
  require(slam)
  require(Matrix)
  #preprocessing
  ds <- gsub("[^a-zA-Z]+", " ", ds)
  corp<- Corpus(VectorSource(ds));

  corp <- tm_map(corp, content_transformer(function(x) iconv(x, to='UTF-8', sub='byte')))
  corp <- tm_map(corp, tolower)

  corp <- tm_map(corp, content_transformer(tolower)) # convert all text to lower case
  corp <- tm_map(corp, removePunctuation)
  corp <- tm_map(corp, removeNumbers)
  corp <- tm_map(corp, removeWords, stopwords("SMART"))
  corp <- tm_map(corp, removeWords, stopwords("english"))
  if(stemvar==1){
    #saving corpus before stem for stem completion at the end
    corp.stemComp<-corp
    #preprocessing continued
    corp <- tm_map(corp, stemDocument, language = "english") ## Stemming the words
  }
  corp<-tm_map(corp,stripWhitespace)
  #row.names(corp)
  #corp<- tm_map(corp, PlainTextDocument)
  ################################### create a term document matrix #######################################
  # library(SnowballC)
  # library(RWeka)
  # BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
  # corp.tdm<- DocumentTermMatrix(corp, control = list(Tokenizer=BigramTokenizer,weight=weightTf))

  corp.tdm <- DocumentTermMatrix(corp)
  dim(corp.tdm)
  #######################################Controlling Sparse Terms #########################################



  corp.tdm.sp <- removeSparseTerms(corp.tdm, sparse=spvar)
  # ## Convert document term matrix to data frame

  corp.tdm.sp.t<-(corp.tdm.sp)

  ####################################### TF IDF as a preprocessing Step ##################################


  # transpose document term matrix, necessary for the next steps using mean term
  #frequency-inverse document frequency (tf-idf)
  #to select the vocabulary for topic modeling
  #corp.tdm.sp.t <- corp.tdm
  summary(col_sums(corp.tdm.sp.t))
  # calculate tf-idf values
  term_tfidf <- tapply(corp.tdm.sp.t$v/row_sums(corp.tdm.sp.t)[corp.tdm.sp.t$i], corp.tdm.sp.t$j,mean) * log2(nDocs(corp.tdm.sp.t)/col_sums(corp.tdm.sp.t>0))
  summary(term_tfidf)
  #plot(term_tfidf)
  df0 <- as.data.frame.table(term_tfidf)
  #fix(df0)
  #Reducing vocabulary using Tf-Idf scores
  # keep only those terms above lower quartile
  corp.tdm.sp.t.tdif <- corp.tdm.sp.t[,df0$Freq >=median(term_tfidf)]
  corp.tdm.sp.t.tdif <- corp.tdm.sp.t.tdif[row_sums(corp.tdm.sp.t) > 0, ]
  summary(col_sums(corp.tdm.sp.t.tdif))
  dim(corp.tdm)
  dim(corp.tdm.sp.t.tdif)

  #my stop words
  #dtm
  if(length(myStopWords)!=0){
    corp.tdm <- DocumentTermMatrix(corp,control = list(stopwords=myStopWords))

    #######################################Controlling Sparse Terms #########################################
    corp.tdm.sp <- removeSparseTerms(corp.tdm, sparse=spvar)
    # ## Convert document term matrix to data frame

    corp.tdm.sp.t<-(corp.tdm.sp)

    ####################################### TF IDF as a preprocessing Step ##################################


    # transpose document term matrix, necessary for the next steps using mean term
    #frequency-inverse document frequency (tf-idf)
    #to select the vocabulary for topic modeling
    #corp.tdm.sp.t <- corp.tdm
    summary(col_sums(corp.tdm.sp.t))
    # calculate tf-idf values
    term_tfidf <- tapply(corp.tdm.sp.t$v/row_sums(corp.tdm.sp.t)[corp.tdm.sp.t$i], corp.tdm.sp.t$j,mean) * log2(nDocs(corp.tdm.sp.t)/col_sums(corp.tdm.sp.t>0))
    summary(term_tfidf)
    #plot(term_tfidf)
    df0 <- as.data.frame.table(term_tfidf)
    #fix(df0)
    #Reducing vocabulary using Tf-Idf scores
    # keep only those terms above lower quartile
    corp.tdm.sp.t.tdif <- corp.tdm.sp.t[,df0$Freq >=median(term_tfidf)]
    corp.tdm.sp.t.tdif <- corp.tdm.sp.t.tdif[row_sums(corp.tdm.sp.t) > 0, ]
    summary(col_sums(corp.tdm.sp.t.tdif))
    dim(corp.tdm)
    dim(corp.tdm.sp.t.tdif)
  }

  czeros<-which(row_sums(corp.tdm.sp.t)==0)
  ##remove empty rows in DTM
  #24  29 516 858 859
  gc()
  # library(bigmemory)
  # x <- big.matrix(6742, 1, type="integer", init=0,
  #                 dimnames=list(NULL,"rowtotal"))
  ##
  x <- apply(corp.tdm.sp.t.tdif , 1, sum) #Find the sum of words in each Document
  corp.tdm.sp.t.tdif <- corp.tdm.sp.t.tdif[x> 0, ]
  xzeros<-which(x==0)

  zeros<-c(czeros,xzeros)

  return(corp.tdm.sp.t.tdif)
}


#' @export
BullsEye<-function(ds,spvar=0.99,myStopWords=NULL,tno=20,seedno=12345,stemvar=0,stemCompletionvar="prevalent"){


  #load packages
  library(tm)
  library(NLP)
  require(topicmodels)
  require(slam)
  require(Matrix)
  #preprocessing
  ds <- gsub("[^a-zA-Z]+", " ", ds)
  corp<- Corpus(VectorSource(ds));

  corp <- tm_map(corp, content_transformer(function(x) iconv(x, to='UTF-8', sub='byte')))
  corp <- tm_map(corp, tolower)

  corp <- tm_map(corp, content_transformer(tolower)) # convert all text to lower case
  corp <- tm_map(corp, removePunctuation)
  corp <- tm_map(corp, removeNumbers)
  corp <- tm_map(corp, removeWords, stopwords("SMART"))
  corp <- tm_map(corp, removeWords, stopwords("english"))
  if(stemvar==1){
    #saving corpus before stem for stem completion at the end
    corp.stemComp<-corp
    #preprocessing continued
    corp <- tm_map(corp, stemDocument, language = "english") ## Stemming the words
  }
  corp<-tm_map(corp,stripWhitespace)
  #row.names(corp)
  #corp<- tm_map(corp, PlainTextDocument)
  ################################### create a term document matrix #######################################
  # library(SnowballC)
  # library(RWeka)
  # BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
  # corp.tdm<- DocumentTermMatrix(corp, control = list(Tokenizer=BigramTokenizer,weight=weightTf))

  corp.tdm <- DocumentTermMatrix(corp)
  dim(corp.tdm)
  #######################################Controlling Sparse Terms #########################################



  corp.tdm.sp <- removeSparseTerms(corp.tdm, sparse=spvar)
  # ## Convert document term matrix to data frame

  corp.tdm.sp.t<-(corp.tdm.sp)

  ####################################### TF IDF as a preprocessing Step ##################################


  # transpose document term matrix, necessary for the next steps using mean term
  #frequency-inverse document frequency (tf-idf)
  #to select the vocabulary for topic modeling
  #corp.tdm.sp.t <- corp.tdm
  summary(col_sums(corp.tdm.sp.t))
  # calculate tf-idf values
  term_tfidf <- tapply(corp.tdm.sp.t$v/row_sums(corp.tdm.sp.t)[corp.tdm.sp.t$i], corp.tdm.sp.t$j,mean) * log2(nDocs(corp.tdm.sp.t)/col_sums(corp.tdm.sp.t>0))
  summary(term_tfidf)
  #plot(term_tfidf)
  df0 <- as.data.frame.table(term_tfidf)
  #fix(df0)
  #Reducing vocabulary using Tf-Idf scores
  # keep only those terms above lower quartile
  corp.tdm.sp.t.tdif <- corp.tdm.sp.t[,df0$Freq >=median(term_tfidf)]
  corp.tdm.sp.t.tdif <- corp.tdm.sp.t.tdif[row_sums(corp.tdm.sp.t) > 0, ]
  summary(col_sums(corp.tdm.sp.t.tdif))
  dim(corp.tdm)
  dim(corp.tdm.sp.t.tdif)

  #my stop words
  #dtm
  if(length(myStopWords)!=0){
    corp.tdm <- DocumentTermMatrix(corp,control = list(stopwords=myStopWords))

    #######################################Controlling Sparse Terms #########################################
    corp.tdm.sp <- removeSparseTerms(corp.tdm, sparse=spvar)
    # ## Convert document term matrix to data frame

    corp.tdm.sp.t<-(corp.tdm.sp)

    ####################################### TF IDF as a preprocessing Step ##################################


    # transpose document term matrix, necessary for the next steps using mean term
    #frequency-inverse document frequency (tf-idf)
    #to select the vocabulary for topic modeling
    #corp.tdm.sp.t <- corp.tdm
    summary(col_sums(corp.tdm.sp.t))
    # calculate tf-idf values
    term_tfidf <- tapply(corp.tdm.sp.t$v/row_sums(corp.tdm.sp.t)[corp.tdm.sp.t$i], corp.tdm.sp.t$j,mean) * log2(nDocs(corp.tdm.sp.t)/col_sums(corp.tdm.sp.t>0))
    summary(term_tfidf)
    #plot(term_tfidf)
    df0 <- as.data.frame.table(term_tfidf)
    #fix(df0)
    #Reducing vocabulary using Tf-Idf scores
    # keep only those terms above lower quartile
    corp.tdm.sp.t.tdif <- corp.tdm.sp.t[,df0$Freq >=median(term_tfidf)]
    corp.tdm.sp.t.tdif <- corp.tdm.sp.t.tdif[row_sums(corp.tdm.sp.t) > 0, ]
    summary(col_sums(corp.tdm.sp.t.tdif))
    dim(corp.tdm)
    dim(corp.tdm.sp.t.tdif)
  }

  czeros<-which(row_sums(corp.tdm.sp.t)==0)
  ##remove empty rows in DTM
  #24  29 516 858 859
  gc()
  # library(bigmemory)
  # x <- big.matrix(6742, 1, type="integer", init=0,
  #                 dimnames=list(NULL,"rowtotal"))
  ##
  x <- apply(corp.tdm.sp.t.tdif , 1, sum) #Find the sum of words in each Document
  corp.tdm.sp.t.tdif <- corp.tdm.sp.t.tdif[x> 0, ]
  xzeros<-which(x==0)

  zeros<-c(czeros,xzeros)
  ####################################### Model Bhulding ##################################################


  myModel=builtModel<-LDA(corp.tdm.sp.t.tdif, control=list(seed=seedno),tno);
  #head(topics(myModel))
  #save(myModel, file = "my_model1.rda")

  ################################# Extraction of terms  #################################################

  #corpus levels#not useful
  #wordassignments_df <- data.frame (topics = myModel@wordassignments [,]$v,terms = myModel@terms [ myModel@wordassignments [ ,]$j])

  #Word assignment matrix
  simple_triplet_matrix_sparse<-myModel@wordassignments

  simple_triplet_matrix_sparse <-  sparseMatrix(i=simple_triplet_matrix_sparse$i, j=simple_triplet_matrix_sparse$j, x=simple_triplet_matrix_sparse$v,
                                                dims=c(simple_triplet_matrix_sparse$nrow, simple_triplet_matrix_sparse$ncol))


  WA<-as.data.frame(as.matrix(simple_triplet_matrix_sparse))
  ####
  #tempp<-which(WA[1,]!=0)
  #testt<-WA[1,tempp]
  #wtd<-colnames(corp.tdm)[tempp]
  ######
  #put doc.term matrix currently being used

  corp.tdm.sp.t.tdif<-corp.tdm.sp.t.tdif
  #top topic in all documents
  t<-as.vector(topics(myModel,1))
  ########## Extraction of top terms in top topic of each document
  tempp2<-list()
  wtd2<-list()
  term1<-list()
  logic.term<-list()
  ordered.index<-list()
  rep.terms<-list()
  #terms in each topic
  terms.df<-as.data.frame(terms(myModel,20000000))
  #
  representativeWords<-data.frame()

  for(i in 1:dim(corp.tdm.sp.t.tdif)[1]) {
    tempp2[[i]]<-which(WA[i,]==t[i])
    wtd2[[i]]<-colnames(corp.tdm.sp.t.tdif)[tempp2[[i]]]


    term1[[i]]<-as.character(terms.df[,t[i]])
    logic.term[[i]]<-term1[[i]]%in%wtd2[[i]]
    ordered.index[[i]]<-which(logic.term[[i]]==TRUE)
    ordered.index[[i]]<-head(ordered.index[[i]])
    rep.terms[[i]]<-as.character(terms.df[,t[i]][ordered.index[[i]]])

  }


  n.obs <- sapply(rep.terms, length)
  seq.max <- seq_len(max(n.obs))
  mat <- t(sapply(rep.terms, "[", i = seq.max))
  mat<-t(mat)
  #fix(mat)
  if(stemvar==1){
    ######################################## stem completion ################################
    #type = c("prevalent", "first", "longest","none", "random", "shortest")

    sc<-stemCompletion(mat,corp.stemComp, type = c(stemCompletionvar))

    #sc1<-stemCompletion(mat,corp.stemComp, type = c("longest"))
    sc.df<-as.data.frame(sc)
    #sc1.df<-as.data.frame(sc1)
    #sc.df2<-as.data.frame(as.list(sc))
    #fix(sc.df2)
    ##Saving stem completed top topic top words in all documents
    dflist<-list()
    csum<-cumsum(n.obs)
    for(i in 1:dim(corp.tdm.sp.t.tdif)[1]){
      if(i==1){dflist[[i]]<-sc.df[1:n.obs[i],]
      dflist[[i]]<-as.character(dflist[[i]])
      }else{dflist[[i]]<-sc.df[(csum[i-1]+1):csum[i],]
      dflist[[i]]<-as.character(dflist[[i]])
      }
    }

    n.obs2 <- sapply(dflist, length)
    seq.max2 <- seq_len(max(n.obs2))
    mat22 <- t(sapply(dflist, "[", i = seq.max))
    mat22<-t(mat22)

    testmatr<-as.data.frame(mat22)
    for(i in 1:dim(corp.tdm.sp.t.tdif)[1]){
      testmatr[,i] <- sub("^$", "None", testmatr[,i])
    }

    mat<-testmatr
  }


  mat.t<-rbind(mat,t)
  #fix(mat.t)

  tdftdf<-as.data.frame(ds[-zeros])
  tmatdf<-as.data.frame(t(mat.t))



  ##
  mat2<-mat
  mat.t2<-mat2
  tempmatt<-mat.t2
  mat.t2<-as.data.frame(mat.t2)
  #mat.t<-mat.t[,-1]
  mat.ttt<-t(mat.t2)
  mat.ttt<-as.data.frame(mat.ttt)
  #fix(mat.tt)
  #mat.tt$x <- paste(mat.tt$`1`,mat.tt$`2`,mat.tt$V3,mat.tt$V4,mat.tt$V5,mat.tt$V6)
  mat.ttt$x <- paste(mat.ttt[,1], ";",mat.ttt[,2],";",mat.ttt[,3],";",mat.ttt[,4],";",mat.ttt[,5],";",mat.ttt[,6])
  mat.ttt$x<-gsub("NA","",mat.ttt$x)
  tmp<-cbind(mat.ttt$x,t)
  tmp<-as.data.frame(tmp)

  ##

  results.list<-list(tmp,zeros,terms.df)

  return(results.list)

}

