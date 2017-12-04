# BullsEyeR

DESCRIPTION:

1)Helps in intial preprocessing like converting text to lower case, removing (punctuation, numbers,stop words), stemming, sparsity control   and TF-IDF pre-processing.

2) Helps in recognizing domain/corpus specific stop words 

3) makes use of 'ldatunig' output to pick optimal number of topics for LDA modelling 

4) Helps in extracting dominant words or key words that represent the context/topics of the content in each document. 

FUNCTIONS: 

1.freqAnalysis()- returns dataframe with words and their frequencies after initial preprocessing sparsity control and TFIDF analysis is performed.we can pick some words from the high frequency list as custom stop words.

2.createDTM()- repeats first step, now including the custom stop words as well, removes empty documents if any and returns a Document term matrix. This DTM is used for finding optimal number of topics for LDA modelling  using FindTopicsNumber() from 'ldatuning' package

3.BullsEye()- Performs preprocessing with custom stop words,topic number from ldatuning and builds unigram topic model with/without stemming. Returns    

   a)a data frame with keywords/dominant words of each document along with the topic number assigned by LDA model.
  
   b)a list of zero length documents after preprocessing 
  
   c)a data frame with top 20 terms in all the topics discovered by LDA 

PARAMETERS:

ds(character vector of text documents),spvar(sparsity variable- defaults to 0.99),mystopwords(defaults to NULL),tno(number of topics-defaults to 20),seedno(seed defaults to 12345),stemvar(variable indication stemming to be performed or not- defaults to '0':no stemming)

INSTALL:

Install from GitHub :  devtools::install_github("krishnaharsha/BullsEyeR")
