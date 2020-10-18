library(dplyr)
library(tidytext)
library(stringr)
library(stringi)
library(shinyalert)

server <- function(input, output, session) {
  shinyalert(
    title = "Welcome!",
    text = "<p style='font-size:150%'><b>Please wait about 10 seconds for the application to load!<br><br>That's when the 'Nothing Entered' boxes are displayed.<br><br>Note: Some sentences/phrases cannot be predicted because of the limited data available.</p>",
    closeOnEsc = TRUE,
    closeOnClickOutside = FALSE,
    html = TRUE,
    type = "info",
    showConfirmButton = TRUE,
    showCancelButton = FALSE,
    confirmButtonText = "OK",
    confirmButtonCol = "#007BFF",
    timer = 0,
    imageUrl = "",
    animation = TRUE
  )

  `%notin%` = Negate(`%in%`)
  
  bigramCounts = readRDS("data/nGrams/2grams.rds") 
  trigramCounts = readRDS("data/nGrams/3grams.rds")
  quadgramCounts = readRDS("data/nGrams/4grams.rds")
  quintegramCounts = readRDS("data/nGrams/5grams.rds")
  rownames(bigramCounts) = bigramCounts$gram
  rownames(trigramCounts) = trigramCounts$gram
  rownames(quadgramCounts) = quadgramCounts$gram
  rownames(quintegramCounts) = quintegramCounts$gram
  
  ## FOR KNS SMOOTHING MODEL
  
  #tabling ngrams by last word
  bigramLastWordCount = readRDS("data/lastWordCount/bigramLastWordCounts.rds")
  trigramLastWordCount = readRDS("data/lastWordCount/trigramLastWordCounts.rds")
  quadgramLastWordCount = readRDS("data/lastWordCount/quadgramLastWordCounts.rds")
  quintegramLastWordCount = readRDS("data/lastWordCount/quintegramLastWordCounts.rds")
  
  #for continuation count, creating columns without first and second t words
  trigramNoFirstWord = readRDS("data/noFirstWord/trigramNoFirstWord.rds")
  trigramNoSecondWord = readRDS("data/noSecondWord/trigramNoSecondWord.rds")
  
  quadgramNoFirstWord = readRDS("data/noFirstWord/quadgramNoFirstWord.rds")
  quadgramNoSecondWord = readRDS("data/noSecondWord/quadgramNoSecondWord.rds")
  
  quintegramNoFirstWord = readRDS("data/noFirstWord/quintegramNoFirstWord.rds")
  quintegramNoSecondWord = readRDS("data/noSecondWord/quintegramNoSecondWord.rds")
  
  
  numerator <- function(ngrams, gram){
    if (gram == 2){
      noFirstWord = trigramNoFirstWord
    }
    
    if (gram == 3){
      noFirstWord = quadgramNoFirstWord
    }
    
    if (gram == 4){
      noFirstWord = quintegramNoFirstWord
    }
    numerator = noFirstWord[ngrams] - 0.75
    numerator[numerator<0] <- 0
    return(numerator)
  }
  
  denominator <- function(ngrams, gram){
    if (gram == 2){
      noSecondWord = trigramNoSecondWord
    }
    
    if (gram == 3){
      noSecondWord = quadgramNoSecondWord
    }
    
    if (gram == 4){
      noSecondWord = quintegramNoSecondWord
    }
    
    lastWord = stri_locate_last_boundaries(ngrams)[,1] - 2
    withoutLastWord = substring(ngrams, 1, lastWord)
    
    denominator = noSecondWord[withoutLastWord]
    return(denominator)
  }
  
  continuationCount <- function(ngrams, gram){ #if gram != 5 when calculating first term
    ngrams = paste0(' ',ngrams)
    scores = numerator(ngrams, gram) / denominator(ngrams, gram)
    scores[is.na(scores)] <- 0
    return(scores)
  }
  
  firstTerm <- function(results, gram){
    # regular count if ngram is 5
    if (gram == 5){
      sumFreq = sum(results$n)
      return(results$n / sumFreq)
    }
    else{
      # continuation count if ngram is less than 5
      return(continuationCount(results$gram, gram))
    }
  }
  
  lambda <- function(results, gram){ # creates the lambda multiplier for pCont (not dependent on final word)
    if (gram == 5){ return(0)}
    d = 0.75
    fraction = d/sum(results$n)
    return(fraction * nrow(results))
  }
  
  pCont <- function(lambdaVal, results, gramCounts, gram){ #continuation probability
    ngrams = results$gram
    if (gram == 2){
      lastWordCount = bigramLastWordCount
    }
    
    if (gram == 3){
      lastWordCount = trigramLastWordCount
    }
    
    if (gram == 4){
      lastWordCount = quadgramLastWordCount
    }
    
    if (gram == 5){
      lastWordCount = quintegramLastWordCount
    }
    lastWords = stri_extract_last_boundaries(ngrams)
    return((lastWordCount[lastWords]/nrow(gramCounts))*lambdaVal)
  }
  
  gramScorer <- function(results, gram, gramCounts, lambdaVal){ # evaluates scores for a given gram length
    results$firstTerm = firstTerm(results, gram)
    results$pVals = pCont(lambdaVal, results, gramCounts, gram)
    results$scores = results$firstTerm + results$pVal
    results$gram = stri_extract_last_boundaries(results$gram)
    return(results)
  }
  
  knsScorer <- function(input){
    if (input == ""){ return("")}
    input = tolower(gsub("\\!|\\.|\\,|\\?", "", input)) #takes out punctuation
    splitWords = strsplit(input, ' ')[[1]]
    scores = data.frame(gram=character(0), n=character(0))
    maxI = 5
    if (length(splitWords) < 5){
      maxI = length(splitWords) + 1
    }
    for (i in seq(maxI, 2, -1)){
      #finds which substrings to search up
      currentSet = paste(c(splitWords[(length(splitWords)-i+2):(length(splitWords))], ''), collapse= ' ')
      
      if (i == 2){ #bigrams
        biResults = bigramCounts[str_starts(bigramCounts$gram, currentSet), ]
        if (nrow(biResults) == 0){ return("nothing found")}
        else{
          lambdaVal = lambda(biResults, 2)
          s = gramScorer(biResults, 2, bigramCounts, lambdaVal)
          return(s)
        }
        
      }
      
      if (i == 3){ #trigrams
        triResults = trigramCounts[str_starts(trigramCounts$gram, currentSet), ]
        if (nrow(triResults) > 0){
          lambdaVal = lambda(triResults, 3)
          s = gramScorer(triResults, 3, trigramCounts, lambdaVal)
          return(s)
        }
        
      }
      
      if (i == 4){ #quadgrams
        quadResults = quadgramCounts[str_starts(quadgramCounts$gram, currentSet), ]
        if (nrow(quadResults) > 0){
          lambdaVal = lambda(quadResults, 4)
          s = gramScorer(quadResults, 4, quadgramCounts, lambdaVal)
          return(s)
        }
      }
      
      if (i == 5){ #quintegram
        quinteResults = quintegramCounts[str_starts(quintegramCounts$gram, currentSet), ]
        if (nrow(quinteResults) > 0){
          lambdaVal = lambda(quinteResults, 5)
          s = gramScorer(quinteResults, 5, quintegramCounts, lambdaVal)
          return(s)
        }
      }
    }
  }
  
  ## FOR MODIFIED MARKOV CHAIN MODEL
  
  #grams without stop word as last word
  markovBigramCounts = readRDS("data/nGrams/2gramsMarkov.rds")
  markovTrigramCounts = readRDS("data/nGrams/3gramsMarkov.rds")
  markovQuadgramCounts = readRDS("data/nGrams/4gramsMarkov.rds")
  markovQuintegramCounts = readRDS("data/nGrams/5gramsMarkov.rds")
  
  scorer <- function(results, gram){
    #uses maximum likelihood estimation
    sumFreq = sum(results$n)
    results$n = results$n / sumFreq #normalizes each word
    results$gram = stri_extract_last_boundaries(results$gram)
    rownames(results) = results$gram
    return(results)
  }
  
  mergeScores <- function(scores, merger, factor){ #merges two score dataframes together
    for (row in rownames(merger)){ 
      if (row %in% rownames(scores)){ #adds score if word common in both dataframes
        scores[row, 2] = scores[row, 2] + merger[row, 2] * factor
      }else{ #adds score if word not present in score dataframe
        scores = rbind(scores, data.frame(gram = c(row), n = c(merger[row,2] * factor)))
        colnames(scores) = c('gram', 'n')
        rownames(scores)[nrow(scores)] = row
      }
    }
    return(scores)
  }
  
  searcher <- function(input){
    input = tolower(gsub("\\!|\\.|\\,|\\?|\\]|\\[|\\>|\\<|\\@|\\#|\\$|\\%|\\^|\\&|\\*|\\(|\\)|\\+|\\=", "", input)) #takes out punctuation
    if (input == ""){return('')}
    splitWords = strsplit(input, ' ')[[1]]
    scores = data.frame(gram=character(0), n=character(0))
    maxI = 5
    if (length(splitWords) < 5){
      maxI = length(splitWords) + 1
    }
    
    for (i in 2:maxI){
      currentSet = paste(c(splitWords[(length(splitWords)-i+2):(length(splitWords))], ''), collapse= ' ')
      #finds which substrings to search up
      
      if (i == 2){ #bigrams
        biResults = markovBigramCounts[str_starts(markovBigramCounts$gram, currentSet), ]
        if (dim(biResults)[1] > 0){
          scores = scorer(biResults, 2)
        }
      }
      
      if (i == 3){ #trigrams
        triResults = markovTrigramCounts[str_starts(markovTrigramCounts$gram, currentSet), ]
        if (dim(triResults)[1] > 0){
          triScores = scorer(triResults, i)
          scores = mergeScores(scores, triScores, i-1)
        }
      }
      
      if (i == 4){ #quadgrams
        quadResults = markovQuadgramCounts[str_starts(markovQuadgramCounts$gram, currentSet), ]
        if (dim(quadResults)[1] > 0){
          quadScores = scorer(quadResults, i)
          scores = mergeScores(scores, quadScores, i-1)
        }
        
        
      }
      
      if (i == 5){ #quintegram
        quinteResults = markovQuintegramCounts[str_starts(markovQuintegramCounts$gram, currentSet), ]
        if (dim(quinteResults)[1] > 0){
          quinteScores = scorer(quinteResults, i)
          scores = mergeScores(scores, quinteScores, i-1)
        }
        
      }
      
    }
    if (nrow(scores) == 0){return("nothing found")}
    
    return(scores)
  }
  
  printScoresK <- function(s){
    checker = toString(s[1])
    if (checker == ""){ return("Nothing Entered!")}
    if (checker == "nothing found"){
      return("No prediction :(")
    }
    s <- s[order(s$scores, decreasing = TRUE),]
    s <- s[1:4, 1]
    s <- s[!is.na(s)]
    return(s)
  }
  
  printScoresM <- function(s){
    checker = toString(s[1])
    if (checker == ""){ return("Nothing Entered!")}
    if (checker == "nothing found"){
      return("No prediction :(")
    }
    s <- s[order(s$n, decreasing = TRUE),]
    s <- s[1:4, 1]
    s <- s[!is.na(s)]
    return(s)
  }
  
  
  output$knsOutput <- renderTable({
    t(printScoresK(suppressWarnings(knsScorer(input$text))))
  }, colnames = FALSE, rownames=FALSE, width='70%', bordered=TRUE, hover=FALSE, align = 'c')
  
  output$markovOutput <- renderTable({
    t(printScoresM(suppressWarnings(searcher(input$text))))
  }, colnames = FALSE, rownames=FALSE, width='70%', bordered=TRUE, hover=FALSE, align = 'c')  

    
    
    
  
  
  
  
}