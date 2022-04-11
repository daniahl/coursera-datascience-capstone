#Read data from the three files. Use the multiplier to control huw much data is read.
read_data <- function(multiplier) {
  n <- readLines("../en_US/en_US.news.txt", n=16000*multiplier)
  b <- readLines("../en_US/en_US.blogs.txt", n=14000*multiplier)
  t <- readLines("../en_US/en_US.twitter.txt", n=30000*multiplier)
  input <- concatenate(n, b, t)
  preprocess(input, case="lower", remove.punct=T)
  return(input)
}

#Builds an n-gram model. For the deployed application, 2-, 3-, and 4-gram models are built and then saved to an RData file.
build_model <- function(text, n=2) {
  ng <- ngram(text, n)
  ngrams <- get.phrasetable(ng)$ngrams
  return(ngrams)
}

#This function is used by the predict function to perform one prediction with a specific model.
predict_next <- function(model, n, input) {
  if (length(unlist(strsplit(input, " ")))!=(n-1)) return(NA)
  matches <- model[startsWith(model, input)]
  pred_split <- unlist(strsplit(matches[1], " "))
  suppressWarnings({
    if (is.na(pred_split)) return("")
  })
  
  return(pred_split[length(pred_split)])
}

#Predict the next word in input, given the models m2, m3, and m4.
predict <- function(m2, m3, m4, input) {
  input <- preprocess(input, case="lower", remove.punct=T)
  sp <- unlist(strsplit(input, " "))
  pred_input <- ""
  pred <- ""
  if (length(sp)>3) pred_input <- paste(sp[(length(sp)-2):length(sp)], collapse=" ")
  else pred_input <- input
  pred <- predict_next(m4, 4, pred_input)
  if (!is.na(pred)) return(pred)
  pred <- predict_next(m3, 3, pred_input)
  if (!is.na(pred)) return(pred)
  pred <- predict_next(m2, 2, pred_input)
  if (!is.na(pred)) return(pred)
}

#Evaluate the prediction algorithm, given models m2, m3, m4, and text input.
evaluate <- function(m2, m3, m4, input) {
  input <- preprocess(input, case="lower", remove.punct=T)
  sp <- unlist(strsplit(input, " "))
  right = 0
  for (i in 4:length(sp)) {
    pred_inp <- paste(sp[(i-3):(i-1)], collapse=" ")
    pred <- predict(m2,m3,m4,pred_inp)
    if (pred == sp[i]) right <- right + 1
  }
  acc <- right/(length(sp)-4)
  return(acc)
}