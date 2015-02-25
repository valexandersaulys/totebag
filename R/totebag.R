# TO-DO:
# --> Enable other metrics than accuracy to be usedas measurements when doing ensembles
# --> Rewrite when possible in C/C++
# --> Add classification support
# --> Add support for weighting models and/or some algorithm
# --> To use models without formulas

# Needed libraries -> library(caret) for createDataPartition() function

#' Create a new (empty) totebag object
#'
#' @param x matrix of features
#' @param y vector of labels / response values
#' @param type either "regression" or "classification", though the later is not supported at the moment
#' @param wreplacement either True or False, reflects selecting models with replacement or not
#' @param sorted.initialization a number that defines building an initial ensemble with the strongest initial models
#' @param bagged.selection the number of times to use bagged selection in model selection
#' @param hillclimb.ratio the number
#' @param p percentage of models to use during bagged selection
#' 
new.totebag <- function (x, y,
                         type="regression",
                         with.replacement=TRUE,
                         sorted.initialization=0,
                         bagged.selection=20,
                         hillclimb.ratio=0.4,
                         p=0.5 ) {
  x = as.matrix(x);
  type = "regression";
  
  data <- list(x=object$x, y=object$y);
  
  dataPartition <- createDataPartition(data, p=object$hillcimb.ratio, list = FALSE)[, 1];
  training <- data[dataPartition, ];
  hillclimb <- data[-dataPartition, ];
  
  object <- list(
    x=x,                 # Full X data
    y=y,                 # Full Y data
    type=type,
    with.replacement=with.replacement,
    sorted.initialization=sorted.initialization,
    bagged.selection=bagged.selection,
    hillclimb=hillclimb,
    p=p,                 # percentage of models list to use during Bagged Ensemble Selection
    k=k,                 # the number of times to do Bagged Ensemble Selection
    # Lists of added-parts
    training=training,   # to make sure all models are trained on the same models
    hillclimb=hillclimb, # to make sure all models are validated on the same validation data
    models=list(),       # holds the library of models
    model.args=list(),   # their corresponding model params
    model.pred.args=list(), # holds prediction preds for models
    models.finalized=list(), # holds the trained models
    y.preds=list(),      # the predicted y values on hillclimb
    asm=list()           # their corresponding accuracy/error scores
  );
  
  class(object) <- 'totebag';
  
  return(object);
}

#' Adds new model to the totebag ensemble
#' @param object the totebag object to be operated on
#' @param model model fitting the given data
#' @param train.params params during training
#' @param pred.params params during predicting
#' @param formula gives the formula to be used during training
add.totebag <- function(object,model,train.params,pred.params,formula) {
  if (!is.null(formula)) {    # Currently to be implemented
    # First we define a variable to represent the length of our R models
    to.be.added <- length(object$models) + 1;
    
    # Then we add the model to the list of models to be utilized
    object$models[[to.be.added]] <- model;
    object$model.args[[to.be.added]] <- train.params;
    object$model.pred.args[[to.be.added]] <- pred.params;
    
    # Train the model
    # Make predictions on the hillclimb ratio
    # Put those prediction percentages into asm
    
    command <- as.character(model) 
    current.model <- do.call(command,c(object$training,train.params))
    
    command <- as.character(model) + ".predict"
    current.predictions <- do.call(command, c(object$hillcimb))
    
    object$y.preds <- current.predictions
    if (type=='classification') {
      object$asm <- confusionMatrix(data=current.predictions,reference=hillclimb)$overall
    } else {# if (type=="regression") {  # Defaults to regression
      object$asm <- (1 - mean(error.totebag(current.predictions,hillclimb)))
    } 
  } else {    # We have a formula to pass instead of an X and Y value
    # First we define a variable to represent the length of our R models
    to.be.added <- length(object$models) + 1;
    
    # Then we add the model to the list of models to be utilized
    object$models[[to.be.added]] <- model;
    object$model.args[[to.be.added]] <- train.params;
    object$model.pred.args[[to.be.added]] <- preds.params;
    
    # Train the model
    # Make predictions on the hillclimb ratio
    # Put those prediction percentages into asm
     
    current.model <- do.call(as.character(model),c(formula,data=object$training,train.params))
    current.predictions <- do.call(command, c(formula,newdata=object$hillcimb,pred.params))
    
    object$models.finalized[[to.be.added]] <- current.model
    object$y.preds[[to.be.added]] <- current.predictions
    
    if (type=='classification') {
      object$asm[[to.be.added]] <- confusionMatrix(data=current.predictions,reference=hillclimb)$overall
    } else {# if (type=="regression") {  # Defaults to regression
      object$asm[[to.be.added]] <- (1 - mean(error.totebag(current.predictions,hillclimb)))
    } 
  } 
}

#' Predicts newdata with totebag ensemble model
#'
#' @param newdata our new data
#' @param percentage the % of models to use
predict.totebag <- function(object,
                            newdata,
                            percentage
                            ) {
  if (object$bagged.selection==TRUE) {
    bagged.pred.totebag(object,newdata,percentage);
  } else {
    nonbagged.pred.totebag(object,newdata,percentage);
  }
  
}

#' Only meant for internal package use. Totebag prediction with Bagged Selection
#' @param object
#' @param newdata
#' @param percentage
bagged.pred.totebag <- function(object, newdata, percentage) {
  
  avg.predictions <- matrix(data=NA,nrow=nrow(newdata),ncol=object$k)
  
  for (i in 1:object$k) {
      best.model <- list()
      newdata <- as.matrix(newdata);
          
      # Bagged Ensemble Selection:
      # sample(x,size) will randomly create vectors for sampling
      # where x is an integer and sampling takes place from 1 to x, size is the size of the list it returns
      positions <- sample(nrow(object$models),object$p * nrow(object$models))
            
      if (object$sorted.initialization > 1) {     # This needs to be fleshed out
         n <- length(object$asm);
         g <- object$sorted.initialization - 1;
         sort(object$asm,partial=n-g)[n-g];    # This returns the next highest value
      } else {
         g <- which.max(object$asm[positions,]);
         best.models[[0]] <- object$models[[g]];
         ACC <- object$asm[[g]] # Stores accuracy so far
         mlocs[[0]] <- g;    # Stores locations of the models being used
      }
        
        
      while (length(best.models) < (percentage * object$models)) {
          # Loop through and compare
          for (i in 1:length(object$models[positions,])) {
              temp.locs <- c(mlocs,i);
              if (object$type=='classification') { temp.acc = clas.pred.totebag(object,temp.locs); }
              else { temp.acc = reg.pred.totebag(object,temp.locs); }  # else, its a regression

              if (temp.acc > ACC) { mlocs = temp.locs; ACC = temp.acc; } # if its better, replace!
        }
      }
      
      # For holding all of our final predictions
      final.predictions <- matrix(data=NA,nrow=length(newdata),ncol=length(l.locs));
      running.predictions <- matrix(data=NA,nrow=length(newdata),ncol=1);
      
      for (i in 1:length(best.models)) {
        g <- mlocs[[i]];
        command <- object$models.finalized[[g]];
        final.predictions[,i] <- do.call('predict',c(command,newdata,object$model.pred.args[[g]]));
        # It might be good to have a running part here that can add together values and add weighting
      }
      
      # For now, a very rough averaging of the rows
      avg.predictions[,i] <-  rowMeans(final.predictions)
  }

  # return the average of all predictions from bagging
  return(rowMeans(avg.predictions))
}

#' Only meant for internal package use. Totebag prediction without bagged selection.
#' @param object
#' @param newdata
#' @param percentage
nonbagged.pred.totebag <- function(object,newdata,percentage) {
  best.model <- list()
  newdata <- as.matrix(newdata);
  
  if (object$sorted.initialization > 1) {     # This needs to be fleshed out
    n <- length(object$asm);
    g <- object$sorted.initialization - 1;
    sort(object$asm,partial=n-g)[n-g];    # This returns the next highest value
  }
  else {
    g <- which.max(object$asm);
    best.models[[0]] <- object$models[[g]];
    ACC <- object$asm[[g]] # Stores accuracy so far
    mlocs[[0]] <- g;    # Stores locations of the models being used
  }
  
  
  while (length(best.models) < (percentage * object$models)) {
    # Loop through and compare
    for (i in 1:length(object$models)) {
      temp.locs <- c(mlocs,i);
      if (object$type=='classification') { temp.acc = clas.pred.totebag(object,temp.locs); }
      else { temp.acc = reg.pred.totebag(object,temp.locs); }
      
      if (temp.acc > ACC) { mlocs = temp.locs; ACC = temp.acc; } # if its better, replace!
    }
  }
}

#' Only meant for internal package use. Regression prediction in totebag.
#' @param l.locs list of locations of the models in the object
#' @param object totebag object
reg.pred.totebag <- function(object, l.locs) {
  
  # declare temp.preds as a matrix of m x n (NA values)
  # where m is the length l.locs
  # and n is the length of hillclimb
  temp <- matrix(data=NA,nrow=length(object$hillclimb),ncol=length(l.locs))
  
  for (i in 1:length(l.locs)) { 
    # each location list of y.preds is given column i in temp.preds
    temp[[i]] <- object$y.preds[[ l.locs[i] ]]
  }
  # take the average of the rows and put into a separate list of values
  temp.preds <- rowMeans(temp)
  # measure the error with the below function
  temp.error <- error.totebag(temp.preds,object$hillclimb)
  # return 1 - average error rate --> accuracy rate
  return(1 - colMeans(temp.error))    # check during debugging that this is one value
}

#' Only meant for internal package use. Private function for finding error.
#' @param true.vals list of true values
#' @param pred.vals list of predicted values
error.totebag <- function(pred.vals,true.vals) {
  error <- (true.vals - pred.vals)^2
  return(error)
}
