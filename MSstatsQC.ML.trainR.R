#' A function to train random forest classifiers for QC data
#'
#' @param guide.set comma-separated (.csv), metric file. It should contain a "peptide" column and the metrics columns. It should also include "Annotations" for each run.
#' @param Test.set comma-separated (.csv), metric file. It should contain a "peptide" column and the metrics columns. It should also include "Annotations" for each run.
#' @param peptide the name of peptide of interest.
#' @param method the method used to model. Two values can be assigned, "randomforest" or "neuralnetwork".
#' @export
#' @import caret ggplot2 MASS dplyr
#' @import h2o
#' @examples
#' # First process the data to make sure it's ready to use
#' sampleData <- MSstatsQC::DataProcess(S9Site54)
#' head(sampleData)
#' # Find the name of the peptides
#' levels(sampleData$Precursor)
#' # Calculate change point statistics
#' QcClassifierTrain(guide.set = sampleData[1:20,])

MSstatsQC.ML.trainR<- function(guide.set, use_simulation, sim.size, address="", guide.set.annotations= NULL){
  
  source("auto_add_features.R")
  source("sample_density_function.R")
  source("boxcox_transformation.R")
  source("robust_scaling.R")
  source("MSstatsQC.ML.train_data.R")
  
  print("everything ok1")
  
  #function inputs
  nmetric<-ncol(guide.set)-3
  factor.names = colnames(guide.set[,4:ncol(guide.set)])
  #optional 
  guide.set$Precursor <-as.factor(guide.set$Precursor)
  
  print("everything ok2")
  
  #optional 
  peptide.colname <-"Precursor"
  
  if (use_simulation){
  d1 <- QcClassifier_data_step(guide.set,nmetric,factor.names,sim.size*1,peptide.colname, L=3, U=10)
  d2 <- QcClassifier_data_var(guide.set,nmetric,factor.names,sim.size*1,peptide.colname, L=3, U=10)
  d3 <- QcClassifier_data_linear(guide.set,nmetric,factor.names,sim.size*1,peptide.colname, L=3, U=10)
  
  d4 <- QcClassifier_data_step(guide.set,nmetric,factor.names,sim.size*1,peptide.colname, L=-10, U=-3)
  d5 <- QcClassifier_data_var(guide.set,nmetric,factor.names,sim.size*1,peptide.colname, L=-10, U=-3)
  d6 <- QcClassifier_data_linear(guide.set,nmetric,factor.names,sim.size*1,peptide.colname, L=-10, U=-3)
  
  ## Appending all simulations
    if (is.null(dim(guide.set.annotations))==TRUE){
      d<-rbind(d1,d2,d3,d4,d5,d6)
      print("everything ok3")
      
      
    }
  
    else{
      d7 <- QcClassifier_data_annotated(guide.set, guide.set.annotations)
      d <- rbind(d1,d2, d3, d4,d5,d6, d7)
      print("everything ok4")
    }
  }
  else{

    RESPONSE <- guide.set$Annotations
    guide_feature_df <- add_features(guide.set[,4:ncol(guide.set)])
    guide_feature_df <- guide_feature_df[,order(names(guide_feature_df))]
    guide_feature_df <- cbind(guide_feature_df,RESPONSE)


    guide.set.annotations <-  guide.set.annotations %>%  dplyr::filter(guide.set.annotations$Annotations == 'FAIL')

    anno_feature_df <- QcClassifier_data_annotated(guide.set, guide.set.annotations)


    full_feature_df <- rbind(guide_feature_df, anno_feature_df)


    df <- rbind(guide.set,guide.set.annotations)

    RESPONSE <- df$Annotations


    full_feature_df <-  cbind(df[4:ncol(df)], RESPONSE)
    print("everything ok5")

    d <-  full_feature_df
  }
  

  ## 80% of the sample size
  smp_size <- floor(0.8 * nrow(d))
  
  set.seed(123)
  train_ind <- sample(seq_len(nrow(d)), size = smp_size)
  
  train <- d[train_ind,]
  validation <- d[-train_ind,]
  
  #launch h2o cluster
  # localH2O <- h2o.init(nthreads = -1)
  
  #import r objects to h2o cloud
  train_h2o <- as.h2o(train)
  validation_h2o <- as.h2o(validation)
  print(train_h2o)
 
  
  ## run our first predictive model
  rf_model <- h2o.randomForest(
    x = colnames(train_h2o),
    y = train_h2o$RESPONSE,                 ## h2o.randomForest function
    training_frame = train_h2o,             ## the H2O frame for training
    model_id = "rf_model",
    validation_frame = validation_h2o,      ## the H2O frame for validation (not required)## name the model in H2O
    nfolds = 10,
    ntrees = 100,                           
    stopping_rounds = 2,                    ## Stop fitting new trees when the 2-tree average is within 0.001 (default) of the prior two 2-tree averages.Can be thought of as a convergence setting
    score_each_iteration = TRUE,
    max_depth = 20,
    seed = 123
    )
  print("everything ok7")
  
  message(paste("Constructed the RF model"))
  # print(rf_model)
  return(rf_model)
  
 
}


