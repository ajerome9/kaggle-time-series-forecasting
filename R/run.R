source('util.R')

all.data <- read.data()
train <- all.data[[1]]; test <- all.data[[2]]; store <- all.data[[3]]

# ============ Config =============
do.plot.cv <- FALSE
do.incremental.ensemble <- TRUE

# ============ Models =============
model.namelist <- c(  'xgb_on_merged'
                    #, 'lm.res_xgb_no_intfeat'
                    , 'tslm.res_xgb'
                    , 'lm.res_xgb'
                    #, 'lm.res_glmnet'
                    , 'per_store_xgb'
                    , 'xgb_on_merged_3000'
                    , 'tslm.res_glmnet'
                    , 'lm.res_glmnet.b'
                    , 'xgb_on_merged.gt_loc_idfeats'
                    #, 'stl.res_xgb'
                    #, 'lm.res_merged_xgb'
                    , 'xgb_on_merged.b'
                    )
model.namelist <- c('xgb.sim.n')

# ============ Stores =============
#stores <- c(1,3)
#stores <- stores.py
#set.seed(0);stores <- sample(unique(train$Store), 99);
stores <- unique(train$Store)
#stores <- unique(test$Store)


# ============ Cross Validation =============
RunCV <- function() {
  model.scores <- 'Model scores'
  
  res.all <- LoadCVResults()
  res.rmspe <- res.all[[1]]
  res.ypred <- res.all[[2]]
  res.cvsummary <- res.all[[3]]
  
  
  for (model.name in model.namelist) {
    if (!model.name %in% colnames(res.rmspe)) {
      source(paste0('models/', model.name, '.R'))
      model.res <- model.cv(copy(train), copy(test), copy(store), model.name)
  
      model.scores <- paste0(model.scores, ', ', model.name, ' = ', 
                             sprintf('%.3f',100*model.res[[3]]))
      
      if (nrow(res.rmspe) == 0) {
        res.rmspe <- model.res[[1]]
        res.ypred <- model.res[[2]]
      } else {
        res.rmspe <- res.rmspe[model.res[[1]], nomatch=0]
        res.ypred <- res.ypred[model.res[[2]], nomatch=0]
      }
      res.cvsummary[, (model.name) := model.res[[3]], with=FALSE]
      
    } else {
      saved.model.rmspe <- res.cvsummary[summary==0, get(model.name)]
      print(sprintf('Using previous results for %s (%.3f)', 
                    model.name, 100*saved.model.rmspe))
      model.scores <- paste0(model.scores, ', ', model.name, ' = ', 
                             sprintf('%.3f',100*saved.model.rmspe))
    }
  }
  
  print('==========================')
  
  SaveCVResults(res.rmspe, res.ypred, res.cvsummary)
  
  res.ypred <- ComputeCombinedPred(res.ypred)
  net.rmspe <- ComputeRmspe(res.ypred$yreal, res.ypred$Sales)
  print(model.scores)
  print(sprintf('Net rmspe across models = %.3f, sum spe = %f, count = %d', 
                100*net.rmspe[[1]], net.rmspe[[2]], net.rmspe[[3]]))
  print('CV done')
  
  list(res.rmspe, res.ypred)
}

# ============ Leaderboard =============
RunLB <- function() {
  dt.result <- LoadLBResults()
  
  for (model.name in model.namelist) {
    if (!model.name %in% colnames(dt.result)) {
      source(paste0('models/', model.name, '.R'))
      model.res <- model.lb(copy(train), copy(test), copy(store), model.name)
  
      if (nrow(dt.result) == 0) {
        dt.result <- model.res
      } else {
        dt.result <- dt.result[model.res, nomatch=0]
      }
    } else {
      print(sprintf('Using previous results for %s', model.name))
    }
  }
  
  SaveLBResults(dt.result)
  
  dt.result <- ComputeCombinedPred(dt.result)
  
  write.csv(dt.result[, .(Id, Sales)], '../submissions/output.csv', row.names = F)
  
  print('LB done')
  
  dt.result
}


res <- RunCV()
#res <- RunLB()
