source('util.R')

DoXgbOnResiduals <- function(mtx.tr, mtx.test, ytr.res) {
  dmx.tr <- xgb.DMatrix(data = data.matrix(mtx.tr), label = ytr.res)
  dmx.test <- xgb.DMatrix(data = data.matrix(mtx.test))
  
  param <- list(objective = "reg:linear",
                eta = 0.01,
                max_depth = 5,
                subsample = 0.95,
                colsample_bytree = 0.8)
  
  reg <- xgb.train(params = param, 
                   data = dmx.tr, 
                   nrounds = 300,
                   verbose = 1)
  
  predict(reg, dmx.test)
}

FitResiduals <- function(dt.tr, dt.test, ytr.res) {
  dt <- rbind(dt.tr, dt.test)
  GenDummy(dt, dt$StateHoliday, 'hol_type_')
  GenDummy(dt, dt$DayOfWeek, 'dow_')
  GenDummy(dt, dt$DayOfMonth, 'dom_')
  GenDummy(dt, dt$Month, 'mon_')
  
  col.list <- c('Open', 'Promo', 'SchoolHoliday')
  all.cols <- names(dt)
  col.list <- c(col.list, all.cols[grep("^hol_type_", all.cols)])
  col.list <- c(col.list, all.cols[grep("^dow_", all.cols)])
  col.list <- c(col.list, all.cols[grep("^dom_", all.cols)])
  col.list <- c(col.list, all.cols[grep("^mon_", all.cols)])
  
  dt <- dt[,col.list, with=F]
  
  dt.tr <- dt[1:(nrow(dt.tr))]
  dt.test <- dt[(nrow(dt.tr)+1):nrow(dt)]
  
  mtx.tr <- AddFeatInteractions(dt.tr)
  mtx.test <- AddFeatInteractions(dt.test)
  
  DoXgbOnResiduals(mtx.tr, mtx.test, ytr.res)
}

ProcessOneStore <- function(dt.tr, dt.test, xtr, xtest, ytr) {
  store.lm <- lm(ytr ~ x + I(x^2), data=xtr)
  ytest.predict <- predict(store.lm, newdata=xtest)
  ytr.fit <- fitted(store.lm)
  ytr.fit <- replace(ytr.fit, ytr.fit>-1&ytr.fit<1, 1)  # replace (-1,1) with 1
  
  ytr.res <- ytr/ytr.fit
  
  ytest.res <- FitResiduals(dt.tr, dt.test, ytr.res)
  
  list(ytest.predict * ytest.res, ytest.predict, ytest.res)
}

# ================================================
# Leaderboard
# ================================================
model.lb <- function(train, test, store, curr.model){
  print(paste0('======= ', curr.model, '======='))
  set.seed(0)
  
  dt.result <- data.table(Id=numeric(0), store=numeric(0))
  dt.result[, eval(curr.model) := numeric(0)]
  
  for (j in seq_along(stores)) {
    i <- stores[j]
    print(sprintf('%d. %s-LB: Store #%d',j, curr.model, i))
    
    dt.tr <- train[Store==i]
    setorder(dt.tr, Date)
    dt.tr <- AlignOverMissingDays(dt.tr)
    
    dt.test <- test[Store==i]
    setorder(dt.test, Date)
    
    ytr <- dt.tr$Sales
    
    n.tr <- nrow(dt.tr)
    n.test <- nrow(dt.test)
    xtr <- data.table(x=1:(n.tr))
    xtest <- data.table(x=(n.tr+1):(n.tr+n.test))
    
    test.id <- dt.test$Id
    
    dt.tr <- dt.tr[,-c('Sales', 'Customers'), with=FALSE]
    dt.test <- dt.test[,-c('Id'), with=FALSE]
    
    result <- ProcessOneStore(dt.tr, dt.test, xtr, xtest, ytr)
    ypred <- result[[1]]
    ypred.trend <- result[[2]]
    ypred.res <- result[[3]]
    
    dt.result <- rbind(dt.result, list(test.id, rep(i, length(ypred)),
                                       as.numeric(ypred)))
  }
  
  setkey(dt.result, Id, store)
  dt.result
}

# ================================================
# Cross Validation
# ================================================
model.cv <- function(train, test, store, curr.model){
  print(paste0('======= ', curr.model, ' ======='))
  set.seed(0)
  errsums <- vector()
  counts <- vector()
  
  modelres.rmspe <- data.table(store=numeric(0))
  modelres.rmspe[, eval(curr.model) := numeric(0)]
  
  modelres.ypred <- data.table(store=numeric(0), dateid=numeric(0), yreal=numeric(0))
  modelres.ypred[, eval(curr.model) := numeric(0)]
  
  for (j in seq_along(stores)) {
    i <- stores[j]
    print(sprintf('%d. %s-CV: Store #%d',j, curr.model, i))
    
    dt <- train[Store==i]
    setorder(dt, Date)
    
    dt <- AlignOverMissingDays(dt)
    
    n <- nrow(dt)
    
    ytr <- dt[1:(n-48), Sales]
    ytest <- dt[(n-48+1):n, Sales]
    
    xtr <- data.table(x=1:(n-48))
    xtest <- data.table(x=(n-48+1):n)
    
    dt.tr <- dt[1:(n-48)]
    dt.test <- dt[(n-48+1):n]
    
    result <- ProcessOneStore(dt.tr, dt.test, xtr, xtest, ytr)
    ypred <- result[[1]]
    ypred.trend <- result[[2]]
    ypred.res <- result[[3]]
    
    res.spe <- ComputeRmspe(ytest, ypred)
    modelres.rmspe <- rbind(modelres.rmspe, list(i, res.spe[[1]]))
    modelres.ypred <- rbind(modelres.ypred, 
                            list(rep(i, length(ypred)), seq_along(ypred), 
                                 ytest, as.numeric(ypred)))
    errsums <- c(errsums, res.spe[[2]])
    counts <- c(counts, res.spe[[3]])
    print(sprintf('rmspe = %f', res.spe[[1]]))
    
    if (do.plot.cv) {
      plot(ytest, type='o', col=1, 
           main=sprintf('%s #%d, %f', curr.model, i, res.spe[[1]]))
      lines(seq_along(ypred), ypred, type='l', col=2)
      lines(seq_along(ypred.trend), ypred.trend, type='l', lty=2, col=3)
      lines(seq_along(ypred), 500*dt.test$Open, type='p', col=4)
      lines(seq_along(ypred), 1000*dt.test$Promo, type='p', col=5)
      lines(seq_along(ypred), 1500*dt.test$SchoolHoliday, type='p', col=6)
      legend('topleft', lty=1, col=c(1,2,3,4,5,6), 
             c('actual', 'pred', 'pred stl', 'open', 'promo', 'school-hol'), 
             bg='transparent', cex=0.35)
    }
  }
  
  net.rmspe <- sqrt(sum(errsums)/sum(counts))
  print(sprintf('Net rmspe for model: %s = %.3f', curr.model, 100*net.rmspe))
  
  setkey(modelres.rmspe, store)
  setkey(modelres.ypred, store, dateid, yreal)
  list(modelres.rmspe, modelres.ypred, net.rmspe)
}