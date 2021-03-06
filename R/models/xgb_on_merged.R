source('util.R')

DoXgb <- function(mtx.tr, mtx.test, ytr) {
  dmx.tr <- xgb.DMatrix(data = data.matrix(mtx.tr), label = ytr)
  dmx.test <- xgb.DMatrix(data = data.matrix(mtx.test))
  
  reg <- xgboost(data = dmx.tr, max.depth = 8, eta = .05, nround = 300,
                 objective = "reg:linear", verbose = 1, print.every.n = 10,
                 subsample = 0.95, colsample_bytree = 0.8)
  
  predict(reg, dmx.test)
}

GenFeatsAndDoXgb <- function(dt.tr, dt.test, ytr, storemeta) {
  dt <- rbind(dt.tr, dt.test)
  dt <- LabelEncode(dt, dt$StateHoliday, 'holtype_int')
  dt <- LabelEncode(dt, dt$StoreType, 'stype_int')
  dt <- LabelEncode(dt, dt$Assortment, 'asrtm_int')
  
  for (st in storemeta$Store) {
    st.row <- storemeta[Store==st]
    
    if (!is.na(st.row$CompetitionOpenSinceYear)) {
      comp.opendate <- as.Date(ymd(paste0(st.row$CompetitionOpenSinceYear, '-', 
                              st.row$CompetitionOpenSinceMonth, '-15')))
      dt[Store==st, CompOpen := as.integer(Date-comp.opendate)]
    } else {
      dt[Store==st, CompOpen := as.integer(-9999)]
    }
    
    # if (!is.na(st.row$Promo2SinceYear)) {
    #   promo2.opendate <- as.Date(as.POSIXlt(paste(st.row$Promo2SinceYear, 
    #                                       st.row$Promo2SinceWeek, '3'), 
    #                                 format = "%Y %U %u"))
    #   dt[Store==st, Promo2Flag := ifelse(Date>=promo2.opendate, 
    #                                  as.integer(Date-promo2.opendate), 99999)]
    # } else {
    #   dt[Store==st, Promo2Flag := 0]
    # }
  }
  
  dt[is.na(CompetitionDistance), CompetitionDistance := 99999]
  
  col.list <- c('Open', 'Promo', 'SchoolHoliday', 'Store',
                'CompOpen', 'CompetitionDistance', 'holtype_int',
                'DayOfWeek', 'DayOfMonth', 'Month', 'stype_int',
                'asrtm_int')
  
  dt <- dt[,col.list, with=F]
  
  dt.tr <- dt[1:(nrow(dt.tr))]
  dt.test <- dt[(nrow(dt.tr)+1):nrow(dt)]
  
  #mtx.tr <- AddFeatInteractions(dt.tr)
  #mtx.test <- AddFeatInteractions(dt.test)
  
  mtx.tr <- data.matrix(dt.tr)
  mtx.test <- data.matrix(dt.test)
  
  DoXgb(mtx.tr, mtx.test, ytr)
}


# ================================================
# Leaderboard
# ================================================
model.lb <- function(train, test, storemeta, curr.model){
  print(paste0('======= ', curr.model, '======='))
  set.seed(0)
  
  if (length(stores) < 200) {
    print('Restricting train/test/store to stores list')
    train <- train[Store %in% stores]
    test <- test[Store %in% stores]
    store <- store[Store %in% stores]
  } else {
    print('Will use full train/test/store')
  }

  dt.result <- data.table(Id=numeric(0), store=numeric(0))
  dt.result[, eval(curr.model) := numeric(0)]
  
  setkey(train, Store)
  setkey(test, Store)
  setkey(storemeta, Store)
  
  dt.tr <- train[storemeta, nomatch=0]
  dt.test <- test[storemeta, nomatch=0]
  
  setorder(dt.tr, Date, Store)
  setorder(dt.test, Date, Store)
  
  ytr.log <- log1p(dt.tr[, Sales])
  
  test.id <- dt.test$Id
  test.store <- dt.test$Store
  
  dt.tr <- dt.tr[,-c('Sales', 'Customers'), with=FALSE]
  dt.test <- dt.test[,-c('Id'), with=FALSE]
  
  ypred.log <- GenFeatsAndDoXgb(dt.tr, dt.test, ytr.log, storemeta)
  ypred <- expm1(ypred.log)
  
  dt.result <- rbind(dt.result, list(test.id, test.store,
                                     as.numeric(ypred)))
  
  setkey(dt.result, Id, store)
  dt.result
}

# ================================================
# Cross Validation
# ================================================
model.cv <- function(train, test, storemeta, curr.model){
  print(paste0('======= ', curr.model, ' ======='))
  set.seed(0)

  # Restrict data to the stores we are interested in.
  # For the CV run right before LB, set stores to entire set of train stores.
  # During LB, we will pick the model-store combinations that we are interested in.
  train <- train[Store %in% stores]
  test <- test[Store %in% stores]
  store <- store[Store %in% stores]
  
  modelres.rmspe <- data.table(store=numeric(0))
  modelres.rmspe[, eval(curr.model) := numeric(0)]
  
  modelres.ypred <- data.table(store=numeric(0), dateid=numeric(0), yreal=numeric(0))
  modelres.ypred[, eval(curr.model) := numeric(0)]
  
  #train <- train[Open==1 & Sales!=0] # get only the open days
  
  setkey(train, Store)
  setkey(storemeta, Store)
  
  dt <- train[storemeta, nomatch=0]

  setorder(dt, Date, Store)
  n <- nrow(dt)
  nstores <- length(unique(train$Store))
  
  dt.tr <- dt[1:(n-(48*nstores))]
  dt.test <- dt[(n-(48*nstores)+1):n]
  
  ytr.log <- log1p(dt.tr[, Sales])
  ytest <- dt.test[, Sales]
  
  ypred.log <- GenFeatsAndDoXgb(dt.tr, dt.test, ytr.log, storemeta)
  ypred <- expm1(ypred.log)
  
  net.rmspe <- ComputeRmspe(ytest, ypred)[[1]]
  
  setkey(dt.test, Store)
  setorder(dt.test, Date, Store)
  for (j in seq_along(stores)) {
    i <- stores[j]
    print(sprintf('%d. %s-CV: Store #%d',j, curr.model, i))
    
    sel <- dt.test$Store==i
    ytest.st <- ytest[sel]
    ypred.st <- ypred[sel]
    
    rmspe.st <- ComputeRmspe(ytest.st, ypred.st)
    modelres.rmspe <- rbind(modelres.rmspe, list(i, rmspe.st[[1]]))
    modelres.ypred <- rbind(modelres.ypred, 
                            list(rep(i, length(ytest.st)), seq_along(ytest.st), 
                                 ytest.st, as.numeric(ypred.st)))
   
    if (do.plot.cv) {
      plot(ytest.st, type='o', col=1, 
           main=sprintf('%s #%d, %f', curr.model, i, rmspe.st[[1]]))
      lines(seq_along(ypred.st), ypred.st, type='l', col=2)
      legend('topleft', lty=1, col=c(1,2), 
             c('actual', 'pred'), bg='transparent', cex=0.35)
    }
  }
  
  print(sprintf('Net rmspe for model: %s = %.3f', curr.model, 100*net.rmspe))  
  
  setkey(modelres.rmspe, store)
  setkey(modelres.ypred, store, dateid, yreal)
  list(modelres.rmspe, modelres.ypred, net.rmspe)
}
