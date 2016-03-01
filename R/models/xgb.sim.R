source('util.R')

DoXgb <- function(mtx.tr, mtx.test, ytr, ytest=NULL) {
  dmx.tr <- xgb.DMatrix(data = data.matrix(mtx.tr), label = ytr)
  
  param <- list(objective        = "reg:linear", 
                booster          = "gbtree",
                eta              = 0.015,
                max_depth        = 18,
                subsample        = 0.7,
                colsample_bytree = 0.7
  )
  param.nrounds.cv = 3000
  param.nrounds.lb = 270  # set based on cv
  
  if (is.null(ytest)) {
    dmx.test <- xgb.DMatrix(data = data.matrix(mtx.test))
    watchlist <- list(train=dmx.tr)
    reg <- xgb.train(params = param, data = dmx.tr, nrounds = param.nrounds.lb, 
                     verbose = 1, print.every.n = 10, watchlist = watchlist)
  } else {
    dmx.test <- xgb.DMatrix(data = data.matrix(mtx.test), label = ytest)
    watchlist <- list(test=dmx.test, train=dmx.tr)
    reg <- xgb.train(params = param, data = dmx.tr, nrounds = param.nrounds.cv,
                     verbose = 1, print.every.n = 10, watchlist = watchlist,
                     early.stop.round = 2, maximize=FALSE)
  }
  
  predict(reg, dmx.test)
}

GenFeatsAndDoXgb <- function(dt.tr, dt.test, ytr, storemeta, is.lb, ytest=NULL) {
  
  if (is.lb) {
    dt <- rbind(dt.tr[,-c('Sales', 'Customers'), with=FALSE], 
                dt.test[,-c('Id'), with=FALSE])
  } else {
    dt <- rbind(dt.tr, dt.test)
  }
  
  dt <- LabelEncode(dt, dt$StateHoliday, 'holtype_int')
  
  for (st in storemeta$Store) {
    st.row <- storemeta[Store==st]
    
    if (!is.na(st.row$CompetitionOpenSinceYear)) {
      comp.opendate <- as.Date(ymd(paste0(st.row$CompetitionOpenSinceYear, '-', 
                                          st.row$CompetitionOpenSinceMonth, '-15')))
      dt[Store==st, CompOpen := as.integer(Date-comp.opendate)]
    } else {
      dt[Store==st, CompOpen := as.integer(99999)]
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
  
  sid <- dt.tr[,.(SidM=mean(Sales), SidS=sd(Sales)), 
               by=.(Store, Month, DayOfWeek, Promo)]
  setkey(sid, Store, Month, DayOfWeek, Promo)
  setkey(dt, Store, Month, DayOfWeek, Promo)
  dt <- dt[sid, nomatch=0]
  
  aid <- dt.tr[,.(AidM0=mean(Sales)), 
               by=.(Assortment)]
  setkey(aid, Assortment)
  setkey(dt, Assortment)
  dt <- dt[aid, nomatch=0]
  
  aid <- dt.tr[,.(AidM=mean(Sales), AidS=sd(Sales)), 
               by=.(Assortment, State, Month, Promo)]
  setkey(aid, Assortment, State, Month, Promo)
  setkey(dt, Assortment, State, Month, Promo)
  dt <- dt[aid, nomatch=0]
  
  styid <- dt.tr[,.(StyidM0=mean(Sales)), 
                 by=.(StoreType)]
  setkey(styid, StoreType)
  setkey(dt, StoreType)
  dt <- dt[styid, nomatch=0]
  
  styid <- dt.tr[,.(StyidM=mean(Sales), StyidS=sd(Sales)), 
                 by=.(StoreType, State, Month, Promo)]
  setkey(styid, StoreType, State, Month, Promo)
  setkey(dt, StoreType, State, Month, Promo)
  dt <- dt[styid, nomatch=0]
  
  stateid <- dt.tr[,.(StateidM0=mean(Sales)), 
                   by=.(State)]
  setkey(stateid, State)
  setkey(dt, State)
  dt <- dt[stateid, nomatch=0]
  
  stateid <- dt.tr[,.(StateidM=mean(Sales), StateidS=sd(Sales)), 
                   by=.(State, Month, Promo)]
  setkey(stateid, State, Month, Promo)
  setkey(dt, State, Month, Promo)
  dt <- dt[stateid, nomatch=0]
  
  sholid <- dt.tr[,.(SholidM0=mean(Sales)), 
                  by=.(StateHoliday)]
  setkey(sholid, StateHoliday)
  setkey(dt, StateHoliday)
  dt <- dt[sholid, nomatch=0]
  
  sholid <- dt.tr[,.(SholidM=mean(Sales)), 
                  by=.(StateHoliday, State, Month)]
  setkey(sholid, StateHoliday, State, Month)
  setkey(dt, StateHoliday, State, Month)
  dt <- dt[sholid, nomatch=0]
  
  dt[, ':=' (Year = year(Date), WeekOfYear = week(Date))]
  dt <- LabelEncode(dt, dt$StoreType, 'stype_int')
  dt <- LabelEncode(dt, dt$Assortment, 'asrtm_int')
  dt <- LabelEncode(dt, dt$PromoInterval, 'promo_ival_int')
  
  for (i in seq_along(dt)) {
    set(dt, i=which(is.na(dt[[i]])), j=i, value=0)
  }
  
  setorder(dt, Date, Store)
  
  col.list <- c('Open', 'Promo', 'SchoolHoliday', 'Store',
                'CompOpen', 'CompetitionDistance', 'holtype_int',
                'DayOfWeek', 'DayOfMonth', 'Month', 'Year', 'WeekOfYear',
                'SidM', 'SidS', 
                'AidM0', 'AidM', 'AidS', 
                'StyidM0', 'StyidM', 'StyidS',
                'StateidM0', 'StateidM', 'StateidS',
                'SholidM0', 'SholidM', 
                'rossmann', 'dm.drogerie.markt', 'geschenk',
                'CompetitionOpenSinceMonth', 'CompetitionOpenSinceYear',
                'Promo2', 'Promo2SinceWeek', 'Promo2SinceYear',
                'promo_ival_int')
  
  
  #   col.list <- c('Open', 'Promo', 'SchoolHoliday', 'Store',
  #                 'CompOpen', 'CompetitionDistance', 'holtype_int',
  #                 'DayOfWeek', 'DayOfMonth', 'Month', 'stype_int',
  #                 'asrtm_int', 'Year','WeekOfYear',
  #                 'CompetitionOpenSinceMonth', 'CompetitionOpenSinceYear',
  #                 'Promo2', 'Promo2SinceWeek', 'Promo2SinceYear',
  #                 'promo_ival_int')
  
  dt <- dt[,col.list, with=F]
  
  dt.tr <- dt[1:(nrow(dt.tr))]
  dt.test <- dt[(nrow(dt.tr)+1):nrow(dt)]
  
  # Filter open days in train/test
  tr.openfilter <- dt.tr$Open==1
  dt.tr.open <- dt.tr[tr.openfilter]
  y.tr.open <- ytr[tr.openfilter]
  
  es.openfilter <- dt.test$Open==1
  dt.es.open <- dt.test[es.openfilter]
  y.es.open <- ytest[es.openfilter]
  
  mx.tr <- data.matrix(dt.tr.open)
  mx.es <- data.matrix(dt.es.open)
  
  y.pr.open <- DoXgb(mx.tr, mx.es, y.tr.open, y.es.open)
  
  # Merge results by inserting pred=0 for closed days
  idx.y.pr <- 1
  merged.result <- vector(mode="numeric", length=nrow(dt.test))
  for (idx in seq_len(nrow(dt.test))) {
    open.status <- es.openfilter[idx]
    if (open.status) {
      merged.result[idx] <- y.pr.open[idx.y.pr]
      idx.y.pr <- idx.y.pr + 1
    } else {
      merged.result[idx] <- 0
    }
  }
  
  merged.result
}

ProcessOneStore <- function(dt.tr, dt.es, x.tr, x.es, y.tr) {
  store.lm <- lm(y.tr ~ x + I(x^2), data=x.tr)
  y.es.pred <- predict(store.lm, newdata=x.es)
  y.tr.fitd <- fitted(store.lm)
  #y.tr.fitd <- replace(y.tr.fitd, abs(y.tr.fitd-1)<1, 1)  # replace (-1,1) with 1
  
  y.tr.resi <- y.tr - y.tr.fitd
  
  list(y.es.pred, y.tr.resi)
}

ProcessOneStore2 <- function(dt.tr, dt.test, xtr, xtest, ytr) {
  ytest.pred <- rep(0, length(xtest))
  
  list(ytest.pred, ytr)
}

# ================================================
# Leaderboard
# ================================================
model.lb <- function(train, test, store, curr.model){
  print(paste0('======= ', curr.model, '======='))
  set.seed(0)
  
  all.tr <- data.table()
  all.test <- data.table()
  
  dt.result <- data.table(Id=numeric(0), store=numeric(0))
  dt.result[, eval(curr.model) := numeric(0)]
  
  for (j in seq_along(stores)) {
    i <- stores[j]
    #print(sprintf('%d. %s-LB: Store #%d',j, curr.model, i))
    
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
    
    result <- ProcessOneStore2(dt.tr[,-c('Sales', 'Customers'), with=FALSE], 
                               dt.test[,-c('Id'), with=FALSE], 
                               xtr, xtest, ytr)
    y.es.pred <- result[[1]]
    y.tr.resi <- result[[2]]
    
    all.tr <- rbind(all.tr, dt.tr[, Resi:=y.tr.resi])
    all.test <- rbind(all.test, dt.test[, Trend:=y.es.pred])
  }
  
  setkey(all.tr, Store)
  setkey(all.test, Store)
  setkey(store, Store)
  all.tr <- all.tr[store, nomatch=0]
  all.test <- all.test[store, nomatch=0]
  
  setorder(all.tr, Date, Store)
  setorder(all.test, Date, Store)
  
  all.y.tr.resi <- (all.tr[,Resi])
  y.es.pred.resi <- GenFeatsAndDoXgb(all.tr[,-c('Resi'), with=FALSE], 
                                     all.test[,-c('Trend'), with=FALSE], 
                                     all.y.tr.resi, store, is.lb=TRUE, 
                                     NULL)
  
  all.test[, Resi:=(y.es.pred.resi)]
  all.test[, Pred:=(Trend + Resi)]
  
  dt.result <- rbind(dt.result, list(all.test$Id, all.test$Store,
                                     as.numeric(all.test$Pred)))
  
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
  
  sink(file='/Users/aa/lab/common-repo/kaggle/rossmann/R/log.txt')
  
  modelres.rmspe <- data.table(store=numeric(0))
  modelres.rmspe[, eval(curr.model) := numeric(0)]
  
  modelres.ypred <- data.table(store=numeric(0), dateid=numeric(0), yreal=numeric(0))
  modelres.ypred[, eval(curr.model) := numeric(0)]
  
  setkey(train, Store)
  setkey(test, Store)
  setkey(store, Store)
  train <- train[store, nomatch=0]
  test <- test[store, nomatch=0]
  
  FillNa(train, 0)
  FillNa(test, 0)
  
  train <- train[((Open==1) & (Sales != 0))]
  
  train[, ':=' (Year = year(Date))]
  test[, ':=' (Year = year(Date))]
  
  train <- train[,-c('Date', 'StateHoliday'), with=FALSE]
  test <- test[,-c('Date', 'StateHoliday'), with=FALSE]
  
  feature.names <- c('Store', 'DayOfWeek', 'Open', 'Promo',
    'SchoolHoliday', 'StoreType', 'Assortment', 
    'CompetitionDistance', 'CompetitionOpenSinceMonth',
    'CompetitionOpenSinceYear', 'Promo2', 'Promo2SinceWeek',
    'Promo2SinceYear', 'PromoInterval', 'Month', 'Year', 'DayOfMonth', 
    'rossmann', 'State')
  
  test.id <- test$Id
  train.sales <- train$Sales
  
  train <- train[, feature.names, with=FALSE]
  test <- test[, feature.names, with=FALSE]
  
  for (i in seq_along(train)) {
    if (class(train[[i]])=='character') {
      levels <- unique(c(train[[i]], test[[i]]))
      set(train, j=i, value=as.integer(factor(train[[i]], levels=levels)))
      set(test, j=i, value=as.integer(factor(test[[i]], levels=levels)))
    }
  }
  
  sample.count <- ifelse(length(stores)>100, 10000, nrow(train)*0.1)
  h <- sample(nrow(train), sample.count)
  
  dval <- xgb.DMatrix(data=data.matrix(train[h]), label=log(train.sales+1)[h])
  dtrain <- xgb.DMatrix(data=data.matrix(train[-h,]), label=log(train.sales+1)[-h])
  watchlist <- list(val=dval,train=dtrain)
  
  param <- list(  objective           = "reg:linear", 
                  booster = "gbtree",
                  eta                 = 0.02,
                  max_depth           = 10,
                  subsample           = 0.9,
                  colsample_bytree    = 0.7
  )
  
  reg <- xgb.train(   params              = param, 
                      data                = dtrain, 
                      nrounds             = 4000,
                      verbose             = 1,
                      early.stop.round    = 100,
                      watchlist           = watchlist,
                      maximize            = FALSE,
                      feval=RMSPE
  )
  
  pred1 <- exp(predict(reg, data.matrix(test))) - 1
  
  submission <- data.table(Id=test.id, Sales=pred1)
  write.csv(submission, '../submissions/output-xgb-sim.csv', row.names = F)
  
  sink()
  browser()
  
}

RMSPE <- function(preds, dtrain) {
  labels <- getinfo(dtrain, 'label')
  elab <- exp(as.numeric(labels)) - 1
  epreds <- exp(as.numeric(preds)) - 1
  err <- sqrt(mean((epreds/elab-1)^2))
  return(list(metric = "RMSPE-custom", value = err))
}
