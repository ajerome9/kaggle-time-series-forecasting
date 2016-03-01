source('util.R')

DoElasticNetOnResiduals <- function(mtx.tr, mtx.test, ytr.res) {
  store.glmnet <- cv.glmnet(mtx.tr, as.numeric(ytr.res), alpha = 1.0)
  predict(store.glmnet, newx=mtx.test, s='lambda.min')
}

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
  param.nrounds.lb = 430  # set based on cv
  
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
  ytest.pred <- rep(1, length(xtest))

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
    
    result <- ProcessOneStore(dt.tr[,-c('Sales', 'Customers'), with=FALSE], 
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
  
  modelres.rmspe <- data.table(store=numeric(0))
  modelres.rmspe[, eval(curr.model) := numeric(0)]
  
  modelres.ypred <- data.table(store=numeric(0), dateid=numeric(0), yreal=numeric(0))
  modelres.ypred[, eval(curr.model) := numeric(0)]
  
  all.tr <- data.table()
  all.test <- data.table()
  
  for (j in seq_along(stores)) {
    i <- stores[j]
    #print(sprintf('%d. %s-CV: Store #%d',j, curr.model, i))
    
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
  all.y.es.resi <- (all.test$Sales - all.test$Trend)
  
  y.es.pred.resi <- GenFeatsAndDoXgb(all.tr[,-c('Resi'), with=FALSE], 
                                     all.test[,-c('Trend'), with=FALSE], 
                                     all.y.tr.resi, store, is.lb=FALSE, 
                                     all.y.es.resi)
  
  all.test[, Resi:=(y.es.pred.resi)]
  all.test[, Pred:=(Trend + Resi)]
  
  net.rmspe <- ComputeRmspe(all.test$Sales, all.test$Pred)[[1]]
  
  setkey(all.test, Store)
  setorder(all.test, Date, Store)
  for (j in seq_along(stores)) {
    i <- stores[j]
    #print(sprintf('%d. %s-CV: Store #%d',j, curr.model, i))
    
    sel <- all.test$Store==i
    ytest.st <- all.test[sel, Sales]
    ypred.st <- all.test[sel, Pred]
    
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