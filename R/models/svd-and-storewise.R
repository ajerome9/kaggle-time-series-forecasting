library(data.table)
source('util.R')
set.seed(0);

GenIntFeats <- function(dt, int.feats) {
  comb.mx <- combn(int.feats, 2)
  for (j in seq_len(ncol(comb.mx))) {
    col.pair <- comb.mx[,j]
    new.col <- paste(col.pair[1], col.pair[2], sep=':')
    set(dt, j=new.col, value=dt[[col.pair[1]]]*dt[[col.pair[2]]])
  }
  dt
}

RMSPE <- function(preds, dtrain) {
  labels <- getinfo(dtrain, 'label')
  elab <- exp(as.numeric(labels)) - 1
  epreds <- exp(as.numeric(preds)) - 1
  err <- sqrt(mean((epreds/elab-1)^2))
  return(list(metric = "RMSPE-custom", value = err))
}


sink(file='/Users/aa/lab/common-repo/kaggle/rossmann/misc/log.txt')

all.train <- fread("../misc/nobak/all.train.csv", header = TRUE, sep = ",")
all.test <- fread("../misc/nobak/all.test.csv", header = TRUE, sep = ",")


#stores <- sample(unique(all.test$Store), 30);
#stores <- c(1,3)
#train <- all.train[Store %in% stores]
#test <- all.test[Store %in% stores]
train <- all.train
test <- all.test

feature.names <- c('DayOfWeek', 'Promo',
                   'SchoolHoliday', 'StoreType', 'Assortment', 
                   'CompetitionDistance', 
                   'CompetitionOpenSinceYear', 'Promo2', 
                   'Promo2SinceYear', 'PromoInterval', 'Month', 'Year', 
                   'DayOfMonth', 'StateHoliday',
                   'rossmann', 'State', 'IsCompOpen', 'IsP2Open','WeekOfYear', 
                   'dm.drogerie.markt', 'geschenk',
                   'StoreIdMean', 'StoreIdSD', 'RossmannAdj',
                   'gt.daily2.country', 'gt.daily2.state',
                   'StateArea', 'StatePopulation', 'StateDensity', 'StateGDP',
                   'PayDayEffect')
feature.names <- c(feature.names, "Neujahr", 
                   "Heilige.Drei.Könige", "Karfreitag", "Ostermontag", 
                   "Tag.der.Arbeit", "Christi.Himmelfahrt", "Pfingstmontag", 
                   "Fronleichnam", "Mariä.Himmelfahrt", 
                   "Tag.der.Deutschen.Einheit", "Reformationstag", 
                   "Allerheiligen", "Buß..und.Bettag", "Weihnachtsfeiertag1", 
                   "Weihnachtsfeiertag2")

feature.names <- c(feature.names, 'Weiberfastnacht', 'Rosenmontag', 
                   'Fastnacht', 'Aschermittwoch', 'Valentinstag', 
                   'Palmsonntag', 'Gründonnerstag', 'Karsamstag', 
                   'Walpurgisnacht', 'Muttertag', 'JuniHoliday', 
                   'Erntedankfest', 'Halloween', 'Allerseelen', 
                   'Martinstag', 'Volkstrauertag', 'Totensonntag', 
                   'Advent1', 'Barbara', 'Nikolaus', 'Advent2', 
                   'Advent3', 'Advent4', 'Heiligabend', 'Silvester')

dt <- rbind(train[, feature.names, with=FALSE], test[, feature.names, with=FALSE])

dt <- GenDummyAndRemove(dt, dt$StoreType, 'StoreType')
dt <- GenDummyAndRemove(dt, dt$Assortment, 'Assortment')
dt <- GenDummyAndRemove(dt, dt$Year, 'Year')
dt <- GenDummyAndRemove(dt, dt$Month, 'Month')
dt <- GenDummyAndRemove(dt, dt$DayOfMonth, 'DayOfMonth')
dt <- GenDummyAndRemove(dt, dt$DayOfWeek, 'DayOfWeek')
dt <- GenDummyAndRemove(dt, dt$WeekOfYear, 'WeekOfYear')
dt <- GenDummyAndRemove(dt, dt$State, 'State')

dt[, CompetitionDistance:=log1p(CompetitionDistance)]
dt[, StatePopulation:=log1p(StatePopulation)]
dt[, StateArea:=log1p(StateArea)]
dt[, StateDensity:=log1p(StateDensity)]
dt[, StateGDP:=log1p(StateGDP)]

dt <- dt[,-c('Buß..und.Bettag'), with=FALSE]

dt <- alloc.col(dt, 300)

dt <- GenIntFeats(dt, c('Promo', 'CompetitionDistance'))
dt <- GenIntFeats(dt, c('Promo', 'StoreIdMean'))
dt <- GenIntFeats(dt, c('Promo', 'StoreIdSD'))

dt <- GenIntFeats(dt, c('Promo', 'PayDayEffect'))
dt <- GenIntFeats(dt, c('Promo', 'StoreType2'))
dt <- GenIntFeats(dt, c('Promo', 'StoreType3'))
dt <- GenIntFeats(dt, c('Promo', 'StoreType4'))
dt <- GenIntFeats(dt, c('Promo', 'Assortment2'))
dt <- GenIntFeats(dt, c('Promo', 'Assortment3'))
dt <- GenIntFeats(dt, c('Promo', 'Month2'))
dt <- GenIntFeats(dt, c('Promo', 'Month3'))
dt <- GenIntFeats(dt, c('Promo', 'Month4'))
dt <- GenIntFeats(dt, c('Promo', 'Month5'))
dt <- GenIntFeats(dt, c('Promo', 'Month6'))
dt <- GenIntFeats(dt, c('Promo', 'Month7'))
dt <- GenIntFeats(dt, c('Promo', 'Month8'))
dt <- GenIntFeats(dt, c('Promo', 'Month9'))
dt <- GenIntFeats(dt, c('Promo', 'Month10'))
dt <- GenIntFeats(dt, c('Promo', 'Month11'))
dt <- GenIntFeats(dt, c('Promo', 'Month12'))
dt <- GenIntFeats(dt, c('Promo', 'DayOfWeek2'))
dt <- GenIntFeats(dt, c('Promo', 'DayOfWeek3'))
dt <- GenIntFeats(dt, c('Promo', 'DayOfWeek4'))
dt <- GenIntFeats(dt, c('Promo', 'DayOfWeek5'))

dt <- GenIntFeats(dt, c('Promo2', 'PayDayEffect'))
dt <- GenIntFeats(dt, c('Promo2', 'StoreType2'))
dt <- GenIntFeats(dt, c('Promo2', 'StoreType3'))
dt <- GenIntFeats(dt, c('Promo2', 'StoreType4'))
dt <- GenIntFeats(dt, c('Promo2', 'Assortment2'))
dt <- GenIntFeats(dt, c('Promo2', 'Assortment3'))
dt <- GenIntFeats(dt, c('Promo2', 'Month2'))
dt <- GenIntFeats(dt, c('Promo2', 'Month3'))
dt <- GenIntFeats(dt, c('Promo2', 'Month4'))
dt <- GenIntFeats(dt, c('Promo2', 'Month5'))
dt <- GenIntFeats(dt, c('Promo2', 'Month6'))
dt <- GenIntFeats(dt, c('Promo2', 'Month7'))
dt <- GenIntFeats(dt, c('Promo2', 'Month8'))
dt <- GenIntFeats(dt, c('Promo2', 'Month9'))
dt <- GenIntFeats(dt, c('Promo2', 'Month10'))
dt <- GenIntFeats(dt, c('Promo2', 'Month11'))
dt <- GenIntFeats(dt, c('Promo2', 'Month12'))
dt <- GenIntFeats(dt, c('Promo2', 'DayOfWeek2'))
dt <- GenIntFeats(dt, c('Promo2', 'DayOfWeek3'))
dt <- GenIntFeats(dt, c('Promo2', 'DayOfWeek4'))
dt <- GenIntFeats(dt, c('Promo2', 'DayOfWeek5'))
dt <- GenIntFeats(dt, c('Promo2', 'DayOfWeek6'))
dt <- GenIntFeats(dt, c('Promo2', 'DayOfWeek7'))

dt <- GenIntFeats(dt, c('CompetitionDistance', 'IsCompOpen'))
dt <- GenIntFeats(dt, c('CompetitionDistance', 'StoreIdMean'))
dt <- GenIntFeats(dt, c('CompetitionDistance', 'StoreIdSD'))
dt <- GenIntFeats(dt, c('CompetitionDistance', 'StateDensity'))
dt <- GenIntFeats(dt, c('CompetitionDistance', 'StoreType2'))
dt <- GenIntFeats(dt, c('CompetitionDistance', 'StoreType3'))
dt <- GenIntFeats(dt, c('CompetitionDistance', 'StoreType4'))
dt <- GenIntFeats(dt, c('CompetitionDistance', 'Assortment2'))
dt <- GenIntFeats(dt, c('CompetitionDistance', 'Assortment3'))

dt <- GenIntFeats(dt, c('StoreIdMean', 'StateGDP'))
dt <- GenIntFeats(dt, c('StoreIdMean', 'StateDensity'))
dt <- GenIntFeats(dt, c('StoreIdMean', 'PayDayEffect'))
dt <- GenIntFeats(dt, c('StoreIdMean', 'DayOfWeek2'))
dt <- GenIntFeats(dt, c('StoreIdMean', 'DayOfWeek3'))
dt <- GenIntFeats(dt, c('StoreIdMean', 'DayOfWeek4'))
dt <- GenIntFeats(dt, c('StoreIdMean', 'DayOfWeek5'))
dt <- GenIntFeats(dt, c('StoreIdMean', 'DayOfWeek6'))
dt <- GenIntFeats(dt, c('StoreIdMean', 'DayOfWeek7'))
dt <- GenIntFeats(dt, c('StoreIdMean', 'StoreType2'))
dt <- GenIntFeats(dt, c('StoreIdMean', 'StoreType3'))
dt <- GenIntFeats(dt, c('StoreIdMean', 'StoreType4'))
dt <- GenIntFeats(dt, c('StoreIdMean', 'Assortment2'))
dt <- GenIntFeats(dt, c('StoreIdMean', 'Assortment3'))
dt <- GenIntFeats(dt, c('StoreIdMean', 'Month2'))
dt <- GenIntFeats(dt, c('StoreIdMean', 'Month3'))
dt <- GenIntFeats(dt, c('StoreIdMean', 'Month4'))
dt <- GenIntFeats(dt, c('StoreIdMean', 'Month5'))
dt <- GenIntFeats(dt, c('StoreIdMean', 'Month6'))
dt <- GenIntFeats(dt, c('StoreIdMean', 'Month7'))
dt <- GenIntFeats(dt, c('StoreIdMean', 'Month8'))
dt <- GenIntFeats(dt, c('StoreIdMean', 'Month9'))
dt <- GenIntFeats(dt, c('StoreIdMean', 'Month10'))
dt <- GenIntFeats(dt, c('StoreIdMean', 'Month11'))
dt <- GenIntFeats(dt, c('StoreIdMean', 'Month12'))
dt <- GenIntFeats(dt, c('StoreIdMean', 'DayOfWeek2'))
dt <- GenIntFeats(dt, c('StoreIdMean', 'DayOfWeek3'))
dt <- GenIntFeats(dt, c('StoreIdMean', 'DayOfWeek4'))
dt <- GenIntFeats(dt, c('StoreIdMean', 'DayOfWeek5'))
dt <- GenIntFeats(dt, c('StoreIdMean', 'DayOfWeek6'))
dt <- GenIntFeats(dt, c('StoreIdMean', 'DayOfWeek7'))
dt <- GenIntFeats(dt, c('StoreIdMean', 'DayOfMonth2'))
dt <- GenIntFeats(dt, c('StoreIdMean', 'DayOfMonth3'))
dt <- GenIntFeats(dt, c('StoreIdMean', 'DayOfMonth4'))
dt <- GenIntFeats(dt, c('StoreIdMean', 'DayOfMonth5'))
dt <- GenIntFeats(dt, c('StoreIdMean', 'DayOfMonth6'))
dt <- GenIntFeats(dt, c('StoreIdMean', 'DayOfMonth7'))
dt <- GenIntFeats(dt, c('StoreIdMean', 'DayOfMonth8'))
dt <- GenIntFeats(dt, c('StoreIdMean', 'DayOfMonth9'))
dt <- GenIntFeats(dt, c('StoreIdMean', 'DayOfMonth10'))
dt <- GenIntFeats(dt, c('StoreIdMean', 'DayOfMonth11'))
dt <- GenIntFeats(dt, c('StoreIdMean', 'DayOfMonth12'))
dt <- GenIntFeats(dt, c('StoreIdMean', 'DayOfMonth13'))
dt <- GenIntFeats(dt, c('StoreIdMean', 'DayOfMonth14'))
dt <- GenIntFeats(dt, c('StoreIdMean', 'DayOfMonth15'))
dt <- GenIntFeats(dt, c('StoreIdMean', 'DayOfMonth16'))
dt <- GenIntFeats(dt, c('StoreIdMean', 'DayOfMonth17'))
dt <- GenIntFeats(dt, c('StoreIdMean', 'DayOfMonth18'))
dt <- GenIntFeats(dt, c('StoreIdMean', 'DayOfMonth19'))
dt <- GenIntFeats(dt, c('StoreIdMean', 'DayOfMonth20'))
dt <- GenIntFeats(dt, c('StoreIdMean', 'DayOfMonth21'))
dt <- GenIntFeats(dt, c('StoreIdMean', 'DayOfMonth22'))
dt <- GenIntFeats(dt, c('StoreIdMean', 'DayOfMonth23'))
dt <- GenIntFeats(dt, c('StoreIdMean', 'DayOfMonth24'))
dt <- GenIntFeats(dt, c('StoreIdMean', 'DayOfMonth25'))
dt <- GenIntFeats(dt, c('StoreIdMean', 'DayOfMonth26'))
dt <- GenIntFeats(dt, c('StoreIdMean', 'DayOfMonth27'))
dt <- GenIntFeats(dt, c('StoreIdMean', 'DayOfMonth28'))
dt <- GenIntFeats(dt, c('StoreIdMean', 'DayOfMonth29'))
dt <- GenIntFeats(dt, c('StoreIdMean', 'DayOfMonth30'))
dt <- GenIntFeats(dt, c('StoreIdMean', 'DayOfMonth31'))

dt.orig <- dt

n.comp <- 50
all.pca <- prcomp(dt, scale = TRUE, center = TRUE)
dt.svd <- as.data.table(predict(all.pca)[,1:n.comp])

dt.train <- dt.svd[1:nrow(train),]
dt.test <- dt.svd[(nrow(train)+1):nrow(dt.svd),]

x.tr <- dt.train
y.tr <- train[, Sales]

dtrain <- xgb.DMatrix(data=data.matrix(x.tr), label=log(y.tr+1))
watchlist <- list(train=dtrain)

param <- list(  objective           = "reg:linear", 
                booster             = "gbtree",
                eta                 = 0.02,
                max_depth           = 10,
                subsample           = 0.9,
                colsample_bytree    = 0.7,
                min_child_weight    = 3
)

reg <- xgb.train(   params              = param, 
                    data                = dtrain, 
                    nrounds             = 3000,
                    verbose             = 1,
                    watchlist           = watchlist,
                    maximize            = FALSE,
                    feval=RMSPE
)

x.test <- dt.test

dtest <- xgb.DMatrix(data=data.matrix(x.test))
y.test.pred <- exp(predict(reg, dtest)) - 1


#====================================================

res.hol.names <- c("Neujahr", 
                   "Heilige.Drei.Könige", "Karfreitag", "Ostermontag", 
                   "Tag.der.Arbeit", "Christi.Himmelfahrt", "Pfingstmontag", 
                   "Fronleichnam", "Mariä.Himmelfahrt", 
                   "Tag.der.Deutschen.Einheit", "Reformationstag", 
                   "Allerheiligen", "Buß..und.Bettag", "Weihnachtsfeiertag1", 
                   "Weihnachtsfeiertag2")
res.hol.names <- c(res.hol.names, 'Weiberfastnacht', 'Rosenmontag', 
                   'Fastnacht', 'Aschermittwoch', 'Valentinstag', 
                   'Palmsonntag', 'Gründonnerstag', 'Karsamstag', 
                   'Walpurgisnacht', 'Muttertag', 'JuniHoliday', 
                   'Erntedankfest', 'Halloween', 'Allerseelen', 
                   'Martinstag', 'Volkstrauertag', 'Totensonntag', 
                   'Advent1', 'Barbara', 'Nikolaus', 'Advent2', 
                   'Advent3', 'Advent4', 'Heiligabend', 'Silvester')
res.feature.names <- c('Promo', 'SchoolHoliday', 'IsCompOpen', 'IsP2Open', 
                       'gt.daily2.state', 'PayDayEffect')
res.feature.names <- c(res.feature.names, res.hol.names)


# Compute residuals
dtrain.for.resi <- xgb.DMatrix(data=data.matrix(x.tr))
y.tr.pred <- exp(predict(reg, dtrain.for.resi)) - 1
y.tr.resi <- y.tr - y.tr.pred

# train residuals store-by-store
combined.submission <- data.table(Id=numeric(0), Sales=numeric(0))
combined.train.pred <- data.table(Store=numeric(0), 
                                  PredBase=numeric(0), 
                                  PredResi=numeric(0),
                                  PredComb=numeric(0),
                                  Actual=numeric(0))
#test.stores <- c(665)
#test.stores <- c(1026, 665, 597, 61, 689, 295, 1003, 863, 663, 946, 387, 206)
test.stores <- unique(test$Store)
for (test.store.idx in seq_along(test.stores)) {
  onestore <- test.stores[test.store.idx]
  print(sprintf('%d. Processing residual for %d', test.store.idx, onestore))
  
  train.onestore.rowids <- train[, .I[Store==onestore]]
  train.onestore.rowdates <- train[Store==onestore, .(Year, Month, DayOfMonth)]
  train.onestore.rowinfo <- cbind(train.onestore.rowdates, train.onestore.rowids)
  setorder(train.onestore.rowinfo, Year, Month, DayOfMonth)
  train.onestore.rowids <- train.onestore.rowinfo$train.onestore.rowids
  
  test.onestore.rowids <- test[, .I[Store==onestore]]
  test.onestore.rowdates <- test[Store==onestore, .(Year, Month, DayOfMonth)]
  test.onestore.rowinfo <- cbind(test.onestore.rowdates, test.onestore.rowids)
  setorder(test.onestore.rowinfo, Year, Month, DayOfMonth)
  test.onestore.rowids <- test.onestore.rowinfo$test.onestore.rowids
  
  #print(length(train.onestore.rowids))
  #print(length(test.onestore.rowids))
  
  x.tr.store <- train[train.onestore.rowids,]
  x.test.store <- test[test.onestore.rowids,]
  y.tr.resi.store <- y.tr.resi[train.onestore.rowids]
  y.tr.pred.store <- y.tr.pred[train.onestore.rowids]
  y.tr.actual.store <- y.tr[train.onestore.rowids]
  y.test.pred.store <- y.test.pred[test.onestore.rowids]
  id.test.store <- test[test.onestore.rowids, Id]

  dt.onestore <- rbind(x.tr.store[, res.feature.names, with=FALSE], 
                       x.test.store[, res.feature.names, with=FALSE])
  
  dt.onestore.mg <- cbind(dt.onestore, 
        dt.onestore[, shift(.SD, n=1, 0, 'lag', TRUE), .SDcols=res.hol.names], 
        dt.onestore[, shift(.SD, n=1, 0, 'lead', TRUE), .SDcols=res.hol.names])

  x.tr.store <- dt.onestore.mg[1:nrow(x.tr.store),]
  x.test.store <- dt.onestore.mg[(nrow(x.tr.store)+1):nrow(dt.onestore.mg),]
  
  dtrain.onestore <- xgb.DMatrix(data=data.matrix(x.tr.store), 
                                 label=y.tr.resi.store)
  watchlist.onestore <- list(train=dtrain.onestore)
  
  param.onestore <- list(  objective           = "reg:linear", 
                           booster             = "gbtree",
                           eta                 = 0.10,
                           max_depth           = 2,
                           subsample           = 1.0,
                           colsample_bytree    = 0.7,
                           min_child_weight    = 1
  )
  
  reg.onestore <- xgb.train(   params              = param.onestore, 
                               data                = dtrain.onestore, 
                               nrounds             = 350,
                               verbose             = 1,
                               print.every.n       = 80,
                               watchlist           = watchlist.onestore,
                               maximize            = FALSE
  )
  
  dtest.onestore <- xgb.DMatrix(data=data.matrix(x.test.store))
  y.test.resi.pred <- predict(reg.onestore, dtest.onestore)
  y.test.comb.pred.store <- y.test.pred.store + y.test.resi.pred
  
  combined.submission <- rbind(combined.submission, 
                               list(id.test.store, y.test.comb.pred.store))
  
  # for rmspe computation
  y.tr.resi.pred <- predict(reg.onestore, dtrain.onestore)
  y.tr.combined.pred <- y.tr.pred.store + y.tr.resi.pred
  
  combined.train.pred <- rbind(combined.train.pred, 
                          list(rep(onestore, length(y.tr.pred.store)), 
                               y.tr.pred.store, 
                               y.tr.resi.pred,
                               y.tr.combined.pred,
                               y.tr.actual.store))
}

write.csv(combined.submission, 
          '../submissions/output-svd-and-storewise.csv', row.names = F)
combined.train.pred[, Pred.Err:=(abs(Actual-PredComb)/Actual)]
overall.rmspe <- combined.train.pred[, sqrt(mean(Pred.Err^2))]
print(sprintf('Overall rmspe = %f', overall.rmspe))
storewise.rmspe <- combined.train.pred[,.(Stor.Err=
                                        sqrt(mean((Pred.Err)^2)),
                                      AvgErr=mean(Pred.Err),
                                      StDev=sd(Pred.Err),
                                      MinErr=min(Pred.Err),
                                      MaxErr=max(Pred.Err))
                                  ,by=Store]














#===============================================
# GLMNET approach

# train residuals store-by-store
combined.submission <- data.table(Id=numeric(0), Sales=numeric(0))
combined.train.pred <- data.table(Store=numeric(0), 
                                  PredBase=numeric(0), 
                                  PredResi=numeric(0),
                                  PredComb=numeric(0),
                                  Actual=numeric(0))
#test.stores <- c(665)
#test.stores <- c(1026, 665, 597, 61, 689, 295, 1003, 863, 663, 946, 387, 206)
test.stores <- unique(test$Store)
for (test.store.idx in seq_along(test.stores)) {
  onestore <- test.stores[test.store.idx]
  print(sprintf('%d. Processing residual for %d', test.store.idx, onestore))
  
  train.onestore.rowids <- train[, .I[Store==onestore]]
  train.onestore.rowdates <- train[Store==onestore, .(Year, Month, DayOfMonth)]
  train.onestore.rowinfo <- cbind(train.onestore.rowdates, train.onestore.rowids)
  setorder(train.onestore.rowinfo, Year, Month, DayOfMonth)
  train.onestore.rowids <- train.onestore.rowinfo$train.onestore.rowids
  
  test.onestore.rowids <- test[, .I[Store==onestore]]
  test.onestore.rowdates <- test[Store==onestore, .(Year, Month, DayOfMonth)]
  test.onestore.rowinfo <- cbind(test.onestore.rowdates, test.onestore.rowids)
  setorder(test.onestore.rowinfo, Year, Month, DayOfMonth)
  test.onestore.rowids <- test.onestore.rowinfo$test.onestore.rowids
  
  #print(length(train.onestore.rowids))
  #print(length(test.onestore.rowids))
  
  x.tr.store <- train[train.onestore.rowids,]
  x.test.store <- test[test.onestore.rowids,]
  y.tr.resi.store <- y.tr.resi[train.onestore.rowids]
  y.tr.pred.store <- y.tr.pred[train.onestore.rowids]
  y.tr.actual.store <- y.tr[train.onestore.rowids]
  y.test.pred.store <- y.test.pred[test.onestore.rowids]
  id.test.store <- test[test.onestore.rowids, Id]
  
  dt.onestore <- rbind(x.tr.store[, res.feature.names, with=FALSE], 
                       x.test.store[, res.feature.names, with=FALSE])
  
  dt.onestore.mg <- cbind(dt.onestore, 
                          dt.onestore[, shift(.SD, n=1, 0, 'lag', TRUE), .SDcols=res.hol.names], 
                          dt.onestore[, shift(.SD, n=1, 0, 'lead', TRUE), .SDcols=res.hol.names])
  
  x.tr.store <- dt.onestore.mg[1:nrow(x.tr.store),]
  x.test.store <- dt.onestore.mg[(nrow(x.tr.store)+1):nrow(dt.onestore.mg),]
  
  
  
  store.glmnet <- cv.glmnet(data.matrix(x.tr.store), 
                            y.tr.resi.store, alpha = 1.0)
  y.test.resi.pred <- predict(store.glmnet, 
                              newx=data.matrix(x.test.store), s='lambda.min')
  
  

  y.test.comb.pred.store <- y.test.pred.store + y.test.resi.pred
  
  combined.submission <- rbind(combined.submission, 
                               list(id.test.store, y.test.comb.pred.store))
  
  # for rmspe computation
  y.tr.resi.pred <- predict(store.glmnet, 
                            newx=data.matrix(x.tr.store), s='lambda.min')
  y.tr.combined.pred <- y.tr.pred.store + y.tr.resi.pred
  
  combined.train.pred <- rbind(combined.train.pred, 
                               list(rep(onestore, length(y.tr.pred.store)), 
                                    y.tr.pred.store, 
                                    y.tr.resi.pred,
                                    y.tr.combined.pred,
                                    y.tr.actual.store))
}

write.csv(combined.submission, 
          '../submissions/output-svd-and-storewise.csv', row.names = F)
combined.train.pred[, Pred.Err:=(abs(Actual-PredComb)/Actual)]
overall.rmspe <- combined.train.pred[, sqrt(mean(Pred.Err^2))]
print(sprintf('Overall rmspe = %f', overall.rmspe))
storewise.rmspe <- combined.train.pred[,.(Stor.Err=
                                            sqrt(mean((Pred.Err)^2)),
                                          AvgErr=mean(Pred.Err),
                                          StDev=sd(Pred.Err),
                                          MinErr=min(Pred.Err),
                                          MaxErr=max(Pred.Err))
                                       ,by=Store]




#===============================================
#### per-pred error analysis ####

y.va.pred <- exp(predict(reg, dval)) - 1
y.va.real <- y.va
y.va.stor <- train[h, Store]
y.va.date <- train[h, ymd(paste(Year, Month, DayOfMonth, sep='-'))]

va.pred.compare <- data.table(Stor=y.va.stor, 
                              Real=y.va.real,
                              Pred=y.va.pred,
                              Date=y.va.date)

va.pred.compare[, Pred.Err:=(abs(Real-Pred)/Real)]
setorder(va.pred.compare, -Pred.Err)
View(va.pred.compare)


library(ggplot2)
ggplot(va.pred.compare[1:100], 
       aes(x = Date, y = Pred.Err, 
           color = factor(Stor))) + 
  geom_point(size = 3) + ggtitle("Store with 100 highest Pred.Err")


ggplot(va.pred.compare[1:200], 
       aes(x = factor(Stor), y = Pred.Err)) +
  geom_jitter(alpha = 0.1) +
  geom_point(size = 3) + 
  ggtitle("PredErrors for various stores (top 200 Pred.Errs)")

#### store-wise error analysis ####

va.stor.compare <- va.pred.compare[,.(Stor.Err=
                                        sqrt(mean( ((Real-Pred)/Real)^2 )),
                                      Count=.N,
                                      AvgErr=mean(Pred.Err),
                                      StDev=sd(Pred.Err),
                                      MinErr=min(Pred.Err),
                                      MaxErr=max(Pred.Err)
)
,by=Stor]
setorder(va.stor.compare, -Stor.Err)
View(va.stor.compare)

ggplot(va.pred.compare[Stor %in% va.stor.compare[1:100, Stor]], 
       aes(x = factor(Stor), y = Pred.Err)) +
  geom_jitter(alpha = 1) +
  geom_point(size = 3) + 
  ggtitle("PredErrors for various stores with high error")


#### Plot store-sales around the time we had difficulty predicting sales ####

va.store.id <- 665
dt.va.stor <- train[Store==va.store.id,
                    .(Sales, 
                  Date=ymd(paste(Year, Month, DayOfMonth, sep='-')))][order(Date)]

dt.va.pred.stor <- va.pred.compare[Stor==va.store.id][order(Date)]
dt.va.stor.mrgd <- dt.va.pred.stor[dt.va.stor, on="Date"][order(Date)]

# Get resid for this store
va.store.rowids <- train[, .I[Store==va.store.id]]
va.store.rowdates <- train[Store==va.store.id, .(Year, Month, DayOfMonth)]
va.store.rowinfo <- cbind(va.store.rowdates, va.store.rowids)
setorder(va.store.rowinfo, Year, Month, DayOfMonth)


va.dmx <- xgb.DMatrix(data=dt.train[va.store.rowinfo$va.store.rowids,])
va.tr.pred <- exp(predict(reg, va.dmx)) - 1
va.tr.resi <- dt.va.stor$Sales - va.tr.pred


ggplot() + 
  geom_line(data=dt.va.stor, aes(x = Date, y = Sales)) +
  geom_line(data=dt.va.stor, aes(x = Date, y = va.tr.resi), color='grey') +
  geom_point(data=dt.va.stor.mrgd[!is.na(Pred)], 
             aes(x = Date, y = Pred),
             color='red',
             size=3) +
  geom_point(data=dt.va.stor.mrgd[!is.na(Pred)], 
             aes(x = Date, y = Real),
             color='blue',
             size=3) +
  ggtitle(paste0("Sales and Preds of store ", va.store.id))


View(train[Store==va.store.id, ][order(Year, Month, DayOfMonth)])







