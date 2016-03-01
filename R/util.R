library(data.table)
library(lubridate)
library(xgboost)
library(forecast)
library(glmnet)

stores.py <- c(1007, 1111, 188, 897, 414, 502, 547, 882, 960, 269, 319, 
               652, 705, 583, 71, 823, 709, 1060, 331, 404, 240, 284, 
               466, 486, 398, 912, 857, 795, 852, 15)

AddDateFields <- function(dt) {
  dt[, Date := as.Date(fast_strptime(Date, '%Y-%m-%d'))]
  dt[, ':=' (DayOfMonth = day(Date), Month = month(Date))]
}

FillNa <- function(dt, fill.value) {
  for (i in seq_along(dt)) {
    set(dt, i=which(is.na(dt[[i]])), j=i, value=fill.value)
  }
}

get.holiday.table <- function() {
  state.holidays <- fread("../data/ext/stateholidays.csv", header = FALSE, sep = ",")
  setnames(state.holidays, c(1:3), c('HolidayName', 'Date', 'State'))
  state.holidays[, Date := as.Date(fast_strptime(Date, '%d.%m.%Y'))]
  state.holidays <- state.holidays[, list(State=unlist(strsplit(State, ", "))), 
                                   by=.(HolidayName, Date)]
  
  all.dates <- as.Date(fast_strptime('2013-01-01', '%Y-%m-%d'))+0:1094
  state.list <- c('HE', 'TH', 'NW', 'BE', 'SN', 'SH', 'HBNI', 
                  'BY', 'BW', 'RP', 'ST', 'HH')
  
  hol.mx <- CJ(all.dates, state.list)
  setnames(hol.mx, c(1:2), c('Date', 'State'))
  
  for (idx in seq_len(nrow(state.holidays))) {
    hol.row <- state.holidays[idx,]
    hol.name <- hol.row$HolidayName
    hol.date <- hol.row$Date
    hol.state <- hol.row$State
    hol.mx[Date==hol.date & State==hol.state, hol.name:=1, with=FALSE]
  }
  
  setnames(hol.mx, colnames(hol.mx), make.names(colnames(hol.mx)))
  
  hol.mx
}

get.otherholiday.table <- function() {
  other.holidays <- fread("../data/ext/otherholidays.csv", header = FALSE, sep = ",")
  setnames(other.holidays, c(1:2), c('OtherHoliday', 'Date'))
  other.holidays[, Date := as.Date(fast_strptime(Date, '%d.%m.%Y'))]
  
  all.dates <- as.Date(fast_strptime('2013-01-01', '%Y-%m-%d'))+0:1094
  otherhol.mx <- data.table(Date=all.dates)
  
  for (idx in seq_len(nrow(other.holidays))) {
    hol.row <- other.holidays[idx,]
    hol.name <- hol.row$OtherHoliday
    hol.date <- hol.row$Date
    otherhol.mx[Date==hol.date, hol.name:=1, with=FALSE]
  }
  
  setnames(otherhol.mx, colnames(otherhol.mx), make.names(colnames(otherhol.mx)))
  
  otherhol.mx
}

read.gtdaily.combined <- function() {
  gt.daily <- fread("../data/ext/gtdaily.csv", header = TRUE, sep = ",")
  gt.daily[, Date := as.Date(fast_strptime(Date, '%Y-%m-%d'))]
  
  gt.daily
}

read.gtdaily.2 <- function() {
  gt.daily2 <- fread("../data/ext/gtdaily2.csv", header = TRUE, sep = ",")
  
  orig.colnames <- c('daily_rossmann_DE', 'daily_rossmann_DE-SN', 
                     'daily_rossmann_DE-RP', 'daily_rossmann_DE-BE', 
                     'daily_rossmann_DE-HB', 'daily_rossmann_DE-BW', 
                     'daily_rossmann_DE-ST', 'daily_rossmann_DE-NI', 
                     'daily_rossmann_DE-SH', 'daily_rossmann_DE-TH', 
                     'daily_rossmann_DE-HE', 'daily_rossmann_DE-HH', 
                     'daily_rossmann_DE-BY', 'daily_rossmann_DE-NW', 'Day')
  
  new.colnames <- c('DE', 'SN', 'RP', 'BE', 'HB', 'BW', 'ST', 'NI', 
                    'SH', 'TH', 'HE', 'HH', 'BY', 'NW', 'Date')
  
  setnames(gt.daily2, orig.colnames, new.colnames)
  gt.daily2[, Date := as.Date(fast_strptime(Date, '%Y-%m-%d'))]
  
  gt.daily2[, 'HBNI':=HB+NI, with=FALSE]
  gt.daily2 <- gt.daily2[, -c('V1', 'HB', 'NI'), with=FALSE]
  
  gt.daily2.country <- gt.daily2[, c('Date', 'DE'), with=FALSE]
  
  gt.daily2.statewise <- melt(gt.daily2, id=1, measure=3:14, 
                              variable.name='State', value.name='gt.daily2.state')
  
  setnames(gt.daily2.country, 'DE', 'gt.daily2.country')
  
  list(gt.daily2.country, gt.daily2.statewise)
}

read.state.stats <- function() {
  st.stats <- fread("../data/ext/state_stats.csv", header = TRUE, sep = ",")
  st.stats <- st.stats[, -c('StateName'), with=FALSE]
  setnames(st.stats, c('Area', 'Population', 'Density', 'GDP'),
           c('StateArea', 'StatePopulation', 'StateDensity', 'StateGDP'))
  
  st.stats
}

get.payday.dt <- function() {
  date.list <- do.call(paste, c(expand.grid(2013:2015, 1:12, 1), sep='-'))
  
  pday.dt <- data.table(Date=as.Date(fast_strptime('2013-01-01', '%Y-%m-%d'))+0:1094, 
                        PayDayEffect=0)
  setkey(pday.dt, 'Date')
  
  for(one.date.str in date.list) {
    one.date <- as.Date(fast_strptime(one.date.str, '%Y-%m-%d'))
    wday.one.date <- wday(one.date)
    
    if (wday.one.date == 7) {
      d.1 <- one.date - days(1)
      d.2 <- one.date
      d.3 <- one.date + days(1)
    } else if (wday.one.date == 1) {
      d.1 <- one.date - days(2)
      d.2 <- one.date - days(1)
      d.3 <- one.date
    } else {
      d.1 <- one.date
      d.2 <- one.date + days(1)
      d.3 <- one.date + days(2)
    }
    
    pday.dt[Date==d.1, PayDayEffect:=3]
    pday.dt[Date==d.2, PayDayEffect:=2]
    pday.dt[Date==d.3, PayDayEffect:=1]
  }
  
  pday.dt
}

read.gt.data.onestate <- function(state) {
  filename <- paste0('../data/ext/gtrends/report-', state, '.csv')
  gt.data <- fread(filename, header = TRUE, sep = ",")
  gt.data[, c("Start", "End") := tstrsplit(Week, " - ")]
  gt.data[, Start := as.Date(fast_strptime(Start, '%Y-%m-%d'))]
  gt.data[, End := as.Date(fast_strptime(End, '%Y-%m-%d'))]
  
  setnames(gt.data, colnames(gt.data), make.names(colnames(gt.data)))
  gt.data <- gt.data[,.(Date=seq(Start, End, by='day'), State=state,
                        rossmann, dm.drogerie.markt, geschenk), 
                     by=Week]
  
  gt.data[,.(State, Date, rossmann, dm.drogerie.markt, geschenk)]
}

read.gt.data <- function(unique.states) {
  gt.data <- data.table()
  
  for (state in unique.states) {
    if (state=='HBNI') {
      gt.data.hb <- read.gt.data.onestate('HB')
      gt.data.ni <- read.gt.data.onestate('NI')
      gt.data.hb$rossmann <- (gt.data.hb$rossmann + gt.data.ni$rossmann)/2
      gt.data.hb$dm.drogerie.markt <- (gt.data.hb$dm.drogerie.markt + 
                                         gt.data.ni$dm.drogerie.markt)/2
      gt.data.hb$geschenk <- (gt.data.hb$geschenk + gt.data.ni$geschenk)/2
      gt.data.hb[,State:='HBNI']
      gt.data <- rbind(gt.data, gt.data.hb)
    } else {
      gt.data.onestate <- read.gt.data.onestate(state)
      gt.data <- rbind(gt.data, gt.data.onestate)
    }
  }
  gt.data
}

read.data <- function(){
  train <- fread("../data/train.csv", header = TRUE, sep = ",")
  store <- fread("../data/store.csv", header = TRUE, sep = ",")
  storeloc <- fread("../data/ext/storeloc.csv", header = TRUE, sep = ",")
  test <- fread("../data/test.csv", header = TRUE, sep = ",")
  gt.data <- read.gt.data(unique(storeloc$State))
  
  train[, Sales:=as.numeric(Sales)]
  
  setkey(store, Store)
  setkey(storeloc, Store)
  store <- store[storeloc, nomatch=0]
  
  train[,SchoolHoliday := as.numeric(SchoolHoliday)]
  test[,SchoolHoliday := as.numeric(SchoolHoliday)]
  
  train <- AddDateFields(train)
  test <- AddDateFields(test)
  
  setkey(train, Store)
  setkey(test, Store)
  setkey(storeloc, Store)
  train <- train[storeloc]
  test <- test[storeloc]
  
  setkey(train, Date, State)
  setkey(test, Date, State)
  setkey(gt.data, Date, State)
  train <- train[gt.data, nomatch=0]
  test <- test[gt.data, nomatch=0]
  
  train[,State:=NULL]
  test[,State:=NULL]
  
  test[is.na(Open), Open := 1]  # fill NAs in test$Open (treating it as Open)
  
  list(train, test, store)
}

PrintXgbFi <- function(bst, mtx.tr) {
  names <- dimnames(mtx.tr)[[2]]
  importance.matrix <- xgb.importance(names, model = bst)
  print(importance.matrix)
}

CreateLaggedFeats <- function(dt, lagby) {
  dt1 <- dt[, shift(.SD, n=lagby, 0, 'lag', TRUE)]
  cbind(dt, dt1)
}

AddFeatInteractions <- function(dt) {
  all.cols <- names(dt)
  hol_type_cols <- grep("^hol_type_", all.cols, value = T)
  dow_cols <- grep("^dow_", all.cols, value = T)
  dom_cols <- grep("^dom_", all.cols, value = T)
  mon_cols <- grep("^mon_", all.cols, value = T)
  
  intr_open_dow <- do.call(paste, c(expand.grid('Open', dow_cols), sep=':'))
  intr_promo_dow <- do.call(paste, c(expand.grid('Promo', dow_cols), sep=':'))
  intr_mon_dow <- do.call(paste, c(expand.grid(mon_cols, dow_cols), sep=':'))
  
  feat_open_dow <- paste(intr_open_dow, collapse = ' + ')
  feat_promo_dow <- paste(intr_promo_dow, collapse = ' + ')
  feat_mon_dow <- paste(intr_mon_dow, collapse = ' + ')
  feat_hol <- paste(hol_type_cols, collapse = ' + ')
  feat_dow <- paste(dow_cols, collapse = ' + ')
  feat_dom <- paste(dom_cols, collapse = ' + ')
  feat_mon <- paste(mon_cols, collapse = ' + ')
  
  fm <- paste(c('~ Open + Promo + SchoolHoliday', feat_open_dow, feat_promo_dow, 
                feat_mon_dow, feat_hol, feat_dow, feat_dom, feat_mon), 
              collapse=' + ')
  
  model.matrix(as.formula(fm), dt)
}

AlignOverMissingDays <- function(dt) {
  date.diff <- dt[, Date-shift(Date, type='lag')]
  
  prob.idx <- which(!is.na(date.diff) & date.diff != 1)
  skip.days <- vector()
  for (idx in prob.idx) {
    #print(sprintf('    Idx after gap = %d', idx))
    # 1,4 => skip 4,5,6,7,1
    # 4,1 => skip 1,2,3,4
    # 3,1 => skip 1,2,3
    # 3,5 => skip 5,6,7,1,2,3
    #print(sprintf('    Prev = %d, Next = %d', dt[idx-1]$DayOfWeek, 
    #              dt[idx]$DayOfWeek))
    reqd.nextday <- (dt[idx-1]$DayOfWeek + 1)
    for (k in 0:7) {
      found.nextday <- dt[idx+k]$DayOfWeek
      if (found.nextday != reqd.nextday) {
        #print(sprintf('    move fwd to %d', found.nextday))
        skip.days <- c(skip.days, idx+k)
      } else {
        #print(sprintf('    aligned at %d', found.nextday))
        break
      }
    }
  }
  
  if (length(skip.days) > 0) {
    #print(sprintf('    Skipped days = %s', paste(skip.days, collapse =', ')))
    dt <- dt[-(skip.days)]
  }
  dt
}


ComputeRmspe <- function(yreal, ypred) {
  opendays <- yreal != 0
  spe <- ((yreal[opendays]-ypred[opendays])/yreal[opendays])^2  # sq. pct error
  rmspe <- sqrt(mean(spe))

  list(rmspe, sum(spe), length(spe))
}


GenDummy <- function(dt, dummy.col, prefix) {
  inds <- sort(unique(dummy.col))
  if (length(inds) > 1) {
    inds <- inds[2:length(inds)]  # remove first value to avoid multicollinearity
    dt[,(paste0(prefix, inds)):=lapply(inds,function(x)as.integer(dummy.col==x))]
  }
  dt
}

GenDummyAndRemove <- function(dt, dummy.col, dummy.col.str) {
  dt <- GenDummy(dt, dummy.col, dummy.col.str)
  dt <- dt[,-c(dummy.col.str), with=FALSE]
  dt
}
  
LabelEncode <- function(dt, dummy.col, newcolname) {
  dt[,(newcolname) := as.integer(as.factor(dummy.col))]
  dt
}

Combiner1 <- function(model.store.perf, dt.result) {
  for (j in 2:length(model.store.perf)) {
    set(model.store.perf, i=which(model.store.perf[[j]]>=1), j=j, value=0)
    nzrows <- which(model.store.perf[[j]]>0)
    set(model.store.perf, i=nzrows, j=j, 
        value=(1/(model.store.perf[nzrows,j,with=FALSE])^4))
  }
  
  w.model.namelist <- paste0('w.', model.namelist)
  
  for (s in stores) {
    dnr <- 0
    
    for (mid in seq_along(model.namelist)) {
      m.name <- model.namelist[mid]
      w.m.name <- w.model.namelist[mid]
      weight <- model.store.perf[store==s, get(m.name)]
      dnr <- dnr + weight
      dt.result[store==s, (w.m.name) := weight*get(m.name), with=FALSE]
    }
    
    dt.result[store==s, Sales := rowSums(.SD)/dnr, .SDcols=w.model.namelist]
  }
  
  dt.result
}

Combiner2 <- function(model.store.perf, dt.result) {
  
  model.store.perf[, MinPerf := do.call(pmin,.SD), .SDcols=model.namelist]
  
  for (mid in seq_along(model.namelist)) {
    m.name <- model.namelist[mid]
    #model.store.perf[, (m.name) := 1/exp(3*100*(get(m.name) - MinPerf))]
    model.store.perf[, (m.name) := 1/(1+exp((80*(get(m.name) - MinPerf)) - 5.5))]
  }
  
  w.model.namelist <- paste0('w.', model.namelist)
  
  for (s in stores) {
    dnr <- 0
    
    for (mid in seq_along(model.namelist)) {
      m.name <- model.namelist[mid]
      w.m.name <- w.model.namelist[mid]
      weight <- model.store.perf[store==s, get(m.name)]
      dnr <- dnr + weight
      dt.result[store==s, (w.m.name) := weight*get(m.name), with=FALSE]
    }
    
    dt.result[store==s, Sales := rowSums(.SD)/dnr, .SDcols=w.model.namelist]
  }
  
  dt.result
}

Combiner3.Avg <- function(model.store.perf, dt.result) {
  model.count <- length(model.namelist)
  dt.result[, Sales := rowSums(.SD)/model.count, .SDcols=model.namelist]
  
  dt.result
}

ComputeCombinedPred <- function(dt.result) {
  
  fsuffix <- GetFileSuffix()
  perf.fn <- paste0('generated/cv-modelstoreperf-', fsuffix, '.csv')
  
  model.store.perf <- fread(perf.fn, header = TRUE, sep = ",")
  setkey(model.store.perf, store)
  
  dt.result <- Combiner1(model.store.perf, dt.result)
  
  dt.result
}

GetFileSuffix <- function() {
  ifelse(length(stores)>100, 1115, length(stores))
}

SaveCVResults <- function(res.rmspe, res.ypred, res.cvsummary) {
  fsuffix <- GetFileSuffix()
  perf.fn <- paste0('generated/cv-modelstoreperf-', fsuffix, '.csv')
  cvpreds.fn <- paste0('generated/cv-modelstorepreds-', fsuffix, '.csv')
  cvsummary.fn <- paste0('generated/cv-summary-', fsuffix, '.csv')
  
  setorder(res.rmspe, store)
  setorder(res.ypred, store, dateid)
  
  write.csv(res.rmspe, file = perf.fn, row.names = FALSE)
  write.csv(res.ypred, file = cvpreds.fn, row.names = FALSE)
  write.csv(res.cvsummary, file = cvsummary.fn, row.names = FALSE)
}

SaveLBResults <- function(dt.result) {
  fsuffix <- GetFileSuffix()
  lbpreds.fn <- paste0('generated/lb-modelstorepreds-', fsuffix, '.csv')
  
  setorder(dt.result, Id, store)
  
  write.csv(dt.result, file = lbpreds.fn, row.names = FALSE)
}

LoadCVResults <- function() {
  fsuffix <- GetFileSuffix()
  perf.fn <- paste0('generated/cv-modelstoreperf-', fsuffix, '.csv')
  cvpreds.fn <- paste0('generated/cv-modelstorepreds-', fsuffix, '.csv')
  cvsummary.fn <- paste0('generated/cv-summary-', fsuffix, '.csv')
  
  if (do.incremental.ensemble & file.exists(perf.fn)) {
    perf.dt <- fread(perf.fn, header = TRUE, sep = ",")
    cvpreds.dt <- fread(cvpreds.fn, header = TRUE, sep = ",")
    cvsummary.dt <- fread(cvsummary.fn, header = TRUE, sep = ",")
    setkey(perf.dt, store)
    setkey(cvpreds.dt, store, dateid, yreal)
  } else {
    perf.dt <- data.table()
    cvpreds.dt <- data.table()
    cvsummary.dt <- data.table(summary=c(0))
  }
  
  list(perf.dt, cvpreds.dt, cvsummary.dt)
}

LoadLBResults <- function() {
  fsuffix <- GetFileSuffix()
  lbpreds.fn <- paste0('generated/lb-modelstorepreds-', fsuffix, '.csv')

  if (file.exists(lbpreds.fn)) {
    lbpreds.dt <- fread(lbpreds.fn, header = TRUE, sep = ",")
    setkey(lbpreds.dt, Id, store)
  } else {
    lbpreds.dt <- data.table()
  }
  
  lbpreds.dt
}
