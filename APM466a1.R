library(tidyverse)

data = read.csv(file.choose())
data = data %>% 
  mutate(m.month = 12*(as.numeric(substring(maturity.date,7,10))-2021) +
                         (as.numeric(substring(maturity.date,1,2))-2))

# Q3

# (a)

# calculates PV of cash flow, semi-annual compounding
cashflow = function(cpn, month, ir, price) {
  
  ncpn = floor(month/6)
  payment = 100*cpn/2
  df = 1/(1+ir/2)

  # PV at first coupon date
  total = 0
  # coupon only
  for (i in 0:ncpn) {
    total = total + payment*(df)^i
  }
  # FV at maturity
  total = total + 100*(df)^ncpn
  
  # PV now
  rest = month - 6*ncpn
  df.rest = 1/(1+ir*rest/12)
  total = total * df.rest
  
  if (price==0) {
    # don't consider price
    dirty = 0
  } else {
    dirty = payment/6*(6-rest) + price
  }
  
  return (total-dirty)
}


ytm.all = c()

for (i in 1:10) {
  ytm = c()
  for (j in 5:14) {
    # adjust month by 1/3 for data before Jan-20 
    sol = uniroot(cashflow, cpn=data$coupon[i], month=data$m.month[i]+(j<8)/3, price=data[i,j],
                  lower=-1, upper=1, tol = .Machine$double.eps)
    ytm = c(ytm, sol$root)
  }
  ytm.all = rbind(ytm.all, ytm)  
}

maturities = data$m.month/12

# include r0=0
maturities = c(0, maturities)
ytm.all = rbind(rep(0, 10), ytm.all)
matplot(maturities, ytm.all, type='l', col=1:10, ylab='YTM', xlab='year',
        main='0-5 year YTM curves')
legend('topleft',colnames(data)[5:14], col=1:10, lty=1:10, cex=0.6)


# (b)

# compute discount factor, semi-annual compounding
semi_df = function(month, ir) {
  n = floor(month/6)
  df = 1/(1+ir/2)
  rest = month - 6*n
  df.rest = 1/(1+ir*rest/12)

  return (df^n*df.rest)
}


spot.all = c()

for (j in 1:10) {
  
  months = c(0)
  dfs = c(1)
  
  for (i in 1:10) {
    
    ncpn = floor(data$m.month[i]/6)+1
    payment = 100*data$coupon[i]/2
    cpn.month = rev(seq(data$m.month[i], 0, -6))
    
    total = 0
    
    for (k in 1:ncpn) {
      m = cpn.month[k]
      
      # utilize computed rates
      if (m %in% months) {
        df = dfs[match(m, months)]
        total = total + payment*df
      } else {
        # bootstrap from ytm curve
        ytm = approx(maturities, ytm.all[,j], m/12)$y
        p = cashflow(data$coupon[i], m, ytm, 0)
        df = (p-total)/(100+payment)
        total = total + payment*df
        months = c(months, m)
        dfs = c(dfs, df)
      }
    }
  }
  
  spot = c()
  for (l in 2:length(dfs)) {
    f = function(y) {return (semi_df(months[l], y)-dfs[l])}
    sol = uniroot(f, lower=-1, upper=1, tol = .Machine$double.eps)
    spot= c(spot, sol$root)
  }
  spot.all = cbind(spot.all, spot)
}

# only plot 1 yr onwards
months = months[-1]
times15 = months[months>=12]/12
spot.sub = spot.all[months>=12,]
spot.sub = spot.sub[times15 %in% maturities,]
times15 = times15[times15 %in% maturities]
matplot(times15, spot.sub, type='l', col=1:10, ylab='spot rate', xlab='year',
        main='1-5 year spot curves')
legend('topleft',colnames(data)[5:14], col=1:10, lty=1:10, cex=0.6)


# (c)

fwd.all = c()

for (j in 1:10) {
  
  fwd = c()
  
  for (i in 1:4) {
    # interpolate spot rates
    si = approx(times15, spot.sub[,j], i)$y
    snext = approx(times15, spot.sub[,j], i+1)$y
    # compounding factor
    ci = (1+si/2)^(2*i)
    cnext = (1+snext/2)^(2*i+2)
    # solve for fwd rate
    rate = (sqrt(cnext/ci)-1)*2
    fwd = c(fwd, rate)
  }  
  
  fwd.all = cbind(fwd.all, fwd)
}

# plot 4 points
matplot(1:4, fwd.all, type='l', col=1:10, ylab='forward rate', xlab='',
        main='1-year forward curves', xaxt="n")
axis(1, at=1:4, labels=c('1yr-1yr', '1yr-2yr', '1yr-3yr', '1yr-4yr'))
legend('topleft',colnames(data)[5:14], col=1:10, lty=1:10, cex=0.6)


# Q5

# interpolate ytm
ytm15 = c()
for (j in 1:10) {
  ytm15 = cbind(ytm15, approx(maturities, ytm.all[,j], 1:5)$y)
}
X.ytm = log(ytm15[,2:10]/ytm15[,1:9])
cov.ytm = cov(t(X.ytm))
round(cov.ytm, 4)

fwd14 = fwd.all
X.fwd = log(fwd14[,2:10]/fwd14[,1:9])
cov.fwd = cov(t(X.fwd))
round(cov.fwd, 4)

# Q6

eigen(cov.ytm)
v.ytm = eigen(cov.ytm)$values
v.ytm/sum(v.ytm)

eigen(cov.fwd)
v.fwd = eigen(cov.fwd)$values
v.fwd/sum(v.fwd)

