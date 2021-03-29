question 1:
  d<-google_review_ratings.fix
  
  d<-data.frame(d,stringsAsFactors = F) 
  d2=as.data.frame(lapply(d,as.numeric))
  
  library(foreign)
  library(CCA)
  library(yacca)
  library(MASS)
  
  head(d)
  
  ccaWilks = function(set1, set2, cca)
  {
    ev = ((1 - cca$cor^2))
    ev
    
    n = dim(set1)[1]
    p = length(set1)
    q = length(set2)
    k = min(p, q)
    m = n - 3/2 - (p + q)/2
    m
    
    w = rev(cumprod(rev(ev)))
    
    # initialize
    d1 = d2 = f = vector("numeric", k)
    
    for (i in 1:k) 
    {
      s = sqrt((p^2 * q^2 - 4)/(p^2 + q^2 - 5))
      si = 1/s
      d1[i] = p * q
      d2[i] = m * s - p * q/2 + 1
      r = (1 - w[i]^si)/w[i]^si
      f[i] = r * d2[i]/d1[i]
      p = p - 1
      q = q - 1
    }
    
    pv = pf(f, d1, d2, lower.tail = FALSE)
    dmat = cbind(WilksL = w, F = f, df1 = d1, df2 = d2, p = pv)
  }
  
  names(d2)
  
  head(d2)
  str(d2)
  sum(is.na(d2))
  training <- na.omit(d2)
  sum(is.na(training))
  
  
  
  public = training[, 1:8]
  business = training[,9:24]
  
  
  names(public)
  names(business)
  
  
  
  c = cancor(public, business)
  c
  
  library(CCA)
  matcor(public, business)
  
  cc_mm = cc(public, business)
  cc_mm$cor
  
  ls(cc_mm)
  cc_mm$xcoef
  cc_mm$ycoef
  
  loadings_mm = comput(public, business, cc_mm)
  ls(loadings_mm)
  loadings_mm$corr.X.xscores
  loadings_mm$corr.Y.yscores
  
  wilks_mm = ccaWilks(public, business, cc_mm)
  round(wilks_mm, 2)
  
  s1 = diag(sqrt(diag(cov(psych))))
  s1 %*% cc_mm$xcoef
  
  s2 = diag(sqrt(diag(cov(academic))))
  s2 %*% cc_mm$ycoef
  
  plt.cc(cc_mm)
  
  
  library(yacca)
  
  public = training[, 1:8]
  business = training[,9:24]
  
  c2 = cca(business,public)
  
  summary(c2)
  
  helio.plot(c2, cv=1, x.name="bussiness Values", 
             y.name="public Values")
  
  helio.plot(c2, cv=2, x.name="business Values", 
             y.name="public Values")