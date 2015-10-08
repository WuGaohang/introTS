#2-2
require(fUnitRoots)
da = read.table("m-dec125910-6111.txt", header = T)
d2 = da[,3]
d10 = da[,6]
#(a)
res = Box.test(d2,  lag = 12, type = 'Ljung')
pvalue = res$p.value
if (pvalue > 0.05)
{
  cat("p-value = ",pvalue," > 0.05, cannot reject the null hypothesis")
}else
{
  cat("p-value = ",pvalue," <= 0.05, reject the null hypothesis")
}
res = Box.test(d10, lag = 12, type = 'Ljung')
pvalue = res$p.value
if (pvalue > 0.05)
{
  cat("p-value = ",pvalue," > 0.05, cannot reject the null hypothesis")
}else
{
  cat("p-value = ",pvalue," <= 0.05, reject the null hypothesis")
}
#(b)
m1 = ar(diff(d2), method='mle')
p = m1$order
acf(d2)
q = 0
d = 0
temp = d2
while (pvalue > 0.05)
{
  d2 = diff(d2)
  m2 = ar(diff(d2), method = 'mle')
  res = adfTest(d2,lags=m2$order,type=c("c"))
  pvalue = res@test$p.value
  if (pvalue > 0.05)
  {
    d = d + 1
  }
}
m3 = arima(d2, order = c(p, d, q))
m3
#(c)
tsdiag(m3, gof = 12)
predict(m3, 12)