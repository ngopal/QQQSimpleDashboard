# Weekly
# install.packages("quantmod")
# install.packages("randomForest")
library(quantmod)
library(randomForest)
tickers <- c("QQQ", "MSFT", "AAPL", "INTC", "ORCL", "AMZN", "JNJ", 
             "VRSN", "USO", "ALTX", "CAT", "CRM", "GLD", "CMG", "AZO", 
             "SPY", "GLW", "KO", "JWN", "GM", "DUK", "XLE", "XLB", "XLI", 
             "XLY", "XLP", "XLV", "XLF", "XLK", "XTL", "XLU", "XLRE", "TWTR",
             "TGT", "VRSN", "WDC", "WMT", "PG", "SLV", "BAC", "SQ", "BIDU")
getSymbols(tickers, to="2019-09-24")

data = (cbind(lag(weeklyReturn(QQQ), -1), weeklyReturn(MSFT), weeklyReturn(AAPL), 
              weeklyReturn(INTC), weeklyReturn(ORCL), weeklyReturn(AMZN), weeklyReturn(JNJ),
              weeklyReturn(VRSN), weeklyReturn(USO), weeklyReturn(ALTX), weeklyReturn(CAT), 
              weeklyReturn(CRM), weeklyReturn(GLD), weeklyReturn(CMG), weeklyReturn(AZO),
              weeklyReturn(SPY), weeklyReturn(GLW), weeklyReturn(KO), weeklyReturn(JWN), weeklyReturn(GM),
              weeklyReturn(DUK), weeklyReturn(XLE), weeklyReturn(XLB), weeklyReturn(XLI), weeklyReturn(XLY),
              weeklyReturn(XLP), weeklyReturn(XLV), weeklyReturn(XLF), weeklyReturn(XLK), weeklyReturn(XTL),
              weeklyReturn(XLU), weeklyReturn(XLRE), weeklyReturn(TWTR), weeklyReturn(TGT), weeklyReturn(VRSN),
              weeklyReturn(WDC), weeklyReturn(WMT), weeklyReturn(PG), weeklyReturn(SLV), weeklyReturn(BAC),
              weeklyReturn(SQ), weeklyReturn(BIDU)))
data = na.fill(data, "extend")
data = data["::2019-09-20"] ## setting for this week
md_rf = randomForest(weekly.returns ~ ., data = data)
md_rf$importance / sum(md_rf$importance)
png(filename="./dashboard.png", res=3000)
par(mfrow=c(2,2))
plot(predict(md_rf, data), data[,1], main="QQ Plot")
barplot(t(data.frame(md_rf$importance / sum(md_rf$importance))), las=2, names.arg = tickers[2:length(tickers)], main="Variable Importance")
weeks <- 8
days = weeks
plot(c(as.vector(data[(dim(data)[1]-days):(dim(data)[1]-1),1]), NULL), type="l", xlim=c(1,days+1), main="Next Week Forecast")
lines(c(NULL,as.vector(predict(md_rf, data[(dim(data)[1]-days):(dim(data)[1])]))), col="red")

predict(md_rf, data[dim(data)[1]])

plot(c(as.vector(data[(dim(data)[1]-dim(data)[1]):(dim(data)[1]-1),1]), NULL), type="l", xlim=c(1,dim(data)[1]+1), main="Full Model")
lines(c(NULL,as.vector(predict(md_rf, data[(dim(data)[1]-dim(data)[1]):(dim(data)[1])]))), col="red")

dev.off()

