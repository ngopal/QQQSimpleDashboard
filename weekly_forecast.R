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
#getSymbols(tickers, to="2019-09-24")

data = (cbind(lag(weeklyReturn(QQQ), -1), 
              weeklyReturn(MSFT), lag(weeklyReturn(MSFT), 1:12), 
              weeklyReturn(AAPL), lag(weeklyReturn(AAPL), 1:12), 
              weeklyReturn(INTC), lag(weeklyReturn(INTC), 1:12), 
              weeklyReturn(ORCL), lag(weeklyReturn(ORCL), 1:12),
              weeklyReturn(AMZN), lag(weeklyReturn(AMZN), 1:12),
              weeklyReturn(JNJ),lag(weeklyReturn(JNJ), 1:12),
              weeklyReturn(VRSN), lag(weeklyReturn(VRSN), 1:12),
              weeklyReturn(USO), lag(weeklyReturn(USO), 1:12),
              weeklyReturn(ALTX), lag(weeklyReturn(ALTX), 1:12),
              weeklyReturn(CAT), lag(weeklyReturn(CAT), 1:12),
              weeklyReturn(CRM), lag(weeklyReturn(CRM), 1:12),
              weeklyReturn(GLD), lag(weeklyReturn(GLD), 1:12),
              weeklyReturn(CMG), lag(weeklyReturn(CMG), 1:12),
              weeklyReturn(AZO),lag(weeklyReturn(AZO), 1:12),
              weeklyReturn(SPY), lag(weeklyReturn(SPY), 1:12),
              weeklyReturn(GLW), lag(weeklyReturn(GLW), 1:12),
              weeklyReturn(KO), lag(weeklyReturn(KO), 1:12),
              weeklyReturn(JWN), lag(weeklyReturn(JWN), 1:12),
              weeklyReturn(GM),lag(weeklyReturn(GM), 1:12),
              weeklyReturn(DUK), lag(weeklyReturn(DUK), 1:12),
              weeklyReturn(XLE), lag(weeklyReturn(XLE), 1:12),
              weeklyReturn(XLB), lag(weeklyReturn(XLB), 1:12),
              weeklyReturn(XLI), lag(weeklyReturn(XLI), 1:12),
              weeklyReturn(XLY),lag(weeklyReturn(XLY), 1:12),
              weeklyReturn(XLP), lag(weeklyReturn(XLP), 1:12),
              weeklyReturn(XLV), lag(weeklyReturn(XLV), 1:12),
              weeklyReturn(XLF), lag(weeklyReturn(XLF), 1:12),
              weeklyReturn(XLK), lag(weeklyReturn(XLK), 1:12),
              weeklyReturn(XTL),lag(weeklyReturn(XTL), 1:12),
              weeklyReturn(XLU), lag(weeklyReturn(XLU), 1:12),
              weeklyReturn(XLRE), lag(weeklyReturn(XLRE), 1:12),
              weeklyReturn(TWTR), lag(weeklyReturn(TWTR), 1:12),
              weeklyReturn(TGT), lag(weeklyReturn(TGT), 1:12),
              weeklyReturn(VRSN),lag(weeklyReturn(VRSN), 1:12),
              weeklyReturn(WDC), lag(weeklyReturn(WDC), 1:12),
              weeklyReturn(WMT), lag(weeklyReturn(WMT), 1:12),
              weeklyReturn(PG), lag(weeklyReturn(PG), 1:12),
              weeklyReturn(SLV), lag(weeklyReturn(SLV), 1:12),
              weeklyReturn(BAC),lag(weeklyReturn(BAC), 1:12),
              weeklyReturn(SQ), lag(weeklyReturn(SQ), 1:12),
              weeklyReturn(BIDU), lag(weeklyReturn(BIDU), 1:12)
              ))
data = na.fill(data, "extend")
#data = data["::2019-09-20"] ## setting for this week
md_rf = randomForest(weekly.returns ~ ., data = data)
md_rf$importance / sum(md_rf$importance)
png(filename="./dashboard.png")
par(mfrow=c(2,2))
plot(predict(md_rf, data), data[,1], main="QQ Plot")
#barplot(t(data.frame(md_rf$importance / sum(md_rf$importance))), las=2, names.arg = tickers[2:length(tickers)], main="Variable Importance")
barplot(t(data.frame(md_rf$importance / sum(md_rf$importance))), las=2, main="Variable Importance")
weeks <- 8
days = weeks
plot(c(as.vector(data[(dim(data)[1]-days):(dim(data)[1]-1),1]), NULL), type="l", xlim=c(1,days+1), main="Next Week Forecast")
lines(c(NULL,as.vector(predict(md_rf, data[(dim(data)[1]-days):(dim(data)[1])]))), col="red")

predict(md_rf, data[dim(data)[1]])

plot(c(as.vector(data[(dim(data)[1]-dim(data)[1]):(dim(data)[1]-1),1]), NULL), type="l", xlim=c(1,dim(data)[1]+1), main="Full Model")
lines(c(NULL,as.vector(predict(md_rf, data[(dim(data)[1]-dim(data)[1]):(dim(data)[1])]))), col="red")

dev.off()

