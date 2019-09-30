# Weekly
# install.packages("quantmod")
# install.packages("randomForest")
library(quantmod)
library(randomForest)
library(gbm)
tickers <- c("QQQ", "MSFT", "AAPL", "INTC", "ORCL", "AMZN", "JNJ", 
             "VRSN", "USO", "ALTX", "CAT", "CRM", "GLD", "CMG", "AZO", 
             "SPY", "GLW", "KO", "JWN", "GM", "DUK", "XLE", "XLB", "XLI", 
             "XLY", "XLP", "XLV", "XLF", "XLK", "XTL", "XLU", "XLRE", "TWTR",
             "TGT", "VRSN", "WDC", "WMT", "PG", "SLV", "BAC", "SQ", "BIDU",
             "MU", "TSLA", "BTCUSD=X", "NFLX", "GOOG", "BRK-B", "BAC", "JPM", "MS")
#getSymbols(tickers, to="2019-09-24")
getSymbols(tickers)
window = 12
data = (cbind(lag(weeklyReturn(QQQ), -1), 
              weeklyReturn(MSFT), lag(weeklyReturn(MSFT), 1:window), 
              weeklyReturn(AAPL), lag(weeklyReturn(AAPL), 1:window), 
              weeklyReturn(INTC), lag(weeklyReturn(INTC), 1:window), 
              weeklyReturn(ORCL), lag(weeklyReturn(ORCL), 1:window),
              weeklyReturn(AMZN), lag(weeklyReturn(AMZN), 1:window),
              weeklyReturn(JNJ),lag(weeklyReturn(JNJ), 1:window),
              weeklyReturn(VRSN), lag(weeklyReturn(VRSN), 1:window),
              weeklyReturn(USO), lag(weeklyReturn(USO), 1:window),
              weeklyReturn(ALTX), lag(weeklyReturn(ALTX), 1:window),
              weeklyReturn(CAT), lag(weeklyReturn(CAT), 1:window),
              weeklyReturn(CRM), lag(weeklyReturn(CRM), 1:window),
              weeklyReturn(GLD), lag(weeklyReturn(GLD), 1:window),
              weeklyReturn(CMG), lag(weeklyReturn(CMG), 1:window),
              weeklyReturn(AZO),lag(weeklyReturn(AZO), 1:window),
              weeklyReturn(SPY), lag(weeklyReturn(SPY), 1:window),
              weeklyReturn(GLW), lag(weeklyReturn(GLW), 1:window),
              weeklyReturn(KO), lag(weeklyReturn(KO), 1:window),
              weeklyReturn(JWN), lag(weeklyReturn(JWN), 1:window),
              weeklyReturn(GM),lag(weeklyReturn(GM), 1:window),
              weeklyReturn(DUK), lag(weeklyReturn(DUK), 1:window),
              weeklyReturn(XLE), lag(weeklyReturn(XLE), 1:window),
              weeklyReturn(XLB), lag(weeklyReturn(XLB), 1:window),
              weeklyReturn(XLI), lag(weeklyReturn(XLI), 1:window),
              weeklyReturn(XLY),lag(weeklyReturn(XLY), 1:window),
              weeklyReturn(XLP), lag(weeklyReturn(XLP), 1:window),
              weeklyReturn(XLV), lag(weeklyReturn(XLV), 1:window),
              weeklyReturn(XLF), lag(weeklyReturn(XLF), 1:window),
              weeklyReturn(XLK), lag(weeklyReturn(XLK), 1:window),
              weeklyReturn(XTL),lag(weeklyReturn(XTL), 1:window),
              weeklyReturn(XLU), lag(weeklyReturn(XLU), 1:window),
              weeklyReturn(XLRE), lag(weeklyReturn(XLRE), 1:window),
              weeklyReturn(TWTR), lag(weeklyReturn(TWTR), 1:window),
              weeklyReturn(TGT), lag(weeklyReturn(TGT), 1:window),
              weeklyReturn(VRSN),lag(weeklyReturn(VRSN), 1:window),
              weeklyReturn(WDC), lag(weeklyReturn(WDC), 1:window),
              weeklyReturn(WMT), lag(weeklyReturn(WMT), 1:window),
              weeklyReturn(PG), lag(weeklyReturn(PG), 1:window),
              weeklyReturn(SLV), lag(weeklyReturn(SLV), 1:window),
              weeklyReturn(BAC),lag(weeklyReturn(BAC), 1:window),
              weeklyReturn(SQ), lag(weeklyReturn(SQ), 1:window),
              weeklyReturn(BIDU), lag(weeklyReturn(BIDU), 1:window), 
              weeklyReturn(MU), lag(weeklyReturn(MU), 1:window), 
              weeklyReturn(TSLA), lag(weeklyReturn(TSLA), 1:window), 
              weeklyReturn(`BTCUSD=X`), lag(weeklyReturn(`BTCUSD=X`), 1:window), 
              weeklyReturn(NFLX), lag(weeklyReturn(NFLX), 1:window), 
              weeklyReturn(GOOG), lag(weeklyReturn(GOOG), 1:window), 
              weeklyReturn(`BRK-B`), lag(weeklyReturn(`BRK-B`), 1:window), 
              weeklyReturn(BAC), lag(weeklyReturn(BAC), 1:window), 
              weeklyReturn(JPM), lag(weeklyReturn(JPM), 1:window), 
              weeklyReturn(MS), lag(weeklyReturn(MS), 1:window)
              ))
data = na.fill(data, "extend")
#data = data["::2019-09-20"] ## setting for this week
# Models
md_rf = randomForest(weekly.returns ~ ., data = data)
md_rf2 = glm(weekly.returns ~ ., data = data)
md_rf3 = gbm(weekly.returns ~ ., data = data, n.trees = 1000)

# Creating Image
png(filename="./dashboard.png")
par(mfrow=c(3,3)) # rows, cols

weeks <- 8
days = weeks
plot(c(as.vector(data[(dim(data)[1]-days):(dim(data)[1]-1),1]), NULL), type="l", xlim=c(1,days+1), main="Next Week RF Forecast")
lines(c(NULL,as.vector(predict(md_rf, data[(dim(data)[1]-days):(dim(data)[1])]))), col="red")
dd <- cbind(as.vector(data[,1]), predict(md_rf, data[,-1]))
res <- ifelse(((dd[,1] > 0 & dd[,2] > 0) | (dd[,1] < 0 & dd[,2] < 0)), 1, 0)
text(8, 0, summary(res)[4])

plot(c(as.vector(data[(dim(data)[1]-days):(dim(data)[1]-1),1]), NULL), type="l", xlim=c(1,days+1), main="Next Week GLM Forecast")
lines(c(NULL,as.vector(predict(md_rf2, data[(dim(data)[1]-days):(dim(data)[1])]))), col="red")
dd <- cbind(as.vector(data[,1]), predict(md_rf2, data[,-1]))
res <- ifelse(((dd[,1] > 0 & dd[,2] > 0) | (dd[,1] < 0 & dd[,2] < 0)), 1, 0)
text(8, 0, summary(res)[4])

plot(c(as.vector(data[(dim(data)[1]-days):(dim(data)[1]-1),1]), NULL), type="l", xlim=c(1,days+1), main="Next Week GBM Forecast")
lines(c(NULL,as.vector(predict(md_rf3, data[(dim(data)[1]-days):(dim(data)[1])], n.trees=1000))), col="red")
dd <- cbind(as.vector(data[,1]), predict(md_rf3, data[,-1], n.trees=1000))
res <- ifelse(((dd[,1] > 0 & dd[,2] > 0) | (dd[,1] < 0 & dd[,2] < 0)), 1, 0)
text(8, 0, summary(res)[4])

plot(predict(md_rf, data), data[,1], main="QQ Plot RF")
plot(predict(md_rf2, data), data[,1], main="QQ Plot GLM")
plot(predict(md_rf3, data, n.trees = 1000), data[,1], main="QQ Plot GBM")

barplot(t(data.frame(md_rf$importance / sum(md_rf$importance))), las=2, main="Variable RF Importance")
barplot(t(data.frame(md_rf2$coefficients)), las=2, main="GLM Coefficients")
barplot(t(data.frame(summary(md_rf3)$rel.inf)), las=2, main="GBM Variable Importance")


#predict(md_rf, data[dim(data)[1]])
#plot(c(as.vector(data[(dim(data)[1]-dim(data)[1]):(dim(data)[1]-1),1]), NULL), type="l", xlim=c(1,dim(data)[1]+1), main="Full Model")
#lines(c(NULL,as.vector(predict(md_rf, data[(dim(data)[1]-dim(data)[1]):(dim(data)[1])]))), col="red")

dev.off()

