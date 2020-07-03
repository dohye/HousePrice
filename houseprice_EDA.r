
require('gridExtra')
library(ggplot2)
library(ggpubr)
library(dplyr)
library(corrplot)
library(GGally)
library(ggcorrplot)
#theme_set(theme_minimal())
theme_set(theme_light())

setwd("C:/Users/dohye/Desktop/bayseian")
house <- read.csv("train_new.csv", header=TRUE)
str(house)
summary(house)
house[1:6,]


house$date = as.numeric(substr(house$date,1,8)) # date 변수 : 뒤에는 시간이어서

house$log_price = log(house$price) # log변환
house$room = house$bedrooms + house$bathrooms

cor(house[,c(3,24)]) # 0.47

####변수####
id = house$id
price = house$price
date = house$date
bedrooms= house$bedrooms
bathrooms = house$bathrooms
sqft_living = house$sqft_living
sqft_lot = house$sqft_lot
sqft_above = house$sqft_above
sqft_basement = house$sqft_basement
floors = house$floors
waterfront = house$waterfront
view = house$view
condition = house$condition
grade = house$grade
yr_built = house$yr_built
yr_renovated = house$yr_renovated
zipcode = house$zipcode
lat = house$lat
long = house$long

#######################

g_price <- ggplot(house, aes(x=price))
# ggplot (density)
g_price + geom_density(aes(y = ..count..), fill="lightgray") + geom_vline(aes(xintercept=mean(price)), linetype="dashed",size=0.6)
# rplot
plot(density(house$price), xlab="price (dollar)", main="price",sub="Price of each home sold")
# ggplot (hist + density)
g_price + geom_histogram(aes(y = ..density..), colour="black", fill="white") + geom_density(alpha=0.2, fill="#FF6666")


summary(house$price)
summary(log(house$price))
summary(scale(house$price))
plot(density(log(house$price)))
plot(density(scale(log(house$price))))

# price_log변환 ####
p1 <- ggplot(house, aes(x=price)) + geom_density(fill="lightgray") + 
      geom_vline(aes(xintercept=mean(price)), linetype="dashed",size=0.6) + 
      xlab("price") + ggtitle("Density plot of price")
p2 <- ggplot(house, aes(x=log(price))) + geom_density(alpha=0.5,fill='antiquewhite3') + 
      geom_vline(aes(xintercept=mean(log(price))), linetype="dashed",size=0.6) + 
      xlab("log price") + ggtitle("Density plot of log price")

p3 <- ggplot(house, aes(y=price)) + geom_boxplot(alpha=0.5) + 
      xlab("price") + ggtitle("Boxplot of price")
p4 <- ggplot(house, aes(y=log(price))) + geom_boxplot(alpha=0.5)+ 
      xlab("log price") + ggtitle("Boxplot of log price")
  
grid.arrange(p1, p2, p3, p4, ncol=2)



# correlation ####
M <- cor(house %>% select(price, sqft_living15, sqft_lot15, sqft_above, sqft_basement))
corrplot(M, method = "circle", tl.col="black")
corrplot.mixed(M, upper = "circle", lower="number", tl.col="black", tl.pos = "lt", lower.col = "black",number.cex = .7)
?corrplot.mixed

# 연속형 범주들만 상관계수 구해보기
M <- cor(house %>% select(price, sqft_living, sqft_lot, sqft_living15, sqft_lot15, sqft_above, sqft_basement,bedrooms,bathrooms,floors,view,condition,grade,yr_built))
corrplot.mixed(M, upper = "circle", lower="number", tl.col="black", tl.pos = "lt", lower.col = "black",number.cex = .7)

## 스피어만 상관계수로 전체 다 
corrplot.mixed(cor(house[,3:21], method="spearman"), upper = "circle", lower="number", tl.col="black", tl.pos = "lt", lower.col = "black",number.cex = .7)

cor.mat <- cor(house %>% select(price, sqft_living, sqft_lot, sqft_living15, sqft_lot15, sqft_above, sqft_basement))
par(mfrow=c(1,1))
corrplot.mixed(cor.mat, lower = "ellipse", upper="number", tl.col="black")

M <- cor(house %>% select(price, sqft_living15, sqft_lot15, sqft_above, sqft_basement))
corrplot(M, method = "circle")
corrplot.mixed(M, lower = "ellipse", upper="number", tl.col="black")

ggpairs(house[,3:5])




my_data <- house[,c(c(3:14),c(17:22))] #yr_built, yr_renovated 뺀거
corr <- round(cor(my_data, method="spearman"), 2)
ggcorrplot(corr, #, p.mat = cor_pmat(my_data),
           hc.order = TRUE, #type = "lower",
           color = c("#FC4E07", "white", "#00AFBB"),
           outline.col = "white", lab = TRUE)




#### 범주형 변수 ####
# log scale 비교 price & zipcode ####
p1 <- ggplot(house, aes(x=factor(zipcode), group=factor(zipcode), y=price)) + 
  geom_boxplot() + 
  ggtitle("Boxplot of price by zipcode")

p2 <- ggplot(house, aes(x=factor(zipcode), group=factor(zipcode), y=log(price))) + 
  geom_boxplot() + 
  ggtitle("Boxplot of log price by zipcode")

grid.arrange(p1, p2, nrow=2)

# boxplot (x=zipcode, y=price) ####
p1<-ggplot(house, aes(x=factor(zipcode), group=factor(zipcode), y=log(price), fill=factor(zipcode))) + geom_boxplot() + 
  xlab("zipcode")+ ylab("log price")+ ggtitle("Boxplot of log price by zipcode")+
  theme(legend.position = "none", axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5 ))


temp <- house %>% group_by(zipcode) %>% summarize(freq=n(), mean_log_price=mean(log(price)))
temp$zipcode <- factor(temp$zipcode)
p2<-temp %>% arrange(desc(mean_log_price)) %>% 
  ggplot(aes(x=reorder(zipcode, mean_log_price), y=mean_log_price, group=1)) + 
  geom_line() +
  geom_point() +
  geom_hline(yintercept=mean(house$log_price), linetype="dashed", color = "red") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5 ))+
  xlab("zipcode")+ ylab("mean_log price")+
  ggtitle("mean log price by zipcode")


grid.arrange(p1, p2, nrow=2)


# boxplot (x=bathroom, y=price) ####
p1 <- ggplot(house, aes(x=factor(bathrooms), group=factor(bathrooms), y=log(price), fill=factor(bathrooms))) + 
  geom_boxplot() + 
  theme(legend.position = "none")+
  ggtitle("Boxplot of log price by bathrooms")
# full bath : 화장실, 세면대, 샤워실, 욕조 3/4 bath : 화장실, 세면대, 샤워실 half bath : 화장실, 세면대 미국에서는 보통 위와 같이 bathroom을 구분
# 그래서 0.5 + 0.75를 하면 1.25 같은 값이 나올 수도 있음

# boxplot (x=floor, y=price) ####

p2<-ggplot(house, aes(x=factor(floors), group=factor(floors), y=log(price), fill=factor(floors))) + 
  geom_boxplot() + 
  theme(legend.position = "none")+
  ggtitle("Boxplot of log price by floors")
grid.arrange(p1, p2, nrow=2)

# boxplot (x=view, y=price) ####
table(house$view)
p1 <- ggplot(house, aes(x=factor(view), group=factor(view), y=log(price), fill=factor(view))) + 
  geom_boxplot() + 
  theme(legend.position = "none")+
  ggtitle("Boxplot of log price by view")

# boxplot (x=condition, y=price) ####
table(house$condition)
p2 <- ggplot(house, aes(x=factor(condition), group=factor(condition), y=log(price), fill=factor(condition))) + 
  geom_boxplot() + 
  theme(legend.position = "none")+
  ggtitle("Boxplot of log price by condition")

# boxplot (x=grade, y=price) ####
table(house$grade)
ggplot(house, aes(x=factor(grade), fill=)) + geom_bar()
p1 <- ggplot(house, aes(x=factor(grade), group=factor(grade), y=log(price), fill=factor(grade))) + 
  geom_boxplot() +
  theme(legend.position = "none")+
  ggtitle("Boxplot of log price by grade")

# boxplot (x=bedrooms, y=price) ####
p2 <- ggplot(house, aes(x=factor(bedrooms), group=factor(bedrooms), y=log(price), fill=factor(bedrooms))) + 
  geom_boxplot() +
  theme(legend.position = "none")+
  ggtitle("Boxplot of log price by bedrooms")

grid.arrange(p1, p2, nrow=2)

# boxplot (x=waterfront, y=price) ####
table(house$waterfront)

p3<- ggplot(house, aes(x=factor(waterfront), group=factor(waterfront), y=log(price), fill=factor(waterfront))) + 
  geom_boxplot() + 
  theme(legend.position = "none")+
  ggtitle("Boxplot of log price by waterfront")

grid.arrange(p1, p2, p3, p4, nrow=2)



# , col=log(lat) # 색 다르게 할 수 있음




#### 연속형 변수 ####
# density & scatter plot (x=sqft_living, y=price)####
p1<-ggplot(house, aes(sqft_living)) + 
  geom_density(alpha=0.3, fill='lightgray') +
  ggtitle("Density plot of sqft_living")

p2<-ggplot(house, aes(x=sqft_living, y=log(price))) +
  geom_point(shape=1, alpha=0.1) +
  geom_smooth(method=lm, color="red", se=FALSE) +
  ggtitle("Scatter plot of sqft_living and log price")
#grid.arrange(p1, p2, ncol=2)

p3<-ggplot(house, aes(log(sqft_living))) + 
  geom_density(alpha=0.3, fill='antiquewhite3') +
  ggtitle("Density plot of sqft_living")

p4<-ggplot(house, aes(x=log(sqft_living), y=log(price))) +
  geom_point(shape=1, alpha=0.1) +
  geom_smooth(method=lm, color="red", se=FALSE) +
  ggtitle("Scatter plot of sqft_living and log price")
grid.arrange(p1, p2, p3, p4, ncol=2)

# density & scatter plot (x=sqft_living15, y=price)####
p1<-ggplot(house, aes(sqft_living15)) + 
  geom_density(alpha=0.3, fill='antiquewhite3') +
  ggtitle("Density plot of sqft_living15")

p2<-ggplot(house, aes(x=sqft_living15, y=log(price))) +
  geom_point(shape=1, alpha=0.1) +
  geom_smooth(method=lm, color="red", se=FALSE) +
  ggtitle("Scatter plot of sqft_living15 and log price")
grid.arrange(p1, p2, ncol=2)

# density & scatter plot (x=sqft_lot, y=price)####
ggplot(house, aes(sqft_lot)) + 
  geom_density(alpha=0.3) +
  ggtitle("Density plot of sqft_lot")

ggplot(house, aes(x=sqft_lot, y=log(price))) +
  geom_point(shape=1, alpha=0.1) +
  geom_smooth(method=lm, color="red", se=FALSE) +
  ggtitle("Scatter plot of sqft_lot and log price")

# density & scatter plot (x=sqft_lot15, y=price)####
ggplot(house, aes(sqft_lot15)) + 
  geom_density(alpha=0.3) +
  ggtitle("Density plot of sqft_lot15")

ggplot(house, aes(x=sqft_lot15, y=log(price))) +
  geom_point(shape=1, alpha=0.1) +
  geom_smooth(method=lm, color="red", se=FALSE) +
  ggtitle("Scatter plot of sqft_lot15 and log price")

# density & scatter plot (x=sqft_above, y=price)####
p1<-ggplot(house, aes(sqft_above)) + 
  geom_density(alpha=0.3, fill='antiquewhite3') +
  ggtitle("Density plot of sqft_above")

p2<-ggplot(house, aes(x=sqft_above, y=log(price))) +
  geom_point(shape=1, alpha=0.1) +
  geom_smooth(method=lm, color="red", se=FALSE) +
  ggtitle("Scatter plot of sqft_above and log price")
grid.arrange(p1, p2, ncol=2)

# density & scatter plot (x=sqft_basement, y=price)####
ggplot(house, aes(sqft_basement)) + 
  geom_density(alpha=0.3) +
  ggtitle("Density plot of sqft_basement")

ggplot(house, aes(x=sqft_basement, y=log(price))) +
  geom_point(shape=1, alpha=0.1) +
  geom_smooth(method=lm, color="red", se=FALSE) +
  ggtitle("Scatter plot of sqft_basement and log price")

