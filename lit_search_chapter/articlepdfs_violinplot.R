#files
#install.packages("vioplot")
library(sm)
library(vioplot)
library(ggplot2)
setwd("/home/ross/Dropbox/PantonScience/indexzootaxa/2013/")
x <- read.csv("forR_ArticlepdfFileSizes.csv", sep="\t")
names(x)
attach(x)

median(PLOS)

boxplot(PLOS,Zootaxa, notch=TRUE, names=c("PLOS","Zootaxa"))
boxplot(log(PLOS),log(Zootaxa), notch=TRUE, names=c("PLOS","Zootaxa"))
boxplot((PLOS/1048576),(Zootaxa/1048576), notch=TRUE, names=c("PLOS","Zootaxa"))
boxplot(log(PLOS/1048576),log(Zootaxa/1048576), las=1, ylab=expression(log[10]~"Megabytes"), notch=FALSE, names=c("PLOS","Zootaxa"),yaxp = c(-5,5,10), main="Article PDF file size distribution")

vioplot(xx,yy, col="cyan", names=c("PLOS","Zootaxa"))

median(PLOS/1048576)
median(Zootaxa/1048576, na.rm=TRUE)

xx <- log(PLOS/1048576)
xx2 <- rep("PLOS",length(xx))
xx3 <- cbind(xx,xx2)
yy <- log(Zootaxa[1:12476]/1048576)
yy2 <- rep("Zootaxa",length(yy))
yy3 <- cbind(yy,yy2)
zz <- data.frame(cbind(xx,yy))
colnames(zz) <- c("PLOS","Zootaxa")

###THIS WORKS
df <- data.frame(matrix(c(xx,yy),nrow=1))
names(df) <- paste0(c(rep("PLOS",length(xx)),rep("Zootaxa",length(yy))))
xdf <- stack(df)
xdf$values[33160]
xdf$group[33160]
xdf <- transform(xdf, group = substring(ind, 1, 4), obs = substring(ind, 6))

head(xdf)
ff <- median(xx)
ee <- median(yy)

med <- rbind(median(xx),median(yy))

plt <- ggplot(xdf, aes(x = group, y = values)) +
  geom_boxplot() + ggtitle("Article PDF File Size Distribution") + theme(plot.title = element_text(lineheight=.8, face="bold"))
plt + scale_x_discrete(breaks=c("PLOS", "Zoot"), labels=c("PLOS", "Zootaxa")) +
  scale_y_continuous(breaks=seq(-5, 5, 1)) +
  ylab(log[10]~"Megabytes (Mb)") +
  xlab("Article Corpus") +
  annotate("text", x = 1, y = 1, label = "n= 20694                     ") +
  annotate("text", x = 2, y = 1, label = "n= 11563                     ") +
  annotate("text", x = 1, y = -0.45, label = "median= 0.48Mb") +
  annotate("text", x = 2, y = -0.45, label = "median= 0.49Mb")

plt <- ggplot(xdf, aes(x = group, y = values)) +
  geom_violin() + ggtitle("Article PDF File Size Distribution") + theme(plot.title = element_text(lineheight=.8, face="bold"))
plt + scale_x_discrete(breaks=c("PLOS", "Zoot"), labels=c("PLOS", "Zootaxa")) +
  scale_y_continuous(breaks=seq(-5, 5, 1)) +
  ylab(log[10]~"Megabytes (Mb)") +
  xlab("Article Corpus") +
  annotate("text", x = 1, y = 3, label = "n= 20694                     ") +
  annotate("text", x = 2, y = 3, label = "n= 11563                     ") +
  annotate("text", x = 1, y = -5.3, label = "median =   0.48Mb ") +
  annotate("text", x = 2, y = -5.3, label = "median =   0.49Mb ") +
  stat_summary(aes(group=group), fun.y=median, geom="point",
             fill="black", shape="-", size=15, position = position_dodge(width = 1))
