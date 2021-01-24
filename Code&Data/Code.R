library(dplyr)
library(psych)
library(betareg)
library(corrplot)
library(glmnet)
library(bamlss)
library(lavaan)
library(semPlot)

#########################################################Survey Data############################################################
SurveyData <- read.csv("SurveyData.csv")
######################################Area Ratio ~ Test Score

SurveyData["Ratio.Trans"]<-asin(sqrt(SurveyData$Ratio)) 

#png("AreaRatio_TestScore.png",600,600)
p.value <- t.test(SurveyData$Ratio.Trans[SurveyData$TestScore>=quantile(SurveyData$TestScore,0.75)],SurveyData$Ratio.Trans[SurveyData$TestScore<=quantile(SurveyData$TestScore,0.25)])$p.value
SpRatio <- SurveyData$Ratio.Trans[SurveyData$TestScore>=quantile(SurveyData$TestScore,0.75)]
UsRatio <- SurveyData$Ratio.Trans[SurveyData$TestScore<=quantile(SurveyData$TestScore,0.25)]
XBar <- c(mean(SpRatio)
          ,sd(SpRatio)
          ,mean(UsRatio)
          ,sd(UsRatio))
Inter <- rbind(t.test(SpRatio)$conf.int
               ,t.test(UsRatio)$conf.int)

barR <- barplot(XBar[c(1,3)],axes = F
                #,names.arg = c("Bottom 5%","Top 5%"),
                #,space = c(0,0.03,0.5,0.03)
                ,ylim = c(0,max(Inter)+0.2),col ="cornflowerblue"
                ,ylab = "",main = "")
axis(1,labels = c("",""),at=barR)
axis(2)
segments(barR,Inter[,1],barR,Inter[,2], lwd = 1.5)
arrows(barR,Inter[,1],barR,Inter[,2]
       ,lwd = 1, angle = 90
       ,code = 3,length = 0.1)
#text(barR,0.05,round(XBar[c(1,3)],2))
#dev.off()
#######Regression

Mod1 <- betareg(Ratio~TestScore+Gender+Age,data = SurveyData)
summary(Mod1)
Mod2 <- betareg(Ratio~TestScore + Gender + Age + SpeakerNum + Frequency + Particular + SubjectKnowledge,data = SurveyData)
summary(Mod2)


################################ Idea novelty ~ Area ratio######################
Flag.top <- SurveyData$Ratio>=quantile(SurveyData$Ratio,0.75)
Flag.bot <- SurveyData$Ratio<=quantile(SurveyData$Ratio,0.25)

##Resampled idea novelty to individual
MonCarTop.Mean30 <- c()
MonCarBot.Mean30 <- c()
set.seed(202008006)
for (i in 1:100){
        MonCarTop.Mean30<- c(MonCarTop.Mean30,mean(sample(SurveyData$IndNovetyl[Flag.top],30)))
}
set.seed(202008006)
for (i in 1:100){
        MonCarBot.Mean30<- c(MonCarBot.Mean30,mean(sample(SurveyData$IndNovetyl[Flag.bot],30)))
}

t.test(MonCarTop.Mean30,MonCarBot.Mean30)


#MonCarTop.Mean20 <- c()
#MonCarBot.Mean20 <- c()
#set.seed(202008006)
#for (i in 1:100){
#        MonCarTop.Mean20<- c(MonCarTop.Mean20,mean(sample(SurveyData$IndNovetyl[Flag.top],20)))
#}
#set.seed(202008006)
#for (i in 1:100){
#        MonCarBot.Mean20<- c(MonCarBot.Mean20,mean(sample(SurveyData$IndNovetyl[Flag.bot],20)))
#}



#t.test(MonCarTop.Mean20,MonCarBot.Mean20)

#MonCarTop.Mean40 <- c()
#MonCarBot.Mean40 <- c()
#set.seed(202008006)
#for (i in 1:100){
#        MonCarTop.Mean40<- c(MonCarTop.Mean40,mean(sample(SurveyData$IndNovetyl[Flag.top],40)))
#}
#set.seed(202008006)
#for (i in 1:100){
#        MonCarBot.Mean40<- c(MonCarBot.Mean40,mean(sample(SurveyData$IndNovetyl[Flag.bot],40)))
#}


#t.test(MonCarTop.Mean40,MonCarBot.Mean40)


#png("Individual_Median Novelty.png",600,600)
p.value <- t.test(MonCarTop.Mean30,MonCarBot.Mean30)$p.value

Ori.top <- MonCarTop.Mean30
Ori.bot <- MonCarBot.Mean30
XBar <- c(mean(Ori.top)
          #,sd(SpRatio)
          ,mean(Ori.bot)
          #,sd(UsRatio)
)
Inter <- rbind(t.test(Ori.top)$conf.int
               ,t.test(Ori.bot)$conf.int)

barR <- barplot(XBar,axes = F
                #,names.arg = c("Bottom 5%","Top 5%"),
                #,space = c(0,0.03,0.5,0.03)
                ,ylim = c(0,max(Inter)+0.1),col = c(rep(c("brown3"),2))
                ,ylab = "",main = "")
axis(1,labels = c("",""),at=barR[1:2])
axis(2)
segments(barR[c(1,2)],Inter[,1],barR[c(1,2)],Inter[,2], lwd = 1.5)
arrows(barR[c(1,2)],Inter[,1],barR[c(1,2)],Inter[,2]
       ,lwd = 1, angle = 90
       ,code = 3,length = 0.1)
text(barR,0.15,round(XBar,2))
#legend("topright",c("Mean","SD"),col = c("brown3","cornflowerblue"),pch = 15)
#dev.off()


#p.value <- t.test(SurveyData$IndNovelty[Flag.top],SurveyData$IndNovelty[Flag.bot])$p.value

#Ori.top <- SurveyData$IndNovelty[Flag.top]
#Ori.bot <- SurveyData$IndNovelty[Flag.bot]
#XBar <- c(mean(Ori.top)
          #,sd(SpRatio)
#          ,mean(Ori.bot)
          #,sd(UsRatio)
#)
#Inter <- rbind(t.test(Ori.top)$conf.int
#               ,t.test(Ori.bot)$conf.int)

#barR <- barplot(XBar,axes = F
                #,names.arg = c("Bottom 5%","Top 5%"),
                #,space = c(0,0.03,0.5,0.03)
 #               ,ylim = c(0,max(Inter)+0.1),col = c(rep(c("brown3"),2))
#                ,ylab = "",main = "")
#axis(1,labels = c("",""),at=barR[1:2])
#axis(2)
#segments(barR[c(1,2)],Inter[,1],barR[c(1,2)],Inter[,2], lwd = 1.5)
#arrows(barR[c(1,2)],Inter[,1],barR[c(1,2)],Inter[,2]
#       ,lwd = 1, angle = 90
#       ,code = 3,length = 0.1)
#text(barR,0.15,round(XBar,2))
#legend("topright",c("Mean","SD"),col = c("brown3","cornflowerblue"),pch = 15)
#dev.off()


########idea novelty to group ~ area ratio
#png("Lexical Group Novelty.png",600,600)
Idf.Top <- SurveyData$GroNovelty[Flag.top]
Idf.Bot <- SurveyData$GroNovelty[Flag.bot]

p.value <- t.test(Idf.Top,Idf.Bot)$p.value
XBar <- c(mean(Idf.Top)
         ,mean(Idf.Bot))

Inter <- rbind(t.test(Idf.Top)$conf.int
               ,t.test(Idf.Bot)$conf.int)


barR <- barplot(XBar,axes = F
                ,ylim = c(0,max(Inter)+0.1),col = "brown3",main="" 
                ,ylab = "")
axis(1,labels = c("",""),at=barR)
axis(2)
segments(barR,Inter[,1],barR,Inter[,2], lwd = 1.5)
arrows(barR,Inter[,1],barR,Inter[,2]
       ,lwd = 1, angle = 90
       ,code = 3,length = 0.1)
text(barR,2,round(XBar,2))
#dev.off()


####Idea novelty to history 
#png("Lexical History Novelty.png",600,600)
HisNovelty.Top <- SurveyData$HisNovelty[Flag.top]
HisNovelty.Bot <- SurveyData$HisNovelty[Flag.bot]

p.value <- t.test(HisNovelty.Top,HisNovelty.Bot)$p.value
XBar <- c(mean(HisNovelty.Top)
          ,mean(HisNovelty.Bot))

Inter <- rbind(t.test(HisNovelty.Top)$conf.int
               ,t.test(HisNovelty.Bot)$conf.int)


barR <- barplot(XBar,axes = F
                ,ylim = c(0,max(Inter)+0.1),col = "brown3",main= ""
                ,ylab = "")
axis(1,labels = c("",""),at=barR)
axis(2)
segments(barR,Inter[,1],barR,Inter[,2], lwd = 1.5)
arrows(barR,Inter[,1],barR,Inter[,2]
       ,lwd = 1, angle = 90
       ,code = 3,length = 0.1)
text(barR,2,round(XBar,2))
#dev.off()





##LASSO regression#
lambdas <- 10^seq(2, -3, by = -.1)

set.seed(1234)
Model3.lasso <- cv.glmnet( as.matrix(SurveyData[,c("Ratio","Gender","Age","SpeakerNum","Particular","SubjectKnowledge","Frequency","TestScore")]),SurveyData$IndNovelty
                           , alpha = 1, lambda = lambdas)
coef(Model3.lasso,Model3.lasso$lambda.min)

set.seed(1234)
Model4.lasso <- cv.glmnet( as.matrix(SurveyData[,c("Ratio","Gender","Age","SpeakerNum","Particular","SubjectKnowledge","Frequency","TestScore")]),SurveyData$GroNovelty
          , alpha = 1, lambda = lambdas)
coef(Model4.lasso,Model4.lasso$lambda.min)

set.seed(1234)
Model5.lasso <- cv.glmnet(as.matrix(SurveyData[,c("Ratio","Gender","Age","SpeakerNum","Particular","SubjectKnowledge","Frequency","TestScore")]),SurveyData$HisNovelty
                         , alpha = 1, lambda = lambdas)
coef(Model5.lasso,Model5.lasso$lambda.min)

#png("FigS2.png",1600,1600)
pairs.panels(SurveyData[,c("IndNovelty","GroNovelty","HisNovelty"
                     ,"Ratio","TestScore","Gender"
                 ,"Age","SpeakerNum","Frequency"
                 ,"Particular","SubjectKnowledge")]
             ,stars=T
             ,cex.cor = 0.8
             ,hist.col="grey"
             ,breaks =20
             ,rug=F
             ,labels = "")
#dev.off()

##########Path Analysis#

SurveyData.std <- scale(SurveyData) %>% as.data.frame()
colnames(SurveyData.std) <- colnames(SurveyData)

Formu6 <- "
Ratio~TestScore 
IndNovelty~  Ratio +TestScore"
Model6.Path <- cfa(Formu6,data = SurveyData.std)
summary(Model6.Path, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)
semPaths(Model6.Path, "par",
         sizeMan = 15, sizeInt = 15, sizeLat = 15,
         edge.label.cex=1.5,
         fade=FALSE)



Formu7 <- "
Ratio~TestScore 
GroNovelty~ Ratio + TestScore 
"
Model7.Path <- cfa(Formu7,data = SurveyData.std)
summary(Model7.Path, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)
semPaths(Model7.Path, "par",
         sizeMan = 15, sizeInt = 15, sizeLat = 15,
         edge.label.cex=1.5,
         fade=FALSE)


Formu8 <- "
Ratio~TestScore 
HisNovelty~ Ratio + TestScore 
"
Model8.Path <- cfa(Formu8,data = SurveyData.std)
summary(Model8.Path, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)
semPaths(Model8.Path, "par",
         sizeMan = 15, sizeInt = 15, sizeLat = 15,
         edge.label.cex=1.5,
         fade=FALSE)

summary(SurveyData$IndNovelty)
#########################################################Amazon review Data############################################################
AmazonData <- read.csv("AmazonData.csv")

######################################Area Ratio ~ Distance to Specialists
AmazonData$Ratio.Trans <- asin(sqrt(AmazonData$Ratio))

Flag.C <- AmazonData$Dis2Specialist >= quantile(AmazonData$Dis2Specialist,0.75)
Flag.E <- AmazonData$Dis2Specialist <= quantile(AmazonData$Dis2Specialist,0.25)

#png("AreaRate_Dis2Specialist.png",600,600)
p.value <- t.test(AmazonData$Ratio.Trans[Flag.E],AmazonData$Ratio.Trans[Flag.C])$p.value
SpRate <- AmazonData$Ratio.Trans[Flag.E]
UsRate <- AmazonData$Ratio.Trans[Flag.C]
XBar <- c(mean(AmazonData$Ratio.Trans[Flag.E])
          ,sd(AmazonData$Ratio.Trans[Flag.E])
          ,mean(AmazonData$Ratio.Trans[Flag.C])
          ,sd(AmazonData$Ratio.Trans[Flag.C]))
Inter <- rbind(t.test(SpRate)$conf.int
               ,t.test(UsRate)$conf.int)

barR <- barplot(XBar[c(1,3)],axes = F
                #,names.arg = c("Bottom 5%","Top 5%"),
                #,space = c(0,0.03,0.5,0.03)
                ,ylim = c(0,max(Inter)+0.2),col ="grey"
                ,ylab = "",main = "")
axis(1,labels = c("",""),at=barR)
axis(2)
segments(barR,Inter[,1],barR,Inter[,2], lwd = 1.5)
arrows(barR,Inter[,1],barR,Inter[,2]
       ,lwd = 1, angle = 90
       ,code = 3,length = 0.1)
text(barR,0.05,round(XBar[c(1,3)],2))
#dev.off()

#Regression
ModelS1 <- betareg(Ratio~Dis2Specialist+LogWordCount+TargetID,data = AmazonData)
summary(ModelS1)

###########################Review Novelty##

########Group Novelty
Flag.bot <- AmazonData$Ratio <= quantile(AmazonData$Ratio,0.25)
Flag.top <- AmazonData$Ratio >= quantile(AmazonData$Ratio,0.75)

######
p.value <- t.test(AmazonData$GroNovelty[Flag.bot],AmazonData$GroNovelty[Flag.top])$p.value


GroNovelty.Bot <- AmazonData$GroNovelty[Flag.bot]
GroNovelty.Top <- AmazonData$GroNovelty[Flag.top]


#png("LexicalGroupNovelty.png",600,600)
XBar <- c(mean(GroNovelty.Top, na.rm=T),mean(GroNovelty.Bot,na.rm = T))
Inter <- rbind(t.test(GroNovelty.Top)$conf.int,t.test(GroNovelty.Bot)$conf.int)


barR <- barplot(XBar,axes = F
                ,ylim = c(0,max(Inter)+0.1),col = "grey",main= #paste("Group Novelty\np-value:",round(p.value,3),"***")
                        ,ylab = "")
axis(1,labels = c("",""),at=barR)
axis(2)
segments(barR,Inter[,1],barR,Inter[,2], lwd = 1.5)
arrows(barR,Inter[,1],barR,Inter[,2]
       ,lwd = 1, angle = 90
       ,code = 3,length = 0.1)
text(barR,0.5,round(XBar,2))
#dev.off()
##########History Novelty

p.value <- t.test(AmazonData$HisNovelty[Flag.bot],AmazonData$HisNovelty[Flag.top])$p.value


CH.Bot <- AmazonData$HisNovelty[Flag.bot]
CH.Top <- AmazonData$HisNovelty[Flag.top]


XBar <- c(mean(CH.Top, na.rm =T),mean(CH.Bot, na.rm =T))
Inter <- rbind(t.test(CH.Top)$conf.int,t.test(CH.Bot)$conf.int)

#png("LexicalHistoryNovelty.png",600,600)
#XBar
#Inter
barR <- barplot(XBar,axes = F
                ,ylim = c(0,max(Inter)),col = "grey",main= #paste("History Novelty\np-value:",round(p.value,3),"***")
                        ,ylab = "")
axis(1,labels = c("",""),at=barR)
axis(2)
segments(barR,Inter[,1],barR,Inter[,2], lwd = 1.5)
arrows(barR,Inter[,1],barR,Inter[,2]
       ,lwd = 1, angle = 90
       ,code = 3,length = 0.1)
text(barR,0.1,round(XBar,2))
#dev.off()

#Regression
ModelS2 <- lm(GroNovelty~Ratio+Dis2Specialist+LogWordCount,data=AmazonData)
ModelS3 <- lm(HisNovelty~Ratio+Dis2Specialist+LogWordCount,data=AmazonData)
summary(ModelS2)
summary(ModelS3)


#########################################################
AmazonData.std <- scale(AmazonData) %>% as.data.frame()
colnames(AmazonData.std) <- colnames(AmazonData)


FormuS4 <- "
Ratio~Dis2Specialist
GroNovelty~Ratio+Dis2Specialist
"
ModelS4.Path <- cfa(FormuS4,data = AmazonData.std)
summary(ModelS4.Path, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)
semPaths(ModelS4.Path, "par",
         sizeMan = 15, sizeInt = 15, sizeLat = 15,
         edge.label.cex=1.5,
         fade=FALSE)


FormuS5 <- "
Ratio~Dis2Specialist
HisNovelty~Ratio+Dis2Specialist
"
ModelS5.Path <- cfa(FormuS5,data = AmazonData.std)
summary(ModelS5.Path, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)
semPaths(ModelS5.Path, "par",
         sizeMan = 15, sizeInt = 15, sizeLat = 15,
         edge.label.cex=1.5,
         fade=FALSE)

