library(ggplot2)
library(scales)
library(dplyr)
library(randomForest)

#install.packages("mice")
library(mice)
#install.packages("ggthemes")
library(ggthemes)

train=read.csv("train.csv",stringsAsFactors = F)
test=read.csv("test.csv",stringsAsFactors = F)

full=bind_rows(train,test)

str(full)

#get titles
full$Title=gsub('(.*, )|(\\..*)','',full$Name)

table(full$Sex,full$Title)

rare_title=c("Dona","Lady","the Countess","Capt","Col","Don","Dr","Major","Rev","Sir","Jonkheer")

#re-assign titles
full$Title[full$Title=="Mlle"]="Miss"
full$Title[full$Title=="Ms"]="Miss"
full$Title[full$Title=="Mme"]="Mrs"
full$Title[full$Title %in% rare_title]="Rare Title"
#TODO: what is %in%?

table(full$Sex,full$Title)

#get surname
full$Surname=sapply(full$Name, function(x) strsplit(x, split = '[,.]')[[1]][1])
#TODO: What is sapply?

cat(paste("We have <b>", nlevels(factor(full$Surname)), "</b> unique surnames. I would be interested to infer ethnicity based on surname --- another time."))
#TODO: what is cat?

#do families sink together?
full$Fsize=full$SibSp + full$Parch + 1
full$Family=paste(full$Surname,full$Fsize,sep = "_")

ggplot(full[1:891,],aes(x= Fsize,fill=factor(Survived)))+
        geom_bar(stat="count",position="dodge")+
        scale_x_continuous(breaks = c(1:11))+
        labs(x="Family Size")+
        theme_few()


#collapse this variable in 3 levels
full$FsizeD[full$Fsize==1]="singleton"
full$FsizeD[full$Fsize<5 & full$Fsize>1]="small"
full$FsizeD[full$Fsize>4]="large"

#show family size by survival in a mosaic plot
mosaicplot(table(full$FsizeD,full$Survived),main = "Family Size by Survival",
           shade = TRUE)

#look into passanger cabin
full$Cabin[1:28]

strsplit(full$Cabin[2],NULL)[[1]]

#create a deck variable
full$Dec=factor(sapply(full$Cabin,function(x) strsplit(x,NULL)[[1]][1]))
#TODO:learn factor and sapply

#dealing with missing values
full[c(62,830),"Embarked"]

cat(paste('We will infer their values for **embarkment** based on present data that we can imagine may be relevant: **passenger class** and **fare**. We see that they paid<b> $', full[c(62, 830), 'Fare'][[1]][1], '</b>and<b> $', full[c(62, 830), 'Fare'][[1]][2], '</b>respectively and their classes are<b>', full[c(62, 830), 'Pclass'][[1]][1], '</b>and<b>', full[c(62, 830), 'Pclass'][[1]][2], '</b>. So from where did they embark?'))

#get rid of our missing passanger ID's
embark_fare=full %>% 
        filter(PassengerId != 62 & PassengerId != 830)

ggplot(embark_fare,aes(x=Embarked,y=Fare,fill=factor(Pclass)))+
        geom_boxplot()+
        geom_hline(aes(yintercept=80),
                   colour="red",linetype="dashed",lwd=2)+
        scale_y_continuous(labels=dollar_format())+
        theme_few()

#voila! we can replace 80$ for 1st class safely
full$Embarked[c(62,830)]="C"

#table(full$Embarked)

#show row 1044
full[1044,]

ggplot(full[full$Pclass=="3" & full$Embarked=="S",],
       aes(x=Fare))+
        geom_density(fill="#99d6ff",alpha=0.4)+
        geom_vline(aes(xintercept=median(Fare,na.rm=T)),
                   colour="red",linetype="dashed",lwd=1)+
        scale_x_continuous(labels=dollar_format())+
        theme_few()

#for this visualization replace missing fare with the median
full$Fare[1044]=median(full[full$Pclass=="3" & full$Embarked=="S",]$Fare,na.rm=TRUE)

#for ages, we will predict ages based on other values
#show number of missing values
sum(is.na(full$Age))

factor_vars=c("PassengerId","Pclass","Sex","Embarked","Title","Surname","Family","FsizeD")

full[factor_vars]=lapply(full[factor_vars],function(x) as.factor(x))

set.seed(129)

#use Multiple Imputations using Chined Equations (mice)
mice_mod=mice(full[,!names(full) %in% c("PassengerId","Name","Ticket","Cabin",
                                        "Family","Surname","Survide")],method="rf")

mice_output=complete(mice_mod)

#let see if there is nothing awry
par(mfrow=c(1,2))
hist(full$Age,freq = F,main="Age: Original Data",
     col="darkgreen",ylim=c(0,0.04))
hist(mice_output$Age,freq=F,main = "Age: MICE output",
     col="lightgreen",ylim=c(0,0.04))

#things look good. so we replace it
full$Age=mice_output$Age
sum(is.na(full$Age))

#feature engineering: Round 2
#we will create 2 new variables: child and mother

ggplot(full[1:891,], aes(Age,fill=factor(Survived)))+
        geom_histogram()+
        facet_grid(.~Sex)+
        theme_few()

full$Child[full$Age<18]="Child"
full$Child[full$Age>=18]="Adult"

table(full$Child,full$Survived)

full$Mother="Not Mother"
full$Mother[full$Sex=="female" & full$Parch>0 & full$Age >18 & full$Title != "Miss"]="Mother"

table(full$Mother,full$Survived)

full$Child=factor(full$Child)
full$Mother=factor(full$Mother)
#TODO: find out factor

md.pattern(full)
#TODO: find out md.pattern

#Wow! We have finally finished treating all of the relevant missing values 
#in the Titanic dataset which has included some fancy imputation with mice. 
#We have also successfully created several new variables which we hope will 
#help us build a model which reliably predicts survival

#########################
# PREDICTION WITH RANDOM FOREST
#######################

train=full[1:891,]
test=full[892:1309,]

set.seed(754)

rf_model=randomForest(factor(Survived)~Pclass + Sex + Age + SibSp + Parch +
                              Fare + Embarked + Title +
                              FsizeD + Child + Mother,
                      data=train)

#show model error
plot(rf_model,ylim=c(0,0.36))
legend("topright",colnames(rf_model$err.rate),col=1:3,fill=1:3)

#let see variable importance
importance=importance(rf_model)
varImportance=data.frame(Variables=row.names(importance),
                         Importance=round(importance[,"MeanDecreaseGini"],2))

rankImportance=varImportance %>% 
        mutate(Rank=paste0("#",dense_rank(desc(Importance))))

ggplot(rankImportance,aes(x=reorder(Variables,Importance),
                          y=Importance,fill=importance))+
        geom_bar(stat="identity")+
        geom_text(aes(x=Variables,y=0.5,label=Rank),
                  hjust=0,vjust=0.55,size=4,colour="red")+
        labs(x="Variables")+
        coord_flip()+
        theme_few()
#TODO: find out variable importance by Gini

#Predict usint the test set
prediction=predict(rf_model,test)

#save the solution to a dataframe with two columns: PassengerID and Survived
solution=data.frame(PassengerID=test$PassengerId,Survived=prediction)

#write the solution to a file
write.csv(solution,file="rf_mod_Solution.csv",row.names = F)
