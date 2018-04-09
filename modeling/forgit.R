# for Log transformation

dt$campaign <-log(dt$campaign)
dt$cons.price.idx <-log(dt$cons.price.idx)
dt$cons.conf.idx <-log(dt$cons.conf.idx +1 - min(dt$cons.conf.idx))

#####################################
## for staratified random sampling on y to have the same proportion of yes and no
dt$y<-as.factor(dt$y)
prop.table(table(dt$y))
barplot(prop.table(table(dt$y)), col=rainbow(2), ylim=c(0,0.7), main="class distribution")
#       no       yes 
#0.8830152 0.1169848 
yes=dt[dt$y=="yes",]
no=dt[dt$y!="yes",]
idx.y <-sample(1:nrow(yes),size=.75*nrow(yes))
idx.n <-sample(1:nrow(no),size=.75*nrow(no))
yes.train= yes[idx.y,]
yes.test = yes[-idx.y,]
no.train=no[idx.n,]
no.test = no[-idx.n,]
combo.train <-rbind(yes.train,no.train)
combo.test <-rbind(yes.test,no.test)
# to check the proportion of yes and no in both test and train data
prop.table(table(combo.train$y))
prop.table(table(combo.test$y))
par(mfrow=c(1,2))
#showing the same ratio in both train and test set in terms of # of yes and # of NO
barplot(prop.table(table(combo.train$y)), col=rainbow(2), ylim=c(0,0.7), main="class distribution")
barplot(prop.table(table(combo.test$y)), col=rainbow(2), ylim=c(0,0.7), main="class distribution")