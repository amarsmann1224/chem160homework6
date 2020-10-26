data1<-read.table("secher.txt",header=T)
model1<-lm(bwt~bpd,data=data1)
model2<-lm(bwt~ad,data=data1)
png(filename="bwt_ad.png")
bwt_ad.png<-plot(bwt~ad,data=data1)
abline(model2)
dev.off()
png(filename="bwt_bpd.png")
bwt_ad.png<-plot(bwt~bpd,data=data1)
abline(model1)
dev.off()

sum.model1<-summary(model1)
R21<-sum.model1$r.squared
f1<-sum.model1$fstatistic
intercept<-model1$coefficients[1]
slope<-model1$coefficients[2]
p.value<-pf(f1[1],f1[2],f1[3],lower.tail=F)
output1<-sprintf("for bwt~bpd model R2=%f,p value=%f,slope=%f,intercept=%f",R21,p.value,slope,intercept)
cat(output1,"\n")


sum.model2<-summary(model2)
R21<-sum.model2$r.squared
f1<-sum.model2$fstatistic
intercept<-model2$coefficients[1]
slope<-model2$coefficients[2]
p.value<-pf(f1[1],f1[2],f1[3],lower.tail=F)
output1<-sprintf("for bwt~ad model R2=%f,p-value=%f,slope=%f,intercept=%f",R21,p.value,slope,intercept)
cat(output1,"\n")

model3<-lm(bwt~bpd+ad,data=data1)
R21<-sum.model2$r.squared
f1<-sum.model2$fstatistic
p.value<-pf(f1[1],f1[2],f1[3],lower.tail=F)
output1<-sprintf("for bwt~bpd+ad model R2=%f and p-value=%f",R21,p.value)
cat(output1,"\n")

