# 5.2 数据整形
## `reshape2`包

(sdat<-sim.dat[1:5,1:6])
(mdat <- melt(sdat, measure.vars=c("store_exp","online_exp")))
# 这里为了展示回归需要更多的数据，所以用原数据框的所有行
mdat<-melt(sim.dat[,1:6], measure.vars=c("store_exp","online_exp"))
fit<-lm(value~gender+house+income+variable+age,data=mdat)
summary(fit)

# 这里用所用的数据
# 缺失值填补
demo_imp<-impute(sim.dat,method="median/mode")
mdat <- melt(demo_imp, measure.vars=c("store_exp","online_exp"),id.vars=c("house","gender"))
head(mdat)

dcast(mdat, house+gender~ variable, sum)

##  `tidyr`包

sdat<-sim.dat[1:5,]%>%
  dplyr::select(age,gender,store_exp,store_trans)
sdat %>% tbl_df()

msdat<-tidyr::gather(sdat,"variable","value",store_exp,store_trans)
msdat %>% tbl_df()

# 这里不显示输出结果
sdat%>%gather("variable","value",store_exp,store_trans)

msdat %>% spread(variable,value)

sepdat<- msdat %>% separate(variable,c("Source","Type"))
sepdat %>% tbl_df()

sepdat %>% unite("variable",Source,Type,sep="_")
