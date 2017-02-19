# Load library
library(meta)

# Load input.tyx
dat <- read.csv("df.csv", header = TRUE)

# If RR's and 95% CI's, run these commented out lines
dat$yi <- with(dat, log(RR))
dat$sei<- with(dat, (log(ci.ub)-log(ci.lb))/(2*1.96))

# Data subsetting
data1<-subset(dat, endpoint=="incidence")
data2<-subset(dat, endpoint=="death" & disease=="hd" &exposure=="workplace")
data3<-subset(dat, endpoint=="death" & disease=="cd" &exposure=="workplace")
data4<-subset(dat, endpoint=="death" & disease=="lc" &exposure=="workplace")

data5<-subset(dat, endpoint=="death" & disease=="all" &exposure=="env")
data6<-subset(dat, endpoint=="death" & disease=="cvd" &exposure=="env")
data7<-subset(dat, endpoint=="death" & disease=="hd" &exposure=="env")

# Env All death
meta1<-metagen(data5$yi, data5$sei, studlab=paste(author,year), sm="RR",data=data5)
forest(meta1)

# Env Cvd death
meta2<-metagen(data6$yi, data6$sei, studlab=paste(author,year), sm="RR",data=data6)
forest(meta2)

# Env hd death
meta3<-metagen(data7$yi, data7$sei, studlab=paste(author,year), sm="RR",data=data7)
forest(meta3)

# workplace cvd incidence
meta4<-metagen(data1$yi, data1$sei, studlab=paste(author,year), sm="RR",data=data1)
forest(meta4)

# workplace hd death
meta5<-metagen(data2$yi, data2$sei, studlab=paste(author,year), sm="RR",data=data2)
forest(meta5)

# workplace cd death
meta6<-metagen(data3$yi, data3$sei, studlab=paste(author,year), sm="RR",data=data3)
forest(meta6)

# workplace lc death
meta7<-metagen(data4$yi, data4$sei, studlab=paste(author,year), sm="RR",data=data4)
forest(meta7)



