# Load library
library(meta)
library(grid)

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
pdf("fig5.pdf", height=3, width=9)
meta1<-metagen(data5$yi, data5$sei, studlab=paste(author,year), sm="RR",data=data5)
forest(meta1)
dev.off()
#grid.text("Environmrtal Fine PM and All-Cause Mortality", .5, .8, gp=gpar(cex=1.5))


# Env Cvd death
pdf("fig6.pdf", height=3, width=9)
meta2<-metagen(data6$yi, data6$sei, studlab=paste(author,year), sm="RR",data=data6)
forest(meta2)
dev.off()
#grid.text("Environmrtal Fine PM and Cardiovascular Mortality", .5, .8, gp=gpar(cex=2))

# Env hd death
pdf("fig7.pdf", height=3, width=9)
meta3<-metagen(data7$yi, data7$sei, studlab=paste(author,year), sm="RR",data=data7)
forest(meta3)
dev.off()
#grid.text("Environmrtal Fine PM and Heart Disease Mortality", .5, .8, gp=gpar(cex=2))


# workplace cvd incidence
meta4<-metagen(data1$yi, data1$sei, studlab=paste(author,year), sm="RR",data=data1)
forest(meta4)

# workplace hd death
pdf("fig1.pdf", height=5.4, width=9)
meta5<-metagen(data2$yi, data2$sei, studlab=paste(author,year), sm="RR",data=data2)
forest(meta5)
#grid.text("Environmrtal Fine PM and Ischemic Heart Disease Mortality", .5, .8, gp=gpar(cex=2))
dev.off()

# workplace cd death
pdf("fig2.pdf", height=4.4, width=9)
meta6<-metagen(data3$yi, data3$sei, studlab=paste(author,year), sm="RR",data=data3)
forest(meta6)
#grid.text("Environmrtal Fine PM and Cerebrovascular Disease Mortality", .5, .8, gp=gpar(cex=2))
dev.off()

# workplace lc death
pdf("fig3.pdf", height=4.2, width=9)
meta7<-metagen(data4$yi, data4$sei, studlab=paste(author,year), sm="RR",data=data4)
forest(meta7)
#grid.text("Environmrtal Fine PM and Lung Cancer Mortality", .5, .8, gp=gpar(cex=2))
dev.off()

#


table1<-data.frame(Reference=as.factor(c("Sjogren et al.","1987",
                                         "Steenland et al.","1998","","","","","","",
                                         "Randem et al.","2003",
                                         "Finkelstein et al.","2004","","","","","","",
                                         "Laden et al.","2007",
                                         "Toren et al.","2007","","",
                                         "Garshick et al.","2012","",
                                         "Silverman et al.","2012","",
                                         "Costello et al.","2013","","",
                                         "Mohner et al.", "2013")), 
                  Cohort=as.factor(c("234 welder","",
                                     "92 control dockworkers","604 control long-haul","drivers","134 control short-haul drivers","50  control truck","mechanics","143  control other","potentially exposed",
                                     "8,610 male asphalt","workers",
                                     "1,009 Heavy","equipment operators","271 boilermakers","1,533 electricians","201 insulators","220 painters","3,561 plumbers","505 sheet metal",
                                     "54,319  male  in the","trucking industry",
                                     "248,087 male","construction workers","","",
                                     "29,324 male workers","in trucking industry","",
                                     "228 control male miners","157 control male miners","123 control male miners",
                                     "39,412 autoworkers","","","",
                                     "5,862 potash miners","")),
                  Exposure=as.factor(c("Hexavalent","chromium",
                                       "Diesel fume","","","","","","","",
                                       "Bitumen fume","and PAH",
                                       "Diesel fume","","","","","","","",
                                       "Diesel fume","",
                                       "Diesel fume","","Asphalt fume","Metal fume",
                                       "Diesel fume","in diffewrent","levels",
                                       "Diesel fume","","",
                                       "Metal fume","in different","levels","",
                                       "Diesel fume","")),
                  Cause=as.factor(c("Ischaemic heart","disease (IHD)",
                                    "Lung cancer (LC)","","","","","","","",
                                    "Cerebrovascular","disease (CBD)/IHD",
                                    "CBD/IHD","","","","","","","",
                                    "CBD/IHD","",
                                    "CBD/IHD","","","",
                                    "LC","","",
                                    "LC","","",
                                    "IHD","","","",
                                    "LC","")),
                  CaseNo.=as.factor(c("10","",
                                      "70","609","121","","37","","99","",
                                      "73/214*","",
                                      "38/259","","9/59","61/332","5/34","5/40","190/876","22/92",
                                      "167/1,133","",
                                      "423/1,720","","45/171","205/831",
                                      "179","202","248",
                                      "50","49","50",
                                      "67","68","68","67",
                                      "68","")))
table2<-data.frame(Reference=as.factor(c("Puett et al. 2009", "Hart et al. 2011","","",
                                         "Lipsett et al. 2011","",
                                         "Puett et al. 2011","","",
                                         "Weichenthal et al.","2014","",
                                         "Ostro et al. 2015","","",
                                         "Hart et al. 2015","")), 
                   Cohort=as.factor(c("66,250 women from the Nurses' Health study",
                                      "53,814 men in the U.S. trucking industry","","",
                                      "73,489 women from the California","Teachers Study",
                                      "17,545 male from Health Professionals ","Follow-Up Study prospective cohort","",
                                      "83,378 subjects included farmers, their ","spouses, and commercial pesticide","applicators.",
                                      "133,479 current and former female teachers","and administrators","",
                                      "108,767 members of the Nurses' Health ","Study 2000-2006")),
                   Cause=as.factor(c("Coronary heart disease", "All-causes",
                                     "Cardiovascular disease","Ischemic heart disease","Cardiovascular disease","",
                                     "All-cause","Cardiovascular disease","IHD",
                                     "All-cause","Cardiovascular disease","",
                                     "All-cause","Cardiovascular disease","IHD",
                                     "All-cause","")),
                   CaseNo.=as.factor(c("1,348","4,806","1,682","1,109","1,630","",
                                       "2,813","1,661","746",
                                       "3,961","1,055","",
                                       "6,285","2,400","1,085",
                                       "8,617","")))




