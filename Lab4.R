library(tidyverse)

s.t. <- tapply(X = fish$parcel.density.m3,INDEX = fish$transect.id,FUN = mean)
s.t.
data.frame(s.t.)
s.t.$mean <- row.names(s.t.)
s.t.
s.t. <- tapply(X = fish$parcel.density.m3,INDEX = fish$transect.id,FUN = mean)
data.frame(s.t.)
s.t.
st2 <- data.frame(s.t.)
st2$mean <- row.names(st2)
st2
s.d. <- tapply(X = fish$parcel.density.m3,INDEX = fish$transect.id,FUN = sd)
s.d.
data.frame(s.d.)
s.d.
sd2 <- data.frame(s.d.)
sd2$standard_deviation <-row.names(sd2)
sd2
merge(x = sd2,y = st2,by = "transect.id")
sd2$transect.id <- row.names(sd2)
sd2
standard_deviation <- tapply(X = fish$parcel.density.m3,INDEX = fish$transect.id,FUN = sd)
data.frame(standard_deviation)
sd2 <- data.frame(standard_deviation)
sd2$transect.id <- row.names(sd2)
sd2
mean <- tapply(X = fish$parcel.density.m3,INDEX = fish$transect.id,FUN = mean)
data.frame(mean)
st2 <- data.frame(mean)
st2$transect.id <- row.names(st2)
st2
sd2
stand.dev <-tapply(X = fish$parcel.density.m3,INDEX = fish$transect.id,FUN = sd)
data.frame(stand.dev)
sd2 <- data.frame(stand.dev)
sd2$transect.id <- row.names(sd2)
sd2
merge(x = mean,y = stand.dev,... = "transect.id")
sdt <- merge(x = sd2,y = st2,by = "transect.id")
sdt
count <- tapply(X = fish$parcel.density.m3,INDEX = fish$transect.id,FUN = sum)
count
data.frame(count)
ct <- data.frame(count)
ct$transect.id <- row.names(ct)
ct
merge(x = ct,y = sdt,by = "transect.id")
install.packages("tidyverse")
Mean <- fish %>% group_by (transect.id) %>% summarise(mean=mean(parcel.density.m3))
Mean
data.frame(Mean)
Mean$mean.pd <- row.names(Mean)
Mean
Mean <- fish %>% group_by (transect.id) %>% summarise(mean.pd=mean(parcel.density.m3))
data.frame(Mean)
transect.id  mean.pd
1   OST14-1E-D 1.901954
2   OST14-1E-M 1.770026
3   OST14-1E-S 2.098717
4   OST14-1W-M 1.768641
5   OST14-1W-S 1.983198
6   OST14-2C-D 2.086289
7   OST14-2C-M 1.535166
8   OST14-2C-S 1.673188
9   OST14-2E-D 1.892880
10  OST14-2E-M 2.033088
11  OST14-2E-S 2.402454
12  OST14-3C-D 2.009058
13  OST14-3C-M 2.046898
14  OST14-3C-S 1.564965
15  OST14-3W-D 1.603930
16  OST14-3W-M 1.785874
17  OST14-3W-S 2.143646
18  OST14-4W-D 2.015992
19  OST14-4W-M 1.969309
20  OST14-4W-S 1.518438
21  OST14-5W-D 1.850275
22  OST14-5W-M 6.321862
23  OST14-5W-S 1.747230
24 OST15-10W-M 1.636471
25 OST15-10W-S 1.791265
26  OST15-6E-M 2.590905
27  OST15-6E-S 1.378165
28  OST15-6W-M 1.560250
29  OST15-6W-S 1.509818
30  OST15-7C-M 2.457173
31  OST15-7C-S 1.666048
32  OST15-7E-M 2.491521
33  OST15-7E-S 3.048735
34  OST15-7W-M 1.994220
35  OST15-7W-S 2.326290
36  OST15-8W-M 1.917494
37  OST15-8W-S 1.702851
38  OST15-9W-M 2.882370
39  OST15-9W-S 1.599573
st.dev <- tapply(X = fish$parcel.density.m3,INDEX = fish$transect.id,FUN = sd)
data.frame(st.dev)
mpd <- data.frame(Mean)
sdpd <- data.frame(st.dev)
sdpd$transect.id <- row.names(sdpd)
sdpd
?join
inner_join(x = mpd,y = sdpd,"transect.id")
Count <- tapply(X = fish$parcel.density.m3,INDEX = fish$transect.id,FUN = sum)
data.frame(Count)
Count <- data.frame(Count)
Count$transect.id <- row.names(Count)
Count
inner_join(x = sdt,y = Count,"transect.id")
minimum <- tapply(X = fish$parcel.length.m,INDEX = fish$depth_fac,FUN = min)
data.frame(minimum)
MINIMUM <- data.frame(minimum)
MINIMUM$depth <- row.names(MINIMUM)
median <- tapply(X = fish$parcel.length.m,INDEX = fish$depth_fac,FUN = median)
data.frame(median)
MEDIAN <- data.frame(median)
MEDIAN$depth <- row.names(MEDIAN)
mean.pl <- tapply(X = fish$parcel.length.m,INDEX = fish$depth_fac,FUN = mean)
data.frame(mean.pl)
MEAN <-data.frame(mean.pl)
MEAN$depth <- row.names(MEAN)
maximum <- tapply(X = fish$parcel.length.m,INDEX = fish$depth_fac,FUN = max)
data.frame(maximum)
MAXIMUM <- data.frame(maximum)
MAXIMUM$depth <- row.names(MAXIMUM)
MaxMin <- inner_join(x = MAXIMUM,y = MINIMUM,"depth")
MedMean <- inner_join(x = MEDIAN,y = MEAN,"depth")
MaxMin.MedMean <- inner_join(x = MaxMin,y = MedMean,"depth")                 
MaxMin.MedMean
max <- tapply(X = fish$parcel.length.m,INDEX = fish$area_fac,FUN = min)
data.frame(max)
MAX.A <- data.frame(max)
MAX.A$area <- row.names(MAX.A)
MAX.A
min <- tapply(X = fish$parcel.length.m,INDEX = fish$area_fac,FUN = min)
data.frame(min)
MIN.A <- data.frame(min)
MIN.A$area <- row.names(MIN.A)
MIN.A
med <- tapply(X = fish$parcel.length.m,INDEX=fish$area_fac,FUN = median)
data.frame(med)
MED.A <- data.frame(med)
MED.A$area <- row.names(MED.A)
MED.A
m.e.a.n <- tapply(X = fish$parcel.length.m,INDEX = fish$area_fac,FUN = mean)
data.frame(m.e.a.n)
MEAN.A <- data.frame(m.e.a.n)
MEAN.A$area <- row.names(MEAN.A)
MEAN.A
MaxMin.A <- inner_join(x = MAX.A,y = MIN.A,"area")
MedMean.A <- inner_join(x = MED.A,y = MEAN.A,"area")
A.MaxMinMedMean <- inner_join(x = MaxMin.A,y = MedMean.A,"area")
A.MaxMinMedMean
merge(x = sd2,y = st2,b.y=mean)
s.d. <- tapply(X = fish$parcel.density.m3,INDEX = fish$transect.id,FUN = sd)
fish
s.d. <- tapply(X = fish$parcel.density.m3,INDEX = fish$transect.id,FUN = sd)
rm(`fish_data.(1)`)
data.frame(s.d.)
sd2 <- data.frame(s.d.)
sd2$standard_deviation <-row.names(sd2)
merge(x = sd2,y = st2,by = "mean")
s.t. <- tapply(X = fish$parcel.density.m3,INDEX = fish$transect.id,FUN = mean)
data.frame(s.t.)
st2 <- data.frame(s.t.)
st2$mean <- row.names(st2)
merge(x = sd2,y = st2,by = "mean")
head(sd2)
head (st2)
merge(x = sd2,y = st2,by.x = "standard_deviation",by.y = "mean")
combo <- merge(x = sd2,y = st2,by.x = "standard_deviation",by.y = "mean")
head(combo)
?join()
?rename()
rename(sd2,standard_deviation=transect.id)
rename(st2,mean=transect.id)
sd2<-rename(sd2, transect.id=standard_deviation)
st2<-rename(st2,transect.id=mean)
head(sd2)
head(st2)
combo2 <- inner_join(x = sd2,y = st2,"transect.id")
combo2 <- combo2[,c("transect.id","s.d.","s.t.")]
combo2
minimum <- tapply(X = fish$parcel.length.m,INDEX = fish$depth_fac,FUN = min)
data.frame(minimum)
MINIMUM <- data.frame(minimum)
MINIMUM$depth <- row.names(MINIMUM)
maximum <- tapply(X = fish$parcel.length.m,INDEX = fish$depth_fac,FUN = max)
data.frame(maximum)
MAXIMUM <- data.frame(maximum)
MAXIMUM$depth <- row.names(MAXIMUM)
median <- tapply(X = fish$parcel.length.m,INDEX = fish$depth_fac,FUN = median)
data.frame(median)
MEDIAN <- data.frame(median)
MEDIAN$depth <- row.names(MEDIAN)
?group_by()
minimum <- group_by(fish,depth,area)
names(fish)
minimum <- group_by(fish,tow.depth,area_fac)
minimum
summarise(minimum,min.p=min(parcel.length.m))
summarise(minimum,max.p=max(parcel.length.m),min.p=min(parcel.length.m))
summarise(minimum,max.p=max(parcel.length.m),min.p=min(parcel.length.m),med.p=median(parcel.length.m))
?quantile()
quantile(x = fish$parcel.length.m,probs = c(.05,.95))
summarise(minimum,max.p=max(parcel.length.m),min.p=min(parcel.length.m),med.p=median(parcel.length.m),upper95.p=quantile(x = parcel.length.m,.05),lower95.p=quantile(x = parcel.length.m,.95))
