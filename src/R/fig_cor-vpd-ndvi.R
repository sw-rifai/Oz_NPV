
pacman::p_load(tidyverse,data.table,lubridate,stars,mgcv)
source("src/R/gg_sci_themes.R")
vec_div_clim <- pals::brewer.rdylbu(5)


dat <- arrow::read_parquet("../data_general/proc_oz_npv/mod-red-nir_coarse.parquet")
met <- arrow::read_parquet("../data_general/proc_oz_npv/terraclimate_pr-pet-def-vpd.parquet")
dat <- merge(dat,met,by=c("x","y","year","month","date"))
rm(met); gc(full=T)

dat[,`:=`(hydro_year = year(date-months(6)))]
dat[,`:=`(
  def = def*0.1, 
  pet = pet*0.1,
  vpd = vpd*0.001)]
dat[,`:=`(
  def_u = def_u*0.1, 
  pet_u = pet_u*0.1,
  vpd_u = vpd_u*0.001)]
dat[,`:=`(
  def_sd = def_sd*0.1, 
  pet_sd = pet_sd*0.1,
  vpd_sd = vpd_sd*0.001)]
dat[,`:=`(ndvi_z = (ndvi-ndvi_u)/ndvi_sd, 
          nirv_z = (nirv-nirv_u)/nirv_sd)]
dat <- dat[ndvi_sd>0]
dat <- dat[nirv_sd>0]
dat[,`:=`(season = case_when(
  month==12~'DJF',
  between(month,1,2)~'DJF',
  between(month,3,5)~'MAM',
  between(month,6,8)~'JJA',
  between(month,9,11)~'SON'))][
    ,`:=`(season=factor(season,ordered = T,levels=c("SON","DJF","MAM","JJA")))
  ]


dat[,`:=`(xc= round(x*8)/8,
          yc = round(y*8)/8)]
dat[,`:=`(id_c = .GRP),by=.(xc,yc)]

p_vpd <- na.omit(dat)[,.(rho = cor(ndvi_z,(vpd-vpd_u)/vpd_sd)),by=.(xc,yc,season)]
p_vpd %>% ggplot(data=.,aes(xc,yc,fill=rho))+
  geom_tile()+
  scale_fill_gradient2(
    low=vec_div_clim[1],
    mid=vec_div_clim[3],
    high=vec_div_clim[5],
    limits=c(-0.75,0.75),
    oob=scales::squish
  )+
  coord_sf()+
  labs(x=NULL,y=NULL,
    fill=expression(paste(rho(NDVI[z],VPD[z]))))+
  facet_wrap(~season)+
  guides(fill = guide_colorbar(title.position = 'top'))+
  theme_linedraw()+
  theme(panel.grid = element_blank(), 
    legend.position = 'bottom',
    legend.key.height = unit(0.2,'cm'),
    legend.key.width = unit(1,'cm'))
ggsave(filename = 'figures/cor_ndvi-z_vpd-z_by-season.png',
  width=15,
  height=15,
  units='cm',
  dpi=350)

p_def <- na.omit(dat)[,.(rho = cor(ndvi_z,(def-def_u)/def_sd)),by=.(xc,yc,season)]
p_def %>% ggplot(data=.,aes(xc,yc,fill=rho))+
  geom_tile()+
  scale_fill_gradient2(
    low=vec_div_clim[1],
    mid=vec_div_clim[3],
    high=vec_div_clim[5],
    limits=c(-0.75,0.75),
    oob=scales::squish
  )+
  coord_sf()+
  labs(x=NULL,y=NULL,
    fill=expression(paste(rho(NDVI[z],def[z]))))+
  facet_wrap(~season)+
  guides(fill = guide_colorbar(title.position = 'top'))+
  theme_linedraw()+
  theme(panel.grid = element_blank(), 
    legend.position = 'bottom',
    legend.key.height = unit(0.2,'cm'),
    legend.key.width = unit(1,'cm'))
ggsave(filename = 'figures/cor_ndvi-z_def-z_by-season.png',
  width=15,
  height=15,
  units='cm',
  dpi=350)


# SCRATCH -----------------------------------------------------------------



p_def <- na.omit(dat)[,.(rho = cor(ndvi_z,(def-def_u)/def_sd)),by=.(x,y)]
p_def %>% ggplot(data=.,aes(x,y,fill=rho))+
  geom_tile()+
  scale_fill_gradient2(
    limits=c(-0.75,0.75),
    oob=scales::squish
  )+
  coord_sf()+
  theme_linedraw()+
  theme(panel.grid = element_blank())

dat[id_c %in% sample.int(11900,30)] %>% 
  ggplot(data=.,
    aes((vpd-vpd_u)/vpd_sd,
    ndvi_z))+
  # geom_point(alpha=0.01)+
  geom_smooth(method='lm')+
  facet_wrap(~id_c)+
  theme_linedraw()+
  theme(panel.grid = element_blank())
dat[id_c %in% sample.int(11900,30)] %>% 
  ggplot(data=.,
    aes((pet-pet_u)/pet_sd,
    ndvi_z))+
  # geom_point(alpha=0.01)+
  geom_smooth(method='lm')+
  facet_wrap(~id_c)+
  theme_linedraw()+
  theme(panel.grid = element_blank())
dat[id_c %in% sample.int(11900,30)] %>% 
  ggplot(data=.,
    aes((def-def_u)/def_sd,
    ndvi_z))+
  # geom_point(alpha=0.01)+
  geom_smooth(method='lm')+
  facet_wrap(~id_c)+
  theme_linedraw()+
  theme(panel.grid = element_blank())
dat[id_c %in% sample.int(11900,30)] %>% 
  ggplot(data=.,
    aes((precip-precip_u)/precip_sd,
    ndvi_z))+
  # geom_point(alpha=0.01)+
  geom_smooth(method='lm')+
  facet_wrap(~id_c)+
  theme_linedraw()+
  theme(panel.grid = element_blank())


pdat <- unique(dat[,.(x,y)])
pdat <- expand_grid(pdat,month=1:12)

d_train <- dat[sample(.N, 1e6)][is.na(ndvi_z)==F]
dim(d_train)

b1 <- bam(ndvi_z~
    te(x,y,month),
  data=d_train, 
  discrete = T)
summary(b1)


b2 <- bam(ndvi_z~
    te(x,y,month),
  data=d_train, 
  discrete = T)
pdat <- pdat %>% 
  mutate(pred = predict(b1,newdata=.,type='response'))

pdat %>% 
  ggplot(data=.,aes(x,y,fill=pred))+
  geom_tile()+
  scale_fill_gradient2()+
  coord_sf()+
  facet_wrap(~month)

d_train %>% 
  ggplot(data=.,aes(def-def_u, ndvi_z))+
  geom_smooth()+
  geom_smooth(aes(def-def_u,nirv_z),col='black')

dat[year==2001] %>% 
  ggplot(data=.,aes(x,y,fill=ndvi_z))+
  geom_tile()+
  scale_fill_gradient2()+
  coord_sf()+
  facet_wrap(~month)

d_train %>% 
  ggplot(data=.,aes(precip_u/pet_u,ndvi_u))+
  geom_smooth()


b1 <- dat[id_c==1] %>% 
  bam(ndvi~s(month,bs='cc'),data=.)
summary(b1)

dat[id_c==1] %>% 
  ggplot(data=.,aes(date,ndvi))+
  geom_point()+
  geom_smooth(formula=y~s(x,k=20))+
  geom_line(data=. %>% mutate(pred=predict(b1,newdata=.)), 
    aes(date,pred),col='red')

max(unique(dat$id_c))
dat[id_c %in% sample(46152,50)] %>% 
  ggplot(data=.,aes(month,precip,color=factor(id_c)))+
  # geom_point()+
  geom_smooth(se=F)+
  facet_wrap(~id_c)+
  theme(legend.position = 'none')

j1 <- dat[,.(
  q95 = quantile(ndvi,0.95,na.rm=T),
  q50 = median(ndvi,na.rm=T),
  q05 = quantile(ndvi,0.05,na.rm=T)), by=.(xc,yc,hydro_year)]
j1[q05>0] %>% 
  ggplot(data=.,aes(xc,yc,fill=(q95-q05)/q50))+
  geom_tile()+
  scale_fill_viridis_c(limits=c(0,3),
    oob=scales::squish)+
  coord_sf()+
  facet_wrap(~hydro_year)

j2 <- dat[,.(
  ap = mean(precip,na.rm=T)*12,
  apet = mean(pet,na.rm=T)*12,
  adef = mean(def,na.rm=T),
  vpd95 = quantile(vpd,0.95,na.rm=T),
  vpd50 = median(vpd,na.rm=T),
  vpd05 = quantile(vpd,0.05,na.rm=T)), by=.(xc,yc,hydro_year)]

j3 <- merge(j1,j2,by=c('xc','yc','hydro_year'))
j3[,`:=`(appet = ap/apet)]
j3 <- merge(j3, j3[,.(mappet = mean(appet)),by=.(xc,yc)], by=c("xc","yc"))


b3 <- gam(list(q50~
    s(vpd50,bs='cs',k=5)+
    s(ap,bs='cs',k=5)+
    s(apet,bs='cs',k=5)+
    s(adef,bs='cs',k=5), 
  ~s(vpd50,bs='cs',k=5)+
    s(ap,bs='cs',k=5)+
    s(apet,bs='cs',k=5)+
    s(adef,bs='cs',k=5)
  ), 
  family=gaulss(),
  data=j3[q50>0][sample(.N,100000)], 
  select=T
  # discrete = T
  )
summary(b3)
plot(b3)
predict(b3)

j4 <- j3[sample(.N,100000)][q50>0]
yardstick::rsq_trad_vec(truth=j4$q50, 
  estimate = predict(b3,newdata=j4,type='response')[,1])

exp(predict(b3,newdata=j4)[,2]) %>% hist

j3[sample(.N,1000)] %>% 
  mutate(pred = predict(b3,newdata = .,type='response')) %>% 
  ggplot(data=.,aes(pred,q50))+
  geom_point()+
  geom_abline(col='red')

b4 <- bam(q50~
    s(mappet),
    # s(mappet,appet,k=5),
    # s(vpd50,bs='cs',k=5)+
    # s(ap,bs='cs',k=5)+
    # s(mappet,bs='cs',k=5),
    # s(apet,bs='cs',k=5)+
    # s(adef,bs='cs',k=5),
  # family=gaulss(),
  data=j3[q50>0][sample(.N,100000)], 
  select=T,
  discrete = T
  )
summary(b4)
plot(b4,scheme = 2)


j3 %>% 
  ggplot(data=.,aes(mappet,q50))+
  geom_smooth(method='bam',
    formula=y~s(x,bs='cs',k=5),
    method.args=list(discrete=T,
      select=T))+
  geom_smooth(aes(mappet,q95))+
  geom_smooth(aes(mappet,q05))

j3[q05>0 & q50>0][sample(.N,10000)] %>% 
  ggplot(data=.,aes(mappet,(q95-q05)/q50,color=q50))+
  geom_point()+
  scale_color_viridis_c()

j5 <- j3[q05>0 & q50>0][,.(iav_amp = sd(q95-q05),
  ndvi50 = median(q50,na.rm=T)),by=.(xc,yc,mappet)]
j5[sample(.N,10000)] %>% 
  ggplot(data=.,aes(mappet,iav_amp,color=ndvi50))+
  geom_point()+
  scale_color_viridis_c()

b5 <- bam(q50~te(xc,yc,by=ap),
  data=j3[q50>0],
  discrete = T, 
  select=T)
summary(b5)
plot(b5,scheme = 2)

library(gratia)
gratia::smooths(b5)
gratia::get_smooths_by_id(b5,1)
eval_smooth(gratia::get_smooths_by_id(b5,1))
draw(b5)+coord_sf()+scale_fill_viridis_c()

j6 <- j3[q50>0][is.na(q50)==F][,.(rho = cor(q50,ap)),by=.(xc,yc)]
j6 %>% 
  ggplot(data=.,aes(xc,yc,fill=rho))+
  geom_tile()+
  scale_fill_viridis_c(limits=c(0,1))+
  coord_equal()
