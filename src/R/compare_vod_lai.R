pacman::p_load(tidyverse,data.table,lubridate,stars)
dat <- arrow::read_parquet("/media/sami/srifai-ssd/data-ssd/AMSRE/se_oz_vod_2017_2020.parquet")
dim(dat)
ref_grid <- st_as_stars(unique(dat[band=='vod'][,.(qa,x,y)]),coords=c("x","y"))
st_crs(ref_grid) <- st_crs(4326)

lai_djf <- stars::read_stars("../data_general/proc_oz_npv/lai_djf_2016_2019.tif") %>% 
  st_set_dimensions(., 3, values=2016:2019,names='year') %>% 
  set_names('lai') %>% 
  st_warp(., ref_grid,use_gdal = F) %>% 
  as.data.table() %>% 
  .[,`:=`(season='DJF')] %>% 
  .[is.na(lai)==F]
lai_mam <- stars::read_stars("../data_general/proc_oz_npv/lai_mam_2016_2019.tif") %>% 
  st_set_dimensions(., 3, values=2016:2019,names='year') %>% 
  set_names('lai') %>% 
  st_warp(., ref_grid,use_gdal = F) %>% 
  as.data.table() %>% 
  .[,`:=`(season='MAM')] %>% 
  .[is.na(lai)==F]
lai_jja <- stars::read_stars("../data_general/proc_oz_npv/lai_jja_2016_2019.tif") %>% 
  st_set_dimensions(., 3, values=2016:2019,names='year') %>% 
  set_names('lai') %>% 
  st_warp(., ref_grid,use_gdal = F) %>% 
  as.data.table() %>% 
  .[,`:=`(season='JJA')] %>% 
  .[is.na(lai)==F]
lai_son <- stars::read_stars("../data_general/proc_oz_npv/lai_son_2016_2019.tif") %>% 
  st_set_dimensions(., 3, values=2016:2019,names='year') %>% 
  set_names('lai') %>% 
  st_warp(., ref_grid,use_gdal = F) %>% 
  as.data.table() %>% 
  .[,`:=`(season='SON')] %>% 
  .[is.na(lai)==F]

lai_ma <- stars::read_stars("../data_general/proc_oz_npv/lai_mean_2005_2015.tif") %>% 
  set_names('lai_ma') %>% 
  st_warp(., ref_grid,use_gdal = F) %>% 
  as.data.table() %>% 
  .[is.na(lai_ma)==F]

lai_sd <- stars::read_stars("../data_general/proc_oz_npv/lai_sd_2005_2015.tif") %>% 
  set_names('lai_sd') %>% 
  st_warp(., ref_grid,use_gdal = F) %>% 
  as.data.table() %>% 
  .[is.na(lai_sd)==F]


dlai <- rbindlist(list(lai_djf,lai_mam,lai_jja,lai_son))
dlai <- merge(dlai, lai_ma, by=c("x","y"))
dlai <- merge(dlai, lai_sd, by=c("x","y"))

# summarize vod ----------------------------------------------------------
dvod <- dat[band=='vod'][date>=ymd("2015-12-01")]
dvod[,`:=`(month=month(date),
  year=year(date))][,`:=`(season = case_when(
  between(month,1,2) ~ 'DJF', 
  month == 12 ~ 'DJF',
  between(month,3,5)~'MAM',
  between(month,6,8)~'JJA',
  between(month,9,11)~'SON'))]

svod <- dvod[,.(vod = mean(value,na.rm=T)), 
  by=.(x,y,year,season)]
# setnames(svod,c("x","y"),c("x_vod","y_vod"))



# merge -------------------------------------------------------------------
dlai[,`:=`(x = round(x*20)/20, 
  y=round(y*20)/20)]
svod[,`:=`(x = round(x*20)/20, 
  y=round(y*20)/20)]
unique(dlai$x) %in% unique(svod$x) %>% table
unique(dlai$y) %in% unique(svod$y) %>% table
d2 <- merge(dlai,svod,by=c("x","y","year","season"))

norms <- d2[,.(vod_u = mean(vod,na.rm=T),
                vod_sd = sd(vod,na.rm=T),
                lai_u = mean(lai,na.rm=T),
                lai_sd = sd(lai,na.rm=T)), 
  by=.(x,y,season)]
setnames(d2,'lai_sd','lai_ma_sd')

d2 <- merge(d2,norms,by=c('x','y','season'))
d2[,`:=`(
  vod_zanom = ((vod-vod_u)/vod_sd), 
  lai_zanom = ((lai-lai_u)/lai_sd)
  ),
  by=.(x,y,year,season)]

d2[,`:=`(m = case_when(
  season=='DJF'~1,
  season=='MAM'~4,
  season=='JJA'~7,
  season=='SON'~10
))][,`:=`(chrono = ymd(paste(year,m,1)))]



# scratch -----------------------------------------------------------------
kop <- stars::read_stars("../data_general/Koppen_climate/Beck_KG_V1_present_0p083.tif")
kop %>% plot
kop <- arrow::read_parquet('../data_general/Koppen_climate/BOM_Koppen_simplified7.parquet') %>% 
  select(x,y,zone) %>% 
  st_as_stars(kop, coords=c("x","y")) %>% 
  st_set_crs(4326) %>% 
  as.data.table() %>% 
  .[,`:=`(x = round(x*20)/20, 
  y=round(y*20)/20)] %>% 
  .[,(zone = sort(table(zone)) %>% names %>% .[1]), 
    by=.(x,y)]

sort(table(kop$zone)) %>% names %>% .[1]
sort(tabulate(kop$zone)) %>% names %>% .[1]
kop %>% ggplot(data=.,aes(x,y,fill=zone))+
  geom_raster()+coord_sf()


d2 %>% pivot_longer(
    # id_cols=c('chrono','lai_floor'),
    cols=c('lai_zanom','vod_zanom')
    ) %>% 
  mutate(LAI_class = cut_interval(lai_ma, n=5)) %>% 
  ggplot(data=.,aes(chrono,value,color=name))+
  geom_smooth()+
  facet_wrap(~LAI_class)

d2 %>% pivot_longer(
    # id_cols=c('chrono','lai_floor'),
    cols=c('lai_zanom','vod_zanom')
    ) %>% 
  # mutate(LAI_class = cut_interval(lai_ma, n=5)) %>% 
  ggplot(data=.,aes(x,y,fill=value))+
  geom_tile()+
  coord_sf()+
  scale_fill_gradient2()+
  facet_grid(chrono~name)


d2 %>% #[chrono==ymd("2018-04-01")] %>% 
  ggplot(data=.,aes(x,y,fill=vod_zanom))+
  geom_tile()+
  coord_sf()+
  scale_fill_gradient2()+
  facet_wrap(~chrono,nrow = 2)+
  theme_dark()+
  theme(panel.grid = element_blank())






 # merge -------------------------------------------------------------------
coords_vod <- unique(dat[,.(x,y)])
setkeyv(coords_vod,cols=c("x","y"))
coords_lai <- unique(dlai[,.(x,y)])
setkeyv(coords_lai,cols=c("x","y"))
coords_vod[,`:=`(var = 'vod', x_vod = x, y_vod = y)]
coords_lai[,`:=`(var = 'lai', x_lai = x, y_lai = y)]

codex <- nn_dts(coords_lai[,.(x,y)], coords_vod[,.(x,y)])
vec_names <- names(codex) %>% 
  str_replace(.,"_big","_lai") %>% 
  str_replace(.,"_small","_vod")
names(codex) <- vec_names

dlai <- merge(dlai, codex, by=c("x_lai","y_lai"))
svod <- merge(svod, dlai, by=c("x_vod","y_vod","year","season"))

norms <- svod[,.(vod_u = mean(vod,na.rm=T),
                vod_sd = sd(vod,na.rm=T),
                lai_u = mean(lai,na.rm=T),
                lai_sd = sd(lai,na.rm=T)), 
  by=.(x_vod,y_vod,season)]
setnames(svod,'lai_sd','lai_ma_sd')

svod <- merge(svod,norms,by=c('x_vod','y_vod','season'))
svod[,`:=`(
  vod_zanom = ((vod-vod_u)/vod_sd), 
  lai_zanom = ((lai-lai_u)/lai_sd)
  ),
  by=.(x_vod,y_vod,year,season)]

svod[,`:=`(m = case_when(
  season=='DJF'~1,
  season=='MAM'~4,
  season=='JJA'~7,
  season=='SON'~10
))][,`:=`(chrono = ymd(paste(year,m,1)))]



# SCRATCH -----------------------------------------------------------------
d2[year==min(year)][season=='DJF'] %>% 
  ggplot(data=.,aes(x,y,fill=vod))+
  geom_raster()+
  scale_fill_viridis_c()+
  coord_sf()

svod[chrono==min(chrono)] %>% 
  ggplot(data=.,aes(x_vod,y_vod,fill=lai))+
  geom_tile()+
  scale_fill_viridis_c()+
  coord_sf()

tmp <- svod[,`:=`(lai_floor = floor(lai_ma))][,.(val_vod = mean(vod_zanom,na.rm=T), 
  val_lai = mean(lai_zanom,na.rm=T)), 
  by=.(chrono,lai_floor)]
tmp %>% 
  pivot_longer(
    # id_cols=c('chrono','lai_floor'),
    cols=c('val_lai','val_vod')
    ) %>% 
  rename(LAI_MA = lai_floor) %>% 
  ggplot(data=.,aes(chrono, value,color=name))+
  geom_hline(aes(yintercept=0))+
  geom_line()+
  scale_color_viridis_d(end=0.8)+
  facet_wrap(~LAI_MA,labeller = label_both)+
  theme_linedraw()+
  theme(panel.grid = element_blank())

svod %>% 
  ggplot(data=.,
  aes(x=factor(year),
  y=lai,
  # group=year,
  color=season,
  fill=season))+
  geom_boxplot(outlier.colour = NA, 
    alpha=0.25)


svod[sample(.N,10000)] %>% 
  ggplot(data=.,aes(vod_zanom,lai_zanom,color=season))+
  # geom_point()+
  geom_smooth()

tmp <- svod[,.(rho = cor(vod_zanom,lai_zanom)),
  by=.(x_vod,y_vod,season)]
tmp %>% ggplot(data=.,aes(x_vod,y_vod,fill=rho))+
  geom_tile()+
  scale_fill_gradient2()+
  coord_sf()+
  facet_wrap(~season)

# calc quantiles of lai for each vod
qvod <- svod[,as.list(quantile(lai, prob=c(0.1,0.5,0.9))), 
  by=.(x_vod,y_vod,year,season,vod)]


qvod %>% 
  ggplot(data=.,aes(vod,`10%`))+
  geom_point()


DT = data.table(x=rep(c("b","a","c"),each=3), y=c(1,3,6), v=1:9)
melt(DT[, v := as.numeric(v)], 
     "x",
     c("v", "y"),
     variable.name = "id")[, as.list(quantile(value, 
                                              prob = c(.05, .5, .95))), 
                           .(x, id)][order(x, id)]

melt(svod[, v := as.numeric(v)], 
     "x",
     c("v", "y"),
     variable.name = "id")[, as.list(quantile(value, 
                                              prob = c(.05, .5, .95))), 
                           .(x, id)][order(x, id)]

dim(dlai)
dim(svod)
svod[sample(.N,1000)] %>% 
  ggplot(data=.,aes(vod,lai))+
  geom_point()+
  geom_smooth()

tables()
test <- coords_vod[coords_lai,roll=Inf,mult='first']
test[is.na(x_lai)==F]
test[is.na(x_vod)==F]


svod2 <- merge(test,svod)

setnames(dlai,c("x","y"),c("x_lai","y_lai"))
svod2 <- merge(svod2,dlai, by=c("x_lai","y_lai","season","year"))

unique(svod2$x_lai) %in% unique(dlai$x_lai)

test$x %>% unique %>% sort %>% diff

dat$x %>% unique %>% sort %>% diff


dat[band=='vod'][between(date,ymd("2017-01-01"),ymd("2017-01-02"))] %>% 
  ggplot(data=.,aes(x,y,fill=value))+
  geom_tile()+
  coord_sf()

vod_ts <- dat[band=='vod'][,.(val = mean(value,na.rm=T)),by=date]
vod_ts %>% 
  ggplot(data=.,aes(date,val))+
  geom_line()

ref_grid %>% st_get_dimension_values(.,1) %in%
sort(unique(lai_djf$x))

lai_djf %>% 
  ggplot(data=.,aes(x,y,fill=lai))+
  geom_raster()+
  scale_fill_viridis_c()+
  coord_sf()+
  facet_grid(~year)
