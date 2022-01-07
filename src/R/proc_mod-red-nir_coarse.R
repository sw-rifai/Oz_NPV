pacman::p_load(tidyverse,data.table,lubridate,stars)

# proc red ----------------------------------------------------------------
red1 <- stars::read_stars("../data_general/proc_oz_npv/MCD43A4_red_5000m_Australia_mMean_maskFireDefor_2001_2010.tif", 
  proxy=F) %>% 
  st_set_dimensions(.,3,
    values=seq(ymd("2001-01-01"),ymd("2010-12-01"),by='1 month'),names = 'date') %>% 
  set_names('red')
red2 <- stars::read_stars("../data_general/proc_oz_npv/MCD43A4_red_5000m_Australia_mMean_maskFireDefor_2011_2020.tif", 
  proxy=F) %>% 
  st_set_dimensions(.,3,
    values=seq(ymd("2011-01-01"),ymd("2020-12-01"),by='1 month'),names = 'date') %>% 
  set_names('red')
# red3 <- stars::read_stars("../data_general/proc_oz_npv/MCD43A4_red_5000m_Australia_mMean_maskFireDefor_2017_2020.tif", 
#   proxy=F) %>% 
#   st_set_dimensions(.,3,
#     values=seq(ymd("2017-01-01"),ymd("2020-12-01"),by='1 month'),names = 'date') %>% 
#   set_names('red')
# red4 <- stars::read_stars("../data_general/proc_oz_npv/MCD43A4_red_5000m_Australia_mMean_maskFireDefor_2021_2021.tif", 
#   proxy=F) %>% 
#   st_set_dimensions(.,3,
#     values=seq(ymd("2021-01-01"),ymd("2021-11-01"),by='1 month'),names = 'date') %>% 
#   set_names('red')

red <- c(red1,red2,#red3,red4,
  along=3)
rm(red1,red2)
gc(full=T)



# proc NIR ----------------------------------------------------------------
nir1 <- stars::read_stars("../data_general/proc_oz_npv/MCD43A4_nir_5000m_Australia_mMean_maskFireDefor_2001_2010.tif", 
  proxy=F) %>% 
  st_set_dimensions(.,3,
    values=seq(ymd("2001-01-01"),ymd("2010-12-01"),by='1 month'),names = 'date') %>% 
  set_names('nir')
nir2 <- stars::read_stars("../data_general/proc_oz_npv/MCD43A4_nir_5000m_Australia_mMean_maskFireDefor_2011_2020.tif", 
  proxy=F) %>% 
  st_set_dimensions(.,3,
    values=seq(ymd("2011-01-01"),ymd("2020-12-01"),by='1 month'),names = 'date') %>% 
  set_names('nir')
# nir3 <- stars::read_stars("../data_general/proc_oz_npv/MCD43A4_nir_5000m_Australia_mMean_maskFireDefor_2017_2020.tif", 
#   proxy=F) %>% 
#   st_set_dimensions(.,3,
#     values=seq(ymd("2017-01-01"),ymd("2020-12-01"),by='1 month'),names = 'date') %>% 
#   set_names('nir')
# nir4 <- stars::read_stars("../data_general/proc_oz_npv/MCD43A4_nir_5000m_Australia_mMean_maskFireDefor_2021_2021.tif", 
#   proxy=F) %>% 
#   st_set_dimensions(.,3,
#     values=seq(ymd("2021-01-01"),ymd("2021-11-01"),by='1 month'),names = 'date') %>% 
#   set_names('nir')

nir <- c(nir1,nir2,along=3)
rm(nir1,nir2)
gc(full=T)


# merge -------------------------------------------------------------------
dat <- c(red,nir)
rm(red,nir); gc(full=T)

dat <- dat %>% mutate(ndvi = (nir-red)/(nir+red))
dat <- dat %>% mutate(nirv = ndvi*nir)
dat <- dat %>% as.data.table()
gc(full=T)

# calc norms MODIS --------------------------------------------------------------
dat[,`:=`(year=year(date),
  month=month(date))]
gc(full=T)
norms_dat <- dat[between(date,"2001-01-01","2016-12-31")][
  ,.(ndvi_u = mean(ndvi,na.rm=T), 
     ndvi_sd = sd(ndvi,na.rm=T), 
     nirv_u = mean(nirv,na.rm=T),
    nirv_sd = sd(nirv,na.rm=T)),
  by=.(x,y,month)]
dat <- merge(dat,norms_dat,by=c('x','y','month'))
gc(full=T)

rm(norms_dat); gc()

# calc norms met ----------------------------------------------------------
def <- stars::read_stars(
  "../data_general/proc_oz_npv/terraclimate_def_2000_2020.tif",
  proxy = F) %>% 
  st_set_dimensions(.,3,
    values=seq(ymd("2000-01-01"),ymd("2020-12-01"),by='1 month'),
    names = 'date') %>% 
  set_names('def')
precip <- stars::read_stars(
  "../data_general/proc_oz_npv/terraclimate_precip_2000_2020.tif",
  proxy = F) %>% 
  st_set_dimensions(.,3,
    values=seq(ymd("2000-01-01"),ymd("2020-12-01"),by='1 month'),
    names = 'date') %>% 
  set_names('precip')
pet <- stars::read_stars(
  "../data_general/proc_oz_npv/terraclimate_pet_2000_2020.tif",
  proxy = F) %>% 
  st_set_dimensions(.,3,
    values=seq(ymd("2000-01-01"),ymd("2020-12-01"),by='1 month'),
    names = 'date') %>% 
  set_names('pet')
vpd <- stars::read_stars(
  "../data_general/proc_oz_npv/terraclimate_vpd_2000_2020.tif",
  proxy = F) %>% 
  st_set_dimensions(.,3,
    values=seq(ymd("2000-01-01"),ymd("2020-12-01"),by='1 month'),
    names = 'date') %>% 
  set_names('vpd')
met <- c(def,precip,pet,vpd)
rm(def,precip,pet,vpd)
gc(full=T)
met <- met %>% as.data.table()
met[,`:=`(
  year=year(date),
  month=month(date))]

norms_met <- met[date%between%c("2001-01-01","2016-12-31")][
  ,.(def_u = mean(def,na.rm=T), 
     def_sd = sd(def,na.rm=T), 
     precip_u = mean(precip,na.rm=T),
    precip_sd = sd(precip,na.rm=T), 
    vpd_u = mean(vpd,na.rm=T),
    vpd_sd = sd(vpd,na.rm=T),
    pet_u = mean(pet,na.rm=T),
    pet_sd = sd(pet,na.rm=T)),
  by=.(x,y,month)]

met <- merge(met,norms_met,by=c("x","y","month"))
# met <- met[def_sd>0]
rm(norms_met); gc()

# xy_keep <- met[,.(x,y)] %>% unique
# dat <- merge(dat,xy_keep,by=c("x","y"),all.x = F,all.y=T)

# write to disk -----------------------------------------------------------
arrow::write_parquet(dat, 
  sink="../data_general/proc_oz_npv/mod-red-nir_coarse.parquet",
  compression='snappy')
arrow::write_parquet(met, sink="../data_general/proc_oz_npv/terraclimate_pr-pet-def-vpd.parquet",compression='snappy')



# SCRATCH -----------------------------------------------------------------
dat[between(x,145,150) & between(y,-45,-40)][month==7] %>% 
  ggplot(data=.,aes(x,y,fill=ndvi_u))+
  geom_tile()+
  coord_sf()+
  scale_fill_viridis_c(option='B')+
  facet_wrap(~year)

met[between(x,145,150) & between(y,-45,-40)][month==7] %>% 
  ggplot(data=.,aes(x,y,fill=precip))+
  geom_tile()+
  coord_sf()+
  scale_fill_viridis_c(option='B')+
  facet_wrap(~year)

# dat <- merge(dat,met,by=c("x","y","year","month"))
# met[date==min(date)][def_sd>0] %>% 
#   ggplot(data=.,aes(x,y,fill=def_sd))+
#   geom_raster()+
#   scale_fill_viridis_c(limits=c(0,1200))+
#   coord_sf()
# dat[sample(.N,10000)][ndvi>0] %>% 
#   ggplot(data=.,aes(def,nirv))+
#   geom_point(alpha=0.1)+
#   geom_smooth()
# 
#  dat[date==ymd("2015-11-01")] %>% 
#   ggplot(data=.,aes(x,y,fill=def))+
#   geom_tile()+
#   coord_sf()+
#   scale_fill_viridis_c(option='B')
# 
# dat %>% slice('date',1) %>% select(ndvi)
# ggplot()+
#   geom_stars(data=dat %>% slice('date',1) %>% select(nirv))+
#   scale_fill_viridis_c(limits=c(0,0.9))+
#   coord_sf()
# 
# 
# 
# 
# fn_ndvi2 <- function(d) (d['nir']-d['red'])/(d['nir']+d['red']) 
# fn <- function(d) d['red']+d['nir']
# ndvi2 <- st_apply(dat[,,,3:4], 1:2, fn_ndvi2)
# 
# ndvi2
# 
# dat[,200:300,200:300,1:10] %>% as.data.table()
# test <- st_apply(dat[,200:300,200:300,1:10],1:3,fn)
# test %>% as.data.table()
# 
# test <- dat[,200:300,200:300,1:10]$red + dat[,200:300,200:300,1:10]$nir
# 
# 
# dat[,200:300,200:300,1:10] %>% 
#   mutate(ndvi = (nir-red)/(nir+red))
# 
# tif = system.file("tif/L7_ETMs.tif", package = "stars")
# x = read_stars(tif)
# 
