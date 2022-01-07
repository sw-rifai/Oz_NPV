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


iav <- dat[,.(a_ndvi = mean(ndvi,na.rm=T)),by=.(xc,yc,hydro_year)][,
  .(sd_ndvi = sd(a_ndvi,na.rm=T),
    ma_ndvi = mean(a_ndvi,na.rm=T)),by=.(xc,yc)]

p_ma_ndvi <- iav %>% 
  ggplot(data=.,aes(xc,yc,fill=ma_ndvi))+
  geom_tile()+
  scale_fill_viridis_c(option='D',
    direction = 1,
    end=0.9,
    limits=c(0,0.8),oob=scales::squish)+
  coord_sf(expand = F)+
  labs(x=NULL,
    y=NULL,
    fill=expression(paste(NDVI[MA]~" ")))+
  theme_linedraw()+
  theme(panel.grid = element_blank(), 
    legend.position = 'bottom',
    legend.key.height = unit(0.2,'cm'), 
        legend.key.width = unit(1,'cm'))

p_sd_ndvi <- iav %>% 
  ggplot(data=.,aes(xc,yc,fill=sd_ndvi))+
  geom_tile()+
  scale_fill_viridis_c(
    option='F',
    limits=c(0,0.1),oob=scales::squish)+
  coord_sf(expand = F)+
  labs(x=NULL,
    y=NULL,
    fill=expression(paste(NDVI[SD]~" ")))+
  theme_linedraw()+
  theme(panel.grid = element_blank(), 
    legend.position = 'bottom',
    legend.key.height = unit(0.2,'cm'), 
        legend.key.width = unit(1,'cm'))

p_cv_ndvi <- iav %>% 
  ggplot(data=.,aes(xc,yc,fill=sd_ndvi/ma_ndvi))+
  geom_tile()+
  scale_fill_viridis_c(
    option='B',
    limits=c(0,0.5),oob=scales::squish)+
  coord_sf(expand = F)+
  labs(x=NULL,
    y=NULL,
    fill=expression(paste(NDVI[CV]~" ")))+
  theme_linedraw()+
  theme(panel.grid = element_blank(), 
    legend.position = 'bottom',
    legend.key.height = unit(0.2,'cm'), 
        legend.key.width = unit(1,'cm'))

p_out <- (p_ma_ndvi|p_sd_ndvi|p_cv_ndvi)+
  patchwork::plot_annotation(tag_levels = 'a',tag_prefix = '(',tag_suffix = ')')
ggsave(p_out,
  filename = 'figures/fig_map_ma-sd-cv-of-ndvi.png', 
  width=35,
  height=15,
  units='cm',
  dpi=400, 
  device=grDevices::png)
