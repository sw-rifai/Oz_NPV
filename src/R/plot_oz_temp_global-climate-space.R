pacman::p_load(tidyverse,data.table,lubridate,stars,mgcv,patchwork)
library(viridis); library(patchwork)
source("src/R/gg_sci_themes.R")
vec_div_clim <- pals::brewer.rdylbu(5)
 
fluxnet <- fread("data/fluxnet_locs.csv")
fluxnet <- fluxnet[is.na(LOCATION_LAT)==F]
fluxnet <- st_as_sf(fluxnet, coords=c("LOCATION_LONG","LOCATION_LAT"))
st_crs(fluxnet) <- st_crs(4326)

ozflux <- fread("data/oz-flux_locs.csv")
ozflux <- ozflux %>% mutate(x=longitude %>% as.numeric(),
  y=latitude %>% as.numeric())
ozflux$latitude <- str_replace(ozflux$latitude,"−","-")
ozflux <- st_as_sf(ozflux, coords=c("longitude","latitude"))
st_crs(ozflux) <- st_crs(4326)

tmax_sd <- read_stars("../data_general/proc_oz_npv/tc_tmax_sd_1981_2010.tif") %>% 
  set_names("tmax_sd")
tmax_ma <- read_stars("../data_general/proc_oz_npv/tc_matmax_1981_2010.tif") %>% 
  set_names("tmax_ma")
tmax_cv <- tmax_sd/tmax_ma 
tmax_cv <- tmax_cv %>% set_names("tmax_cv")


mod_ndvi <- read_stars("../data_general/proc_oz_npv/modis_mean-ndvi_2001_2015.tif") %>% 
  set_names('ndvi')

tdat <- c(tmax_sd,tmax_cv,tmax_ma)

oz_poly <- rnaturalearth::countries110 %>% 
  st_as_sf() %>% 
  filter(name=='Australia') %>% 
  select(admin) 

ozflux <- st_extract(tdat,ozflux)
fluxnet <- st_extract(tdat,fluxnet)

tdat_oz <- tdat[oz_poly] %>% as.data.table()
tdat_oz <- tdat_oz[is.na(map)==F]

tdat <- tdat %>% as.data.table()
tdat <- tdat[is.na(map)==F]

tdat <- merge(tdat,
  tdat_oz[,`:=`(country='Australia')][,.(x,y,country)],
  by=c('x','y'),all.x=T,all.y=T)
tdat[,`:=`(country = ifelse(is.na(country)==T,'Not Australia','Australia'))]
tdat[,`:=`(region = case_when(
  country=='Australia' & y > -28 ~ 'Northern Australia', 
  country=='Australia' & y < -28 ~ 'Southern Australia', 
  country!='Australia'  ~ 'Elsewhere'))]

tdat <- tdat[tmax_cv < 10][tmax_cv>0]
tdat %>% ggplot(data=.,aes(x,y,fill=tmax_ma))+
  geom_tile()+scale_fill_viridis_c()


p_0 <- tdat %>% 
  .[tmax_ma>1] %>%
  # .[map>20] %>% 
  # .[ndvi>0.1] %>% 
  # .[sample(.N, 50000)] %>%
  .[order(region)] %>%
  ggplot(data=.,aes(tmax_ma,tmax_cv,color=region,alpha=region,size=region))+
  # geom_density_2d_filled()+
  geom_point()+

  # geom_rect(
  #   inherit.aes = F,
  #   aes(xmin=500,xmax=1400, 
  #   ymin=0.8,ymax=1),
  #   fill=NA, 
  #   color='grey30')+
  
  # geom_point(data=fluxnet, inherit.aes = F, 
  #   aes(map,cv),size=1.5,shape=24,color='grey30',fill='grey')+
  # geom_point(aes(x=600,y=0.85), 
  #   inherit.aes = F,size=2,shape=24,
  #   color='grey30',fill='grey')+
  # geom_text(aes(x=1000,y=0.85,label='Fluxnet'), 
  #   inherit.aes = F,
  #   size=5,
  #   color='black')+
  # 
  # geom_point(data=ozflux, inherit.aes = F, 
  #   aes(map,cv),size=1.5,shape=24,
  #   color='yellow',fill='purple')+
  # geom_point(aes(x=600,y=0.95), 
  #   inherit.aes = F,size=2,shape=24,
  #   color='yellow',fill='purple')+
  # geom_text(aes(x=1000,y=0.95,label='OzFlux'), 
  #   inherit.aes = F,
  #   size=5,
  #   color='black')+

    scale_color_manual(
    values=c("Elsewhere"='black',"Northern Australia"='red',"Southern Australia"='blue'))+
  scale_alpha_manual(
    values=c("Elsewhere"=0.05,"Northern Australia"=0.05,"Southern Australia"=0.05))+
  scale_size_manual(
    values=c("Elsewhere"=0.1,"Northern Australia"=0.1,"Southern Australia"=0.1))+
  # geom_point(aes(map,cv),
  #   alpha=0.01,
  #   size=0.01,
  #   data=tdat_oz,color='red',inherit.aes = F)+
  # coord_cartesian(
  # xlim=c(0,5000),
  # ylim=c(0,1))+
  labs(x='Mean Annual Tmax (°C)', 
    y="Annual Tmax Coefficient of Variation", 
    caption = "Locations with mean annual Tmax > 1°C")+
  guides(color=guide_legend(override.aes = list(
    size=1, alpha=1)))+
  theme_linedraw()+
  theme(panel.grid = element_blank(), 
    legend.position = c(0.99,0.99),
    legend.justification = c(0.99,0.99));
p_0
ggsave(p_0,
  filename = "figures/fig_tmax_ma_cv_global.png",
  width=17,
  height=15,
  units='cm',
  dpi=350)

p_h <- tdat[map<8000 & map > 20][ndvi>0.1] %>% 
  # .[sample(.N, 10000)] %>% 
  .[order(region)] %>%
  ggplot(data=.,aes(cv,fill=region))+
  geom_density(alpha=0.5)+
  scale_fill_manual(
  values=c("Elsewhere"='black',"Northern Australia"='red',"Southern Australia"='blue'))+
  labs(x='Annual Precip. Coefficient of Variation', 
    fill='Region')+
  coord_cartesian(expand=F)+
  theme_linedraw()+
  theme(
    panel.grid = element_blank(),
    panel.background = element_rect(fill=NULL),
    plot.background = element_rect(fill=NULL),
    legend.position = c(0.99,0.99),
    legend.justification = c(0.99,0.99)); p_h

p_0 <- p_0 + 
    patchwork::inset_element(p_h,
    left = 0.4,right = 0.99,
    bottom = 0.4,top=0.99)
ggsave(p_0,
  filename = "figures/fig_map_cv_global_oz_no-mask.png", 
  width=17,
  height=15, 
  units='cm',
  dpi=350)


p_3 <- tdat[map<8000][sample(.N,10000)] %>% 
  ggplot(data=.,aes(map,cv))+
  # geom_density_2d_filled()+
  geom_point(alpha=0.1,size=0.1)+
  geom_point(aes(map,cv),
    alpha=0.01,
    size=0.01,
    data=tdat_oz,color='red',inherit.aes = F)+
  labs(x='Mean Annual Precip (mm)', 
    y="Coefficient of Variation")+
  theme_linedraw()+
  theme(panel.grid = element_blank());
ggsave(p_3,
  filename = "figures/fig_map_cv_global_oz-red.png", 
  width=15,
  height=15, 
  units='cm',
  dpi=350)

f_cv <- ggplot()+
  geom_stars(data=p_cv)+
  scale_fill_viridis_c(limits=c(0,1),oob=scales::squish)+
  coord_sf(expand = F)+
  labs(x=NULL,y=NULL,
    y='CV')+
  theme_linedraw()+
  theme(panel.grid = element_blank())
ggsave(f_cv,
  filename = "figures/fig_map-of_cv_global.png", 
  width=30,
  height=15, 
  units='cm',
  dpi=350)

f_sd <- ggplot()+
  geom_stars(data=p_sd)+
  scale_fill_viridis_c(limits=c(0,1000),oob=scales::squish, 
    option='C')+
  coord_sf(expand = F)+
  labs(x=NULL,y=NULL,
    y='MAP (mm)')+
  theme_linedraw()+
  theme(panel.grid = element_blank())
ggsave(f_sd,
  filename = "figures/fig_map-of_IAPrecipSD_global.png", 
  width=30,
  height=15, 
  units='cm',
  dpi=350)


f_map <- ggplot()+
  geom_stars(data=p_map)+
  scale_fill_viridis_c(limits=c(0,3500),oob=scales::squish, 
    option='A',direction=-1)+
  coord_sf(expand = F)+
  labs(x=NULL,y=NULL,
    y='MAP (mm)')+
  theme_linedraw()+
  theme(panel.grid = element_blank())
ggsave(f_map,
  filename = "figures/fig_map-of_mean-annual-precip_global.png", 
  width=30,
  height=15, 
  units='cm',
  dpi=350)

tdat[ndvi>0.15] %>% 
  ggplot(data=.,aes(x,y,fill=cv))+
  geom_tile()+
  scale_fill_viridis_c(limits=c(0,0.7),na.value = 'grey')+
  coord_sf(expand=F)+
  labs(x=NULL,
    y=NULL,
    fill='CV',
    caption = 'Masked to NDVI > 0.1')+
  theme_linedraw()+
  theme(panel.grid = element_blank(), 
    panel.background = element_rect(fill='grey'))
ggsave(
  filename = "figures/fig_map-of_cv_global_w-ndvi-mask.png", 
  width=30,
  height=15, 
  units='cm',
  dpi=350)



tdat[map<8000 & ndvi > 0.1][sample(.N,50000)] %>% 
  ggplot(data=.,aes(map,cv))+
  # geom_density_2d_filled()+
  geom_point(alpha=0.1,size=0.1)+
  geom_point(aes(map,cv),
    alpha=0.01,
    size=0.01,
    data=tdat_oz[sample(.N)],
    color='red',inherit.aes = F)+
  labs(x='Mean Annual Precip (mm)', 
    y="Coefficient of Variation")+
  theme_linedraw()+
  theme(panel.grid = element_blank())



tdat %>% #[map<8000][sample(.N,50000)] %>% 
  ggplot(data=.,aes(map,cv))+
  # geom_density_2d_filled()+
  geom_point(alpha=0.1,size=0.1)+
  geom_point(aes(map,cv,color=region),
    alpha=0.01,
    size=0.01,
    data=tdat,
    # color='red',
    inherit.aes = F)+
  scale_color_manual(
    values=c("Elsewhere"='black',"North Australia"='red',"South Australia"='blue'))+
  labs(x='Mean Annual Precip (mm)', 
    y="Coefficient of Variation")+
  coord_cartesian(
  xlim=c(0,5000),
  ylim=c(0,1))+
  theme_linedraw()+
  theme(panel.grid = element_blank())
