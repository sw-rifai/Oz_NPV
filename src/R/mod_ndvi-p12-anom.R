pacman::p_load(tidyverse,data.table,lubridate,stars,mgcv,mgcViz,tictoc)

## Data prep ------------
oz_poly <- rnaturalearth::countries110 %>% 
  st_as_sf() %>% 
  filter(name=='Australia') %>% 
  select(admin) 

dat <- arrow::read_parquet("../data_general/proc_oz_npv/mod-ndvi-precip_coarse_derivs.parquet")

tmp1 <- dat[,.(nobs = .N), by=.(x,y)]

dat <- dat[order(x,y,date)][,p12 := frollsum(precip,n=12,align='right')]
dat[,`:=`(v_anom = ndvi-ndvi_u)]
p_norm <- dat[,.(p_ma = mean(precip)*12),by=.(x,y)]
dat <- merge(dat,p_norm,by=c('x','y'))
dat <- dat[p_ma>0]
dat[,p12_anom := p12-p_ma]
dat[,hy := year(date+months(6))]


# g <- ncdf4::nc_open("../data_general/proc_oz_npv/CSR_GRACE_GRACE-FO_RL06_Mascons_all-corrections_v02.nc")
grace <- stars::read_stars("../data_general/proc_oz_npv/CSR_GRACE_GRACE-FO_RL06_Mascons_all-corrections_v02.nc") %>% 
  st_set_crs(4326)
grace <- grace[oz_poly]
grace <- grace %>% st_as_stars()
vec_grace_dates <- as.POSIXct("2002-01-01T00:00:00",tz='UTC')+days(floor((st_get_dimension_values(grace,3))))
grace <- grace %>% st_set_dimensions(.,3,values=vec_grace_dates)
gdat <- grace %>% set_names('w') %>% units::drop_units() %>% as.data.table()
setnames(gdat,'time','date')
gdat[,hy := year(date+months(6))]
gdat[,month := month(date)]
# ************************

# hydro year dat --------------------------------------------------------------
sdat <- dat[,.(va12 = mean(v_anom),
               p12 = mean(p12_anom),
               va_ma = mean(ndvi),
               p_ma = mean(p_ma)),by=.(x,y,hy)]
# *****************************************************************************

# Fig NDVI/P12 - NDVI_u/MAP ---------------------------------------------------
inv_oz <- st_difference(st_buffer(st_as_sfc(st_bbox(oz_poly)),100000), oz_poly)
p_out <- dat[month %in% c(12,1,2)][hy>2001] %>% 
  # .[!near(v_anom/ndvi_u,0,tol=0.005)] %>% 
  # .[between(v_anom/ndvi_u, -1,1)] %>% 
  .[,.(val = mean((ndvi/p12)) - mean((ndvi_u/p_ma)) ),
    by=.(x,y,hy,mandvi)] %>%
  .[mandvi>0] %>% 
  ggplot(data=.,aes(x,y,fill=val/mandvi))+
  geom_tile()+
  geom_sf(data=oz_poly, 
        inherit.aes = F, 
        fill='transparent')+
  geom_sf(data=inv_oz, 
    inherit.aes = F,
    fill='grey30',
    color='grey30')+
  scale_fill_gradient2(
    limits=c(-0.002,0.002),
    oob=scales::squish)+
  labs(x=NULL,y=NULL)+
  coord_sf(crs = st_crs(4326), 
    xlim=c(115,152),
    ylim=c(-43,-12), 
    expand = T)+
  facet_wrap(~hy)+
  theme_linedraw()+
  theme(panel.grid = element_blank(),
    panel.background = element_rect(fill='grey30'))
ggsave(p_out, 
  filename = 'figures/fig_map_ndviDIVp12_anom.png',
  width=30,
  height=25,
  units='cm',
  dpi=350)
# ***************************************************


# Fig GRACE map -----------------------------------------------------------
gdat[month %in% c(12,1,2)] %>% 
  # .[!near(v_anom/ndvi_u,0,tol=0.005)] %>% 
  # .[between(v_anom/ndvi_u, -1,1)] %>% 
  .[,.(val = mean(w,na.rm=T)),
    by=.(x,y,hy)] %>% 
  ggplot(data=.,aes(x,y,fill=val))+
  geom_tile()+
  geom_sf(data=oz_poly, 
        inherit.aes = F, 
        fill='transparent')+
  geom_sf(data=inv_oz, 
    inherit.aes = F,
    fill='grey30',
    color='grey30')+
  scale_fill_gradient2(
    limits=c(-10,10),
    oob=scales::squish)+
  labs(x=NULL,y=NULL)+
  coord_sf(crs = st_crs(4326), 
    xlim=c(115,152),
    ylim=c(-43,-12), 
    expand = T)+
  facet_wrap(~hy)+
  theme_linedraw()+
  theme(panel.grid = element_blank(),
    panel.background = element_rect(fill='grey30'), 
    axis.text = element_blank(), 
    axis.ticks = element_blank())
# **************************************************************************

dat[month %in% c(12,1,2)][hy>2001] %>% 
  .[mandvi>0] %>% 
  .[sample(.N,10000)] %>%
  .[,`:=`(val = ((ndvi/p12)-(ndvi_u/p_ma)))] %>% 
  ggplot(data=.,aes( p12_anom/p_ma, val,color=cut_interval(mandvi,5)))+
  # geom_point()+
  geom_smooth(se=F)+
  scico::scale_color_scico_d('batlow')+
  theme_linedraw()

plot(grace[,,,1:12],col=scico::scico(10,palette = 'roma'),breaks='equal')

st_apply(grace, 3, 'mean',na.rm=T) %>% as.data.table() %>% plot(pch=20)

betas <- dat[sample(.N,1000000)] %>% 
  lm(I(v_anom/ndvi_u)~I(p12_anom/p_ma),data=.) %>% 
  coef()

dat[month %in% c(12,1,2)][hy>2001 & hy<2021][sample(.N,1e6)] %>% 
  .[!near(v_anom/ndvi_u,0,tol=0.005)] %>% 
  .[between(v_anom/ndvi_u, -1,1)] %>% 
  ggplot(data=.,aes(p12_anom/p_ma,v_anom/ndvi_u,color=factor(hy)))+
  # geom_point()+
  geom_abline(aes(intercept=betas[1],
    slope=betas[2]),
    col='#ee0000')+
  geom_smooth(method='lm', 
    se=F)+
  scale_color_viridis_d()+
  facet_grid(fct_rev(cut_interval(y,5))~cut_interval(x,5))+
  theme_minimal()


inv_oz <- st_difference(sf::st_boundary(oz_poly),oz_poly) %>% 
  select(admin)




v_p <- list()
for(i in 2002:2021){
  p <- dat[hy==i][sample(.N,1e5)] %>% 
  .[!near(v_anom/ndvi_u,0,tol=0.005)] %>% 
  .[between(v_anom/ndvi_u, -1,1)] %>% 
  ggplot(data=.,aes(p12_anom/p_ma,v_anom/ndvi_u))+
  # geom_point()+
  geom_abline(aes(intercept=betas[1],
    slope=betas[2]),
    col='#ee0000')+
  geom_smooth(method='lm')+
  facet_grid(fct_rev(cut_interval(y,5))~cut_interval(x,5) + hy)+
  theme_minimal()  
  v_p <- append(v_p,p)
}

p_out <- gridExtra::grid.arrange(v_p,byrow=T)


library(patchwork)
p_out <- wrap_plots(v_p)



b3 <- sdat[sample(.N,1e6)] %>% 
  bam(va12~
      te(x,y,by=factor(hy))+
      te(x,y,by=p12,k=c(30,30)),
    discrete = T,
    select = T,
    data=.)
summary(b3)
plot(b3,scheme = 2) 
getViz(b3) %>% plot() %>% print(pages=1)
  plot(too.far=0.005, 
       n2=100) + 
  l_fitRaster()+
  geom_sf(data=oz_poly, 
          inherit.aes = F, 
          fill='transparent')+
  scale_fill_viridis_c(option='B')
  # scico::scale_fill_scico(midpoint = 0, 
  #                         palette = 'roma', 
  #                         limits=c(0,4e-04))  

b4 <- bam(va12~p12,data=sdat[sample(.N,1e6)])
summary(b4)

sdat[hy%in%2002:2021] %>% 
  ggplot(data=.,aes(x,y,fill=va12/va_ma))+
  geom_tile()+
  scale_fill_gradient2(limits=c(-0.5,0.5),
    oob=scales::squish)+
  coord_sf()+
  facet_wrap(~hy)+
  theme_linedraw()+
  theme(panel.grid = element_blank(),
    panel.background = element_rect(fill='grey30'))

b2 <- dat[sample(.N,1e6)] %>% 
  bam(v_anom~
      te(year,p12_anom)+
      te(x,y,month)+
      te(x,y), 
    data=., 
    select = T,
    discrete = T)
summary(b2)
plot(b2,scheme = 2)

# SCRATCH -----------------------------------------------------------------

b1 <- dat[sample(.N,500000)] %>% 
  bam(v_anom~te(x,y,month,year,
                by=p12_anom,k=c(25,25))+
        te(x,y), data=.)
summary(b1)
# plot(b1,scheme=2)
(getViz(b1) %>% sm(1) %>% 
    plotSlice(fix=list('month'=c(11), 
                       'year'=c(2001:2020)), 
              too.far=0.005, 
              n=100))+
  l_fitRaster()+
  geom_sf(data=oz_poly, 
          inherit.aes = F, 
          fill='transparent')+
  scale_fill_viridis_c(option='H')
  scico::scale_fill_scico(midpoint = 0, 
                          palette = 'batlowK', 
                          limits=c(0,4e-04))

p_norm[p_ma>0] %>% 
  ggplot(data=.,aes(x,y,fill=p_ma))+
  geom_tile()+
  scale_fill_viridis_c()+
  coord_sf()

sdat <- dat[,.(val = max(ndvi-ndvi_u,na.rm=T)),
            by=.(x,y,year)]

sdat2 <- dat[is.na(ndvi)==F][,.(val = cor((ndvi-ndvi_u),(precip-precip_u))), 
                             by=.(x,y,year)]
