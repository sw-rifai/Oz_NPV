# Description: Calc smooth NDVI and Precip derivatives with a Savitzky-Golay filter
# Date: 2021-11-26
# Author: Sami Rifai
# 
pacman::p_load(tidyverse,data.table,lubridate,stars,mgcv,furrr,tictoc)

# options------------------------------------------------------------------
n_splits <- 100

# functions ---------------------------------------------------------------
mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

sg_derivs <- function(din,x_var1,x_var2){
  x1 <- din[,eval(as.symbol(x_var1))]
  out1 <- signal::sgolayfilt(x1,p = 5,n = 15,m = 1,1)
  out_var1 <- paste0(x_var1,"_deriv")
  din[,(out_var1) := out1]
  
  x2 <- din[,eval(as.symbol(x_var2))]
  out2 <- signal::sgolayfilt(x2,p = 5,n = 15,m = 1,1)
  out_var2 <- paste0(x_var2,"_deriv")
  din[,(out_var2) := out2]
  return(din)
  }


# main ------------------------------------------------------------------------
## gather data ------
dat <- arrow::read_parquet("../data_general/proc_oz_npv/mod-red-nir_coarse.parquet", 
  col_select = c("x","y","date","month","year","ndvi","ndvi_u"))
met <- arrow::read_parquet("../data_general/proc_oz_npv/terraclimate_pr-pet-def-vpd.parquet",
  col_select = c("x","y","date","month","year","precip"))
dat <- merge(dat,met,by=c("x","y","year","month","date"))
rm(met); gc(full=T)

d_mandvi <- dat[date <= ymd("2017-01-01")][,.(mandvi = mean(ndvi,na.rm=T)),by=.(x,y)]
dat <- merge(dat,d_mandvi,by=c('x','y'))
dat[,`:=`(ndvi = ifelse(is.na(ndvi)==T, ndvi_u, ndvi))]
dat[,`:=`(ndvi = ifelse(is.na(ndvi)==T, mandvi, ndvi))]
dat[,`:=`(id = .GRP),by=.(x,y)]
dat[,`:=`(n = sum(is.na(ndvi_u)==F)),by=.(x,y)]
# dat <- dat[n==max(dat$n)]
dat <- dat[n>0]
gc(full=T)

## splice data -----
vec_ids <- unique(dat$id)
l_ids <- split(vec_ids, cut(seq_along(vec_ids), n_splits, labels = FALSE))

## big parallel process ------
plan(multisession)
all_out <- list()
for(i in 1:n_splits){
  tic()
  print(paste("starting split ",i));
  gc(full=T)
  all_out[[i]] <- dat[id%in%l_ids[[i]]] %>%
                    .[order(id,date)] %>% 
                split(by=c("id"),sorted=T) %>% 
                # split(f=list(.$id),drop=T) %>%
                future_map(~sg_derivs(.x, 'ndvi','precip'),
                  .progress = F,
                  .options = furrr_options(seed=333L)) %>%
    rbindlist(fill = T)
  toc()
}
dat2 <- rbindlist(all_out)
rm(all_out)
gc()


arrow::write_parquet(dat2, sink="../data_general/proc_oz_npv/mod-ndvi-precip_coarse_derivs.parquet",compression='snappy')

# scratch -----------------------------------------------------------------
# dat2 %>% #[n==max(dat$n)]
#   .[date==min(date)] %>% 
#   ggplot(aes(x,y,fill=ndvi_deriv))+
#   geom_tile()+
#   coord_sf()
# 
# dat[is.na(ndvi_u)==T] %>% 
#    ggplot(aes(x,y))+
#   geom_tile()+
#   coord_sf()
# 
# dat2[,`:=`(hydro_year = year(date-months(6)))]
# 
# tmp1 <- dat2[dat2[,.I[ndvi_deriv==max(ndvi_deriv,na.rm=T)],by=.(x,y,hydro_year)]$V1]
# tmp2 <- tmp1[,.(mn = mode(month)),by=.(x,y)]
# 
# p1 <- dat2[dat2[,.I[precip_deriv==max(precip_deriv,na.rm=T)],by=.(x,y,hydro_year)]$V1]
# p2 <- p1[,.(mp = mode(month)),by=.(x,y)]
# j1 <- merge(tmp2,p2,by=c('x','y'))

fn <- function(mn,mp){
  out <- mn-mp
  # if(out < 0){
  #   out <- -(mp-mn) + 12 
  # }
  if(out>6){
    out <- (mn-mp) - 12
  }
  return(out)
}
fn <- Vectorize(fn)
j1[,diff := fn(mn,mp)]
# j1[,`:=`(apply(j1,1,fn(mn,mp)))]
fn(6,5)

j1 %>% ggplot(data=.,aes(x,y,fill=diff))+
  geom_tile()+
  coord_sf()+
  scale_fill_gradient2()
  # scale_fill_viridis_b(option='H',n.breaks=11)

# PCA ---------------------------------------------------------------------
dat2[is.na(ndvi_deriv)==T]

sdat <- dat2[,.(ndvi_deriv = mean(ndvi_deriv,na.rm=T),
  precip_deriv = mean(precip_deriv,na.rm=T)), 
  by=.(x,y,month)]
sdat[is.na(ndvi_deriv)==T | is.na(precip_deriv)==T]

# wdat <- sdat %>% dcast(x+y~month,value.var = c("precip_deriv","ndvi_deriv"), drop=T)
# wdat
wdat <- sdat %>% pivot_wider(id_cols=c("x","y"),names_from='month',values_from=c("ndvi_deriv","precip_deriv")) %>% as.data.table()

apply(wdat,2,function(x) sum(is.na(x)))
wdat[is.na(precip_deriv_7)==F] %>% 
  ggplot(data=.,aes(x,y))+geom_tile()+
  coord_sf()+
  geom_tile(
    data=wdat[is.na(precip_deriv_4)==T],fill='red',
    aes(x,y))

dat %>%
  .[year==2010] %>% 
  # . [between(x,145,150) & between(y,-45,-40)] %>% 
  . [month==7] %>% 
  ggplot(data=.,aes(x,y,fill=ndvi))+
  geom_tile()+
  coord_sf(
    crs = st_crs("+proj=aea +lat_1=29.5 +lat_2=42.5")
    # crs = st_crs(5070)
    )+
  # coord_map(projection = 'azequalarea')+
  scale_fill_viridis_c()+
  facet_wrap(~year)
tmp_s <- dat %>%
  .[year==2010] %>% 
  # . [between(x,145,150) & between(y,-45,-40)] %>% 
  . [month==7] %>% 
  select(x,y,ndvi) %>% 
  st_as_stars() %>% 
  st_set_crs(4326)
# tmp_s2 <- stars::st_transform_proj(tmp_s,crs = st_crs("+proj=aea +lat_1=29.5 +lat_2=42.5"))
tmp_s3 <- stars::st_transform_proj(tmp_s,
  crs=st_crs("+proj=eqearth"))
plot(tmp_s3, col=viridis::inferno(100), breaks='equal')
ggplot()+
  geom_stars(data=tmp_s3)+
  scale_fill_viridis_c()+
    coord_sf(
      crs = st_crs("+proj=eqearth")
    # crs = st_crs("+proj=aea +lat_1=29.5 +lat_2=42.5")
    # crs = st_crs(5070)
    )

points <- data.table(x=c(144.0944,150.723), 
  y=c(-37.4222, -33.615), 
  name=c("Wombat","CMLP"))

map_dat <- dat[,.(map = mean(precip,na.rm=T)*12),by=.(x,y)]
mandvi_dat <- dat[,.(mandvi = mean(ndvi,na.rm=T)),by=.(x,y)]

p_precip <- wdat %>% 
  select(x,y,starts_with("precip")) %>% 
  pivot_longer(cols=starts_with("precip")) %>% 
  merge(., map_dat, by=c("x","y")) %>% 
  mutate(name = str_replace(name,'precip_deriv_',"")) %>% 
  mutate(month = as.numeric(name)) %>%
  ggplot(data=.,aes(x,y,fill=value/map))+
  geom_tile()+
  geom_point(aes(x,y,shape=name),data=points,inherit.aes = F,color='black')+
  coord_sf()+
  scale_fill_gradient2(
    # limits=c(-60,60),
    oob=scales::squish)+
  labs(x=NULL,y=NULL,
    fill=expression(paste(Delta~P/P["MA"])), 
    title='Normalized Monthly Derivative of Precipitation')+
  facet_wrap(~month, nrow=2)+
  theme_linedraw()+
  theme(panel.grid = element_blank(), 
    panel.background = element_rect(fill='grey'))

p_ndvi <- wdat %>% 
  select(x,y,starts_with("ndvi")) %>% 
  pivot_longer(cols=starts_with("ndvi")) %>% 
  merge(., mandvi_dat, by=c("x","y")) %>% 
  mutate(name = str_replace(name,'ndvi_deriv_',"")) %>% 
  mutate(month = as.numeric(name)) %>% 
  ggplot(data=.,aes(x,y,fill=value/mandvi))+
  geom_tile()+
  geom_point(aes(x,y,shape=name),data=points,inherit.aes = F,color='black')+
  coord_sf()+
  scale_fill_gradient2(
    limits=c(-0.2,0.2),
    oob=scales::squish)+
  labs(x=NULL,y=NULL,
    fill=expression(paste(Delta~NDVI/NDVI['MA'])), 
    title='Normalized Monthly Derivative of NDVI')+
  facet_wrap(~month,nrow = 2)+
  theme_linedraw()+
  theme(panel.grid = element_blank(), 
    panel.background = element_rect(fill='grey'))

library(patchwork)
p_out <- (p_precip/p_ndvi)
ggsave(p_out, filename = 'figures/fig_map_deriv-precip_deriv-ndvi_normalized.png',
  width=35,
  height=25,
  unit='cm',
  dpi=350)

wdat %>% 
    ggplot(data=.,aes(x,y,fill=precip_deriv_6))+
  geom_tile()+
  geom_point(aes(x,y,shape=name),data=points,inherit.aes = F,color='black')+
  coord_sf()+
  scale_fill_gradient2(
    # limits=c(-60,60),
    na.value = 'grey',
    oob=scales::squish)+
  labs(x=NULL,y=NULL,
    # fill=expression(paste(Delta~P/P["MA"])), 
    # title='Normalized Monthly Derivative of Precipitation'
    )

mpc <- prcomp(~., data=drop_na(wdat[,-c('x','y')]),scale=T,center=T)
summary(mpc)
# mpc %>% plot
# mpc
# screeplot(mpc)
# biplot(mpc)
mpc$rotation[,1] %>% plot

wdat %>% drop_na()
wdat %>% bind_cols(predict(mpc,newdata = (wdat) %>% as.data.table())) %>% 
  # select(x,y,PC1,PC2,PC3,PC4) %>% 
  # pivot_longer(col=c("PC1",'PC2','PC3','PC4')) %>% 
  ggplot(data=.,aes(x,y,fill=PC1))+
  geom_tile()+
  coord_sf()+
  scale_fill_gradient2(
    limits=c(-10,10),
    oob=scales::squish)+
  theme_linedraw()+
  theme(panel.grid = element_blank())

mpc_n <- prcomp(~., wdat %>% select(starts_with('ndvi')),scale=T,center=T)
summary(mpc_n)
wdat %>% bind_cols(predict(mpc_n,newdata = wdat) %>% as.data.table()) %>% 
  ggplot(data=.,aes(x,y,fill=PC2))+
  geom_tile()+
  coord_sf()+
  scale_fill_gradient2(
    limits=c(-10,10),
    oob=scales::squish)

mpc_p <- prcomp(~., wdat %>% select(starts_with('precip')),scale=T,center=T)
summary(mpc_p)
wdat %>% bind_cols(predict(mpc_p,newdata = wdat) %>% as.data.table()) %>% 
  ggplot(data=.,aes(x,y,fill=PC2))+
  geom_tile()+
  coord_sf()+
  scale_fill_gradient2(
    limits=c(-10,10),
    oob=scales::squish)


j1 %>% 
  ggplot(data=.,aes(x,y,fill=mp))+
  geom_tile()+
  scico::scale_fill_scico(palette='batlow')+
  coord_sf(expand = F)

dat2[id%in%sample.int(dat2$id %>% max,10)][hydro_year>2000] %>% 
  .[,`:=`(hydro_month = ifelse(month<6,month+7,month-6))] %>%  
  ggplot(data=.,aes(hydro_month,ndvi,group=hydro_year,color=hydro_year))+
  geom_line()+
  scale_color_viridis_c(option='H')+
  facet_wrap(~id)

j2 <- dat2[,.(sk_vd = skewness(ndvi_deriv)),by=.(x,y)]
j2 %>% 
  ggplot(data=.,aes(x,y,fill=sk_vd))+
  geom_tile()+
  coord_sf()+
  scale_fill_gradient2(limits=c(-5,5))

dat[date==ymd("2013-01-01")]  %>% 
  ggplot(data=.,aes(x,y,fill=ndvi))+
  geom_tile()+
  coord_sf()+
  scale_fill_gradient2(limits=c(0.1,0.9))

skewness(dat2[id==10]$ndvi_deriv)
dat2[id==1]$precip %>% hist
moments::kurtosis(dat2[id==1]$precip)
moments::skewness(dat2[id==1]$ndvi_deriv)
skewness(rnorm(10000))
kurtosis(rnorm(10000))

dat2$ndvi_deriv %>% hist(1000)
dat2$precip_deriv %>% hist(1000)

all_out[[1]][id%in%sample.int(3400,10)] %>% 
  ggplot(data=.,aes(date,ndvi))+
  geom_line()+
  geom_line(aes(date,ndvi_deriv),col='red')+
  facet_wrap(~id)
all_out[[1]][id%in%sample.int(3400,10)] %>% 
  ggplot(data=.,aes(date,precip))+
  geom_line()+
  geom_line(aes(date,precip_deriv),col='red')+
  facet_wrap(~id)

all_out[[1]][id%in%sample.int(3400,10)] %>% 
  ggplot(data=.,aes(precip_deriv,ndvi))+
  geom_point()+
  facet_wrap(~id)

sg_derivs(dat[id==3],'ndvi','precip')[]

# dat2 <- dat %>% 
#   .[order(id,date)] %>% 
#   split(by=c("id"),sorted=T) %>% 
#   future_map(~sg_deriv(.x,'ndvi')) %>% 
#   rbindlist()



plan(cluster, workers=n_cores)
all_out <- list()
for(i in start_split_idx:n_splits){
  print(paste("starting split ",i));
  gc(full=T)
  system.time(all_out[[i]] <- sdat[proc_id%in%l_ids[[i]]] %>%
                split(f=list(.$id,.$year,.$month),
                  drop=T) %>%
                future_map(~paf_parab(.x),
                  .progress = F,
                  .options = furrr_options(seed=333L)) %>%
    rbindlist(fill = T)
)

    print(paste("writing split ",i));
  arrow::write_parquet(all_out[[i]], sink=paste0("../data_general/proc_sif-optim/mod_fits_max_sif/",
  "lc-",lc_class,"-max-sif-fits_split-",i,"_",Sys.Date(),".parquet"),
  compression = 'snappy')
  
}


test %>% 
  ggplot(data=.,aes(date,ndvi,group=id))+
  geom_line()+
  geom_line(aes(date,ndvi_deriv),col='red')+
  facet_wrap(~id)

all_out <- sdat %>% 
  split(f = list(.$id,.$year,.$month)) %>% 
  future_map(~fn_pb_mod_gpp(.x), 
    .progress=T, 
    .options=furrr_options(seed=333L))




dat <- dat[order(id,date)][,`:=`(dndvi = sg_deriv(ndvi)),by=.(id)]
dat <- dat[order(id,date)][,`:=`(dprecip = sg_deriv(precip)),by=.(id)]


furrr::future_map_dfr()
tmp3 <- tmp3[order(id,date)][,`:=`(dndvi = sg_deriv(ndvi)),by=.(id)]
tmp3 <- tmp3[order(id,date)][,`:=`(dprecip = sg_deriv(precip)),by=.(id)]

tmp3 %>% 
  ggplot(data=.,aes(dprecip,dndvi,color=hydro_year))+
  geom_point()+
  geom_path()+
  scale_color_viridis_c()+
  facet_wrap(~id)


sg_deriv(c(rnorm(100),NA,rnorm(100))) %>% plot

dat[id==1][order(date)]$precip %>% plot(type='l')
sg_deriv(dat[id==1][order(date)]$precip) %>% lines(col='red')

dat[id==1][order(date)]$ndvi
dat[id==1][order(date)]$ndvi %>% plot(type='l')
sg_deriv(dat[id==1][order(date)]$precip) %>% lines(col='red')




tmp1 <- dat[dat[,.I[ndvi==max(ndvi,na.rm=T)],by=.(xc,yc,hydro_year)]$V1]
tmp2 <- tmp1[,.(mn = mode(month)),by=.(xc,yc)]



junk1 <- dat[dat[,.I[precip==max(precip,na.rm=T)],by=.(xc,yc,hydro_year)]$V1]
junk2 <- junk1[,.(mp = mode(month)),by=.(xc,yc)]

p_mn <- tmp2 %>% 
  ggplot(data=.,aes(xc,yc,fill=mn))+
  geom_tile()+
  coord_sf()+
  scico::scale_fill_scico(palette='batlow')
p_mp <- junk2 %>% 
  ggplot(data=.,aes(xc,yc,fill=mp))+
  geom_tile()+
  coord_sf()+
  scico::scale_fill_scico(palette='batlow')

merge(tmp2,junk2,by=c("xc","yc")) %>% 
  ggplot(data=.,aes(xc,yc,fill=mn-mp))+
  geom_tile()+
  coord_sf()+
  scico::scale_fill_scico(palette='batlow')

p_mn|p_mp

`_DT1`[`_DT1`[, .I[ndvi == max(ndvi, na.rm = TRUE)], by = .(hydro_year)]$V1]

# library(dtplyr)
# 
# dat %>% 
#   lazy_dt() %>% 
#   group_by(hydro_year) %>% 
#   filter(ndvi == max(ndvi,na.rm=T)) %>% 
#   ungroup() %>% 
#   show_query()
# 
# 
# 


