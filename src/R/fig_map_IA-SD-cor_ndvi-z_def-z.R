p_def <- na.omit(dat)[,.(rho = cor(ndvi_z,(def-def_u)/def_sd)),by=.(xc,yc,hydro_year)]
p_def %>% 
  .[,.(rho_sd = sd(rho,na.rm=T)),by=.(xc,yc)] %>% 
  ggplot(data=.,aes(xc,yc,fill=rho_sd))+
  geom_tile()+
  scale_fill_viridis_c(
    option='B',
    limits=c(0,0.5), 
    oob=scales::squish)+
  # scale_fill_gradient2(
  #   low=vec_div_clim[1],
  #   mid=vec_div_clim[3],
  #   high=vec_div_clim[5],
  #   limits=c(-0.75,0.75),
  #   oob=scales::squish
  # )+
  coord_sf()+
  labs(x=NULL,y=NULL,
    fill=expression(paste("Interannual SD of ",rho(NDVI[z],def[z]))))+
  # facet_wrap(~hydro_year)+
  guides(fill = guide_colorbar(title.position = 'top'))+
  theme_linedraw()+
  theme(panel.grid = element_blank(), 
    legend.position = 'bottom',
    legend.key.height = unit(0.2,'cm'),
    legend.key.width = unit(1,'cm'))

ggsave(
  filename = 'figures/fig_map_IA-SD-cor_ndvi-z_def-z.png',
  width=15,
  height=15,
  units='cm',
  dpi=350)
