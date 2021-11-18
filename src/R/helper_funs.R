
nn_dts <- function(dbig,dsmall){
  dbig_c <- unique(dbig[,.(x,y)])
  dsmall_c <- unique(dsmall[,.(x,y)])
  dbig_c$idx_nn <- nrow(dbig_c)
  dsmall_c$idx_nn <- nrow(dsmall_c)
  
  # returns idx of left dt for right dt
  nn <- RANN::nn2(dsmall_c[,.(x,y)], dbig_c[,.(x,y)])
  dbig_c$idx_nn <- (nn$nn.idx)[,1]
  
  dout <- merge(dbig_c,dsmall_c, 
    by='idx_nn', 
    allow.cartesian=T, 
    suffix=c("_big","_small"))
  return(dout)
}


# dbig <- coords_lai[,.(x,y)]
# dsmall <- coords_vod[,.(x,y)]
# 
# dim(coords_lai)
# nn_dts(coords_lai[,.(x,y)], coords_vod[,.(x,y)])
# 
# RANN::nn2(coords_vod[,.(x,y)], coords_lai[,.(x,y)])
