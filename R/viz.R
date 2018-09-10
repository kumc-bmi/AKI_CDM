#### visualizations ####
require_libraries("diagram")
consort_diag<-function(consort_tbl){
  tbl<-consort_tbl %>% 
    mutate(cnt_ref=ifelse(CNT_TYPE %in% c("Initial","Total"),ENC_CNT,NA)) %>%
    fill(cnt_ref,.direction="down") %>%
    mutate(ENC_PROP=round(ENC_CNT/cnt_ref,4)) %>%
    mutate(label_val=paste0("(",ENC_CNT,",",ENC_PROP*100,"%)")) %>%
    mutate(label_txt=c("Inpatient visit with LOS > 2\nand of age >= 18",
                       "Has at least 1 SCr record",
                       "Excluded: Initial eGFR below 15",
                       "Excluded: RRT with 48 hours since \nadmission",
                       "Excluded: Has less than 2 \nSCr records",
                       "Excluded: Burn Patients",
                       "Excluded: Pre-existance of \nrenal failure",
                       "Total eligible encounters",
                       "Non-AKI",
                       "AKI1",
                       "AKI2",
                       "AKI1 to AKI2",
                       "AKI3",
                       "AKI2 to AKI3",
                       "AKI1 to AKI2 to AKI3")) %>%
    mutate(label=paste(label_txt,"\n",label_val)) %>%
    mutate(node_id=c(2,5,7,9,10,12,13,17,18,22,23,25,24,26,28))
  
  #prepare canvas
  par(mfrow=c(1,1))
  par(mar=c(0,0,0,0))
  openplotmat()
  
  ##number of elements per row
  elpos<-coordinates(rep(3,10))
  fromto<-matrix(ncol=2,byrow=T,
                 c(2,5,
                   5,8,
                   8,7,
                   8,9,
                   8,11,
                   11,10,
                   11,12,
                   11,14,
                   14,13,
                   14,17,
                   17,18,
                   17,20,
                   20,19,
                   20,21,
                   19,22,
                   20,23,
                   21,24,
                   22,25,
                   23,26,
                   25,28
                   ))
  ##draw arrows
  arrpos <- matrix(ncol = 2, nrow = nrow(fromto))
  for (i in 1:nrow(fromto)){
    arrpos[i, ] <- straightarrow (to = elpos[fromto[i, 2], ],
                                  from = elpos[fromto[i, 1], ],
                                  lwd = 1, arr.pos = 0.6, arr.length = 0.3)
  }
  
  ##draw nodes
  for(i in 1:nrow(tbl)){
    textrect(elpos[tbl$node_id[i],],
             radx=0.15,
             rady=0.05,
             lab=tbl$label[i],
             font=4,
             cex=0.7)
  }
}