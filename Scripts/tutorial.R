
network = network.TILs
modulons = modulons.TILs
cc = cc.TILs
modulon.tmp = '3'
core.tmp = cc[[modulon.tmp]][['cc.3']]

target.analysis.manual.query = function(net,mod,cc,query.mod,query.cc){
  network = net
  modulons = mod
  cc = cc
  modulon.tmp = query.mod
  core.tmp = cc[[modulon.tmp]][[query.cc]]
  
  regulons = split(network$Target,network$Source)
  core.targets.tmp = as.character(unlist(regulons[core.tmp]))
  regulons.subset.core = regulons[core.tmp]
  regulons.subset.no.core = regulons[setdiff(modulons[[modulon.tmp]],core.tmp)]

  no.core.redundancy.wrt.core = lapply(regulons.subset.no.core,function(x){
    tf.targets.tmp = as.character(unlist(x))
    redundancy.tmp = redundancy.wrt(tf.targets.tmp,core.targets.tmp)
    return(redundancy.tmp)
  })
  no.core.redundancy.df = data.frame(TF=names(as.data.frame(no.core.redundancy.wrt.core)),Redundancy = as.numeric(as.character(as.data.frame(no.core.redundancy.wrt.core))))
  rownames(no.core.redundancy.df)=no.core.redundancy.df$TF
  no.core.redundancy.df = no.core.redundancy.df[order(no.core.redundancy.df$Redundancy,decreasing = T),]
  
  no.core.similarity.wrt.core = lapply(regulons.subset.no.core,function(x){
    tf.targets.tmp = as.character(unlist(x))
    redundancy.tmp = jaccard(tf.targets.tmp,core.targets.tmp)
    return(redundancy.tmp)
  })
  no.core.similarity.df = data.frame(TF=names(as.data.frame(no.core.similarity.wrt.core)),Similarity = as.numeric(as.character(as.data.frame(no.core.similarity.wrt.core))))
  rownames(no.core.similarity.df)=no.core.similarity.df$TF
  no.core.similarity.df = no.core.similarity.df[order(no.core.similarity.df$Similarity,decreasing = T),]
  
  no.core.overlap.wrt.core = lapply(regulons.subset.no.core,function(x){
    tf.targets.tmp = as.character(unlist(x))
    overlap.tmp = overlap(tf.targets.tmp,core.targets.tmp)
    return(overlap.tmp)
  })
  no.core.overlap.df = data.frame(TF=names(as.data.frame(no.core.overlap.wrt.core)),Overlap = as.numeric(as.character(as.data.frame(no.core.overlap.wrt.core))))
  rownames(no.core.overlap.df)=no.core.overlap.df$TF
  no.core.overlap.df = no.core.overlap.df[order(no.core.overlap.df$Overlap,decreasing = T),]
  
  target.analysis.results = list()
  target.analysis.results[[paste(query.mod,query.cc,sep = '__')]]=list(Redundancy=no.core.redundancy.df,Similarity = no.core.similarity.df,Overlap=no.core.overlap.df)
  return(target.analysis.results)
}

### Select satellites

Find.Sat = function(data,feature = 'Redundancy',threshold = 0,quant.prob = NULL){
  Satellites = lapply(data,function(x){
    x[[feature]][x[[feature]][feature]>threshold,'TF']
  })
  return(Satellites)
}


Filter.Sat = function(sat.data,DA.data,DA=c('Any'),top.percent=10){
  DA.only.top = lapply(DA.data,function(x){
    y = x[x$Weights > quantile(x$Weights,prob = 1-(top.percent/100)), ,drop=FALSE]
    return(y)
  })
  DA.only.top.names = lapply(DA.data,function(x){
    y = rownames(x)[x$Weights > quantile(x$Weights,prob = 1-(top.percent/100))]
    return(y)
  })
  
  if(DA == 'Any'){
    selection = names(DA.data)
  }else{selection = DA}

  DA.only.top.names.selected = DA.only.top.names[selection]
  
  DA.selection = unique(as.character(unlist(DA.only.top.names.selected)))
  
  Satellites.Filtered = lapply(sat.data,function(x){
    return(intersect(x,DA.selection))
  })
  return(Satellites.Filtered)
}


###
target.analysis.results.out= target.analysis.manual.query.results=target.analysis.manual.query(net = network.TILs,
                    mod = modulons.TILs,
                    cc = cc.TILs,
                    query.mod = '3',
                    query.cc='cc.3'
                    )

satellites = Find.Sat(target.analysis.results.out,feature = 'Redundancy',threshold = 0)

satellites.filtered = Filter.Sat(sat.data=satellites,DA.data = DA.TILs,DA=c("CD8_Tex_Vs_background","CD8_Tpex_Vs_background"),top.percent = 10) 
satellites.filtered = Filter.Sat(sat.data=satellites,DA.data = DA.TILs,c("CD8_Tex_Vs_background"),top.percent = 25) 




# Explore modulon similarity

# Compare redundancy intra- and inter- modulon

# Explore modulon redundancy



# DA

# Similarity






target.analysis = function(net,mod,cc){
  network = net
  modulons = mod
  cc = cc
  regulons = split(network$Target,network$Source)
  target.analysis.results = list()
  for(i in 1:length(names(cc))){
    modulon.tmp = names(cc)[i]
    for(j in 1:length(names(cc[[modulon.tmp]]))){
      query.cc = names(cc[[modulon.tmp]])[j]
      core.tmp = cc[[modulon.tmp]][[query.cc]]
      
      core.targets.tmp = as.character(unlist(regulons[core.tmp]))
      regulons.subset.core = regulons[core.tmp]
      regulons.subset.no.core = regulons[setdiff(modulons[[modulon.tmp]],core.tmp)]
      
      no.core.redundancy.wrt.core = lapply(regulons.subset.no.core,function(x){
        tf.targets.tmp = as.character(unlist(x))
        redundancy.tmp = redundancy.wrt(tf.targets.tmp,core.targets.tmp)
        return(redundancy.tmp)
      })
      no.core.redundancy.df = data.frame(TF=names(as.data.frame(no.core.redundancy.wrt.core)),Redundancy = as.numeric(as.character(as.data.frame(no.core.redundancy.wrt.core))))
      rownames(no.core.redundancy.df)=no.core.redundancy.df$TF
      no.core.redundancy.df = no.core.redundancy.df[order(no.core.redundancy.df$Redundancy,decreasing = T),]
      
      no.core.similarity.wrt.core = lapply(regulons.subset.no.core,function(x){
        tf.targets.tmp = as.character(unlist(x))
        redundancy.tmp = jaccard(tf.targets.tmp,core.targets.tmp)
        return(redundancy.tmp)
      })
      no.core.similarity.df = data.frame(TF=names(as.data.frame(no.core.similarity.wrt.core)),Similarity = as.numeric(as.character(as.data.frame(no.core.similarity.wrt.core))))
      rownames(no.core.similarity.df)=no.core.similarity.df$TF
      no.core.similarity.df = no.core.similarity.df[order(no.core.similarity.df$Similarity,decreasing = T),]
      
      no.core.overlap.wrt.core = lapply(regulons.subset.no.core,function(x){
        tf.targets.tmp = as.character(unlist(x))
        overlap.tmp = overlap(tf.targets.tmp,core.targets.tmp)
        return(overlap.tmp)
      })
      no.core.overlap.df = data.frame(TF=names(as.data.frame(no.core.overlap.wrt.core)),Overlap = as.numeric(as.character(as.data.frame(no.core.overlap.wrt.core))))
      rownames(no.core.overlap.df)=no.core.overlap.df$TF
      no.core.overlap.df = no.core.overlap.df[order(no.core.overlap.df$Overlap,decreasing = T),]
      
      target.analysis.results[[paste(modulon.tmp,query.cc,sep = '__')]]=list(Redundancy=no.core.redundancy.df,Similarity = no.core.similarity.df,Overlap=no.core.overlap.df)
      
    }  
    
  }
  return(target.analysis.results)
}  



target.analysis.results=target.analysis(net = network.TILs,
                                        mod = modulons.TILs,
                                        cc = cc.TILs
)

satellites = Find.Sat(target.analysis.results,feature = 'Redundancy',threshold = 0)

satellites.filtered = Filter.Sat(sat.data=satellites,DA.data = DA.TILs,DA=c("CD8_Tex_Vs_background","CD8_Tpex_Vs_background"),top.percent = 10) 
satellites.filtered = Filter.Sat(sat.data=satellites,DA.data = DA.TILs,c("CD8_Tex_Vs_background"),top.percent = 25) 

  