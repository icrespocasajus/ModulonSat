
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





# Plot features
# Libraries
library(stringr)
library(pheatmap)
library(ggpubr)
library(operators)

modulons = modulons.TILs
network = network.TILs
regulons = split(network$Target,network$Source)

regulons.big = regulons[modulons[['3']]]


# Regulon redundancy
feature = 'Redundancy'
#feature = 'Overlap'

feature.df = as.data.frame(matrix(NA,length(names(regulons.big)),length(names(regulons.big))))
rownames(feature.df)=  str_sort( names(regulons.big),numeric = T)
colnames(feature.df)= str_sort( names(regulons.big),numeric = T)

for(i in 1:nrow(feature.df)){
  row.tmp = rownames(feature.df)[i]
  for(j in 1:ncol(feature.df)){
    col.tmp = colnames(feature.df)[j]
    if(feature == 'Redundancy'){feature.df[row.tmp,col.tmp]=redundancy(regulons.big[[row.tmp]],regulons.big[[col.tmp]])}
    if(feature == 'Similarity'){feature.df[row.tmp,col.tmp]=jaccard(regulons.big[[row.tmp]],regulons.big[[col.tmp]])}
    if(feature == 'Overlap'){feature.df[row.tmp,col.tmp]=overlap(regulons.big[[row.tmp]],regulons.big[[col.tmp]])}
  }
  
}
feature.df=as.matrix(feature.df)
diag(feature.df)=NA




name.tmp = 'Modulon.3.TILs'
phm=pheatmap::pheatmap(
  feature.df,
  main = paste(feature,name.tmp,sep = ' '),
  display_numbers = F,
  cluster_rows = T,
  cluster_cols = T,
  fontsize_row = 8,
  show_rownames = T,
  cellwidth = 8,
  cellheight = 8,
  fontsize_col = 8,
  scale='none'
)


# Feature plot with annotation
modulon = '3'
cc=cc.TILs
regulatory.core = Modulon.Cores.TILs

query = modulons[[modulon]]

for(i in 1:length(query)){
  for(j in 1:nrow(regulatory.core.df)){
    modulon.tmp = regulatory.core
  }  
  query[i] %in% cc[[]]
}


annotation = c()
for(i in 1:length(names(cc))){
  modulon.tmp = names(cc)[i]
  for(j in 1:length(names(cc[[modulon.tmp]]))){
    cc.tmp = names(cc[[modulon.tmp]])[j]
    for(k in 1:length(cc[[modulon.tmp]][[cc.tmp]])){
      TF.tmp = cc[[modulon.tmp]][[cc.tmp]][k]
      annotation = c(annotation,paste(modulon.tmp,cc.tmp,TF.tmp,sep = '__'))
    }
  } 
}  
annotation.df = data.frame(Modulon=ModulonCore::strsplit2(annotation,'__')[,1],
                           cc=ModulonCore::strsplit2(annotation,'__')[,2],
                           TF = ModulonCore::strsplit2(annotation,'__')[,3])

rownames(annotation.df)=annotation.df$TF

annotation.df$id = paste(annotation.df$Modulon,annotation.df$cc,sep = '__')
annotation.df$Regulatory.Core = ifelse(annotation.df$id %in% regulatory.core,'Yes','Not')
annotation.df$Regulatory.Core.Annotation = annotation.df$cc
annotation.df$Regulatory.Core.Annotation[annotation.df$id %!in% regulatory.core]=NA

tab.tmp = table(annotation.df$id) > 1
true.cc = names(tab.tmp)[tab.tmp ]
annotation.df[annotation.df$id %!in% true.cc,'cc']=NA


#annotation.c = data.frame(Regulatory.Core = annotation.df[rownames(feature.df),'Regulatory.Core.Annotation'])
#annotation.c = data.frame(Regulatory.Core = annotation.df[rownames(feature.df),'cc'])

annotation.c = data.frame(Regulatory.Core = annotation.df[rownames(feature.df),'Regulatory.Core'],Connected.Component=annotation.df[rownames(feature.df),'cc'],Modulon.Membership=res[rownames(feature.df),'RegCore.PC1'])
rownames(annotation.c)=rownames(feature.df)

#annotation.r = data.frame(Regulatory.Core = annotation.df[rownames(feature.df),'Regulatory.Core'],Membership=res[rownames(feature.df),'RegCore.PC1'])

annotation.r = data.frame(Regulatory.Core = annotation.df[rownames(feature.df),'Regulatory.Core'],Connected.Component=annotation.df[rownames(feature.df),'cc'])
rownames(annotation.r)=rownames(feature.df)


ann_colors = list(
  Modulon.Membership = c("white", "firebrick"),
  Regulatory.Core = c(Yes = 'black', Not='white')
)


name.tmp = 'Modulon.3.TILs'
phm.input = feature.df[order(annotation.c$Connected.Component,decreasing = T),order(annotation.c$Connected.Component,decreasing = T)]

generate.gaps = function(data,col){
  character.tmp = data[,col,drop=T]
  gaps = c()
  for(i in 1:length(character.tmp)){
    if(!(is.na(character.tmp[i]))&!(is.na(character.tmp[i+1]))&!(character.tmp[i] == character.tmp[i+1])){gaps = c(gaps,i)}
  }
  for(i in 1:length(character.tmp)){
    if(!(is.na(character.tmp[i]))&(is.na(character.tmp[i+1]))){gaps = c(gaps,i)}
  }
  return(gaps)
}


gaps = generate.gaps(data=annotation.c[rownames(phm.input),],col='Connected.Component')

#phm.input[upper.tri(phm.input)] = NA

phm.annotated=pheatmap::pheatmap(
  phm.input ,
  main = paste(feature,name.tmp,sep = ' '),
  display_numbers = F,
  cluster_rows = F,
  cluster_cols = F,
  annotation_col = annotation.c,
  annotation_row = annotation.r,
  annotation_color = ann_colors,
  fontsize_row = 8,
  show_rownames = T,
  cellwidth = 8,
  cellheight = 8,
  fontsize_col = 8,
  scale='none'
)

pdf(file="~/Desktop/rem.pdf",height = 12,width = 12)
phm.annotated
dev.off()


modulon.query = '3'
regulatory.core.query = 'cc.3'

# Only show the query regulatory core


# Regulatory Core Heatmap
annotation.c.core = data.frame(
  Regulatory.Core = annotation.df[rownames(feature.df),'Regulatory.Core'],
  Regulatory.Core.Satellite = ifelse(rownames(feature.df) %in% satellites.filtered[[paste(modulon.query,regulatory.core.query,sep = '__')]],'Yes','Not'),
  Connected.Component=annotation.df[rownames(feature.df),'cc'],
  Regulatory.Core.Membership =res.core[rownames(feature.df),'RegCore.PC1'],
  Modulon.Membership=res[rownames(feature.df),'RegCore.PC1'])

rownames(annotation.c.core)=rownames(feature.df)



annotation.r.core = data.frame(
  Regulatory.Core = annotation.df[rownames(feature.df),'Regulatory.Core'],
  Regulatory.Core.Satellite = ifelse(rownames(feature.df) %in% satellites.filtered[[paste(modulon.query,regulatory.core.query,sep = '__')]],'Yes','Not'),
  Connected.Component=annotation.df[rownames(feature.df),'cc'])

rownames(annotation.r.core)=rownames(feature.df)




ann_colors.core = list(
  Modulon.Membership = c("white", "darkgreen"),
  Regulatory.Core.Membership = c("white", "firebrick"),
  Regulatory.Core = c(Yes = 'black', Not='white'),
  Regulatory.Core.Satellite = c(Yes = 'red', Not='white')
)


phm.annotated.core=pheatmap::pheatmap(
  phm.input ,
  main = paste(feature,name.tmp,sep = ' '),
  display_numbers = F,
  cluster_rows = F,
  cluster_cols = F,
  annotation_col = annotation.c.core,
  annotation_row = annotation.r.core,
  annotation_color = ann_colors.core,
  fontsize_row = 8,
  show_rownames = T,
  cellwidth = 8,
  cellheight = 8,
  fontsize_col = 8,
  scale='none'
)


# Regulatory Core Heatmap 2
DA.data = DA.TILs
DA.comparison = "CD8_Tex_Vs_background"

annotation.c.core = data.frame(
  Regulatory.Core = annotation.df[rownames(feature.df),'Regulatory.Core'],
  Regulatory.Core.Satellite = ifelse(rownames(feature.df) %in% satellites.filtered[[paste(modulon.query,regulatory.core.query,sep = '__')]],'Yes','Not'),
  Connected.Component=annotation.df[rownames(feature.df),'cc'],
  Discriminant.Power = (DA.data[[DA.comparison]][rownames(feature.df),'Weights']), 
  Regulatory.Core.Membership =res.core[rownames(feature.df),'RegCore.PC1'],
  Modulon.Membership=res[rownames(feature.df),'RegCore.PC1'])

rownames(annotation.c.core)=rownames(feature.df)



annotation.r.core = data.frame(
  Regulatory.Core = annotation.df[rownames(feature.df),'Regulatory.Core'],
  Regulatory.Core.Satellite = ifelse(rownames(feature.df) %in% satellites.filtered[[paste(modulon.query,regulatory.core.query,sep = '__')]],'Yes','Not'),
  Connected.Component=annotation.df[rownames(feature.df),'cc'])

rownames(annotation.r.core)=rownames(feature.df)


ann_colors.core = list(
  Modulon.Membership = c("white", "darkgreen"),
  Discriminant.Power = c("white", "black"),
  Regulatory.Core.Membership = c("white", "firebrick"),
  Regulatory.Core = c(Yes = 'black', Not='white'),
  Regulatory.Core.Satellite = c(Yes = 'red', Not='white')
)


phm.annotated.core=pheatmap::pheatmap(
  phm.input ,
  main = paste(feature,name.tmp,sep = ' '),
  gaps_row = gaps,
  gaps_col = gaps,
  display_numbers = F,
  cluster_rows = F,
  cluster_cols = F,
  annotation_col = annotation.c.core,
  annotation_row = annotation.r.core,
  annotation_color = ann_colors.core,
  fontsize_row = 8,
  show_rownames = T,
  cellwidth = 8,
  cellheight = 8,
  fontsize_col = 8,
  scale='none'
)








RegAUC = RegAUC.TILs

Core.Membership.manual = function(data,mod,core){
  prcomp.res = prcomp(data[,core],center = T,scale. = T,rank. = 1)
  prcomp.res.df = as.data.frame(prcomp.res[["x"]])
  cor.results = as.data.frame(abs(t(cor(x = prcomp.res.df[rownames(data),'PC1'],y=data[rownames(data),mod],method = 'spearman'))))
  colnames(cor.results)='R.PC1'
  cor.results = cor.results[order(cor.results[,'R.PC1'],decreasing = T),,drop=F]
  return(cor.results)
}

Modulon.Membership = function(data,mod){
  membership.results = lapply(mod,function(mod.tmp){
    prcomp.res = prcomp(data[,mod.tmp],center = T,scale. = T,rank. = 1)
    prcomp.res.df = as.data.frame(prcomp.res[["x"]])
    cor.results = as.data.frame(abs(t(cor(x = prcomp.res.df[rownames(data),'PC1'],y=data[rownames(data),mod.tmp],method = 'spearman'))))
    colnames(cor.results)='R.PC1'
    cor.results = cor.results[order(cor.results[,'R.PC1'],decreasing = T),,drop=F]
    return(cor.results)
  })
  return(membership.results)
}


test = Modulon.Membership(data=RegAUC,mod=modulons)


res.core = Core.Membership.manual(data=RegAUC,mod=modulons[['3']],core=cc[['3']][['cc.3']] )
res = Modulon.Membership(data=RegAUC,mod=modulons[['3']],core=modulons[['3']])




library(ComplexHeatmap)
phm.annotated + plot(phm.annotated)