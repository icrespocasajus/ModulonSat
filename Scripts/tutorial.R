library(ModulonSat)
library(corrplot)
library(ggplot2)

# Modulon target analysis
mod.query = '3'
results.target.analysis.modulon=target.analysis.modulon(net=network.TILs,mod=modulons.TILs,mod.query = mod.query)
target.analysis.modulon.plot(data=results.target.analysis.modulon,feature = 'Redundancy')

pdf(file=paste('./Modulon',mod.query,feature,'.pdf',sep = "_"),height = 12,width = 12)
target.analysis.modulon.plot(data=results.target.analysis.modulon,feature = 'Redundancy')
dev.off()


# Modulon target analysis wrt cc
mod.query = '3'
cc.query = 'cc.3'
results.target.analysis.modulon.wrt.cc = target.analysis.modulon.wrt.cc.manual.query(net = network.TILs,mod = modulons.TILs,cc = cc.TILs,mod.query = mod.query,cc.query=cc.query)




# All

results.target.analysis.modulon.wrt.cc = target.analysis.modulon.wrt.cc(net = network.TILs,mod = modulons.TILs,cc = cc.TILs)
# Plot wrt cc

satellites = Find.Sat(data = results.target.analysis.modulon.wrt.cc,feature = 'Redundancy',threshold = 0)
satellites.filtered = Filter.Sat(sat.data=satellites,DA.data = DA.TILs,DA=c("Any"),top.percent = 10)
Modulon.Core.Satellites = satellites.filtered[Modulon.Cores.TILs]

# Heatmap with satellites


 HERE!!!!!!!

summary(results.target.analysis.modulon.wrt.cc)

# Modulon target analysis wrt connected components
plots = Modulon.heatmap(
net = network.TILs,
mod = modulons.TILs,
cc = cc.TILs,
regulatory.core = Modulon.Cores.TILs,
feature = 'Overlap',
RegAUC = RegAUC.TILs)
print(plots[['3']])
dev.off()
print(plots[['5']])




res.core = Core.Membership.manual(data=RegAUC,mod=modulons[['3']],core=cc[['3']][['cc.3']] )
res = Modulon.Membership(data=RegAUC,mod=modulons[['3']],core=modulons[['3']])

