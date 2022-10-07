library(ModulonSat)

net = network.TILs
mod = modulons.TILs
cc = cc.TILs
regulatory.core = Modulon.Cores.TILs
feature = 'Redundancy'
#feature = 'Overlap'
RegAUC = RegAUC.TILs

DA.data = DA.TILs
DA = 'Any'
target.analysis.results=target.analysis(net = network.TILs,mod = modulons.TILs,cc = cc.TILs)
satellites = Find.Sat(target.analysis.results,feature = feature,threshold = 0)
satellites.filtered = Filter.Sat(sat.data=satellites,DA.data = DA.TILs,DA=DA,top.percent = 5)




sat = satellites.filtered


hetmap.sat = Modulon.heatmap.sat(net=network.TILs,mod=modulons.TILs,cc=cc.TILs,regulatory.core=Modulon.Cores.TILs,feature='Redundancy',sat=satellites.filtered,DA.data = DA.TILs,DA='Any',RegAUC = RegAUC.TILs)

corplot.modulon = Modulon.corrplot(net=network.TILs,mod=modulons.TILs,cc=cc.TILs,regulatory.core=Modulon.Cores.TILs,feature='Redundancy',RegAUC = RegAUC.TILs)


print(hetmap.sat[['3__cc.3']])
dev.off()
print(hetmap.sat[['5__cc.1']])


print(corplot.modulon[['3__cc.3']])

library(corrplot)