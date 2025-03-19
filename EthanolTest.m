files = ["TestData/10E-Exp33.txt","TestData/10E-Trial1-Exp36.txt","TestData/10E-Trial3-Exp38.txt","TestData/5E-Trial1-Exp30.txt","TestData/5E-Trial2-Exp31.txt","TestData/5E-Trial3-Exp32.txt","TestData/W-Trial1-Exp26.txt","TestData/W-Trial2-Exp27.txt","TestData/W-Trial3-Exp28.txt"];
files = fliplr(files);
filenicknames = ["10% Ethanol Exp1","10% Ethanol Exp2","10% Ethanol Exp3","5% Ethanol Exp1","5% Ethanol Exp2","5% Ethanol Exp3", "Water Exp1", "Water Exp2", "Water Exp3"];
filenicknames = fliplr(filenicknames);
fileparameters = [10,10,10,5,5,5,0,0,0];
fileparameters = fliplr(fileparameters);
maximumcutoffdiameter = 100;
minzoomdiameter = 0;
maxzoomdiameter = 100;
rfthreshold = 4;
fpfthreshold = 5;

ethanoltest1 = SAWDataComparison(files,filenicknames, fileparameters, maximumcutoffdiameter, minzoomdiameter, maxzoomdiameter, rfthreshold, fpfthreshold)
ethanoltest1.getRFGraph();
ethanoltest1.getFPFGraph();
ethanoltest1.getMMADGraph();
ethanoltest1.AnalyseNebVarsSnapshots();
ethanoltest1.AnalyseDistribution();