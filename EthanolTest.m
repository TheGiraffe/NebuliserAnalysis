%% New code, checking some of that output format
files = ["Data/Water100pc-Trial1-Exp026.txt", "Data/Water100pc-Trial2-Exp027.txt", "Data/Water100pc-Trial3-Exp028.txt", "Data/Ethanol5pc-Trial1-Exp030.txt", "Data/Ethanol5pc-Trial2-Exp031.txt", "Data/Ethanol5pc-Trial3-Exp032.txt"]
filenicknames = ["100% Water Trial 1", "100% Water Trial 2", "100% Water Trial 3", "5% Ethanol Trial 1", "5% Ethanol Trial 2", "5% Ethanol Trial 3"]
fileparameters = [0, 0, 0, 5, 5, 5]
ethanoltest = SAWDataComparison(files, filenicknames, fileparameters, 100, 0, 100, 4, 5)
% Change graph name to be about the range, not "100 um and below"
% Will need to rewrite my code to accomodate having better data. Maybe
% instead have it be snapshots records like from the spraytec software? Or
% just average everything out.

% Also, need to update the refractive index in the Edit Records dialog so
% that the Spraytec algorithm is using the correct refractive index to
% calculate the particle distribution. Use the table of refractive indices
% for ethanol solutions found before.

ethanoltest.getRFGraph();
ethanoltest.getFPFGraph();
ethanoltest.getMMADGraph();
ethanoltest.AnalyseDistribution();
ethanoltest.AnalyseNebVarsSnapshots();


%% Old code
% files = ["TestData/10E-Exp33.txt","TestData/10E-Trial1-Exp36.txt","TestData/10E-Trial3-Exp38.txt","TestData/5E-Trial1-Exp30.txt","TestData/5E-Trial2-Exp31.txt","TestData/5E-Trial3-Exp32.txt","TestData/W-Trial1-Exp26.txt","TestData/W-Trial2-Exp27.txt","TestData/W-Trial3-Exp28.txt"];
% files = fliplr(files);
% filenicknames = ["10% Ethanol Exp1","10% Ethanol Exp2","10% Ethanol Exp3","5% Ethanol Exp1","5% Ethanol Exp2","5% Ethanol Exp3", "Water Exp1", "Water Exp2", "Water Exp3"];
% filenicknames = fliplr(filenicknames);
% fileparameters = [10,10,10,5,5,5,0,0,0];
% fileparameters = fliplr(fileparameters);
% maximumcutoffdiameter = 100;
% minzoomdiameter = 0;
% maxzoomdiameter = 100;
% rfthreshold = 4;
% fpfthreshold = 5;
% 
% ethanoltest1 = SAWDataComparison(files,filenicknames, fileparameters, maximumcutoffdiameter, minzoomdiameter, maxzoomdiameter, rfthreshold, fpfthreshold)
% ethanoltest1.getRFGraph();
% ethanoltest1.getFPFGraph();
% ethanoltest1.getMMADGraph();
% ethanoltest1.AnalyseNebVarsSnapshots();
% ethanoltest1.AnalyseDistribution();