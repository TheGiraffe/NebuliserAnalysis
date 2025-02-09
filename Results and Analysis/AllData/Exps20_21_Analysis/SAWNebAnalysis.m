classdef SAWNebAnalysis
    properties
        filename
        maxdiameter
        numvariables = 69;
        filedata
        categories
        trials
        trialavgs
        trialnlist
        plot
        mmad
        rf
        fpf
    end
    methods
        function obj = SAWNebAnalysis(fn, md)
            if nargin == 2
                obj.filename = fn;
                obj.maxdiameter = md;
                obj.filedata = obj.importFile();
                [obj.categories, obj.trials, obj.trialavgs, obj.trialnlist] = obj.getTrials();
                obj.plot = obj.plotAnalyse();
            end
        end
        function [categories,trials,trialavgs,trialnlist] = getTrials(obj, maxdiameter)
            arguments
                obj
                maxdiameter = obj.maxdiameter;
            end
            rounding_val = 2;
            filedata = obj.filedata;
            numvariables = obj.numvariables;
            endsample = height(filedata);
            trials = [];
            trialavgs = [];
            firststart = false;
            start = false;
            maxbinfound = false;
            trialn = 0;
            trialnlist = [];
            for tr=1:1:endsample
                if filedata{tr, filedata.Properties.VariableNames(10)} > 0
                    if start == false && firststart == true
                        start_tr = tr;
                        trialavg = [];
                        start = true;
                    end
                    if firststart == false
                        firststart = true;
                        for i=10:1:numvariables
                            currentbinvalue = filedata{tr,filedata.Properties.VariableNames(i)};
                            if  currentbinvalue >= maxdiameter && maxbinfound == false
                                if round(currentbinvalue,rounding_val) == maxdiameter
                                    enddiameter_bin = i;
                                else
                                    enddiameter_bin = i-1;
                                end
                                categories = categorical(round(filedata{tr,filedata.Properties.VariableNames(10:enddiameter_bin)},rounding_val));
                                maxbinfound = true;
                                break
                            end
                        end
                    end
                else
                    if start == true
                        end_tr = tr;
                        trial = filedata{start_tr:end_tr,filedata.Properties.VariableNames(10:enddiameter_bin)};
                        trialn = trialn + 1;
                        trialnlist{end+1} = "Trial "+int2str(trialn)+" Avg";
                        for a=10:1:(enddiameter_bin)
                            trialavg(end+1) = mean(filedata{start_tr:(end_tr-1),filedata.Properties.VariableNames(a)});
                        end
                        trials = [trials; trial];
                        trialavgs = [trialavgs; trialavg];
                        start = false;
                    end
                end
            end
            categories;
            trials;
            trialavgs;
            trialnlist;
        end

        function plt = plotAnalyse(obj, pltsize, pltbgcol, pltfont)
            arguments
                obj
                pltsize = [50,50,800,400];
                pltbgcol = "#a4d6fc";
                pltfont = 'Century Gothic';
            end
            plot_p_size = pltsize; %[50,50,800,400]
            plotbgcolor = pltbgcol; %"#a4d6fc"
            
            umrange_cat = obj.categories;
            umrange_val = obj.trialavgs;
            umrange_val = transpose(umrange_val);

            plt = figure('Position',plot_p_size,'InvertHardcopy','off','Color',plotbgcolor)
            set(gcf,'InvertHardcopy', 'off')
            set(gcf,'color',plotbgcolor);
            bar(umrange_cat, umrange_val)
            xlabel("Droplet Diameter - \mum")
            ylabel("Volume Frequency (%)")
            title(sprintf('Volume Frequency of Droplet Diameters - %s\\mum or below',int2str(obj.maxdiameter)))
            subtitle("As percentages of total volume for each trial.", "FontSize",9)
            grid on
            fontname(gcf,pltfont);
            legend(obj.trialnlist,'FontSize',8,'Location','northeastoutside')
        end

        function filedata = importFile(obj)
            fn = obj.filename;
            % Set up the Import Options and import the data
            numvariables = obj.numvariables;
            opts = delimitedTextImportOptions("NumVariables", numvariables);
            % Specify range and delimiter
            opts.DataLines = [1, Inf];
            opts.Delimiter = "\t";
            % Specify column names and types
            opts.VariableNames = ["DateTime", "Dx10", "Dx50", "Dx90", "Cv", "D43", "D32", "Transmission", "VarName9", "VarName10", "VarName11", "VarName12", "VarName13", "VarName14", "VarName15", "VarName16", "VarName17", "VarName18", "VarName19", "VarName20", "VarName21", "VarName22", "VarName23", "VarName24", "VarName25", "VarName26", "VarName27", "VarName28", "VarName29", "VarName30", "VarName31", "VarName32", "VarName33", "VarName34", "VarName35", "VarName36", "VarName37", "VarName38", "VarName39", "VarName40", "VarName41", "VarName42", "VarName43", "VarName44", "VarName45", "VarName46", "VarName47", "VarName48", "VarName49", "VarName50", "VarName51", "VarName52", "VarName53", "VarName54", "VarName55", "VarName56", "VarName57", "VarName58", "VarName59", "VarName60", "VarName61", "VarName62", "VarName63", "VarName64", "VarName65", "VarName66", "VarName67", "VarName68", "VarName69"];
            opts.VariableTypes = ["string", "double", "double", "double", "double", "double", "double", "double", "categorical", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double"];
            % Specify file level properties
            opts.ExtraColumnsRule = "ignore";
            opts.EmptyLineRule = "read";
            % Specify variable properties
            opts = setvaropts(opts, "DateTime", "WhitespaceRule", "preserve");
            opts = setvaropts(opts, ["DateTime", "VarName9"], "EmptyFieldRule", "auto");
            % Import the data
            filedata = readtable((fn), opts);
            % Clear temporary variables
            clear opts
        end
    end
end