classdef SAWDataAnalysis
    properties
        filename
        cutoffdiameter_max
        zoomdiameter_min
        zoomdiameter_max
        rf_thresh
        fpf_thresh
        numvariables = 69;
        filedata
        categories
        snapshots
        snapshotavgs
        snapshotstds
        snapshotnlist
        plot
        mmad
        avgmmad
        rf
        rferrors
        avgrf
        fpf
        fpferrors
        avgfpf
    end
    methods
        function obj = SAWDataAnalysis(fn, madc, midz, madz, rft, fpft)
            if nargin == 6
                obj.filename = fn;
                obj.cutoffdiameter_max = madc;
                obj.zoomdiameter_min = midz;
                obj.zoomdiameter_max = madz;
                obj.rf_thresh = rft;
                obj.fpf_thresh = fpft;
                obj.filedata = obj.importFile();
                [obj.categories, obj.snapshots, obj.snapshotavgs, obj.snapshotstds, obj.snapshotnlist] = obj.getSnapshots();
                [obj.mmad, obj.avgmmad] = obj.getMMAD();
                [obj.rf, obj.rferrors, obj.avgrf] = obj.getRF();
                [obj.fpf, obj.fpferrors ,obj.avgfpf] = obj.getFPF();
                %obj.plot = obj.plotAnalyse();
            end
        end

        function [fpfs, fpferrors, avgfpfs] = getFPF(obj)
            arguments
                obj
            end
            fpfs = [];
            fpferrors = [];
            for i=1:1:length(obj.snapshotnlist)
                fpf = 0;
                fpferror = 0;
                for j = 1:1:length(obj.categories)
                    if obj.categories(j)>obj.fpf_thresh
                        break
                    else
                        fpf = fpf + obj.snapshotavgs(i,j);
                        fpferror = fpferror + obj.snapshotstds(i,j);
                    end
                end
                fpf = fpf/100;
                fpferror = fpferror/100;
                fpfs = [fpfs, fpf];
                fpferrors = [fpferrors, fpferror];
            end
            fpfs;
            fpferrors;
            avgfpfs = mean(fpfs);
        end

        function [rfs, rferrors, avgrf] = getRF(obj)
            arguments
                obj
            end
            rfs = [];
            rferrors = [];
            for i=1:1:length(obj.snapshotnlist)
                rf = 0;
                rferror = 0;
                for j = 1:1:length(obj.categories)
                    if obj.categories(j)>obj.rf_thresh
                        break
                    else
                        rf = rf + obj.snapshotavgs(i,j);
                        rferror = rferror + obj.snapshotstds(i,j);
                    end
                end
                rf = rf/100;
                rferror = rferror/100;
                rfs = [rfs, rf];
                rferrors = [rferrors, rferror];
            end
            rfs;
            rferrors;
            avgrf = mean(rfs);
        end

        function [mmads,avgmmads] = getMMAD(obj)
            arguments
                obj
            end
            means = [];
            for i=1:1:length(obj.snapshotnlist)
                num = 0;
                dem = 0;
                for j = 1:1:length(obj.categories)
                    num = num + obj.snapshotavgs(i,j);
                    if num >= 50
                        mid = [obj.categories(1,j)];
                        break
                    end
                    
                end
                means = [means, mid];
            end
            mmads = means;
            avgmmads = mean(mmads);
        end

        function [categories,snapshots,snapshotavgs,snapshotstds,snapshotnlist] = getSnapshots(obj)
            arguments
                obj
            end
            cutoffdiameter_max = obj.cutoffdiameter_max;
            rounding_val = 2;
            filedata = obj.filedata;
            numvariables = obj.numvariables;
            endsample = height(filedata);
            snapshots = [];
            snapshotavgs = [];
            snapshotstds = [];
            firststart = false;
            start = false;
            maxbinfound = false;
            snapshotn = 0;
            snapshotnlist = [];
            for tr=1:1:endsample
                if filedata{tr, filedata.Properties.VariableNames(10)} > 0
                    if start == false && firststart == true
                        start_tr = tr;
                        snapshotavg = [];
                        snapshotstd = [];
                        start = true;
                    end
                    if firststart == false
                        firststart = true;
                        for i=10:1:numvariables
                            currentbinvalue = filedata{tr,filedata.Properties.VariableNames(i)};
                            if  currentbinvalue >= cutoffdiameter_max && maxbinfound == false
                                if round(currentbinvalue,rounding_val) == cutoffdiameter_max
                                    enddiameter_bin = i;
                                else
                                    enddiameter_bin = i-1;
                                end
                                categories = round(filedata{tr,filedata.Properties.VariableNames(10:enddiameter_bin)},rounding_val);
                                maxbinfound = true;
                                break
                            end
                        end
                    end
                else
                    if start == true
                        end_tr = tr;
                        snapshot = filedata{start_tr:end_tr,filedata.Properties.VariableNames(10:enddiameter_bin)};
                        snapshotn = snapshotn + 1;
                        snapshotnlist{end+1} = "Snapshot "+int2str(snapshotn); %+" Avg"
                     
                        for a=10:1:(enddiameter_bin)
                            dta = (filedata{start_tr:(end_tr-1),filedata.Properties.VariableNames(a)});
                            snapshotavg(end+1) = mean(dta);
                            snapshotstd(end+1) = std(dta);
                        end
                        snapshots = [snapshots; snapshot];
                        adjust = 100/sum(snapshotavg(1,1:end));
                        snapshotavg = snapshotavg.*adjust;
                        snapshotstd = snapshotstd.*adjust;
                        snapshotavgs = [snapshotavgs; snapshotavg];
                        sum(snapshotavg(1,1:end));
                        snapshotstds = [snapshotstds; snapshotstd];
                        start = false;
                    end
                end
            end
            categories;
            snapshots;
            snapshotavgs;
            snapshotstds;
            snapshotnlist;
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

            for i=1:1:length(obj.categories)
                if obj.categories(i) > obj.zoomdiameter_max
                    zoom_enddiameter_bin = i-1;
                    break
                elseif obj.categories(i) == obj.zoomdiameter_max
                    zoom_enddiameter_bin = i;
                    break
                else
                    zoom_enddiameter_bin = i;
                end
            end

            for i=1:1:length(obj.categories)
                if obj.categories(i) >= obj.zoomdiameter_min
                    zoom_startdiameter_bin = i;
                    break
                else
                    zoom_startdiameter_bin = 1;
                end
            end
            
            umrange_cat = categorical(obj.categories(zoom_startdiameter_bin:zoom_enddiameter_bin));
            umrange_val = obj.snapshotavgs(:, zoom_startdiameter_bin:zoom_enddiameter_bin);
            umrange_val = transpose(umrange_val);

            plt1 = figure('Position',plot_p_size,'InvertHardcopy','off','Color',plotbgcolor)
            set(gcf,'InvertHardcopy', 'off')
            set(gcf,'color',plotbgcolor);
            bar(umrange_cat, umrange_val)
            hold on
%             bar(categorical(obj.mmad(:,1)),obj.mmad(:,2))
            if length(obj.snapshotnlist) == 1
                er = errorbar(umrange_cat, umrange_val, zeros(size(obj.snapshotstds(zoom_startdiameter_bin:zoom_enddiameter_bin))), obj.snapshotstds(zoom_startdiameter_bin:zoom_enddiameter_bin));
                er.LineStyle = 'none';
                er.Color = [0 0 0];
            end
            
            xlabel("Droplet Diameter - \mum")
            ylabel("Volume Frequency (%)")
            title(sprintf('Volume Frequency of Droplet Diameters - %s\\mum or below',int2str(obj.cutoffdiameter_max)))
            subtitle(obj.filename, "FontSize",9)
            grid on
            fontname(gcf,pltfont);
            legend([obj.snapshotnlist],'FontSize',8,'Location','northeastoutside')
            
            plot_p_size(3) = pltsize(3)*4/5;
            plot_p_size(4) = pltsize(3)*4/5;

            rounding_val = 3;

            plt2 = figure('Position',plot_p_size,'InvertHardcopy','off','Color',plotbgcolor)
            
            sgtitle(obj.filename, "FontSize",9)
            subplot(3,1,1)
            plot(obj.rf)
            hold on
            yline(obj.avgrf,'-',['Avg: ',num2str(round(obj.avgrf,rounding_val))],'LabelHorizontalAlignment','center')
            title("RF")
            ylabel("RF")

            subplot(3,1,2)
            plot(obj.fpf)
            hold on
            yline(obj.avgfpf,'-',['Avg: ',num2str(round(obj.avgfpf,rounding_val))],'LabelHorizontalAlignment','center')
            title("FPF")
            ylabel("FPF")

            subplot(3,1,3)
            plot(obj.mmad)
            hold on
            yline(obj.avgmmad,'-',['Avg: ',num2str(round(obj.avgmmad,rounding_val))],'LabelHorizontalAlignment','center')
            title("MMAD")
            xlabel("Snapshot Number")
            ylabel("MMAD")
            plt = [plt1, plt2]
            
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