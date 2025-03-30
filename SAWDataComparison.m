classdef SAWDataComparison
    properties
        filenames
        filenicknames
        parameters
        madc
        midz
        madz
        rft
        fpft
        filedata
        plot
    end
    methods
        function obj = SAWDataComparison(fns, dnn,para, madc, midz, madz, rft, fpft)
            if nargin == 8
                obj.filenames = fns;
                obj.filenicknames = dnn;
                obj.parameters = para;
                obj.madc = madc;
                obj.midz = midz;
                obj.madz = madz;
                obj.rft = rft;
                obj.fpft = fpft;
                obj.filedata = obj.importFiles();
                % obj.plot = obj.plotAnalyse();
            end
        end
        function filedata = importFiles(obj)
            fi_sz = size(obj.filenames);
            fi_sz = fi_sz(2);
            filedata = [];
            for f=1:1:fi_sz
                filedata = [filedata; SAWDataAnalysis(obj.filenames(f),obj.madc,obj.midz,obj.madz,obj.rft,obj.fpft)];
            end
        end

        function distribution = AnalyseDistribution(obj, pltsize, pltbgcol, pltfont)
            arguments
                obj
                pltsize = [50,50,800,400];
                pltbgcol = "#a4d6fc";
                pltfont = 'Century Gothic';
            end
            plot_p_size = pltsize; %[50,50,800,400]
            plotbgcolor = pltbgcol; %"#a4d6fc"
            
            f = 1;
            for i=1:1:length(obj.filedata(f).categories)
                if obj.filedata(f).categories(i) > obj.filedata(f).zoomdiameter_max
                    zoom_enddiameter_bin = i-1;
                    break
                elseif obj.filedata(f).categories(i) == obj.filedata(f).zoomdiameter_max
                    zoom_enddiameter_bin = i;
                    break
                else
                    zoom_enddiameter_bin = i;
                end
            end

            for i=1:1:length(obj.filedata(f).categories)
                if obj.filedata(f).categories(i) >= obj.filedata(f).zoomdiameter_min
                    zoom_startdiameter_bin = i;
                    break
                else
                    zoom_startdiameter_bin = 1;
                end
            end

            umrange_cat = categorical(obj.filedata(f).categories(zoom_startdiameter_bin:zoom_enddiameter_bin));
            fi_sz = size(obj.filedata);
            fi_sz = fi_sz(1);
            umrange_vals = {};
            for f = 1:1:fi_sz
                umrange_val = obj.filedata(f).snapshotavgs(:, zoom_startdiameter_bin:zoom_enddiameter_bin);
                umrange_val = transpose(umrange_val);
                umrange_vals{end+1} = umrange_val;
            end

            plt1 = figure('Position',plot_p_size,'InvertHardcopy','off','Color',plotbgcolor)
            set(gcf,'InvertHardcopy', 'off')
            set(gcf,'color',plotbgcolor);
            default_colors = ["#0072BD","#D95319","#EDB120","#7E2F8E","#77AC30","#4DBEEE","#A2142F"];
            for f = 1:1:fi_sz
                if f < length(default_colors)
                    barcolor = default_colors(f);
                else
                    barcolor = rand(1,3);
                end
                % uniqueparameters = unique(obj.parameters);
                % uncomment above and change below to
                % length(uniqueparameters) if you want the each 
                colormap(cool(length(obj.parameters))) 
                cmap = colormap;
                % col = find(uniqueparameters==obj.parameters(f));
                col = f % Comment this out and 
                plot(umrange_cat, umrange_vals{f}, 'Color', cmap(col,:))
%                 bar(umrange_cat, umrange_vals{f}, 'FaceColor', cmap(col,:))
%                 Data = categorical(umrange_vals{f});
%                 A = countcats(Data);
%                 C = categories(Data);
                %pareto(A,C, 'FaceColor',barcolor)
                hold on
%               bar(categorical(obj.mmad(:,1)),obj.mmad(:,2))
%                 if length(obj.filedata(f).snapshotnlist) == 1
%                     er = errorbar(umrange_cat, umrange_vals(1), zeros(size(obj.filedata(f).snapshotstds(zoom_startdiameter_bin:zoom_enddiameter_bin))), obj.filedata(f).snapshotstds(zoom_startdiameter_bin:zoom_enddiameter_bin));
%                     er.LineStyle = 'none';
%                     er.Color = [0 0 0];
%                 end
            end
            
            xlabel("Droplet Diameter - \mum")
            ylabel("Volume Frequency (%)")
            title(sprintf('Volume Frequency of Droplet Diameters - %s\\mum or below',int2str(obj.filedata(f).cutoffdiameter_max)))
            subtitle("Using Averages of Each Snapshot", "FontSize",9)
            grid on
            fontname(gcf,pltfont);
            snapshotslist = [];
            filenicknameslist = [];
            for f = 1:1:fi_sz 
                snapshotslist = [snapshotslist [obj.filenicknames(f) + " " + obj.filedata(f).snapshotnlist]];
                blanklegendentries = cell(1,(length(obj.filedata(f).snapshotnlist)-1));
                blanklegendentries(:) = {""};
                filenicknameslist = [filenicknameslist [obj.filenicknames(f) + " Snapshots (n=" + length(obj.filedata(f).snapshotnlist) + ")"] blanklegendentries];
            end
            %legend(snapshotslist,'FontSize',7,'Location','northeastoutside')
            legend(filenicknameslist,'FontSize',8,'Location','northeastoutside')
            distribution = plt1;
        end

        function nebvar_t = AnalyseNebVarsSnapshots(obj, pltsize, pltbgcol, pltfont)
            arguments
                obj
                pltsize = [50,50,800,400];
                pltbgcol = "#a4d6fc";
                pltfont = 'Century Gothic';
            end
            plot_p_size = pltsize; %[50,50,800,400]
            plotbgcolor = pltbgcol; %"#a4d6fc"

            plot_p_size(3) = pltsize(3)*4/5;
            plot_p_size(4) = pltsize(3)*3/5;

            rounding_val = 3;

            fi_sz = size(obj.filedata);
            fi_sz = fi_sz(1);

            plt2 = figure('Position',plot_p_size,'InvertHardcopy','off','Color',plotbgcolor)
            
            %sgtitle(obj.filenames, "FontSize",9)
            subplot(3,1,1)
            legendlabels = [];
            for f = 1:1:fi_sz
                plot(obj.filedata(f).rf)
                hold on
                yline(obj.filedata(f).avgrf,'-',['Avg: ',num2str(round(obj.filedata(f).avgrf,rounding_val))],'LabelHorizontalAlignment','center')
                legendlabels = [legendlabels obj.filenicknames(f) '']
            end
            title("RF")
            ylabel("RF")
            legend(legendlabels,'FontSize',8)

            subplot(3,1,2)
            for f = 1:1:fi_sz
                plot(obj.filedata(f).fpf)
                hold on
                yline(obj.filedata(f).avgfpf,'-',['Avg: ',num2str(round(obj.filedata(f).avgfpf,rounding_val))],'LabelHorizontalAlignment','center')
            end
            title("FPF")
            ylabel("FPF")
            legend(legendlabels,'FontSize',8)

            subplot(3,1,3)
            for f = 1:1:fi_sz
                plot(obj.filedata(f).mmad)
                hold on
                yline(obj.filedata(f).avgmmad,'-',['Avg: ',num2str(round(obj.filedata(f).avgmmad,rounding_val))],'LabelHorizontalAlignment','center')
            end
            title("MMAD")
            xlabel("Snapshot Number")
            ylabel("MMAD")
            legend(legendlabels,'FontSize',8)
            nebvar_t = plt2;
        end

        function mmadGraph = getMMADGraph(obj, pltsize, pltbgcol, pltfont)
            arguments
                obj
                pltsize = [50,50,800,400];
                pltbgcol = "#a4d6fc";
                pltfont = 'Century Gothic';
            end
            x = [];
            mmads = [];
            fi_sz = size(obj.filedata);
            fi_sz = fi_sz(1);

            for f = 1:1:fi_sz
                mmads = [mmads obj.filedata(f).mmad];
                for i = 1:1:length(obj.filedata(f).mmad)
                    x(end+1) = obj.parameters(f);
                end
            end
            
            plotbgcolor = pltbgcol; %"#a4d6fc"
            plot_p_size = pltsize;
            plt3 = figure('Position',plot_p_size,'InvertHardcopy','off','Color',plotbgcolor)
            scatter(x,mmads)
            hold on
            mmadline = lsline
            mmadp = polyfit(get(mmadline,'xdata'),get(mmadline,'ydata'),1);
            mmadeq = sprintf("Line: y = "+mmadp(1)+"x + "+mmadp(2))
            text(mean(x),max(mmads),mmadeq)
            title("Mass Median Aerodynamic Diameter (MMAD) versus Ethanol Percentage")
            xlabel("Ethanol (%)")
            ylabel("MMAD (\mum)")
            grid on
            mmadGraph = plt3;
        end
        function fpfGraph = getFPFGraph(obj, pltsize, pltbgcol, pltfont)
            arguments
                obj
                pltsize = [50,50,800,400];
                pltbgcol = "#a4d6fc";
                pltfont = 'Century Gothic';
            end
            x = [];
            fpfs = [];
            fpferrors = [];
            fi_sz = size(obj.filedata);
            fi_sz = fi_sz(1);

            for f = 1:1:fi_sz
                fpfs = [fpfs obj.filedata(f).fpf];
                fpferrors = [fpferrors obj.filedata(f).fpferrors];
                for i = 1:1:length(obj.filedata(f).fpf)
                    x(end+1) = obj.parameters(f);
                end
            end
            plotbgcolor = pltbgcol; %"#a4d6fc"
            plot_p_size = pltsize;
            plt4 = figure('Position',plot_p_size,'InvertHardcopy','off','Color',plotbgcolor)
            scatter(x,fpfs)
            fpfline = lsline
            fpfp = polyfit(get(fpfline,'xdata'),get(fpfline,'ydata'),1);
            fpfeq = sprintf("Line: y = "+fpfp(1)+"x + "+fpfp(2))
            text(mean(x),max(fpfs),fpfeq)
            hold on
            errorbars = errorbar(x,fpfs,fpferrors,'LineStyle','none','Color',"#0072BD");
            title("Fine Particle Fraction (FPF) versus Ethanol Percentage")
            ylabel("FPF")
            xlabel("Ethanol (%)")
            grid on
            fpfGraph = plt4;
        end
        function rfGraph = getRFGraph(obj, pltsize, pltbgcol, pltfont)
            arguments
                obj
                pltsize = [50,50,800,400];
                pltbgcol = "#a4d6fc";
                pltfont = 'Century Gothic';
            end
            x = [];
            rfs = [];
            rferrors = [];
            fi_sz = size(obj.filedata);
            fi_sz = fi_sz(1);

            for f = 1:1:fi_sz
                rfs = [rfs obj.filedata(f).rf];
                rferrors = [rferrors obj.filedata(f).rferrors];
                for i = 1:1:length(obj.filedata(f).rf)
                    x(end+1) = obj.parameters(f);
                end
            end
            plotbgcolor = pltbgcol; %"#a4d6fc"
            plot_p_size = pltsize;
            plt5 = figure('Position',plot_p_size,'InvertHardcopy','off','Color',plotbgcolor)
            scatter(x,rfs)
            rfline = lsline
            rfp = polyfit(get(rfline,'xdata'),get(rfline,'ydata'),1);
            rfeq = sprintf("Line: y = "+rfp(1)+"x + "+rfp(2))
            text(mean(x),max(rfs),rfeq)
            hold on
            errorbars = errorbar(x,rfs,rferrors,'LineStyle','none','Color',"#0072BD");
            title("Respirable Fraction (RF) versus Ethanol Percentage")
            ylabel("RF")
            xlabel("Ethanol (%)")
            grid on
            rfGraph = plt5;
        end
    end
end