filename = 'Exp20-22022023-longestrange40s.txt';

% Set up the Import Options and import the data
numvariables = 69;
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
exp20_longestrange = readtable((filename), opts);

% Clear temporary variables
clear opts
%%
plot_p_size = [50,50,800,400];
plotbgcolor = "#a4d6fc";
set(0,'defaultfigurecolor',plotbgcolor)
figure()
%%
rounding_val = 2;
endsample = 44;
% 44 is maximum

bar_categories = categorical(round(exp20_longestrange{1,exp20_longestrange.Properties.VariableNames(10:numvariables)},rounding_val));
% Try removing categorical at home and see if the graphics are handled
% better.
bar_values = exp20_longestrange{2:endsample,exp20_longestrange.Properties.VariableNames(10:numvariables)};
bar_values = transpose(bar_values);

figure('Position',plot_p_size,'InvertHardcopy','off','Color',plotbgcolor)
set(gcf,'InvertHardcopy', 'off')
set(gcf,'color',plotbgcolor);
bar(bar_categories, bar_values)
xlabel("Droplet Diameter - \mum")
ylabel("Volume Frequency (%)")
title("Volume Frequency of Droplet Diameters")
subtitle("As percentages of total volume for each trial. 42 snapshots total.","FontSize",9)
grid on
fontname(gcf,'Century Gothic');

%%
maxdiameter = 101;

for i=10:1:numvariables
    if exp20_longestrange{1,exp20_longestrange.Properties.VariableNames(i)} > maxdiameter
        enddiameter_bin = i-1
        break
    end
end

umrange_cat = categorical(round(exp20_longestrange{1,exp20_longestrange.Properties.VariableNames(10:enddiameter_bin)},rounding_val));
umrange_val = exp20_longestrange{2:endsample,exp20_longestrange.Properties.VariableNames(10:enddiameter_bin)};
umrange_val = transpose(umrange_val);

figure('Position',plot_p_size,'InvertHardcopy','off','Color',plotbgcolor)
set(gcf,'InvertHardcopy', 'off')
set(gcf,'color',plotbgcolor);
bar(umrange_cat, umrange_val)
xlabel("Droplet Diameter - \mum")
ylabel("Volume Frequency (%)")
title("Volume Frequency of Droplet Diameters - 100\mum or below")
%title("Volume Frequency of Droplet Diameter Ranges below 10\mum")
subtitle("As percentages of total volume for each trial. 42 snapshots total.", "FontSize",9)
grid on
fontname(gcf,'Century Gothic');

total = sum(exp20_longestrange{3,exp20_longestrange.Properties.VariableNames(10:numvariables)})

%%
maxdiameter = 10;

for i=10:1:numvariables
    if exp20_longestrange{1,exp20_longestrange.Properties.VariableNames(i)} > maxdiameter
        enddiameter_bin = i-1
        break
    end
end

umrange_cat = categorical(round(exp20_longestrange{1,exp20_longestrange.Properties.VariableNames(10:enddiameter_bin)},rounding_val));
umrange_val = exp20_longestrange{2:endsample,exp20_longestrange.Properties.VariableNames(10:enddiameter_bin)};
umrange_val = transpose(umrange_val);

figure('Position',plot_p_size,'InvertHardcopy','off','Color',plotbgcolor)
set(gcf,'InvertHardcopy', 'off')
set(gcf,'color',plotbgcolor);
bar(umrange_cat, umrange_val)
xlabel("Droplet Diameter - \mum")
ylabel("Volume Frequency (%)")
title("Volume Frequency of Droplet Diameters below 10\mum")
%title("Volume Frequency of Droplet Diameter Ranges below 10\mum")
subtitle("As percentages of total volume for each trial. 42 snapshots total.", "FontSize",9)
grid on
fontname(gcf,'Century Gothic');

total = sum(exp20_longestrange{3,exp20_longestrange.Properties.VariableNames(10:numvariables)})

%%

bin_size = 2;
bin_size = bin_size-1;
summed_bars = [];
all_summed_bars = [];
for s=2:1:endsample
    summed_bars = [];
    for i=10:(bin_size+1):(enddiameter_bin)
        if (i+bin_size) > enddiameter_bin
            l = enddiameter_bin - i;
            summed_bars(end+1) = exp20_longestrange{s,exp20_longestrange.Properties.VariableNames(i+l)} + exp20_longestrange{s,exp20_longestrange.Properties.VariableNames(i)};
        else
        summed_bars(end+1) = exp20_longestrange{s,exp20_longestrange.Properties.VariableNames(i+bin_size)} + exp20_longestrange{s,exp20_longestrange.Properties.VariableNames(i)};
        end
    end
    all_summed_bars = [all_summed_bars; summed_bars];
end

summed_bars_categories = [""];
for i=10:(bin_size+1):(enddiameter_bin)
    if (i+bin_size) > enddiameter_bin
        l = enddiameter_bin - i;
        a = string(round(exp20_longestrange{1,exp20_longestrange.Properties.VariableNames(i)}, rounding_val))+"-"+string(round(exp20_longestrange{1,exp20_longestrange.Properties.VariableNames(i+l)},rounding_val));
    else
        a = string(round(exp20_longestrange{1,exp20_longestrange.Properties.VariableNames(i)}, rounding_val))+"-"+string(round(exp20_longestrange{1,exp20_longestrange.Properties.VariableNames(i+bin_size)},rounding_val));
    end
    summed_bars_categories(end+1) = a;
end

summed_bars_categories(1) = [];
summed_bars_categories = categorical(summed_bars_categories);

all_summed_bars = transpose(all_summed_bars);

figure('Position',plot_p_size,'InvertHardcopy','off','Color',plotbgcolor)
set(gcf,'InvertHardcopy', 'off')
set(gcf,'color',plotbgcolor);
bar(summed_bars_categories,all_summed_bars)
xlabel("Droplet Diameter - \mum")
ylabel("Volume Frequency (%)")
title("Volume Frequency of Droplet Diameter Ranges below 10\mum")
subtitle("As percentages of total volume for each trial. 42 snapshots total.", "FontSize",9)
grid on
fontname(gcf,'Century Gothic');

