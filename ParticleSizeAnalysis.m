classdef ParticleSizeAnalysis
    properties
        filenames
        filenicknames
        parameters
        mincutoffdiameter
        maxcutoffdiameter
        minzoomdiameter
        maxzoomdiameter
        rf_threshold
        fpf_threshold
        numvariables=69
        filedata
        plots
    end
    methods
        function obj = ParticleSizeAnalysis(filenames, filenicknames, parameters, mincutoffdiameter, maxcutoffdiameter, minzoomdiameter, maxzoomdiameter, rf_threshold, fpf_threshold)
            if nargin == 9
                obj.filenames = filenames;
                obj.filenicknames = filenicknames;
                obj.parameters = parameters;
                obj.mincutoffdiameter = mincutoffdiameter;
                obj.maxcutoffdiameter = maxcutoffdiameter;
                obj.minzoomdiameter = minzoomdiameter;
                obj.maxzoomdiameter = maxzoomdiameter;
                obj.rf_threshold = rf_threshold;
                obj.fpf_threshold = fpf_threshold;
                obj.filedata = obj.importFiles();
            end
        end
        function filedata = importFiles(obj)
            filedata = [];
            % separator_indices = [];
            for filename = obj.filenames
                % Set up the Import Options and import the data
                opts = delimitedTextImportOptions("NumVariables", obj.numvariables);
                % Specify range and delimiter
                opts.DataLines = [1, Inf];
                opts.Delimiter = "\t";
                opts.VariableNames = ["DateTime", "Dx10", "Dx50", "Dx90", "Cv", "D43", "D32", "Transmission", "ParticleSize1", "ParticleSize2", "ParticleSize3", "ParticleSize4", "ParticleSize5", "ParticleSize6", "ParticleSize7", "ParticleSize8", "ParticleSize9", "ParticleSize10", "ParticleSize11", "ParticleSize12", "ParticleSize13", "ParticleSize14", "ParticleSize15", "ParticleSize16", "ParticleSize17", "ParticleSize18", "ParticleSize19", "ParticleSize20", "ParticleSize21", "ParticleSize22", "ParticleSize23", "ParticleSize24", "ParticleSize25", "ParticleSize26", "ParticleSize27", "ParticleSize28", "ParticleSize29", "ParticleSize30", "ParticleSize31", "ParticleSize32", "ParticleSize33", "ParticleSize34", "ParticleSize35", "ParticleSize36", "ParticleSize37", "ParticleSize38", "ParticleSize39", "ParticleSize40", "ParticleSize41", "ParticleSize42", "ParticleSize43", "ParticleSize44", "ParticleSize45", "ParticleSize46", "ParticleSize47", "ParticleSize48", "ParticleSize49", "ParticleSize50", "ParticleSize51", "ParticleSize52", "ParticleSize53", "ParticleSize54", "ParticleSize55", "ParticleSize56", "ParticleSize57", "ParticleSize58", "ParticleSize59", "ParticleSize60", "ParticleSize61"];
                opts.VariableTypes = ["string", "double", "double", "double", "double", "double", "double", "double", "categorical", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double"];
                % Specify file level properties
                opts.ExtraColumnsRule = "ignore";
                opts.EmptyLineRule = "read";
                % Specify variable properties
                opts = setvaropts(opts, "DateTime", "WhitespaceRule", "preserve");
                opts = setvaropts(opts, ["DateTime", "ParticleSize1"], "EmptyFieldRule", "auto");
                % Import the data
                table_from_file = readtable((filename), opts);
                separator_indices = transpose(find(strcmp([table_from_file{:,1}], "Date-Time")))
                filedata = [filedata; table_from_file];
                % Clear temporary variables
                clear opts
            end
            % separator_indices
        end

    end
end