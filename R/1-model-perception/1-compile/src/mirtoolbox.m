pkg_path = '/Users/peter/MATLAB/MIRtoolbox1.6.1');

addpath(genpath(pkg_path));

in_dir = '/Users/peter/Dropbox/Academic/projects/harmony/HarmonyDissonancePaper/input/bowling2018/materials/audio';
out_dir = '/Users/peter/Dropbox/Academic/projects/harmony/HarmonyDissonancePaper/output/mirtoolbox/mirroughness/v1';
out_file = strcat(out_dir, '/results.csv');

fileID = fopen(out_file, 'w');
fprintf(fileID, 'id,seth,seth.min_weighted,vass,inharmonicity\n');

in_files = dir(strcat(in_dir, '/*.wav'));
in_num = size(in_files, 1);

h = waitbar(0,...
    sprintf('0 / %i stimuli analysed', in_num), ...
    'Name','Computing MirToolbox analyses...',...
    'CreateCancelBtn',...
    'setappdata(gcbf,''canceling'',1)');
setappdata(h,'canceling',0)

for i = 1:in_num
    if getappdata(h,'canceling')
        break
    end
    in_name_i = in_files(i).name;
    in_path_i = fullfile(in_dir, in_name_i);
    id_i = strrep(in_name_i, '.wav', '');
    % out_name_i = strrep(in_name_i, '.wav', '.txt');
    % out_path_i = fullfile(out_dir, out_name_i);
    audio = miraudio(in_path_i);
    N = size(mirgetdata(audio), 1);
    sethares = mirgetdata(mirroughness(audio, 'Sethares', 'Frame', N, 'sp'));
    min = mirgetdata(mirroughness(audio, 'Min', 'Frame', N, 'sp'));
    vass = mirgetdata(mirroughness(audio, 'Vassilakis', 'Frame', N, 'sp'));
    % doesn't work in MIRtoolbox1.7 but works in MIRtoolbox1.6.1
    inharmonicity = mirgetdata(mirinharmonicity(audio));
    fprintf(fileID, '%s,%f,%f,%f,%f\n', id_i, sethares, min, vass, inharmonicity);
    waitbar(i/in_num, h, sprintf('%i / %i stimuli analysed', i, in_num));
end
delete(h)

fclose(fileID);
