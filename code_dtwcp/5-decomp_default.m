% decompose 
% it is necessary to specify the appropriate path and tensor data (AKI/sepsis, which day)


nc_min = 2; 
nc_max = 30; 



for s = 1:10  % all splits
    
    % ------ day 1 ------ %
    fprintf('Loading dataset: day1, set %d\n', s)
    load(sprintf('/data/path/tensors/day1_trtensor_default_split%d.mat', s));

    [day1_featmats, rt] = CPdecomp_multi(trtensor, nc_min, nc_max);
    
    save(sprintf('/resultpath/day1sep_featmats_default_split%d.mat', s), 'day1_featmats');
    save(sprintf('/resultpath/day1sep_runtime_default_split%d.mat', s), 'rt');

    fprintf('day1 (sepsis), split %d complete\n', s)

end


