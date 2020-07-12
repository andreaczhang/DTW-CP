% function to carry out tensor decomposition for desired number of components


function [featMatList, runtime] = CPdecomp_multi(TRtensor, ncomp_min, ncomp_max)
  % placeholder: if from 2 to 15, then have 15-2+1 = 14
  nslots = ncomp_max - ncomp_min + 1; 
  featMatList = cell(nslots, 1);
  runtime = zeros(1, nslots); 
  
  
  for nc = ncomp_min:ncomp_max
      tic; 
      featmat = CPdecomp(TRtensor, nc);
      featMatList{(nc-ncomp_min+1)} = featmat; % it is -1 because we don't do the first comp
      
      runtime(nc) = toc;
      
      fprintf('Component %d complete\n', nc)
  end
end



% this is for the single one component option 

function [featMat] = CPdecomp(TRtensor, ncomp)

    dim = size(TRtensor);
    d1 = dim(1); 
    
    fprintf('Tensor size %d\n', d1)
   % decompose and take the 3rd element as feature matrix
    decomp = cpd(TRtensor, ncomp);  % 
    featMat = decomp{3};
    dimfeat = size(featMat);
    df1 = dimfeat(1); 
    df2 = dimfeat(2);
    
    fprintf('Feature matrix of %d rows ', df1)
    fprintf('and %d columns \n', df2)

end





