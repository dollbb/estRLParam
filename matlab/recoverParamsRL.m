% recoverParams estimates parameters from 20 simulated subjects
% (model = Temporal Difference learning) playing a restless bandit task, with
% individual generative parameters sampled from the same
% distributions (see runWriteTask).
% User enters estimation technique: Maximum Likelihood (ML) or Maximum A
% Posteriori (MAP) estimation. Estimated parameters written to file
% (compare to generative params per simulated subject: genParams.csv)


generate = input('generate new choice data?\n enter Y or N\n', 's');
generate = upper(generate);
assert(strcmp(generate, 'Y')||strcmp(generate, 'N'), ['answer must ' ...
                    'be Y or N']);

prior = str2num(input('estimation: enter 1 for ML, 2 for MAP\n', 's'));
assert((prior==1||prior==2), 'answer must be 1 or 2');


model = 'TD';
%number of random initial start points
nStPts = 10;

if prior == 1
    LLE_fun = @LLE_TD;
elseif prior == 2
    LLE_fun = @LLE_Prior;
end

     lower_bnd = [0 -Inf];
     upper_bnd = [1 Inf];
    init_params = [rand(nStPts,1) normrnd(1.5, 1, nStPts,1)];
    

if strcmp(generate, 'Y');
    % data = [sub, trl, choice, rew];
    write = true;
    [dat]= runWriteTask(write);
else
    assert(exist('datTD.csv', 'file')==2, 'no data file!');
    dat = load('datTD.csv');
end

options = optimset(@fmincon); 
options = optimset(options, 'TolX', 1e-06, 'TolFun', 1e-06, ...
                   'MaxFunEvals', 100000, 'LargeScale','off', ...
                   'GradObj','off','derivativecheck', 'off', ...
                   'display','final', 'Algorithm', 'interior-point'); %sqp 

subs = unique(dat(:, 1));

fits =[];
stdevs = [];

for s =  1:length(subs)
    this_sub = subs(s,1);
    disp(['Subject  ' num2str(this_sub)]);
        
    sub_dat = dat(find(dat(:,1)==this_sub),:);
    choice = sub_dat(:,3);
    rew = sub_dat(:,4);
        
    sub_params =[];
    sub_LLEs = [];
    sub_flags = [];

 for reps = 1:nStPts

     [params, LLE, exitflag, out]=fmincon(@(params) LLE_fun(params, ...
                 choice, rew), ...
	         init_params(nStPts,:), [],[],[],[], ...
                 lower_bnd, upper_bnd, [], options);
     
         sub_params=[sub_params; params];
         sub_LLEs=[sub_LLEs; LLE];
	 sub_flags=[sub_flags; exitflag];

 end

        %sub mat
        sub_output = [ones(size(sub_params,1),1)*this_sub sub_params ...
		      sub_LLEs sub_flags];


        %pull best params
        best_lle = min(sub_output(:,end-1));
        best_fit = sub_output(find(sub_output(:,end-1)==best_lle),:);

        if size(best_fit, 1)>1
            best_fit = best_fit(1,:);
        end     
        
        fits = [fits; best_fit];

end


if prior == 1
    est = 'ML';
elseif prior == 2
    est = 'MAP';
end

fits_fname  = strcat('indiv_fits_',num2str(nStPts),'StPts_', est ,'.csv');
dlmwrite(fits_fname, fits)

