function [dat genParams] = runWriteTask(write)
% simulate 20 subjects on task. learning rate sampled from
% beta(2,5) (mean=0.2857). softmax temp sampled from gamma(2,2)
% (mean=4).

if nargin == 0
    write = false;
end

numSubs = 20;

alph = betarnd(2, 5, numSubs, 1);
iTemp = gamrnd(2, 2, numSubs, 1);
genParams = [alph iTemp];

dat = [];
for i = 1:numSubs
    out = generativeTD(i, alph(i), iTemp(i));
    dat = [dat; out];
end



if write
    filename = 'datTD.csv';
    dlmwrite(filename, dat, 'delimiter', ',');
    filename = 'genParams.csv';
    dlmwrite(filename, genParams, 'delimiter', ',');    
end