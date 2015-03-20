function [LLE] = LLE_TD(params, choice, rew)
 
lr = params(1);
beta = params(2);

Q = 0.5 * ones(1,2);

smxProb = zeros(length(choice), 2);

for i = 1:length(choice)
    %softmax:
    smxProb(i, :) = exp(beta*Q)./repmat(sum(exp(beta*Q), 2), 1, 2);
    
    %update Qs:
    Q(choice(i)) = Q(choice(i)) + lr * (rew(i) - Q(choice(i)));
    Q(3-choice(i)) = (1-lr) * Q(3-choice(i)); %decay unchosen
end

cp1 = smxProb(choice==1, 1);
cp2 = smxProb(choice==2, 2);
allProbs = [cp1; cp2];
LLE = abs(sum(log(allProbs)));