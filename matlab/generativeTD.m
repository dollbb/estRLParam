function [out] = generativeTD(sub, alph, iTemp)
% runs restless bandit task on TD model

totaltrials = 200;

payoff = load('payProbDrift.csv');
%use new rew prob drift for every sub:
% [payoff] = makeDrifts();

choice = [];
rewHist = [];
Q = [0.5, 0.5];
Qsamp = Q;

for i = 1:totaltrials
   %smx prob L choice:
   smxL = exp(iTemp*Q(1)) ./ sum(exp(iTemp*Q));
   
   %choose:
   p = rand;
   [~, choice(i)] = max([smxL p]);
   
   % TD:
   %update TD Qs:
   prob = payoff(i, choice(i)); 
   p = rand;
   [~, rew] = max([p prob]);
   rew = rew-1; % make 0s and 1s
   rewHist(i) = rew;
   Q(choice(i)) = Q(choice(i)) + alph * (rew - Q(choice(i)));

end

%trl nums
trl = 1:totaltrials;
%make output:
out = [sub*ones(totaltrials, 1) trl' choice', rewHist'];