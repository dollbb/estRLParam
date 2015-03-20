function [payoff] = makeDrifts(write, plotDrifts)    
% make new reward probability drifts with reflecting boundaries of
% 0.25 and 0.75. takes logical arguments for write (overwrite
% existing drift file), and plotDrifts (default = false).

if nargin == 0
    write = false;
    plotDrifts = false;
else
    assert(nargin==2, 'specify both write and plot')
end

    totaltrials = 200;
    dsigma = .025;           % reward drift speed
    payoff = .25 + rand(2,1) * .5;

    for i = 2:totaltrials
        payoff(:,i) = payoff(:,i-1) + randn(2,1) * dsigma;
        payoff(find(payoff(:,i) > .75),i) = 1.5-payoff(find(payoff(:,i) > .75),i);
        payoff(find(payoff(:,i)  < .25),i) = .5-payoff(find(payoff(:,i) < .25),i);
    end

    
    if plotDrifts == true
     figure
     plot(payoff(1,:));
     hold on
     plot(payoff(2,:), 'r');
    end
    
    payoff = payoff';
    if write == true
        dlmwrite('payProbDriftLL.csv', payoff, 'delimiter', ',');
    end
    
    
    
    
    
    