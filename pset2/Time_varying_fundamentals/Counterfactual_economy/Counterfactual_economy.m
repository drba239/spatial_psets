
%Trade and Labor Market Dynamics: "General Equilibrium Analysis of the China
%Trade Shock, by Caliendo, Dvorkin, and Parro (Econometrica)"

%This matlab file implements the algorithm described in Appendix 4 to solve
%for the counterfactual economy

% Initialize environment
clear all
close all
clc;

tic

% Parameters
R = 50; %number of U.S. states
J = 22; %number of sectors
RJ1 = R*(J + 1); %number of U.S. labor markets (including non-employment)
time = 200;  %time horizon
beta = 0.99; %discount factor

 %loading the China shock   
As = ones(J, 87, time); % this matrix will store the changes in productivities in china
china=[25.6097
7.6093
5.2773
9.2464
7.6897
45.7781
5.9298
5.6533
64.5661
38.9064
196.6509
3.5324];
china=china.^(1/28);
china(:, :, 1) = china; 
for t = 1:28
As(1:12, 57, t) = china;
end

% Initial Conditions
%load('Counterfactual_economy.mat','V'); %These are the equilibrium values in the counterfactual economy. Code converge in one iteration if used as initial conditions.
%V=V; %Notice that in the code V is defined as the 1/nu times the exponential of the relative time differences in the values as defined in Scetion 3 of the paper 
V=ones(1150,200);

%Loading inputs from the baseline economy
path(path,'../Baseline_economy')
load('Baseline_economy.mat','series_wages','series_mu','L0_initial','series_Ljnhat','series_pi','series_xbilat');
L0=L0_initial; %initial distribution of employment and non-employment
Din_baseline=series_pi; %bilateral trade shares in the baseline economy
wages0=series_wages; %changes in wages in the baseline economy 
Ljn_hat0_usa=series_Ljnhat(:,:,:); %changes in labor allocations across U.S. labor markets  
Ljn_hat0=ones(J,87,time); %creating changes in labor allocations in all regions (U.S. states and other countries)
Ljn_hat0(:,1:R,:)=Ljn_hat0_usa; %the first 50 columns are the U.S. states

mu_baseline=series_mu; %gross flows from the baseline economy
mu00=mu_baseline(:,:,1); %initial mobility matrix

toldyn = 0.001;     %tolerance
max_iter = 1E+20;
Ymax = 1;
iter = 1;

%Algorithm to compute the counterfactual economy

while (iter <= max_iter) && (Ymax > toldyn)
    
    %reformatting the vector of changes in values
    for t = 1:time
        YY(:,:,t) = reshape(V(:, t), J+1, R);
        Yaux0(:, :, t) = reshape(YY(:, :, t), RJ1, 1);
    end
    Yaux = permute(Yaux0, [2, 1, 3]);
    Yaux1 = zeros(RJ1, RJ1, time);
    for t = 1:time
        Yaux1(:, :, t) = repmat(Yaux(:, :, t), RJ1, 1);
    end
    
    %% Step 2 in algorithm described in Appendix 3 Part II (create path of migration matrices) 
    % Calculate mu1_tilde that refers the mobility matrix vartheta in the
    % appendix that has information on the jump in values
    mu1_tilde = mu_baseline(:, :, 2).*(Yaux1(:, :, 2).^beta);
    mu = zeros(RJ1, RJ1, time);
    mu(:, :, 1) = mu1_tilde;
    
    % Computing the mobility matrix at t=1
    num = mu(:, :, 1).*(Yaux1(:, :, 3).^beta);
    num(isnan(num)) = 0;
    den = sum(num')';
    den = den*ones(1, RJ1);
    mu(:, :, 2) = num./den;
    
    % And now for all the other time periods
    for t = 2:time-2
        num = (mu_baseline(:, :, t+1)./mu_baseline(:, :, t)).*mu(:, :, t).*...
            (Yaux1(:, :, t+2).^beta);
        num(isnan(num)) = 0;
        den = sum(num')';
        den = den*ones(1, RJ1);
        mu(:, :, t+1) = num./den;
    end
    
    %% Step 3 in algorithm described in Appendix 3 Part II (create path of labor) 
    L00 = reshape(L0, J+1, R); %reshaping the initial distribution of employment 
    L00_aux0 = reshape(L00, RJ1, 1);
    L00_aux1 = repmat(L00_aux0, 1, RJ1);
    L00_aux1 = mu00.*L00_aux1;
    L1 = sum(L00_aux1)';
    L1 = reshape(L1, J+1, R);
    
    Ldyn = zeros(J+1, R, time);
    Ldyn(:, :, 2) = L1;
    Ldyn(:, :, 1) = reshape(L0, J+1, R);
    
    for t = 2:time-1
        aux0 = reshape(Ldyn(:, :, t), RJ1,1);
        aux1 = repmat(aux0, 1, RJ1);
        aux1 = mu(:, :, t).*aux1;
        aux2 = sum(aux1)';
        Ldyn(:, :, t+1) = reshape(aux2, J+1, R);
    end
    Ldyn(:, :, time) = 0;
        
    %% Step 4 in in Appendix 3 Part II: TEMPORARY EQUILIBRIUM
    
    %loading the initial allocations
    load('Base_year.mat', 'VARjn00', 'VALjn00', 'Din00', 'xbilat00', 'Sn00',...
        'alphas', 'io', 'T', 'B', 'G', 'gamma', 'J', 'N', 'maxit', 'tol', 'R', 'vfactor');
    
    vfactor = -0.05; %factor used to upgrade wages
    tol = 1E-7; %tolerance
    max_iter = 1E+20;
    
    % initial allocations
    VALjn0 = VALjn00;
    VARjn0 = VARjn00;
    Sn = Sn00;
    pi = Din00;
    xbilat = xbilat00;
    
    
    realwages = ones(J, N, time); %this matrix will store the equilibrium changes in real wagesfrom the temporary equilibrium
    Ltemp = Ldyn(2:23, :, :); %evolution of U.S. employment in the counterfactual equilibrium  
    
    

    
 % Solving the tenmporary equilibrium for each t given the changes in
% labor allocations
    for t = 1:  time-2;
         disp(t);
%         
        % changes in fundamentals
        kappa_hat = ones(N*J, N); % relative change in trade costs
        A_hat = As(:, :, t); % relative change in productivities
        A_hat=1./A_hat; %in the counterfactual the productivities in china are constant
        Snp = 0*Sn; % residual deficit not explained by the global portfolio (zero by construction)
        
        % initialize vectors of factor prices (w,r) and good prices (p)
        om = ones(J, N); % initial guess for factor prices
        
        % Initialize vectors of factor/good prices (w, r, p)
        Ljn_hat = ones(J, N);
        Ljn_hat(:, 1:R) = Ltemp(:, :, t+1)./Ltemp(:, :, t); %changes in labor allocations
        pi_tilde0 = Din_baseline(:, :, t); %trade shares in the baseline economy
        pi_tilde1 = Din_baseline(:, :, t+1); %trade shares in the baseline economy
        w0 = wages0(:, :, t+1); %changes in wages in the baseline economy
        Ljn_hat00 = Ljn_hat0(:, :, t+1); %changes in labor allocations in the baseline economy
        om0 = w0.*(Ljn_hat00.^B);  %changes in factor prices in the baseline economy
        om=om0; %initial condition for the changes in factor prices in the counterfactual economy
        
 
        %% Step 4(a), (b), (c), (d) in Appendix 3 Part II: Solving for the temporary equilibrium using the function solvenew_rev
        [om00, wf00, VARjn00, VALjn00, Phat00, rf00, phat00, Din00, X00,...
            Sn00, xbilat00] = solve_tvf(om, Ljn_hat, VARjn0, VALjn0, pi,...
            Snp, kappa_hat, A_hat, alphas, io, T, B, G, gamma, J, N, max_iter,...
            tol, R, vfactor, pi_tilde1, pi_tilde0, om0);
        
               
        % set new initial conditions
        VALjn0 = VALjn00;
        VARjn0 = VARjn00;
        VALjn0_out(:,:,t) =VALjn00;
        VARjn0_out(:,:,t) =VARjn00; 
        Sn = Sn00;
        pi = Din00;
        xbilat = xbilat00;
        
        % adjust real wages with price index Phat and store 
        realwages(:, :, t+1) = (wf00./w0)./(ones(J, 1)*Phat00);

    end
       
    %reshaping changes in real wages to include non employment in the first
    %column 
    realwagesaux = zeros(J+1, N, time);
    for t = 1:time
        realwagesaux(1, :, t) = 1;
        realwagesaux(2:23, :, t) = realwages(:, :, t);
    end
    
    %creating a matrix of changes in real wages across U.S. labor markets
    rwage = ones(R, J+1, time);    
    for t = 1:time
        rwage(:, :, t) = permute(realwagesaux(:, 1:R, t), [2, 1, 3]);
    end
    
   %% Step 5 in Appendix 3 Part II: Updating the changes in values
    nu = 5.3436;
    
    % updating the change in values in t=1 in the model
    t1 = 2;
    maxdixH1 = 10;
    Y0 = V(:, t1);
    Hvf = Y0'; 
    Hvf2 = repmat(Hvf, RJ1, 1);
    mu1_tilde = mu_baseline(:, :, 2).*(Hvf2.^beta);
    auxshock = rwage(:, :, t1).^(1/nu);
    auxvf4 = auxshock';
    auxw = reshape(auxvf4, RJ1, 1);
    auxvf1 = mu1_tilde.*(Yaux1(:, :, t1+1).^beta);
    Y_aux = sum(auxvf1')';
    Y1 = Y_aux.*auxw;
    Y0 = Y1;
        
    %reshaping real wages to update changes in values for all other t
    rw = rwage.^(1/nu);
    rw = permute(rw, [2, 1, 3]);
    rw_aux = zeros(RJ1, 1, time);
    rw_aux1 = zeros(RJ1, RJ1, time);
    for t = 1:time
        rw_aux(:, :, t) = reshape(rw(:, :, t), RJ1, 1);
        rw_aux1(:, :, t) = repmat(rw_aux(:, :, t), 1, RJ1);
    end
    
    
    lambda = zeros(RJ1, RJ1, time); % this is not productivity, it is a term that group mobility flows and real wages
    for t = 1:time-1
        lambda(:, :, t+1) = (mu_baseline(:, :, t+1)./mu_baseline(:, :, t)).*...
            mu(:, : ,t).*rw_aux1(:, :, t+1);
        lambda(isnan(lambda)) = 0;
    end
    
    %now we update the change in values for all other t
    num = zeros(size(lambda));
    for t = 1:time-1
        num(:, :, t) = lambda(:, :, t).*(Yaux1(:, :, t+1).^beta);
    end
    Y = zeros(RJ1, 1, time);
    Y(:, :, 2) = Y0;
    for t = (t1 + 1):time
        Y(:, :, t) = sum(num(:,:,t)')';
    end    
    Y(:, :, time) = 1;
    
    %% Step 6 in note (store new set of initial conditions)
    % store new values
    Ynew = zeros(RJ1, time);
    for t = 1:time
        Ynew(:, t) = Y(:, :, t);
    end
    Ynew(:, time) = 1;
    V(:, time) = 1;
    
    %% Step 7 in note (check and update guess)    
    % excess function
    Ydiff = zeros(time, 1);
    for t = 2:time
        Ydiff(t, 1) = max(abs(Y(:, t) - V(:, t)));
    end
    Ymax = max(Ydiff) 
    Ymax0 = Ymax;
    
    %update guess
    V = 0.5*(Ynew + V);
    iter = iter + 1;
end
%save('Counterfactual_economy.mat','Ldyn', 'mu', 'rwage', 'V', 'realwages','-v7.3');
toc


