%Trade and Labor Market Dynamics: "General Equilibrium Analysis of the China
%Trade Shock, by Caliendo, Dvorkin, and Parro (Econometrica)"

%This matlab file computes the employment effects of the China shock with
%constant fundamentals and creates the figures displayed in the online
%appendix

close all
clear all
clc
tic

%Creating the industries and states considered in the analysis
labelssect = {...
 'Unemployment ',...
 'Food, Bev., Tob. ',...
 'Textiles ',...
 'Wood, Paper ' ,...
 'Petroleum, Coal ' ,...
 'Chemicals ',...
 'Plastics, Rubber ',...
 'Nonmetallic ' ,...
 'Metal ' ,...
 'Machinery ' ,...
 'Computer, Elect. ' ,...
 'Transport Mfg. ' ,...
 'Furniture Mfg. ' ,...
 'Trade ' ,...
 'Construction ',...
 'Transport Serv., ' ,...
 'Information ',...
 'Finance ' ,...
 'Real Estate ' ,...
 'Education ',...
 'Healthcare ',...
 'Hospitality ' ,...
 'Entert., Prof. '};

labelsstate = { ...
'Alabama ',...
'Alaska ',...
'Arizona ',...
'Arkansas ',...
'California ',...
'Colorado ' ,...
'Connecticut ',...
'Delaware ',...
'Florida ',...
'Georgia ',...
'Hawaii ',...
'Idaho ',...
'Illinois ',...
'Indiana ',...
'Iowa ',...
'Kansas ' ,...
'Kentucky ',...
'Louisiana ',...
'Maine ' ,...
'Maryland ',...
'Massachusetts ',...
'Michigan ',...
'Minnesota ',...
'Mississippi ',...
'Missouri ',...
'Montana ',...
'Nebraska ',...
'Nevada ',...
'New Hampshire ',...
'New Jersey ',...
'New Mexico ',...
'New York ',...
'North Carolina ',...
'North Dakota ',...
'Ohio ',...
'Oklahoma ',...
'Oregon ',...
'Pennsylvania ',...
'Rhode Island ',...
'South Carolina ',...
'South Dakota ',...
'Tennessee ',...
'Texas ',...
'Utah ',...
'Vermont ',...
'Virginia ',...
'Washington ',...
'West Virginia ',...
'Wisconsin ',...
'Wyoming ',...
};

%Loading the baseline economy
path(path,'../../../extensions/constant_fundamentals/Baseline_economy_constant_fundamentals')
load('Baseline_economy_constant_fundamentals.mat', 'Ldyn')

L=Ldyn;

%Loading the counterfactual+economy
path(path,'../Counterfactual_economy_example_2')
load('Counterfactual_economy_constant_fundamentals_tariffs_t2.mat', 'Ldyn')
L_kappa=Ldyn;

%Loading the other counterfactual economy
path(path,'../Counterfactual_economy_example_2')
load('Counterfactual_economy_constant_fundamentals_Example_2.mat', 'Ldyn')

L_kappa2=Ldyn;


% Originally, time was set to 200. We set it to 68, so I adjust.
%Time=199;
Time=67;

%Employment baseline economy
for t=1:Time
emp(:,:,t)=L(2:23,:,t);
share_emp(:,:,t)=emp(:,:,t)./(sum(sum(emp(:,:,t))')*ones(22,50));
manuf(:,:,t)=share_emp(1:12,:,t);
share_manuf(:,:,t)=sum(sum(manuf(:,:,t))');
whole(:,:,t)=share_emp(13, :,t);
share_whole(:,:,t)=sum(sum(whole(:,:,t))');
const(:,:,t)=share_emp(14,:,t);
share_const(:,:,t)=sum(sum(const(:,:,t))');
serv(:,:,t)=share_emp(15:22,:,t);
share_serv(:,:,t)=sum(sum(serv(:,:,t))');
nonmanuf(:,:,t)=share_emp(13:22,:,t);
share_nonmanuf(:,:,t)=sum(sum(nonmanuf(:,:,t))');
unem(:,:,t)=L(1,:,t)./sum(sum(L(:,:,t)')');
unemp(:,:,t)=sum(unem(:,:,t));

manuf_all(:,:,t)=sum(sum((emp(1:12,:,t)./sum(sum(L(:,:,t)')'))'));
whole_all(:,:,t)=sum(sum((emp(13,:,t)./sum(sum(L(:,:,t)')'))'));
const_all(:,:,t)=sum(sum((emp(14,:,t)./sum(sum(L(:,:,t)')'))'));
serv_all(:,:,t)=sum(sum((emp(15:22,:,t)./sum(sum(L(:,:,t)')'))'));
end

%Employment counterfactual economy
for t=1:Time
emp_kappa(:,:,t)=L_kappa(2:23,:,t);
share_emp_kappa(:,:,t)=emp_kappa(:,:,t)./(sum(sum(emp_kappa(:,:,t))')*ones(22,50));
manuf_kappa(:,:,t)=share_emp_kappa(1:12,:,t);
share_manuf_kappa(:,:,t)=sum(sum(manuf_kappa(:,:,t))');
whole_kappa(:,:,t)=share_emp_kappa(13, :,t);
share_whole_kappa(:,:,t)=sum(sum(whole_kappa(:,:,t))');
const_kappa(:,:,t)=share_emp_kappa(14,:,t);
share_const_kappa(:,:,t)=sum(sum(const_kappa(:,:,t))');
serv_kappa(:,:,t)=share_emp_kappa(15:22,:,t);
share_serv_kappa(:,:,t)=sum(sum(serv_kappa(:,:,t))');
unem_kappa(:,:,t)=L_kappa(1,:,t)./sum(sum(L_kappa(:,:,t)')');
unemp_kappa(:,:,t)=sum(unem_kappa(:,:,t));

manuf_kappa_all(:,:,t)=sum(sum((emp_kappa(1:12,:,t)./sum(sum(L_kappa(:,:,t)')'))'));
whole_kappa_all(:,:,t)=sum(sum((emp_kappa(13,:,t)./sum(sum(L_kappa(:,:,t)')'))'));
const_kappa_all(:,:,t)=sum(sum((emp_kappa(14,:,t)./sum(sum(L_kappa(:,:,t)')'))'));
serv_kappa_all(:,:,t)=sum(sum((emp_kappa(15:22,:,t)./sum(sum(L_kappa(:,:,t)')'))'));

end

%Creating figures with the employment effects

%Figure 14: Evolution of employment shares 
disp('Figure 14: Evolution of employment shares (share of total employment)')
ll = 62; ii = 1;
YMatrix1=[100*share_serv(:,ii:ll)' 100*share_serv_kappa(:,ii:ll)'];
YMatrix2=[100*share_const(:,ii:ll)' 100*share_const_kappa(:,ii:ll)'];
YMatrix3=[100*share_whole(:,ii:ll)' 100*share_whole_kappa(:,ii:ll)'];
YMatrix4=[100*share_manuf(:,ii:ll)' 100*share_manuf_kappa(:,ii:ll)'];
fig2_new_tariff(YMatrix1, YMatrix2, YMatrix3, YMatrix4)
load('F2template.mat') %loads the variable 'template'
setprinttemplate(gcf,template) 
print('-dpdf','-r1000','Figure_14_t2.pdf')
close

%Figure 15: The effects of the China shock on employment shares
disp('Figure 15: Effect of employment shares (share of total employment)')
ll = 62; ii = 1;
YMatrix1=-[100*share_serv(:,ii:ll)'-100*share_serv_kappa(:,ii:ll)'  ];
YMatrix2=-[100*share_const(:,ii:ll)'-100*share_const_kappa(:,ii:ll)' ];
YMatrix3=-[100*share_whole(:,ii:ll)'-100*share_whole_kappa(:,ii:ll)'  ];
YMatrix4=-[100*share_manuf(:,ii:ll)'-100*share_manuf_kappa(:,ii:ll)'  ];
fig_diff_tariff(YMatrix1, YMatrix2, YMatrix3, YMatrix4)
load('F2template.mat') %loads the variable 'template'
setprinttemplate(gcf,template) 
print('-dpdf','-r1000','Figure_15_t2.pdf')
close

%Computing the changes in manufacturing employment due to the China shock
L=permute(L,[2,1,3]);
L_kappa=permute(L_kappa,[2,1,3]);
empSS=L(:,2:23,Time);
share_empSS=empSS./sum(sum(empSS)');
manufSS=share_empSS(:, 1:12);
share_manufSS=sum(sum(manufSS)');

empSS_china=L_kappa(:,2:23,Time);
share_empSS_china=empSS_china./sum(sum(empSS_china)');
manufSS_china=share_empSS_china(:, 1:12);
share_manufSS_china=sum(sum(manufSS_china)');

manufactures_share_china_shock=100*(share_manufSS-share_manufSS_china)

%Computing the changes in non-manufacturing employment due to the China shock
empSS=L(:,2:23,Time);
share_empSS=empSS./sum(sum(empSS)');
nonmanufSS=share_empSS(:, 13:22);
share_nonmanufSS=sum(sum(nonmanufSS)');

empSS_china=L_kappa(:,2:23,Time);
share_empSS_china=empSS_china./sum(sum(empSS_china)');
nonmanufSS_china=share_empSS_china(:, 13:22);
share_nonmanufSS_china=sum(sum(nonmanufSS_china)');

non_manufactures_share_china_shock=100*(share_nonmanufSS-share_nonmanufSS_china);


%Computing the sectoral contribution to the change in manufacturing
%employment share due to the China shock (Figure 31)
manuf0=manuf(:,:,2)';
manuf0_j=sum(manuf0)';
manufSS_j=sum(manufSS)';
manufSS_j_china=sum(manufSS_china)';

chg_j=manufSS_j-manuf0_j;
chg_j_china=manufSS_j_china-manuf0_j;

china_effect_j=chg_j_china-chg_j;
sectoralmanuf=100*(china_effect_j)./sum(china_effect_j);


%Computing the sectoral contribution to the change in non-manufacturing
%employment share due to the China shock (Figure 34)
nonmanuf0=nonmanuf(:,:,2)';
nonmanuf0_j=sum(nonmanuf0)';
nonmanufSS_j=sum(nonmanufSS)';
nonmanufSS_j_china=sum(nonmanufSS_china)';

chg_j=nonmanufSS_j-nonmanuf0_j;
chg_j_china=nonmanufSS_j_china-nonmanuf0_j;

china_effect_j=chg_j_china-chg_j;
sectoralnonmanuf=100*(china_effect_j)./sum(china_effect_j);


%Computing the regional contribution to the change in manufacturing
%employment share due to the China shock (Figure 32)
manuf0_n=sum(manuf0')';
manufSS_n=sum(manufSS')';
manufSS_n_china=sum(manufSS_china')';

chg_n=manufSS_n-manuf0_n;
chg_n_china=manufSS_n_china-manuf0_n;

chinamanuf_effect_n=chg_n_china-chg_n;

regionalmanuf=100*(chinamanuf_effect_n)./sum(chinamanuf_effect_n);

%Computing the regional contribution to the change in non-manufacturing
%employment share due to the China shock (Figure 35)
nonmanuf0_n=sum(nonmanuf0')';
nonmanufSS_n=sum(nonmanufSS')';
nonmanufSS_n_china=sum(nonmanufSS_china')';

chg_n=nonmanufSS_n-nonmanuf0_n;
chg_n_china=nonmanufSS_n_china-nonmanuf0_n;

chinanonmanuf_effect_n=chg_n_china-chg_n;

regionalnonmanuf=100*(chinanonmanuf_effect_n)./sum(chinanonmanuf_effect_n);

L0=L(:,:,1);
regpop=sum(L0')';
regpop_manuf = (sum(L0(:,2:13)'))./regpop';
regemp=(sum(L0(:,2:23)'));

%%Computing the regional contribution to the industry change in manufacturing
%employment share due to the China shock normalized by regional employment
%shares (not-displayed in the paper)
aux=100*(share_empSS_china-share_empSS);
maps=100*aux./(ones(50,1)*sum(aux));
initial_emp=L0(:,2:23);
sh_aux=(regemp./sum(regemp))';
sh=sh_aux*ones(1,22);
maps_norm=(maps./(100*sh))';


servSS=sum(share_empSS(:,15:22),2);
servSS_china=sum(share_empSS_china(:,15:22),2);
aux2=100*(servSS_china-servSS);
maps=100*aux2./(ones(50,1)*sum(aux2));
initial_emp=sum(L0(:,16:23),2);
sh=(regemp./sum(regemp))';
maps_norm=(maps./(100*sh));

%Figure 16: Manufacturing employment declines (% of the total) due to the
%China trade shock
disp('Figure 16: Sectoral contribution manufactures')
yvector1=sectoralmanuf;
fig3(yvector1)
load('F3template.mat') %loads the variable 'template'
setprinttemplate(gcf,template) 
print('-dpdf','-r1000','Figure_16_t2.pdf')
close

%Figure 17: Regional contribution to U.S. aggregate manufacturing employment
%decline (%)
disp('Figure 17: Regional contribution manufactures')
yvector1=regionalmanuf;
fig4_tariff(yvector1)
load('F4template.mat') %loads the variable 'template'
setprinttemplate(gcf,template) 
print('-dpdf','-r1000','Figure_17_t2.pdf')
close


%% 

%Figure 18: Regional contribution to U.S. agg. mfg. emp. decline normalized by regional emp. share 
disp('Figure 18: Regional contribution manufacturing employment change normalized by employment') 
sh=regemp./sum(regemp);
yvector1=regionalmanuf./(100*sh)'; %normalized by employment
fig4_new(yvector1)
load('F4template.mat') %loads the variable 'template'
setprinttemplate(gcf,template) 
print('-dpdf','-r1000','Figure_18_t2.pdf')
close

%Figure 19: Non-manufacturing employment increases (% of total) due to the
%China trade shock
disp('Figure 19: Sectoral contribution Non-Manufactures')
yvector1=sectoralnonmanuf;
fig3aa(yvector1)
load('F3template.mat') %loads the variable 'template'
setprinttemplate(gcf,template) 
print('-dpdf','-r1000','Figure_19_t2.pdf')
close

%Figure 20: Regional contribution to U.S. aggregate non-manufacturing
%employment increase (%)
disp('Figure 20: Regional Contribution Non_manufactures')
yvector1=regionalnonmanuf;
fig4(yvector1)
load('F4template.mat') %loads the variable 'template'
setprinttemplate(gcf,template) 
print('-dpdf','-r1000','Figure_20_t2.pdf')
close

%%
% NEW STUFF

%%%
% State Share of Total Decline
%%%

%%%%%
% Suppose you want total employment by region i at each time t.
% L is dimension [50 x 22 x Time] after you move unemployment aside, so 
%   summing over the 1st dimension (the 22 industries) gives total emp. 
%   for each region (2nd dim) at each time (3rd dim).  

%Computing the changes in total employment due to the China shock
empSS=L(:,2:23,Time);
share_empSS=empSS./sum(sum(empSS)');
% manufSS=share_empSS(:, 1:12);
% share_manufSS=sum(sum(manufSS)');

empSS_china=L_kappa(:,2:23,Time);
share_empSS_china=empSS_china./sum(sum(empSS_china)');
% manufSS_china=share_empSS_china(:, 1:12);
% share_manufSS_china=sum(sum(manufSS_china)');

emp_share_china_shock=100*(share_empSS-share_empSS_china); 



%Computing the regional contribution to the change in total
%employment share due to the China shock (Figure 32)
emp0=emp(:,:,2)';
emp0_n=sum(emp0')';
empSS_n=sum(empSS')';
empSS_n_china=sum(empSS_china')';
% manuf0=manuf(:,:,2)';
% manuf0_n=sum(manuf0')';
% manufSS_n=sum(manufSS')';
% manufSS_n_china=sum(manufSS_china')';

chg_n=empSS_n-emp0_n;
chg_n_china=empSS_n_china-emp0_n;

china_all_effect_n=chg_n_china-chg_n;
sum_china_all_effect_n =sum(china_all_effect_n);

regionalAll=100*(china_all_effect_n)./sum(china_all_effect_n);
 


% Then plot as before:
disp('Regional contribution - total employment change');
bonus_fig1_tariff(regionalAll);
load('F4template.mat'); 
setprinttemplate(gcf, template);
print('-dpdf','-r1000','Figure_total_employ_share_comparison_t2.pdf');
close;

%%

%%%
% Share of Labor Force in Each State
%%%

%Computing the changes in total employment due to the China shock
employ_share_by_state=sum((share_empSS)');
employ_share_by_state_china=sum((share_empSS_china)');

diff_employ_share=100*(employ_share_by_state_china-employ_share_by_state)';

% share_empSS
% share_empSS_china

% Then plot as before:
disp('Regional contribution - total employment change');
bonus_fig1_tariff(diff_employ_share);
load('F4template.mat'); 
setprinttemplate(gcf, template);
print('-dpdf','-r1000','Figure_total_employ_comparison_t2.pdf');
close;
