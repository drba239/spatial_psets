

function createfigure(yvector1)
%CREATEFIGURE(YVECTOR1)
%  YVECTOR1:  bar yvector

%  Auto-generated by MATLAB on 09-Nov-2016 22:25:26

% Create figure
figure1 = figure('PaperType','<custom>','PaperSize',[8.5 12]);

% Create axes
axes1 = axes('Parent',figure1,'YGrid','on','XGrid','on',...
    'XTickLabel',{'Alabama ','Alaska ','Arizona ','Arkansas ','California ','Colorado ','Connecticut ','Delaware ','Florida ','Georgia ','Hawaii ','Idaho ','Illinois ','Indiana ','Iowa ','Kansas ','Kentucky ','Louisiana ','Maine ','Maryland ','Massachusetts ','Michigan ','Minnesota ','Mississippi ','Missouri ','Montana ','Nebraska ','Nevada ','New Hampshire ','New Jersey ','New Mexico ','New York ','North Carolina ','North Dakota ','Ohio ','Oklahoma ','Oregon ','Pennsylvania ','Rhode Island ','South Carolina ','South Dakota ','Tennessee ','Texas ','Utah ','Vermont ','Virginia ','Washington ','West Virginia ','Wisconsin ','Wyoming '},...
    'XTickLabelRotation',90,...
    'XTick',[1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48 49 50],...
    'GridAlpha',1,...
    'GridLineStyle',':',...
    'Position',[0.255729166666667 0.34409594095941 0.485208333333333 0.529219422921992]);
%% Uncomment the following line to preserve the X-limits of the axes
 xlim(axes1,[0.5 50.5]);
%% Uncomment the following line to preserve the Y-limits of the axes
  ylim(axes1,[-1 10]);
box(axes1,'on');
hold(axes1,'on'); 

% Create bar
bar(yvector1,'FaceColor',[0 0.498039215686275 0]);

% Create ylabel
ylabel('Percentage (%)');

