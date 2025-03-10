%Trade and Labor Market Dynamics: "General Equilibrium Analysis of the China
%Trade Shock, by Caliendo, Dvorkin, and Parro (Econometrica)"

%This matlab file computes the equilibrium allocations at the initial
%period (year 2000)

close all
clear all
clc
tic
% format long
%Data
vfactor  = -0.05;
tol      = 1E-7;
maxit    = 1E+20;
tic
% Inputs
% Inputs
J=22; %sectors 
JNT=9; %non-tradables
JT=13; %tradables
R=50; %regions
C=37; %countries
N=R+C; %total countries and regions

 %dispersion of productivities
 T= [1/2.55 1/5.56 1/9.27 1/51.08 1/4.75 1/1.66 1/2.76 1/6.78 ...
    1/1.52 1/11.70  1/1.01 1/5.00 1/4.55 (1/4.55)*ones(9,1)'];
T=T';

 %DATA
B_usa=importdata('B.txt'); %share of capital structures in value added
B_row=importdata('B_row.txt');
gamma=importdata('gamma.txt');% share of value added in gross production
IO_data=importdata('IO_table_new.txt'); %input-output table
xbilat=importdata('xbilat_new.txt');%bilateral trade flows 2000
GO=importdata('GO_new.txt');%Gross output


%this function reshape and construct some input data
[B, gamma, G, Din, VALjn0, VARjn0,  alphas,  VAR, Bn]=data (J,JNT, JT, R,N,C, B_usa,B_row, gamma,IO_data, GO, xbilat);

%Iotas
Chi  = sum(VAR);
io = (VAR-Bn)./Chi;
Sn = Bn - VAR + io.*Chi;


 %Shocks
 kappa_hat=ones(J*N,N); % relative chane in trade costs
 lambda_hat=ones(J,N); %relative change in technology
 Snp = 0*Sn;
 
% Initialize vectors of factor prices (w,r) and good prices (p)
  L_hat=ones(N,1); pf0=ones(J,N); ommax=1; itw=1;
  Ljn_hat=ones(J,N);
  Ljn_hat(:,R+1:N)=1; %Ljn_hat RoW must be one
  om=ones(J,N); 

  %this function solves for the equilibrium allocations in the initial year
  %2000
[om wf0 VARjnp VALjnp Phat rf0 phat Dinp Xp Snp xbilatp] = solvewnew(om,Ljn_hat,VARjn0,VALjn0,Din,Snp,kappa_hat,lambda_hat,alphas,io,T,B,G,gamma,J,N,maxit,tol,R,vfactor);
  
  VALjn00=VALjnp;
  VARjn00=VARjnp;
  Sn00=Snp;
  Din00=Dinp;
  xbilat00=xbilatp;

save('Base_year_new.mat');


