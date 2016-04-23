load overflateplott.dat
load responsplott.dat

N = 1000;
deltaomega = 3/N;
[steg a] = size(overflateplott);
t=0:steg-1;
tid = 0:0.1:59.9;

figure
plot(tid,overflateplott, tid,responsplott(1:steg,1),....
    tid,responsplott(1:steg,2));
title('Respons mot overflateheving, lekter A');
xlabel('tid[s]');
ylabel('utslag[m]');
legend('Zeta','eta3','eta5');

figure
plot(tid,overflateplott, tid,responsplott(steg+1:2*steg,1),....
    tid,responsplott(steg+1:2*steg,2));
title('Respons mot overflateheving, lekter B');
xlabel('tid[s]');
ylabel('utslag[m]');
legend('Zeta','eta3','eta5');