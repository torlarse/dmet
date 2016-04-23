%% Beregning av hydrodynamiske koeffisienter
n=4;
%% Laster inn dat.-filene
A33 = load ('A33_stor.dat');
B33 = load ('B33.dat');
a33 = load ('A33_liten.dat');
b33 = load ('b33_liten.dat');
%% Lager polynomtilpasninger av kurvene
A33poly = polyfit(A33(:,1),A33(:,2),n);
B33poly = polyfit(B33(:,1),B33(:,2),n);
a33poly = polyfit(a33(:,1),a33(:,2),n);
b33poly = polyfit(b33(:,1),b33(:,2),n);
%% Plotter test av rådata
figure
hold on
plot(A33(:,2))
plot(a33(:,2))
plot(B33(:,2))
plot(b33(:,2))
hold off
%% lager loop som setter koeffisientene i polynomer
matrise = [A33poly; a33poly; B33poly; b33poly]
%fil =fopen('koeffisienter.dat','w');
[a,b] = size(matrise);
polymat = zeros(a,b);
strengalainen = char(a,b)
formatSpec = '%fx^%d          ';

for i = 1:a
    for j=1:b
        streng = sprintf(formatSpec,matrise(i,j),n+1-j)
        polynomer(i,j) = {streng};
        %fprintf(file,formatSpec,'streng')
    end
end
fil = fopen('polynom.txt','w');
fprintf(fil,'%f  %f  %f  %f  %f\n',matrise)
fclose(fil)


