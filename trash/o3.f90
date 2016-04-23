program middelperiode
implicit none
real  hs, tp, gamma, omega, b,s,a, omegap,f, k, integral
integer n
real, parameter :: pi=3.1415

hs = 12
tp = 10
gamma = 3
b = 3
n = 100
S = 0
a = 0
omegap = 2.0*pi / tp
f = 0
k = calk(pi, hs, tp, gamma)
write(*,*)'k fra program  er:', k
call fcal(omegap,k,a,b,n,f)
write(*,*)'f fra program  er:', f
tz = tc(a,b,hs,f,pi,n)
write(*,*) 'Tz fra programmet er: ', tz

end program
