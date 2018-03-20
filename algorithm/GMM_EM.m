function result=em(x)
n=length(x);
p00=repmat(.5,1,n);
mu0=[1,2];
theta0=.33;
mu=[0,0];
for t=1:1000
    p10=theta0*exp(-(x-mu0(1)).^2/2)./(theta0*exp(-(x-mu0(1)).^2/2)+(1-theta0)*exp(-(x-mu0(2)).^2/2));
    mu(1)=sum(p10.*x)/sum(p10);
    mu(2)=sum((1-p10).*x)/sum(1-p10);
    theta=mean(p10);
    if max([abs(p10-p00),abs(mu-mu0),abs(theta-theta0)])<.00001
        break
    end
    if t==1000&&(max([abs(p10-p00),abs(mu-mu0),abs(theta-theta0)])<.00001)
        disp('Does not converge')
    end
    p00=p10;
    mu0=mu;
    theta0=theta;
end
result=[p10,mu,theta];