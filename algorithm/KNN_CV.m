function cv=nfoldcvknn(n,k,x,y)
nrow=size(x,1);
part=randperm(nrow);
smallsize=floor(nrow/n);
error=zeros(n,1);
mo=mod(nrow,n);
sets=cell(n,1);
for i=1:mo;
    sets{i}=((i-1)*(smallsize+1)+1):(i*(smallsize+1));
end
for i=(mo+1):n;
    sets{i}=((i-1)*smallsize+1):(i*smallsize);
end
for i=1:n
    test=part(sets{i});
    train=part;
    train(sets{i})=[];
    dis0=x(test,train);
    re=knn(k,dis0,y(train,:));
    real=y(test,:);
    error(i,1)=length(find(re~=real))/length(re);
end
cv=mean(error);