function cv=nfoldcvsvm(n,C,x,y)
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
    tr=x(train,:);
    te=x(test,:);
    ltr=y(train,:);
    lte=y(test,:);
    model=trainSVM(tr,ltr,C);
    tl=classifySVM(model,te)';
    error(i,1)=length(find(tl~=lte))/length(tl);
end
cv=mean(error);