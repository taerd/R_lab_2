# –абота с файлом data.csv
read.csv2("Data\\data.csv",stringsAsFactors=TRUE)->data

#”даление пропущенных значений
data<-na.omit(data)

# «амен€ем "," на "." и примен€ем as.numeric()
data<-as.data.frame(apply(apply(data,2,gsub,patt=",",replace="."),2,as.numeric))

#—толбец с значени€ 0 и 1
classes<-as.vector(data[,1])
#—толбец со значени€ми варианта
values<-as.vector(data[,10])

#«начени€ у случайной величины 0
data_0<-values[classes==0]

#«начени€ у случайной величины 1
data_1<-values[classes==1]

### ”ровень значимости общий дл€ всех ###
alpha<-0.05

#¬ычисление доверительного интервала дл€ среднего и дисперсии у выборки data
# ритерий стьюдента дл€ оценки среднего
# ритерий хи квадрат дл€ оценки дисперсии
#ѕредпологаетс€ что размер выборки >20 или вз€т из нормального распределени€!
#data- вектор с выборкой
#alpha- уровень значимиости 2alpha
#label- наблюдаема€ случайна€ величина
DovInterval<-function(data,alpha,label){
	sink(paste("Result\\statisticOf ",label,".txt",sep=""))

	#For mean dov interval
	cat(paste("statistic of ",label," about mean  with significance level : ",alpha,"\n"))
	cat(paste("sample from normal distribution (shapiro test) with p-value : ",shapiro.test(data)$p.value,"\n"))
	#left interval
	left<-mean(data)-qt(1-alpha/2,length(data)-1) * sd(data)/sqrt(length(data)-1)
	
	#right interval
	right<-mean(data)+qt(1-alpha/2,length(data)-1)*sd(data)/sqrt(length(data)-1)

	cat(paste("left interval : ",left,"  right interval : ", right, "\n"),sep="\n")

	#For varation dov interval
	cat(paste("statistic of ",label," about varation  with significance level : ",alpha,"\n"))
	left<-length(data)*sd(data)*sd(data)/qchisq(1-alpha/2,length(data)-1)
	right<-length(data)*sd(data)*sd(data)/qchisq(alpha/2,length(data)-1)
	
	cat(paste("left interval : ",left,"  right interval : ", right),sep="\n")
	sink()
}

DovInterval(data_0,alpha,"kci_0")
DovInterval(data_1,alpha,"kci_1")


#¬ычисление дов интервала дл€ разности средних и отношени€ дисперсий двух выборок
#data_0,data_1 - две выборки
#alpha -уровень значимости
#label - название статистики
DovInterval<-function(data_0,data_1,alpha,label){

	sink(paste("Result\\statisticOf ",label,".txt",sep=""))
	
	#For mean difference dov interval
	cat(paste("statistic of ",label," about mean(kci_0)-mean(kci_1) with significance level : ",alpha,"\n"))
	n<-length(data_0)
	m<-length(data_1)
	
	#left interval
	left<-(mean(data_0)-mean(data_1))-qt(1-alpha/2,n+m-2)*sqrt((m+n)*(n*var(data_0)+m*var(data_1))/(n*m*(n+m-2)))
	#right interval
	right<-(mean(data_0)-mean(data_1))+qt(1-alpha/2,n+m-2)*sqrt((m+n)*(n*var(data_0)+m*var(data_1))/(n*m*(n+m-2)))
	
	cat(paste("left interval : ",left,"  right interval : ", right, "\n"),sep="\n")

	#For variance ratio dov interval
	cat(paste("statistic of ",label," about variance ratio var(kci_1)/var(kci_0)  with significance level : ",alpha,"\n"))
	left<-m*(n-1)*var(data_1)/(n*(m-1)*var(data_0))/qf(1-alpha/2,m-1,n-1)
	right<-m*(n-1)*var(data_1)/(n*(m-1)*var(data_0))/qf(alpha/2,m-1,n-1)
	
	cat(paste("left interval : ",left,"  right interval : ", right, "\n"),sep="\n")

	sink()
}

DovInterval(data_0,data_1,alpha,"kci_0 and kci_1")

#Ѕиблиотека дл€ работы следущей функции
#install.packages("stringi")
library("stringi")

#ѕроверка критери€ хи квадрат согласи€ дл€ выборки
#data - вектор выборки
#n - количество разбиений интервала
#label - название случ величины
X2calculate<-function(data,n,label){
	int<-table(cut(data,n))
	res<-0
	num<-length(data)
	for (i in 1:n){

		stroka<-stri_split_fixed(levels(cut(data,n,dig.lab=5))[i],",")
		a1<-stroka[[1]][1]
		a11<-stri_split_fixed(a1,"(")
		a<-as.numeric(a11[[1]][2])

		b1<-stroka[[1]][2]
		b11<-stri_split_fixed(b1,"]")
		b<-as.numeric(b11[[1]][1])
		
		md=mean(data)
		sd=sqrt(var(data))
		p_i<-pnorm(b,md,sd)-pnorm(a,md,sd)

		res=res+(int[[i]]-num*p_i)^2/(num*p_i)
	}
	sink(paste("Result\\X2_for_",label,".txt",sep=""))
	cat(paste("hypothesis: normally distributed ",label,"\n"))
	cat(paste("calculated X2 is : ",res,"\n"))
	cat(paste("degrees of freedom is :",num-3,"significance level is: ",alpha," table X2 is: ",qchisq(1-alpha,num-3),"\n"))
	cat(paste("conclusion is : ",if(res<qchisq(1-alpha,num-3)){"normally distributed"} else {"not normally distributed"},"\n"))
	sink()
#return (res)
}

X2calculate(data_0,6,"data_0")
X2calculate(data_1,10,"data_1")

#ѕроверка равенство средних и дисперсий по стьюденту и фишеру
#data_0,data_1 - две выборки
#label- название файла
TwoSampleNormalTest<-function(data_0,data_1,label){
	n<-length(data_0)
	m<-length(data_1)
	
	t<-sqrt(n*m*(n+m-2)/(n+m))*(mean(data_0)-mean(data_1))/(sqrt(n*var(data_0)+m*var(data_1)))
	sink(paste("Result\\TwoSampleNormalTest_",label,".txt",sep=""))
	cat(paste("hypothesis: their mathematical expectation is equal with significance level : ",alpha," \n"))
	cat(paste("calculated |t| is : ",abs(t),"\n"))
	cat(paste("table t is : ",qt(1-alpha/2,n+m-2),"\n"))
	cat(paste("conclusion is : ",if(abs(t)<qt(1-alpha/2,n+m-2)){"mathematical expectations is equal"} else {"mathematical expectations not equal"},"\n"))
	

	f<-var(data_1)*n*(m-1)/(var(data_0)*m*(n-1))
	cat(paste("\nhypothesis: their varation is equal with significance level : ",alpha," \n"))
	cat(paste("calculated f is : ",f,"\n"))
	cat(paste("left table f is : ",qf(alpha/2,m-1,n-1),"right table f is ",qf(1-alpha/2,m-1,n-1),"\n"))
	cat(paste("conclusion is : ",if(f>qf(alpha/2,m-1,n-1) && f<qf(1-alpha/2,m-1,n-1)){"varations is equal"} else {"varations expectation not equal"},"\n"))	
	sink()
}

TwoSampleNormalTest(data_0,data_1,"data_0 and data_1")









