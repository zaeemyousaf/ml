bestfit <-function(x,y){
	# x: independent variable
	# y: dependent variable	
	xbar = mean(x)
	ybar = mean(y)
	xdif = x-xbar
	ydif = y-ybar	
	pro = xdif*ydif
	num = sum(pro)
	den = sum(xdif^2)
	beta = num/den
	
	alpha= ybar-beta *xbar
	plot(x,y,type='p')
	abline(alpha,beta)
}

bestfit(rnorm(50)*10,rnorm(50)*10)