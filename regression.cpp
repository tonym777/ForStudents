#include <iostream>
#include <vector>

using namespace std;

double linear_regression(const vector<double> &x, const vector<double> &y)
{
	 int n = x.size();
	 double sumX=0, sumXX=0, sumY=0, sumXY=0, a=0, b=0;

	 for(int i=0; i<n; i++) {
		 sumX = sumX + x[i];
		 sumXX = sumXX + x[i]*x[i];
		 sumY = sumY + y[i];
		 sumXY = sumXY + x[i]*y[i];
	 }

	 b = (n*sumXY-sumX*sumY)/(n*sumXX-sumX*sumX);
	 a = (sumY - b*sumX)/n;

	 cout << "The best fitting line is a = " << a << ", b = " << b << endl;
}
