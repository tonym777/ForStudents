//Implied Volatility using Black Scholes with Dividend

#include <iostream>
#include <cmath>
#include <vector>

using namespace std;
// N(0,1) density
double f (double x)
{
  double pi = 4.0 * atan (1.0);
  return exp (-x * x * 0.5) / sqrt (2 * pi);
}

// Boole's Rule
double Boole (double StartPoint, double EndPoint, int n)
{
  vector < double >X (n + 1, 0.0);
  vector < double >Y (n + 1, 0.0);
  
  double delta_x = (EndPoint - StartPoint) / double (n);
  for (int i = 0; i <= n; i++)
  {
    X[i] = StartPoint + i * delta_x;
    Y[i] = f (X[i]);
  }
  
  double sum = 0;
  for (int t = 0; t <= (n - 1) / 4; t++)
  {
    int ind = 4 * t;
    sum +=  (1 / 45.0) * (14 * Y[ind] + 64 * Y[ind + 1] + 24 * Y[ind + 2] +
            64 * Y[ind + 3] + 14 * Y[ind + 4]) * delta_x;
  }
  return sum;
}

// N(0,1) cdf by Boole's Rule
double  N (double x)
{
  return Boole (-10.0, x, 240);
}

// Black-Scholes Call Price
double  BSPrice (double S, double K, double r, double T, double q, double v, char PutCall)
{
  double d = (log (S / K) + T * (r - q + 0.5 * v * v)) / (v * sqrt (T));
  double call = S *exp(-q*T)* N (d) - exp (-r * T) * K * N (d - v * sqrt (T));
  if (PutCall == 'C') 
    return call;
  else
  // Put Parity
    return call - S*exp (-q * T) + K * exp (-r * T);
}

// Bisection Algorithm
double BisecBSV(double S, double K, double r, double T, double q, double a, double b, double MktPrice, char PutCall) 
{
    const int MaxIter = 50000;
    double Tol = 0.0000001;
    double midP = 0.0, midCdif;
    double lowCdif = MktPrice - BSPrice(S, K, r, T, q, a, PutCall);
    double highCdif = MktPrice - BSPrice(S, K, r, T, q, b, PutCall);
    
    if (lowCdif*highCdif > 0)
      return -1;
    else
      for (int i = 0; i <= MaxIter; i++) {
        midP = (a + b) / 2.0;
        midCdif = MktPrice - BSPrice(S, K, r, T, q, midP, PutCall);
        if (abs(midCdif)<Tol) goto LastLine;
        else {
        if (midCdif>0) a = midP;
        else b = midP;
       }
    }
  
LastLine:
    return midP;
}


int main() {
    double S = 100; // Spot Price
    double K = 100; // Exercise
    double r = 0.05; // Interest Rate
    double T = 1; // Maturity in Years
    double q = 0.0;
    double a = 0.00000001; // Bisection algorithm starting value for lower volatility
    double b = 7.0; // Bisection algorithm starting value for higher volatility
    double MktPrice = 10.45;
    char PutCall = 'C'; // C
    
    double IV = BisecBSV(S, K, r, T, q, a, b, MktPrice, PutCall);
    cout << " Implied Volatility = " << IV << " " << endl;
  
    PutCall = 'P'
    double IV = BisecBSV(S, K, r, T, q, a, b, MktPrice, PutCall);
    cout << " Implied Volatility = " << IV << " " << endl;
    system("PAUSE");
}
