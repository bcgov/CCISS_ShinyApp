/*
 * CCISS 2020 - Misc C++ functions
 * Kiri Daust
 */

#include <Rcpp.h>
using namespace Rcpp;
using namespace std;
// Portfolio simulation

// [[Rcpp::export]]
NumericVector gs2gw(NumericVector x, double a, double b){
  int len = x.length();
  NumericVector out(len);
  for(int i = 0; i < len; i++){
    out[i] = a*exp(x[i]*b);
  }
  return(out);
}


// [[Rcpp::export]]
NumericVector SimGrowth(DataFrame DF, double cmdMin, 
                        double cmdMax, double tempMin, double tempMax, double climLoss){
  NumericVector Growth = DF["Growth"]; //convert to vectors
  NumericVector NoMort  = DF["NoMort"];
  NumericVector MeanDead = DF["MeanDead"];
  NumericVector Ruin = DF["Suit"];//think about this one
  NumericVector climCMD = DF["CMD"];
  NumericVector climMax = DF["Tmax_sm"];
  NumericVector climMin = DF["Tmin_sp"];
  
  int numYears = Growth.length();
  NumericVector Returns(numYears);
  double height, percentDead, percentRuin, climDead, climDiff, diffProp;
  int prevTrees, numDead, i;
  int nTrees = 100;
  for(i = 0; i < numYears; i++){
    height = sum(Growth[Rcpp::Range(0,i)]);
    Returns[i] = nTrees*height;
    climDead = 0;
    if(climCMD[i] > cmdMax){ // CMD max 
      climDiff = climCMD[i] - cmdMax;
      diffProp = (climDiff*100)/(cmdMax - cmdMin);
      climDiff = 1 - exp(-climLoss*diffProp);//1 - exp function
      climDead += climDiff*nTrees;
      //Rcout << "Too dry \n";
    }
    if(climCMD[i] < cmdMin){
      climDiff = cmdMin - climCMD[i];
      diffProp = (climDiff*100)/(cmdMax - cmdMin);
      climDiff = 1 - exp(-climLoss*diffProp);//1 - exp function
      climDead += climDiff*nTrees;
      //Rcout << "Too wet \n";
    }
    if(climMax[i] > tempMax){
      climDiff = climMax[i] - tempMax;
      diffProp = (climDiff*100)/(tempMax - tempMin);
      climDiff = 1 - exp(-climLoss*diffProp);//1 - exp function
      climDead += climDiff*nTrees;
      //Rcout << "Too hot \n";
    }
    if(climMin[i] < tempMin){
      climDiff = tempMin - climMin[i];
      diffProp = (climDiff*100)/(cmdMax - cmdMin);
      climDiff = 1 - exp(-climLoss*diffProp);//1 - exp function
      climDead += climDiff*nTrees;
      //Rcout << "Too cold \n";
    }
    if(climDead > nTrees){
      climDead = nTrees;
    }
    nTrees = nTrees - climDead;
    if(Rcpp::runif(1,0,100)[0] > NoMort[i]){//regular environmental loss
      percentDead = Rcpp::rgamma(1, 2, MeanDead[i])[0];
      numDead = (percentDead/100)*prevTrees;
      nTrees = nTrees - numDead;
    }
  }
  return(Returns);
}

//Regular CCISS functions

// [[Rcpp::export(rng=false)]]
NumericVector combCurr(NumericMatrix dat, float wt){
  int cols = dat.ncol();
  int row = dat.nrow();
  if(row == 1){
    return dat(0,_);
  }else{
    NumericVector res(cols);
    for(int i = 0; i < cols; i++){
      res[i] = sum(dat(_,i))/(1+wt);
    }
    return(res);
  }
}

// [[Rcpp::export(rng=false)]]
NumericVector NewSuitNoCurr(NumericMatrix x, NumericVector vals){
  int n = x.nrow();
  NumericVector res(n);
  for(int i = 0; i < n; i++){
    res[i] = vals[0]*x(i,0)+vals[1]*x(i,1)+vals[2]*x(i,2)+vals[3]*x(i,3);
  }
  return(res);
}

// [[Rcpp::export(rng=false)]]
NumericVector FeasSuit(NumericMatrix x,IntegerVector Curr, NumericVector vals, NumericVector CurrAdj){
  int n = x.nrow();
  NumericVector res(n);
  int cPos;
  double cAdj;
  for(int i = 0; i < n; i++){
    cPos = Curr[i] - 1;
    cAdj = CurrAdj[cPos];
    res[i] = cAdj + vals[0]*x(i,0)+vals[1]*x(i,1)+vals[2]*x(i,2)+vals[3]*x(i,3);
  }
  return(res);
}

// [[Rcpp::export(rng=false)]]
NumericVector stepDiff(IntegerVector Year, NumericVector NewSuit, NumericVector Curr){
  NumericVector res(NewSuit.size());
  if(NewSuit.size() == 5){
    res[0] = Curr[0] - NewSuit[0];
    for(int i = 1; i < 5; i++){
      res[i] = NewSuit[i-1] - NewSuit[i];
    }
  }else{
    int n = NewSuit.size();
    if(Year[0] == 2000){
      res[0] = NewSuit[0] - Curr[0];
    }else{
      res[0] = 4 - NewSuit[0];
    }
    if(n > 1){
      if((Year[1] - Year[0]) == 20){
        res[1] = NewSuit[0] - NewSuit[1];
      }else{
        res[1] = 4 - NewSuit[1];
      }
      
    }
  }
  return(res);
}

//' Function for quickly calculating model direction/agreement
//' @name ModelDir
//' @param x data
//' @return NumericVector
// [[Rcpp::export]]
NumericVector ModelDir(NumericMatrix x, NumericVector Curr, std::string dir){
  int n = x.nrow();
  NumericVector res(n);
  NumericVector temp(5);
  NumericVector temp2;
  double curr_suit;
  if(dir == "Improve"){
    for(int i = 0; i < n; i++){
      temp = x(i,_);
      temp.push_front(0);
      curr_suit = Curr[i];
      if(curr_suit == 4){
        curr_suit = 3;
      }
      res[i] = sum(temp[Range(0,curr_suit)]);
    }
  }else{
    for(int i = 0; i < n; i++){
      temp = x(i,_);
      temp.push_back(0);
      curr_suit = Curr[i];
      if(curr_suit == 4){
        curr_suit = 3;
      }
      res[i] = sum(temp[Range(curr_suit,4)]);
    }
  }
  
  return(res);
}

// // [[Rcpp::export(rng=false)]]
// LogicalVector bifurcTrend(NumericVector Imp, NumericVector Decl, double cutoff){
//   int n = Imp.size();
//   LogicalVector res(n);
//   for(int i = 0; i < n; i++){
//     if(Imp[i] >= cutoff && Decl[i] >= cutoff){
//       res[i] = true;
//     }else{
//       res[i] = false;
//     }
//   }
//   return(res);
// }