/*
 * CCISS 2020 - Misc C++ functions
 * Kiri Daust
 */

#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
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

// [[Rcpp::export]]
NumericVector NewSuitNoCurr(NumericMatrix x, NumericVector vals){
  int n = x.nrow();
  NumericVector res(n);
  for(int i = 0; i < n; i++){
    res[i] = vals[0]*x(i,0)+vals[1]*x(i,1)+vals[2]*x(i,2)+vals[3]*x(i,3);
  }
  return(res);
}

// [[Rcpp::export]]
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

// [[Rcpp::export]]
NumericVector stepDiff(IntegerVector Year, NumericVector NewSuit, NumericVector Curr){
  NumericVector res(NewSuit.size());
  if(NewSuit.size() == 4){
    res[0] = Curr[0] - NewSuit[0];
    for(int i = 1; i < 4; i++){
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
      if((Year[1] - Year[0]) == 30){
        res[1] = NewSuit[0] - NewSuit[1];
      }else{
        res[1] = 4 - NewSuit[1];
      }
      
    }
  }
  return(res);
}

// [[Rcpp::export]]
NumericVector ModelDir(NumericMatrix x, NumericVector Curr, std::string dir){
  int n = x.nrow();
  NumericVector res(n);
  NumericVector temp(4);
  NumericVector temp2;
  double curr_suit;
  if(dir == "Improve"){
    for(int i = 0; i < n; i++){
      temp = x(i,_);
      temp.push_front(0);
      curr_suit = Curr[i] - 1;
      res[i] = sum(temp[Range(0,curr_suit)]);
    }
  }else if(dir == "Stable"){
    for(int i = 0; i < n; i++){
      temp = x(i,_);
      temp.push_front(0);
      curr_suit = Curr[i];
      res[i] = temp[curr_suit];
    }
  }else{
    for(int i = 0; i < n; i++){
      temp = x(i,_);
      temp.push_back(0);
      curr_suit = Curr[i];
      res[i] = sum(temp[Range(curr_suit,4)]);
    }
  }
  
  return(res);
}

// [[Rcpp::export]]
LogicalVector bifurcTrend(NumericVector Imp, NumericVector Decl){
  int n = Imp.size();
  LogicalVector res(n);
  for(int i = 0; i < n; i++){
    if(Imp[i] >= 25 && Decl[i] >= 25){
      res[i] = true;
    }else{
      res[i] = false;
    }
  }
  return(res);
}