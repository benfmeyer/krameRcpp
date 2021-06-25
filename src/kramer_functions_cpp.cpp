#include <Rcpp.h>
#include <iostream>
using namespace Rcpp;

// This is a simple example of exporting a C++ function to R. You can
// source this function into an R session using the Rcpp::sourceCpp 
// function (or via the Source button on the editor toolbar). Learn
// more about Rcpp at:
//
//   http://www.rcpp.org/
//   http://adv-r.had.co.nz/Rcpp.html
//   http://gallery.rcpp.org/
//

// [[Rcpp::export]]
NumericVector r_chilling_cpp(NumericVector temp, double tc_min = -19.61, 
                             double tc_max = 77.13, double tc_opt = -0.24) {
  int n = temp.size();
  NumericVector rc(n);
  for (int i = 0; i < n; i++) {
    if (NumericVector::is_na(temp[i])) {
      rc[i] = NA_REAL;
    } else if (temp[i] < tc_min) {
      rc[i] = 0;
    } else if (tc_min <= temp[i] && temp[i] <= tc_opt) {
      rc[i] = (temp[i] - tc_min) / (tc_opt - tc_min);
    } else if (tc_opt <= temp[i] && temp[i] <= tc_max) {
      rc[i] = (temp[i] - tc_max) / (tc_opt - tc_max);
    } else if (temp[i] > tc_max) {
      rc[i] = 0;
    }
  }
  return rc;
}

// [[Rcpp::export]]
NumericVector state_cpp(NumericVector rate, DateVector time) {
  int n = rate.size();
  NumericVector cv(n);
  NumericVector tmp = cumsum(rate);
  for (int i = 0; i < n; i++) {
    Date d = time[i];
    cv[i] = tmp[i] * (d.getDay() == 1 && d.getMonth() == 11);
  }
  NumericVector max = cummax(cv);
  NumericVector st = tmp - max;
  return st;
}

// [[Rcpp::export]]
NumericVector r_forcing_cpp(NumericVector temp, NumericVector s_chill, float tf_min = 0) {
  int n = temp.size();
  NumericVector rf(n);
  for (int i = 0; i < n; i++) {
    if (NumericVector::is_na(temp[i])) {
      rf[i] = NA_REAL;
    } else if (temp[i] <= tf_min) {
      rf[i] = 0;
    } else if (temp[i] > tf_min && s_chill[i] > 125.51) {
      rf[i] = 1 / (1 + exp(-0.1 * (temp[i] + (-32.58))));
    } else if (s_chill[i] < 125.51) {
      rf[i] = 0;
    }
  }
  return rf;
}

// [[Rcpp::export]]
NumericVector leaf_out_status_cpp(NumericVector s_force) {
  int n = s_force.size();
  NumericVector lo_stat(n);
  for (int i = 0; i < n; i++) {
    if (NumericVector::is_na(s_force[i])) {
      lo_stat[i] = NA_REAL;
    } else if (s_force[i] > 3.15) {
      lo_stat[i] = 1;
    } else {
      lo_stat[i] = 0;
    }
  }
  return lo_stat;
}

// [[Rcpp::export]]
NumericVector accumulated_leaf_out_cpp(NumericVector lo_stat) {
  int n = lo_stat.size();
  NumericVector zero(n);
  NumericVector tmp = cumsum(lo_stat);
  for (int i = 0; i < n; i++) {
    zero[i] = tmp[i] * (lo_stat[i] == 0);
  }
  NumericVector max = cummax(zero);
  NumericVector accum_lo = tmp - max;
  return accum_lo;
}

// [[Rcpp::export]]
NumericVector late_frost_status_cpp(NumericVector accum_lo, NumericVector tmin, float thresh, float range) {
  int n = tmin.size();
  NumericVector lfs(n);
  for (int i = 0; i < n; i++) {
    if (NumericVector::is_na(tmin[i])) {
      lfs[i] = NA_REAL;
    } else if (accum_lo[i] >= 1 && accum_lo[i] <= range) {
      if (tmin[i] < thresh) {
        lfs[i] = 1;
      } else {
        lfs[i] = 0;
      }
    } else {
      lfs[i] = 0;
    }
  }
  return lfs;
}

// [[Rcpp::export]]
NumericVector test_fun(NumericVector nums) {
  NumericVector sum = nums + nums;
  return sum;
  }