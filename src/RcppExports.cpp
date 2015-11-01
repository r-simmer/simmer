// This file was generated by Rcpp::compileAttributes
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

// Simulator__new
SEXP Simulator__new(SEXP name_, SEXP verbose_);
RcppExport SEXP simmer_Simulator__new(SEXP name_SEXP, SEXP verbose_SEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< SEXP >::type name_(name_SEXP);
    Rcpp::traits::input_parameter< SEXP >::type verbose_(verbose_SEXP);
    __result = Rcpp::wrap(Simulator__new(name_, verbose_));
    return __result;
END_RCPP
}
// reset_
void reset_(SEXP sim_);
RcppExport SEXP simmer_reset_(SEXP sim_SEXP) {
BEGIN_RCPP
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< SEXP >::type sim_(sim_SEXP);
    reset_(sim_);
    return R_NilValue;
END_RCPP
}
// now_
double now_(SEXP sim_);
RcppExport SEXP simmer_now_(SEXP sim_SEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< SEXP >::type sim_(sim_SEXP);
    __result = Rcpp::wrap(now_(sim_));
    return __result;
END_RCPP
}
// peek_
double peek_(SEXP sim_);
RcppExport SEXP simmer_peek_(SEXP sim_SEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< SEXP >::type sim_(sim_SEXP);
    __result = Rcpp::wrap(peek_(sim_));
    return __result;
END_RCPP
}
// step_
void step_(SEXP sim_);
RcppExport SEXP simmer_step_(SEXP sim_SEXP) {
BEGIN_RCPP
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< SEXP >::type sim_(sim_SEXP);
    step_(sim_);
    return R_NilValue;
END_RCPP
}
// run_
void run_(SEXP sim_, SEXP until_);
RcppExport SEXP simmer_run_(SEXP sim_SEXP, SEXP until_SEXP) {
BEGIN_RCPP
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< SEXP >::type sim_(sim_SEXP);
    Rcpp::traits::input_parameter< SEXP >::type until_(until_SEXP);
    run_(sim_, until_);
    return R_NilValue;
END_RCPP
}
// resource_
void resource_(SEXP sim_, SEXP name_, SEXP capacity_, SEXP queue_size_, SEXP mon_);
RcppExport SEXP simmer_resource_(SEXP sim_SEXP, SEXP name_SEXP, SEXP capacity_SEXP, SEXP queue_size_SEXP, SEXP mon_SEXP) {
BEGIN_RCPP
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< SEXP >::type sim_(sim_SEXP);
    Rcpp::traits::input_parameter< SEXP >::type name_(name_SEXP);
    Rcpp::traits::input_parameter< SEXP >::type capacity_(capacity_SEXP);
    Rcpp::traits::input_parameter< SEXP >::type queue_size_(queue_size_SEXP);
    Rcpp::traits::input_parameter< SEXP >::type mon_(mon_SEXP);
    resource_(sim_, name_, capacity_, queue_size_, mon_);
    return R_NilValue;
END_RCPP
}
// process_
void process_(SEXP sim_, SEXP func_);
RcppExport SEXP simmer_process_(SEXP sim_SEXP, SEXP func_SEXP) {
BEGIN_RCPP
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< SEXP >::type sim_(sim_SEXP);
    Rcpp::traits::input_parameter< SEXP >::type func_(func_SEXP);
    process_(sim_, func_);
    return R_NilValue;
END_RCPP
}
// timeout_
void timeout_(SEXP sim_, SEXP delay_);
RcppExport SEXP simmer_timeout_(SEXP sim_SEXP, SEXP delay_SEXP) {
BEGIN_RCPP
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< SEXP >::type sim_(sim_SEXP);
    Rcpp::traits::input_parameter< SEXP >::type delay_(delay_SEXP);
    timeout_(sim_, delay_);
    return R_NilValue;
END_RCPP
}
// request_
void request_(SEXP sim_, SEXP name_, SEXP amount_);
RcppExport SEXP simmer_request_(SEXP sim_SEXP, SEXP name_SEXP, SEXP amount_SEXP) {
BEGIN_RCPP
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< SEXP >::type sim_(sim_SEXP);
    Rcpp::traits::input_parameter< SEXP >::type name_(name_SEXP);
    Rcpp::traits::input_parameter< SEXP >::type amount_(amount_SEXP);
    request_(sim_, name_, amount_);
    return R_NilValue;
END_RCPP
}
// get_mon_arrivals_
SEXP get_mon_arrivals_(SEXP sim_);
RcppExport SEXP simmer_get_mon_arrivals_(SEXP sim_SEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< SEXP >::type sim_(sim_SEXP);
    __result = Rcpp::wrap(get_mon_arrivals_(sim_));
    return __result;
END_RCPP
}
// get_mon_resource_
SEXP get_mon_resource_(SEXP sim_, SEXP name_);
RcppExport SEXP simmer_get_mon_resource_(SEXP sim_SEXP, SEXP name_SEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< SEXP >::type sim_(sim_SEXP);
    Rcpp::traits::input_parameter< SEXP >::type name_(name_SEXP);
    __result = Rcpp::wrap(get_mon_resource_(sim_, name_));
    return __result;
END_RCPP
}
// get_res_capacity_
int get_res_capacity_(SEXP sim_, SEXP name_);
RcppExport SEXP simmer_get_res_capacity_(SEXP sim_SEXP, SEXP name_SEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< SEXP >::type sim_(sim_SEXP);
    Rcpp::traits::input_parameter< SEXP >::type name_(name_SEXP);
    __result = Rcpp::wrap(get_res_capacity_(sim_, name_));
    return __result;
END_RCPP
}
// get_res_queue_size_
int get_res_queue_size_(SEXP sim_, SEXP name_);
RcppExport SEXP simmer_get_res_queue_size_(SEXP sim_SEXP, SEXP name_SEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< SEXP >::type sim_(sim_SEXP);
    Rcpp::traits::input_parameter< SEXP >::type name_(name_SEXP);
    __result = Rcpp::wrap(get_res_queue_size_(sim_, name_));
    return __result;
END_RCPP
}
