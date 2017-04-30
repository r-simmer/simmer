#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* FIXME:
Check these declarations against the C/Fortran source code.
*/

/* .Call calls */
extern SEXP simmer_Activate__new(SEXP);
extern SEXP simmer_Activate__new_func(SEXP, SEXP);
extern SEXP simmer_activity_chain_(SEXP, SEXP);
extern SEXP simmer_activity_clone_(SEXP);
extern SEXP simmer_activity_get_n_(SEXP);
extern SEXP simmer_activity_get_next_(SEXP);
extern SEXP simmer_activity_get_prev_(SEXP);
extern SEXP simmer_activity_print_(SEXP, SEXP, SEXP);
extern SEXP simmer_add_generator_(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP simmer_add_resource_(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP simmer_add_resource_manager_(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP simmer_Batch__new(SEXP, SEXP, SEXP, SEXP);
extern SEXP simmer_Batch__new_func(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP simmer_Branch__new(SEXP, SEXP, SEXP, SEXP);
extern SEXP simmer_Clone__new(SEXP, SEXP);
extern SEXP simmer_Clone__new_func(SEXP, SEXP, SEXP);
extern SEXP simmer_Deactivate__new(SEXP);
extern SEXP simmer_Deactivate__new_func(SEXP, SEXP);
extern SEXP simmer_get_capacity_(SEXP, SEXP);
extern SEXP simmer_get_mon_arrivals_(SEXP, SEXP, SEXP);
extern SEXP simmer_get_mon_attributes_(SEXP);
extern SEXP simmer_get_mon_resource_(SEXP);
extern SEXP simmer_get_mon_resource_counts_(SEXP);
extern SEXP simmer_get_mon_resource_limits_(SEXP);
extern SEXP simmer_get_n_generated_(SEXP, SEXP);
extern SEXP simmer_get_queue_count_(SEXP, SEXP);
extern SEXP simmer_get_queue_size_(SEXP, SEXP);
extern SEXP simmer_get_server_count_(SEXP, SEXP);
extern SEXP simmer_Leave__new(SEXP);
extern SEXP simmer_Leave__new_func(SEXP, SEXP);
extern SEXP simmer_Log__new(SEXP);
extern SEXP simmer_Log__new_func(SEXP, SEXP);
extern SEXP simmer_now_(SEXP);
extern SEXP simmer_peek_(SEXP, SEXP);
extern SEXP simmer_Release__new(SEXP, SEXP);
extern SEXP simmer_Release__new_func(SEXP, SEXP, SEXP);
extern SEXP simmer_ReleaseSelected__new(SEXP, SEXP);
extern SEXP simmer_ReleaseSelected__new_func(SEXP, SEXP, SEXP);
extern SEXP simmer_RenegeAbort__new();
extern SEXP simmer_RenegeIf__new(SEXP, SEXP);
extern SEXP simmer_RenegeIf__new_func(SEXP, SEXP, SEXP);
extern SEXP simmer_RenegeIn__new(SEXP, SEXP);
extern SEXP simmer_RenegeIn__new_func(SEXP, SEXP, SEXP);
extern SEXP simmer_reset_(SEXP);
extern SEXP simmer_Rollback__new(SEXP, SEXP);
extern SEXP simmer_Rollback__new_func(SEXP, SEXP, SEXP);
extern SEXP simmer_run_(SEXP, SEXP);
extern SEXP simmer_Seize__new(SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP simmer_Seize__new_func(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP simmer_SeizeSelected__new(SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP simmer_SeizeSelected__new_func(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP simmer_Select__new(SEXP, SEXP, SEXP);
extern SEXP simmer_Select__new_func(SEXP, SEXP, SEXP, SEXP);
extern SEXP simmer_Send__new(SEXP, SEXP);
extern SEXP simmer_Send__new_func1(SEXP, SEXP, SEXP);
extern SEXP simmer_Send__new_func2(SEXP, SEXP, SEXP);
extern SEXP simmer_Send__new_func4(SEXP, SEXP, SEXP);
extern SEXP simmer_Separate__new();
extern SEXP simmer_SetAttribute__new(SEXP, SEXP, SEXP);
extern SEXP simmer_SetAttribute__new_func(SEXP, SEXP, SEXP, SEXP);
extern SEXP simmer_SetCapacity__new(SEXP, SEXP);
extern SEXP simmer_SetCapacity__new_func(SEXP, SEXP, SEXP);
extern SEXP simmer_SetCapacitySelected__new(SEXP, SEXP);
extern SEXP simmer_SetCapacitySelected__new_func(SEXP, SEXP, SEXP);
extern SEXP simmer_SetDist__new(SEXP, SEXP);
extern SEXP simmer_SetDist__new_func(SEXP, SEXP, SEXP);
extern SEXP simmer_SetPrior__new(SEXP);
extern SEXP simmer_SetPrior__new_func(SEXP, SEXP);
extern SEXP simmer_SetQueue__new(SEXP, SEXP);
extern SEXP simmer_SetQueue__new_func(SEXP, SEXP, SEXP);
extern SEXP simmer_SetQueueSelected__new(SEXP, SEXP);
extern SEXP simmer_SetQueueSelected__new_func(SEXP, SEXP, SEXP);
extern SEXP simmer_SetTraj__new(SEXP, SEXP);
extern SEXP simmer_SetTraj__new_func(SEXP, SEXP, SEXP);
extern SEXP simmer_Simulator__new(SEXP, SEXP);
extern SEXP simmer_step_(SEXP);
extern SEXP simmer_Synchronize__new(SEXP, SEXP);
extern SEXP simmer_Timeout__new(SEXP);
extern SEXP simmer_Timeout__new_func(SEXP, SEXP);
extern SEXP simmer_Trap__new(SEXP, SEXP, SEXP);
extern SEXP simmer_Trap__new_func(SEXP, SEXP, SEXP, SEXP);
extern SEXP simmer_UnTrap__new(SEXP);
extern SEXP simmer_UnTrap__new_func(SEXP, SEXP);
extern SEXP simmer_Wait__new();

static const R_CallMethodDef CallEntries[] = {
  {"simmer_Activate__new",                 (DL_FUNC) &simmer_Activate__new,                 1},
  {"simmer_Activate__new_func",            (DL_FUNC) &simmer_Activate__new_func,            2},
  {"simmer_activity_chain_",               (DL_FUNC) &simmer_activity_chain_,               2},
  {"simmer_activity_clone_",               (DL_FUNC) &simmer_activity_clone_,               1},
  {"simmer_activity_get_n_",               (DL_FUNC) &simmer_activity_get_n_,               1},
  {"simmer_activity_get_next_",            (DL_FUNC) &simmer_activity_get_next_,            1},
  {"simmer_activity_get_prev_",            (DL_FUNC) &simmer_activity_get_prev_,            1},
  {"simmer_activity_print_",               (DL_FUNC) &simmer_activity_print_,               3},
  {"simmer_add_generator_",                (DL_FUNC) &simmer_add_generator_,                8},
  {"simmer_add_resource_",                 (DL_FUNC) &simmer_add_resource_,                 8},
  {"simmer_add_resource_manager_",         (DL_FUNC) &simmer_add_resource_manager_,         6},
  {"simmer_Batch__new",                    (DL_FUNC) &simmer_Batch__new,                    4},
  {"simmer_Batch__new_func",               (DL_FUNC) &simmer_Batch__new_func,               6},
  {"simmer_Branch__new",                   (DL_FUNC) &simmer_Branch__new,                   4},
  {"simmer_Clone__new",                    (DL_FUNC) &simmer_Clone__new,                    2},
  {"simmer_Clone__new_func",               (DL_FUNC) &simmer_Clone__new_func,               3},
  {"simmer_Deactivate__new",               (DL_FUNC) &simmer_Deactivate__new,               1},
  {"simmer_Deactivate__new_func",          (DL_FUNC) &simmer_Deactivate__new_func,          2},
  {"simmer_get_capacity_",                 (DL_FUNC) &simmer_get_capacity_,                 2},
  {"simmer_get_mon_arrivals_",             (DL_FUNC) &simmer_get_mon_arrivals_,             3},
  {"simmer_get_mon_attributes_",           (DL_FUNC) &simmer_get_mon_attributes_,           1},
  {"simmer_get_mon_resource_",             (DL_FUNC) &simmer_get_mon_resource_,             1},
  {"simmer_get_mon_resource_counts_",      (DL_FUNC) &simmer_get_mon_resource_counts_,      1},
  {"simmer_get_mon_resource_limits_",      (DL_FUNC) &simmer_get_mon_resource_limits_,      1},
  {"simmer_get_n_generated_",              (DL_FUNC) &simmer_get_n_generated_,              2},
  {"simmer_get_queue_count_",              (DL_FUNC) &simmer_get_queue_count_,              2},
  {"simmer_get_queue_size_",               (DL_FUNC) &simmer_get_queue_size_,               2},
  {"simmer_get_server_count_",             (DL_FUNC) &simmer_get_server_count_,             2},
  {"simmer_Leave__new",                    (DL_FUNC) &simmer_Leave__new,                    1},
  {"simmer_Leave__new_func",               (DL_FUNC) &simmer_Leave__new_func,               2},
  {"simmer_Log__new",                      (DL_FUNC) &simmer_Log__new,                      1},
  {"simmer_Log__new_func",                 (DL_FUNC) &simmer_Log__new_func,                 2},
  {"simmer_now_",                          (DL_FUNC) &simmer_now_,                          1},
  {"simmer_peek_",                         (DL_FUNC) &simmer_peek_,                         2},
  {"simmer_Release__new",                  (DL_FUNC) &simmer_Release__new,                  2},
  {"simmer_Release__new_func",             (DL_FUNC) &simmer_Release__new_func,             3},
  {"simmer_ReleaseSelected__new",          (DL_FUNC) &simmer_ReleaseSelected__new,          2},
  {"simmer_ReleaseSelected__new_func",     (DL_FUNC) &simmer_ReleaseSelected__new_func,     3},
  {"simmer_RenegeAbort__new",              (DL_FUNC) &simmer_RenegeAbort__new,              0},
  {"simmer_RenegeIf__new",                 (DL_FUNC) &simmer_RenegeIf__new,                 2},
  {"simmer_RenegeIf__new_func",            (DL_FUNC) &simmer_RenegeIf__new_func,            3},
  {"simmer_RenegeIn__new",                 (DL_FUNC) &simmer_RenegeIn__new,                 2},
  {"simmer_RenegeIn__new_func",            (DL_FUNC) &simmer_RenegeIn__new_func,            3},
  {"simmer_reset_",                        (DL_FUNC) &simmer_reset_,                        1},
  {"simmer_Rollback__new",                 (DL_FUNC) &simmer_Rollback__new,                 2},
  {"simmer_Rollback__new_func",            (DL_FUNC) &simmer_Rollback__new_func,            3},
  {"simmer_run_",                          (DL_FUNC) &simmer_run_,                          2},
  {"simmer_Seize__new",                    (DL_FUNC) &simmer_Seize__new,                    5},
  {"simmer_Seize__new_func",               (DL_FUNC) &simmer_Seize__new_func,               6},
  {"simmer_SeizeSelected__new",            (DL_FUNC) &simmer_SeizeSelected__new,            5},
  {"simmer_SeizeSelected__new_func",       (DL_FUNC) &simmer_SeizeSelected__new_func,       6},
  {"simmer_Select__new",                   (DL_FUNC) &simmer_Select__new,                   3},
  {"simmer_Select__new_func",              (DL_FUNC) &simmer_Select__new_func,              4},
  {"simmer_Send__new",                     (DL_FUNC) &simmer_Send__new,                     2},
  {"simmer_Send__new_func1",               (DL_FUNC) &simmer_Send__new_func1,               3},
  {"simmer_Send__new_func2",               (DL_FUNC) &simmer_Send__new_func2,               3},
  {"simmer_Send__new_func4",               (DL_FUNC) &simmer_Send__new_func4,               3},
  {"simmer_Separate__new",                 (DL_FUNC) &simmer_Separate__new,                 0},
  {"simmer_SetAttribute__new",             (DL_FUNC) &simmer_SetAttribute__new,             3},
  {"simmer_SetAttribute__new_func",        (DL_FUNC) &simmer_SetAttribute__new_func,        4},
  {"simmer_SetCapacity__new",              (DL_FUNC) &simmer_SetCapacity__new,              2},
  {"simmer_SetCapacity__new_func",         (DL_FUNC) &simmer_SetCapacity__new_func,         3},
  {"simmer_SetCapacitySelected__new",      (DL_FUNC) &simmer_SetCapacitySelected__new,      2},
  {"simmer_SetCapacitySelected__new_func", (DL_FUNC) &simmer_SetCapacitySelected__new_func, 3},
  {"simmer_SetDist__new",                  (DL_FUNC) &simmer_SetDist__new,                  2},
  {"simmer_SetDist__new_func",             (DL_FUNC) &simmer_SetDist__new_func,             3},
  {"simmer_SetPrior__new",                 (DL_FUNC) &simmer_SetPrior__new,                 1},
  {"simmer_SetPrior__new_func",            (DL_FUNC) &simmer_SetPrior__new_func,            2},
  {"simmer_SetQueue__new",                 (DL_FUNC) &simmer_SetQueue__new,                 2},
  {"simmer_SetQueue__new_func",            (DL_FUNC) &simmer_SetQueue__new_func,            3},
  {"simmer_SetQueueSelected__new",         (DL_FUNC) &simmer_SetQueueSelected__new,         2},
  {"simmer_SetQueueSelected__new_func",    (DL_FUNC) &simmer_SetQueueSelected__new_func,    3},
  {"simmer_SetTraj__new",                  (DL_FUNC) &simmer_SetTraj__new,                  2},
  {"simmer_SetTraj__new_func",             (DL_FUNC) &simmer_SetTraj__new_func,             3},
  {"simmer_Simulator__new",                (DL_FUNC) &simmer_Simulator__new,                2},
  {"simmer_step_",                         (DL_FUNC) &simmer_step_,                         1},
  {"simmer_Synchronize__new",              (DL_FUNC) &simmer_Synchronize__new,              2},
  {"simmer_Timeout__new",                  (DL_FUNC) &simmer_Timeout__new,                  1},
  {"simmer_Timeout__new_func",             (DL_FUNC) &simmer_Timeout__new_func,             2},
  {"simmer_Trap__new",                     (DL_FUNC) &simmer_Trap__new,                     3},
  {"simmer_Trap__new_func",                (DL_FUNC) &simmer_Trap__new_func,                4},
  {"simmer_UnTrap__new",                   (DL_FUNC) &simmer_UnTrap__new,                   1},
  {"simmer_UnTrap__new_func",              (DL_FUNC) &simmer_UnTrap__new_func,              2},
  {"simmer_Wait__new",                     (DL_FUNC) &simmer_Wait__new,                     0},
  {NULL, NULL, 0}
};

void R_init_simmer(DllInfo *dll) {
  R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
}
