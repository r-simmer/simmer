#include "entity.h"
#include "simulator.h"
#include "activity.h"
#include "random.h"

template <typename T>
T execute_call(Rcpp::Function call, Arrival* arrival, bool provide_attrs) {
  if (provide_attrs)
    return Rcpp::as<T>(call(Rcpp::wrap(*arrival->get_attributes())));
  else return Rcpp::as<T>(call());
}

template <>
void Seize<int>::print(int indent, bool brief) {
  Activity::print(indent, brief);
  if (!brief) Rcpp::Rcout << 
    "resource: " << resource << " | " << "amount: " << amount << " }" << std::endl;
  else Rcpp::Rcout << resource << ", " << amount << std::endl;
}

template <>
void Seize<Rcpp::Function>::print(int indent, bool brief) {
  Activity::print(indent, brief);
  if (!brief) Rcpp::Rcout << 
    "resource: " << resource << " | " << "amount: function() }" << std::endl;
  else Rcpp::Rcout << resource << ", " << amount << std::endl;
}

template <>
double Seize<int>::run(Arrival* arrival) {
  return arrival->sim->get_resource(resource)->seize(arrival, amount, 
                                    priority, preemptible, restart);
}

template <>
double Seize<Rcpp::Function>::run(Arrival* arrival) {
  return arrival->sim->get_resource(resource)->seize(arrival, 
                                    execute_call<int>(amount, arrival, provide_attrs), 
                                    priority, preemptible, restart);
}

template <>
double SeizeSelected<int>::run(Arrival* arrival) {
  resource = arrival->get_selected();
  return Seize<int>::run(arrival);
}

template <>
double SeizeSelected<Rcpp::Function>::run(Arrival* arrival) {
  resource = arrival->get_selected();
  return Seize<Rcpp::Function>::run(arrival);
}

template <>
void Release<int>::print(int indent, bool brief) {
  Activity::print(indent, brief);
  if (!brief) Rcpp::Rcout << 
    "resource: " << resource << " | " << "amount: " << amount << " }" << std::endl;
  else Rcpp::Rcout << resource << ", " << amount << std::endl;
}

template <>
void Release<Rcpp::Function>::print(int indent, bool brief) {
  Activity::print(indent, brief);
  if (!brief) Rcpp::Rcout << 
    "resource: " << resource << " | " << "amount: function() }" << std::endl;
  else Rcpp::Rcout << resource << ", " << amount << std::endl;
}

template <>
double Release<int>::run(Arrival* arrival) {
  return arrival->sim->get_resource(resource)->release(arrival, amount);
}

template <>
double Release<Rcpp::Function>::run(Arrival* arrival) {
  return arrival->sim->get_resource(resource)->release(arrival, 
                                    execute_call<int>(amount, arrival, provide_attrs));
}

template <>
double ReleaseSelected<int>::run(Arrival* arrival) {
  resource = arrival->get_selected();
  return Release<int>::run(arrival);
}

template <>
double ReleaseSelected<Rcpp::Function>::run(Arrival* arrival) {
  resource = arrival->get_selected();
  return Release<Rcpp::Function>::run(arrival);
}

template <>
void Timeout<double>::print(int indent, bool brief) {
  Activity::print(indent, brief);
  if (!brief) Rcpp::Rcout << "delay: " << delay << " }" << std::endl;
  else Rcpp::Rcout << delay << std::endl;
}

template <>
void Timeout<Rcpp::Function>::print(int indent, bool brief) {
  Activity::print(indent, brief);
  if (!brief) Rcpp::Rcout << "task: function() }" << std::endl;
  else Rcpp::Rcout << "function()" << std::endl;
}

template <>
double Timeout<double>::run(Arrival* arrival) { return std::abs(delay); }

template <>
double Timeout<Rcpp::Function>::run(Arrival* arrival) {
  return std::abs(execute_call<double>(delay, arrival, provide_attrs));
}

template <>
void SetAttribute<double>::print(int indent, bool brief) {
  Activity::print(indent, brief);
  if (!brief) Rcpp::Rcout << 
    "key: " << key << ", value: " << value << " }" << std::endl;
  else Rcpp::Rcout << key << ": " << value << std::endl;
}

template <>
void SetAttribute<Rcpp::Function>::print(int indent, bool brief) {
  Activity::print(indent, brief);
  if (!brief) Rcpp::Rcout << 
    "key: " << key << ", value: function() }" << std::endl;
  else Rcpp::Rcout << "function()" << std::endl;
}

template <>
double SetAttribute<double>::run(Arrival* arrival) {
  return arrival->set_attribute(key, value);
}

template <>
double SetAttribute<Rcpp::Function>::run(Arrival* arrival) {
  return arrival->set_attribute(key, 
                                execute_call<double>(value, arrival, provide_attrs));
}

double Branch::run(Arrival* arrival) {
  unsigned int i = execute_call<unsigned int>(option, arrival, provide_attrs);
  if (i < 1 || i > heads.size())
    Rcpp::stop("index out of range");
  selected = heads[i-1];
  return 0;
}

template <>
void Rollback<int>::print(int indent, bool brief) {
  if (!cached) cached = goback();
  Activity::print(indent, brief);
  if (!brief) {
    Rcpp::Rcout << "amount: " << amount << " (" << cached->name << "), ";
    if (times >= 0)
      Rcpp::Rcout << "times: " << times << " }" << std::endl;
    else
      Rcpp::Rcout << "times: Inf }" << std::endl;
  } else Rcpp::Rcout << cached->name << std::endl;
}

template <>
void Rollback<Rcpp::Function>::print(int indent, bool brief) {
  if (!cached) cached = goback();
  Activity::print(indent, brief);
  if (!brief) Rcpp::Rcout << 
    "amount: " << amount << " (" << cached->name << "), " << 
    "check: function() }" << std::endl;
  else Rcpp::Rcout << cached->name << std::endl;
}

template <>
double Rollback<int>::run(Arrival* arrival) {
  if (times >= 0) {
    if (pending.find(arrival) == pending.end()) 
      pending[arrival] = times;
    if (!pending[arrival]) {
      pending.erase(arrival);
      return 0;
    }
    pending[arrival]--;
  }
  
  if (!cached) cached = goback();
  selected = cached;
  return 0;
}

template <>
double Rollback<Rcpp::Function>::run(Arrival* arrival) {
  if (!execute_call<bool>(times, arrival, provide_attrs)) return 0;
  if (!cached) cached = goback();
  selected = cached;
  return 0;
}

template <>
void Select<VEC<std::string> >::print(int indent, bool brief) {
  Activity::print(indent, brief);
  if (!brief) Rcpp::Rcout << 
    "resources: " << resources[0] << (resources.size()>1 ? ", ..." : "") << " | " << 
    "policy: " << policy << " }" << std::endl;
  else Rcpp::Rcout << selected << std::endl;
}

template <>
void Select<Rcpp::Function>::print(int indent, bool brief) {
  Activity::print(indent, brief);
  if (!brief) Rcpp::Rcout << "resources: function() }" << std::endl;
  else Rcpp::Rcout << selected << std::endl;
}

template <>
double Select<VEC<std::string> >::run(Arrival* arrival) {
  if (resources.size() == 1)
    selected_i = 0;
  else {
    if (!policy.compare("shortest-queue")) {
      selected_i = 0;
      int n = resources.size();
      for (int i = 0; i < n; i++) {
        Resource* old = arrival->sim->get_resource(resources[selected_i]);
        Resource* res = arrival->sim->get_resource(resources[i]);
        if (res->get_server_count() + res->get_queue_count() < 
            old->get_server_count() + old->get_queue_count())
          selected_i = i;
      }
    }
    else if (!policy.compare("round-robin")) {
      selected_i++;
      if (selected_i == (int)resources.size())
        selected_i = 0;
    } 
    else if (!policy.compare("first-available")) {
      int i, n = resources.size();
      for (i = 0; i < n; i++) {
        Resource* res = arrival->sim->get_resource(resources[i]);
        if (res->get_server_count() < res->get_capacity())
          goto select;
      }
      for (i = 0; i < n; i++) {
        Resource* res = arrival->sim->get_resource(resources[i]);
        if (res->get_queue_count() < res->get_queue_size())
          goto select;
      }
    select:
      if (i == n) selected_i = 0;
      else selected_i = i;
    }
    else if (!policy.compare("random")) {
      selected_i = rand_int(0, resources.size()-1);
    }
    else Rcpp::stop("policy '" + policy + "' not supported (typo?)");
  }
  selected = resources[selected_i];
  arrival->set_selected(selected);
  return 0;
}

template <>
double Select<Rcpp::Function>::run(Arrival* arrival) {
  selected = execute_call<std::string>(resources, arrival, provide_attrs);
  arrival->set_selected(selected);
  return 0;
}
