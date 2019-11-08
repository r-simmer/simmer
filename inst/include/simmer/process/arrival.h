// Copyright (C) 2015-2016 Bart Smeets and Iñaki Ucar
// Copyright (C) 2016-2019 Iñaki Ucar
//
// This file is part of simmer.
//
// simmer is free software: you can redistribute it and/or modify it
// under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 2 of the License, or
// (at your option) any later version.
//
// simmer is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with simmer. If not, see <http://www.gnu.org/licenses/>.

#ifndef simmer__process_arrival_h
#define simmer__process_arrival_h

#include <simmer/process.h>
#include <simmer/process/task.h>
#include <simmer/process/order.h>
#include <simmer/activity.h>

namespace simmer {

  class Batched;
  class Resource;

  /**
   *  Arrival process.
   */
  class Arrival : public Process {
    friend class Batched;

  public:
    struct ArrTime {
      double start;
      double activity;
      ArrTime() : start(-1), activity(0) {}
    };
    struct ArrStatus {
      double busy_until;
      double remaining;
      ArrStatus() : busy_until(-1), remaining(0) {}
    };
    typedef UMAP<std::string, ArrTime> ResTime;
    typedef UMAP<int, Resource*> SelMap;
    typedef VEC<Resource*> ResVec;

    CLONEABLE(Arrival)

    Order order;        /**< priority, preemptible, restart */

    /**
    * Constructor.
    * @param sim             a pointer to the simulator
    * @param name            the name
    * @param mon             int that indicates whether this entity must be monitored
    * @param order           priority, preemptible, restart
    * @param first_activity  the first activity of a user-defined R trajectory
    */
    Arrival(Simulator* sim, const std::string& name, int mon, Order order,
            Activity* first_activity, int priority = 0)
      : Process(sim, name, mon, priority), order(order), paused(false),
        clones(new int(0)), activity(first_activity), timer(NULL),
        dropout(NULL), batch(NULL) { init(); }

    Arrival(const Arrival& o)
      : Process(o), order(o.order), paused(o.paused), clones(o.clones),
        activity(NULL), attributes(o.attributes), timer(NULL),
        dropout(NULL), batch(NULL) { init(); }

    ~Arrival() { reset(); }

    void run() {
      double delay;

      if (lifetime.start < 0)
        lifetime.start = sim->now();
      if (!activity)
        goto finish;

      if (sim->verbose) {
        sim->print("arrival", name, "activity", activity->name, "", false);
        activity->print(0, false, true);
      }

      delay = activity->run(this);
      if (delay == STATUS_REJECT)
        goto end;
      activity = activity->get_next();
      if (delay == STATUS_ENQUEUE)
        goto end;

      if (delay != STATUS_BLOCK) {
        set_busy(sim->now() + delay);
        update_activity(delay);
      }
      sim->schedule(delay, this, activity ? activity->priority : PRIORITY_MAX);

      end:
        return;
      finish:
        terminate(true);
    }

    void restart() {
      set_busy(sim->now() + status.remaining);
      activate(status.remaining);
      set_remaining(0);
      paused = false;
    }

    void pause() {
      deactivate();
      unset_busy(sim->now());
      if (status.remaining && order.get_restart()) {
        unset_remaining();
        activity = activity->get_prev();
      }
      paused = true;
    }

    bool is_paused() const { return paused; }

    void stop() {
      deactivate();
      if (status.busy_until < sim->now())
        return;
      unset_busy(sim->now());
      unset_remaining();
    }

    virtual void terminate(bool finished);

    int get_clones() const { return *clones; }

    virtual void set_attribute(const std::string& key, double value, bool global=false) {
      if (global) return sim->set_attribute(key, value);
      attributes[key] = value;
      if (is_monitored() >= 2)
        sim->mon->record_attribute(sim->now(), name, key, value);
    }

    double get_attribute(const std::string& key, bool global=false) const {
      if (global) return sim->get_attribute(key);
      Attr::const_iterator search = attributes.find(key);
      if (search == attributes.end())
        return NA_REAL;
      return search->second;
    }

    double get_start(const std::string& name);

    double get_start() const { return lifetime.start; }

    double get_remaining() const { return status.remaining; }

    void set_activity(Activity* ptr) { activity = ptr; }

    Activity* get_activity() const { return activity; }

    void set_resource_selected(int id, Resource* res) { selected[id] = res; }

    Resource* get_resource_selected(int id) const {
      SelMap::const_iterator search = selected.find(id);
      if (search != selected.end())
        return search->second;
      return NULL;
    }

    void register_entity(Resource* ptr);
    void unregister_entity(Resource* ptr);

    void register_entity(Batched* ptr) {
      if (!ptr) Rcpp::stop("illegal register of arrival '%s'", name); // # nocov
      batch = ptr;
    }

    void unregister_entity(Batched* ptr) {
      if (ptr != batch)
        Rcpp::stop("illegal unregister of arrival '%s'", name); // # nocov
      batch = NULL;
    }

    void set_dropout(Activity* next) { dropout = next; }

    void set_renege(double timeout, Activity* next, bool keep_seized) {
      cancel_renege();
      timer = new Task(sim, "Renege-Timer",
                       BIND(&Arrival::renege, this, next, keep_seized),
                       PRIORITY_MAX);
      timer->activate(timeout);
    }

    void set_renege(const std::string& sig, Activity* next, bool keep_seized) {
      cancel_renege();
      signal = sig;
      sim->subscribe(signal, this, BIND(&Arrival::renege, this, next, keep_seized));
    }

    void cancel_renege() {
      if (timer) {
        timer->deactivate();
        delete timer;
        timer = NULL;
      } else if (!signal.empty()) {
        sim->unsubscribe(signal, this);
        signal.clear();
      }
    }

  private:
    bool paused;
    int* clones;          /**< number of active clones */
    ArrStatus status;     /**< arrival timing status */
    ArrTime lifetime;     /**< time spent in the whole trajectory */
    ResTime restime;      /**< time spent in resources */
    Activity* activity;   /**< current activity from an R trajectory */
    Attr attributes;      /**< user-defined (key, value) pairs */
    SelMap selected;      /**< selected resource */
    Task* timer;          /**< timer that triggers reneging */
    std::string signal;   /**< signal that triggers reneging */
    Activity* dropout;    /**< drop-out trajectory */
    Batched* batch;       /**< batch that contains this arrival */
    ResVec resources;     /**< resources that contain this arrival */

    void init() {
      (*clones)++;
      sim->register_arrival(this);
    }

    void reset() {
      cancel_renege();
      if (!--(*clones))
        delete clones;
      sim->unregister_arrival(this);
    }

    void renege(Activity* next, bool keep_seized);

    virtual void report(const std::string& resource) const {
      ArrTime time = restime.find(resource)->second;
      sim->mon->record_release(name, time.start, sim->now(), time.activity, resource);
    }

    virtual void report(const std::string& resource, double start, double activity) const {
      sim->mon->record_release(name, start, sim->now(), activity, resource);
    }

    void leave_resources(bool was_batched, bool keep_seized);

    virtual void update_activity(double value) {
      lifetime.activity += value;
      if (is_monitored()) {
        foreach_ (ResTime::value_type& itr, restime)
          itr.second.activity += value;
      }
    }

    virtual void set_remaining(double value) {
      status.remaining = value;
    }

    virtual void set_busy(double value) {
      status.busy_until = value;
    }

    void unset_remaining() {
      update_activity(-status.remaining);
      set_remaining(0);
    }

    void unset_busy(double now) {
      set_remaining(status.busy_until - now);
      set_busy(now);
    }
  };

} // namespace simmer

#endif
