#ifndef simmer__resource_resource_h
#define simmer__resource_resource_h

#include <simmer/entity.h>
#include <simmer/process.h>

/**
 *  Generic resource, a passive entity that comprises server + a queue.
 */
class Resource : public Entity {
public:
  /**
  * Constructor.
  * @param sim         a pointer to the simulator
  * @param name        the name
  * @param mon         int that indicates whether this entity must be monitored
  * @param capacity    server capacity (-1 means infinity)
  * @param queue_size  room in the queue (-1 means infinity)
  */
  Resource(Simulator* sim, const std::string& name, int mon, int capacity,
           int queue_size, bool queue_size_strict)
    : Entity(sim, name, mon), capacity(capacity), queue_size(queue_size),
      server_count(0), queue_count(0), queue_size_strict(queue_size_strict) {}

  /**
  * Reset the resource: server, queue
  */
  void reset() {
    server_count = 0;
    queue_count = 0;
  }

  /**
  * Seize resources.
  * @param   arrival  a pointer to the arrival trying to seize resources
  * @param   amount   the amount of resources needed
  *
  * @return  SUCCESS, ENQUEUE, REJECT
  */
  int seize(Arrival* arrival, int amount);

  /**
  * Release resources.
  * @param   arrival a pointer to the arrival that releases resources
  * @param   amount  the amount of resources released
  *
  * @return  SUCCESS
  */
  int release(Arrival* arrival, int amount);
  bool erase(Arrival* arrival, bool stay = false);

  void set_capacity(int value);
  void set_queue_size(int value);
  int get_capacity() const { return capacity; }
  int get_queue_size() const { return queue_size; }
  int get_server_count() const { return server_count; }
  int get_queue_count() const { return queue_count; }

protected:
  int capacity;
  int queue_size;
  int server_count;       /**< number of arrivals being served */
  int queue_count;        /**< number of arrivals waiting */
  bool queue_size_strict;

  int post_release();

  void verbose_print(double time, const std::string& arrival, const std::string& status) const {
    Rcpp::Rcout <<
      FMT(10, right) << time << " |" <<
      FMT(12, right) << "resource: " << FMT(15, left) << name << "|" <<
      FMT(12, right) << "arrival: " << FMT(15, left) << arrival << "| " <<
      status << std::endl;
  }

  virtual bool first_in_line(int priority) const = 0;
  virtual bool room_in_server(int amount, int priority) const = 0;
  virtual bool room_in_queue(int amount, int priority) const = 0;
  virtual int try_free_server(bool verbose, double time) = 0;
  virtual int try_free_queue(bool verbose, double time) = 0;
  virtual int try_serve_from_queue(bool verbose, double time) = 0;
  virtual void insert_in_server(bool verbose, double time, Arrival* arrival, int amount) = 0;
  virtual void insert_in_queue(bool verbose, double time, Arrival* arrival, int amount) = 0;
  virtual int remove_from_server(bool verbose, double time, Arrival* arrival, int amount) = 0;
  virtual int remove_from_queue(bool verbose, double time, Arrival* arrival) = 0;
};

#endif
