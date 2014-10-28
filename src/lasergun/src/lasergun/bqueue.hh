// Copyright 2014 (c) Diego Souza <dsouza@c0d3.xxx>
//  
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

#include <queue>
#include <stdexcept>
#include <pthread.h>

namespace lasergun
{

  template <typename T>
  class bqueue
  {
  public:

    /*! Creates a new bounded FIFO queue.
     *
     * \param limit The maximum number of elements to hold before
     *              blocking write requests;
     */
    bqueue (int limit) :
      size_(0),
      limit_(limit),
      closed_(false)
    {
      if (pthread_mutex_init(&mutex_, NULL) != 0)
      { throw(std::runtime_error("bqueue: error initializing mutex")); }

      if (pthread_cond_init(&e_cond_, NULL) != 0)
      {
        pthread_mutex_destroy(&mutex_);
        throw(std::runtime_error("bqueue: error initializing cond e_cond"));
      }

      if (pthread_cond_init(&f_cond_, NULL) != 0)
      {
        pthread_cond_destroy(&e_cond_);
        pthread_mutex_destroy(&mutex_);
        throw(std::runtime_error("bqueue: error initializing cond f_cond"));
      }
    }

    ~bqueue ()
    {
      pthread_cond_destroy(&f_cond_);
      pthread_cond_destroy(&e_cond_);
      pthread_mutex_destroy(&mutex_);
    }

    /*! Reads a value from the queue possibly unblocking writers.
     *
     * \param out The pointer that holds reference to the enqueued
     *            object. The pointer is only valid if this function
     *            returns true;
     * 
     * \return false: Could not read [likely the queue has been
     *                closed];
     *         true:  Success;
     */
    bool recv (T *out)
    {
      if (pthread_mutex_lock(&mutex_) != 0)
      { throw(std::runtime_error("bqueue#pop: error initializing the mutex")); }

      while (!closed_ && size_ == 0)
      { pthread_cond_wait(&e_cond_, &mutex_); }

      bool rc = false;
      if (size_ > 0)
      {
        rc     = true;
        size_ -= 1;
        *out   = queue_.front();
        queue_.pop();
      }

      pthread_cond_signal(&f_cond_);
      pthread_mutex_unlock(&mutex_);

      return(rc);
    }

    /*! Enqueues a value possibly unblocking readers.
     *
     * \param t value to enqueue;
     *
     * \return false: Could not write [likely the queue has been
     *                closed and there are no more values to read];
     * \return true:  Success;
     */
    bool send (const T &t)
    {
      if (pthread_mutex_lock(&mutex_) != 0)
      { throw(std::runtime_error("bqueue#push: error initializing the mutex")); }

      while (!closed_ && size_ >= limit_)
      { pthread_cond_wait(&f_cond_, &mutex_); }

      bool rc = false;
      if (!closed_)
      {
        rc     = true;
        size_ += 1;
        queue_.push(t);
      }

      pthread_cond_signal(&e_cond_);
      pthread_mutex_unlock(&mutex_);

      return(rc);
    }

    /*! Closes this queue possibly waking all read and write threads.
     */
    void close ()
    {
      closed_ = true;
      if (pthread_mutex_lock(&mutex_) == 0)
      {
        pthread_cond_broadcast(&e_cond_);
        pthread_cond_broadcast(&f_cond_);
        pthread_mutex_unlock(&mutex_);
      }
    }

    /*! Returns wheter or not this queue has been closed.
     */
    bool closed ()
    { return(closed_); }

  private:
    bqueue (const bqueue &);
    bqueue & operator= (const bqueue &);
    
    int size_;
    int limit_;
    bool closed_;
    std::queue<T> queue_;
    pthread_mutex_t mutex_;
    pthread_cond_t e_cond_;
    pthread_cond_t f_cond_;
  };

}
