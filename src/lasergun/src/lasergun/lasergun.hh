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

#include <pdh.h>
#include <stdio.h>
#include <pdhmsg.h>
#include <windows.h>
#include "bqueue.hh"

#pragma comment(lib, "pdh.lib")

namespace lasergun
{

  class metric
  {
  public:
    metric (const std::wstring &, int time_, double value_);

    const std::wstring name_;
    const int time_;
    const double value_;
  };

  class lasergun
  {
  public:
    lasergun ();
    ~lasergun ();

    /*! Add a new metric for monitoring.
     *
     * \param name The name of the metric;
     */
    void add_metric (const std::wstring &);

    /*! List available metrics for collection.
     *
     * \param out Used to store the metrics that match the given
     * filter;
     * 
     * \param glob The filter to use;
     */
    void enum_metrics (std::vector<std::wstring> &out, const std::wstring &glob);

    /*! Starts the collection. This metrics does not block.
     *
     * \param interval Time in seconds;
     *
     * \param queue The queue where metrics are stored;
     */
    void start (int interval, bqueue<metric> *queue);

    /*! Cancel the collection.
     */
    void cancel ();

  private:
    int interval_;
  };

}
