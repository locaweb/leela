var LEELA;

if (LEELA === undefined) {
    LEELA = {};
}

LEELA.widget = function (root, opts) {
    var options = opts || {};

    var cspline_i = function (x, xk_1, yk_1, xk, yk, xk1, yk1, xk2, yk2) {
      var t   = (x-xk) / (xk1 - xk);
      var t2  = t*t;
      var t3  = t2*t;
      var h00 = 2*t3 - 3*t2 + 1;
      var h10 = t3 - 2*t2 + t;
      var h01 = -2*t3 + 3*t2;
      var h11 = t3 - t2;
      var m0  = (yk1-yk)/(2*(xk1 - xk)) + (yk-yk_1)/(2*(xk-xk_1));
      var m1  = (yk2-yk1)/(2*(xk2 - xk1)) + (yk1-yk)/(2*(xk1-xk));
      var y   = h00*yk + h10*(xk1-xk)*m0 + h01*yk1 + h11*(xk1-xk)*m1;
      return([x, y]);
    };

    var cspline = function (data) {
      // http://en.wikipedia.org/wiki/Cubic_Hermite_spline
      var ndata = [];
      var len   = data.length;
      var res   = 25;

      for (var k=0; k<len-1; k+=1) {
        var x    = data[k][0];
        var xk_1 = (data[k-1] || [0])[0];
        var yk_1 = (data[k-1] || [0,0])[1];
        var xk   = data[k][0];
        var yk   = data[k][1];
        var xk1  = data[k+1][0];
        var yk1  = data[k+1][1];
        var xk2  = (data[k+2] || [0])[0];
        var yk2  = (data[k+2] || [0,0])[1];
        var s    = (xk1 - xk)/res;
        for (var u=0; u<res; u+=1) {
          x += s;
          ndata.push(cspline_i(x, xk_1, yk_1, xk, yk, xk1, yk1, xk2, yk2));
        }
      }

      return(ndata);
    };

    var format_s = function (json) {
      var series = [];
      for (var k in json.results) {
        if (json.results.hasOwnProperty(k)) {
          series.push({ label: k,
                        data: cspline(json.results[k].series),
                      });
        }
      }
      return(series);
    };

    var install = function (json) {
        var series    = format_s(json);
        var container = document.getElementById(root);
        var myopts    = { xaxis: { mode: "time",
                                   timeUnit: "second",
                                   timeFormat: "%d %b %H:%M",
                                 },
                          yaxis: { min: 0,
                                   autoscale: true
                                 },
                          title: options.title || (json.debug.request_uri),
                          subtitle: options.subtitle || "Powered by locaweb",
                          selection: { mode: "x"
                                     },
                          legend: { position: "ne"
                                  },
                        };
        var resetZoom = function () {
            delete myopts.xaxis.min; delete myopts.xaxis.max;
        };

        Flotr.draw(container, series, myopts);

        Flotr.EventAdapter.observe(container, 'flotr:select', function (area) {
          myopts.xaxis.min = area.x1; myopts.xaxis.max = area.x2;
          Flotr.draw(container, series, myopts);
        });

        Flotr.EventAdapter.observe(container, 'flotr:click', function () { resetZoom(); Flotr.draw(container, series, myopts); });
    };

    return({"install": install});
}
