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
      return([x, h00*yk + h10*(xk1-xk)*m0 + h01*yk1 + h11*(xk1-xk)*m1]);
    };

    var cspline = function (data) {
      var ndata = [];
      var len   = data.length;
      var res   = 50;

      for (var k=0; k<len-1; k+=1) {
        for (var u=1; u<res; u+=1) {
          var x = k + u/res;
          ndata.push(cspline_i(x,
                               k-1,
                               (data[k-1] || [0,0])[1],
                               k,
                               data[k][1],
                               k+1,
                               data[k+1][1],
                               k+2,
                               (data[k+2] || [0,0])[1]));
        }
      }

      return(ndata);
    };

    var format = function (json) {
      var series = [];
      for (var k in json) {
        if (k!=="source" && json.hasOwnProperty(k)) {
          series.push({ label: k,
                        data: cspline(json[k])
                      });
        }
      }
      console.log(series);
      return(series);
    };

    var install = function (json) {
        var series    = format(json);
        var container = document.getElementById(root);
        var myopts    = { xaxis: {mode: "normal"},
                          yaxis: {},
                          title: options.title || (json.source.hostname + " - " + json.source.service),
                          subtitle: options.subtitle || "Powered by locaweb",
                          selection: {mode: "x"}
                        };
        var resetZoom = function () {
            delete myopts.xaxis.min; delete myopts.xaxis.max;
            delete myopts.yaxis.min; delete myopts.yaxis.max;
        };

        Flotr.draw(container, series, myopts);

        Flotr.EventAdapter.observe(container, 'flotr:select', function (area) {
          myopts.xaxis.min = area.x1; myopts.xaxis.max = area.x2;
          myopts.yaxis.min = area.y1; myopts.yaxis.max = area.y2;
          Flotr.draw(container, series, myopts);
        });

        Flotr.EventAdapter.observe(container, 'flotr:click', function () { resetZoom(); Flotr.draw(container, series, myopts); });
    };

    return({"install": install});
}
