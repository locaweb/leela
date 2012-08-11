var LEELA;
if (LEELA === undefined) {
  LEELA = {};
}

LEELA.widget = function (root, opts) {
  var options = opts || {};
  Highcharts.setOptions({
    global: {
      useUTC: false
    }
  });

  var to_milliseconds = function (data) {
    var k;
    for (k=0; k<data.length; k+=1) {
      data[k][0] = data[k][0] * 1000;
    }
    return(data);
  };

  var getseries = function (json) {
    var series = [];
    for (var k in json) {
      if (json.hasOwnProperty(k) && k !== "source") {
        series.push({ name: k,
                      data: to_milliseconds(json[k])
                    });
      }
    }
    return(series);
  };

  var install = function (json) {
    var res = options.resolution || 1;
    var sub = options.subtitle || "Powered by locaweb";
    var title  = options.title || (json.source.hostname + " - " + json.source.service);
    var ylabel = options.ylabel || json.source.service;
    new Highcharts.Chart({
      chart: {
        renderTo: root,
        type: "spline",
        zoomType: "x"
      },

      title: {
        text: title
      },

      subtitle: {
        text: sub
      },

      xAxis: {
        type: "datetime",
        tickInterval: (24 * 60 * 60 * 1000) / res,
        minInterval: (60 / res) * 1000,
      },

      yAxis: {
        title: {
          text: ylabel
        },
        min: 0
      },

      tooltip: {
        enabled: true,
        formatter: function() {
          var date = Highcharts.dateFormat("%d %b %H:%M", this.x);
          var fmt  = "<b>"            +
                     this.series.name +
                     "</b><br />"     +
                     date             +
                     " -> "           +
                     this.y;
          return(fmt);
        }
      },

      series: getseries(json)
    });
  };

  return({"install": install
         });
}
