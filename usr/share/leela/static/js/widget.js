var LEELA;
if (LEELA === undefined) {
  LEELA = {};
}

LEELA.widget = function (root, options) {
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
    new Highcharts.Chart({
      chart: {
        renderTo: root,
        type: "spline",
        zoomType: "x"
      },

      title: {
        text: json.source.hostname + " - " + json.source.service
      },

      subtitle: {
        text: "Powered by locaweb"
      },

      xAxis: {
        type: "datetime",
        tickInterval: (24 * 60 * 60 * 1000) / res,
        minInterval: (60 / res) * 1000,
      },

      yAxis: {
        title: {
          text: json.source.service
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
