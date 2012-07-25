var LEELA;
if (LEELA === undefined) {
  LEELA = {};
}

LEELA.widget = function (root) {
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
    new Highcharts.Chart({
      chart: {
        renderTo: root,
        type: "spline"
      },

      title: {
        text: json.source.hostname + " - " + json.source.service
      },

      subtitle: {
        text: "Powered by locaweb"
      },

      xAxis: {
        type: "datetime",
        tickInterval: 2 * 3600 * 1000,
        tickWidth: 0,
        gridLineWidth: 1
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
          var date = Highcharts.dateFormat("%A %B %e %Y %H:%M:%S", this.x);
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

  return({"install": install});
}
