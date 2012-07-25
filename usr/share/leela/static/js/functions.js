var LEELA;
if (LEELA === undefined) {
  LEELA = {};
}

LEELA.f = (function () {

  var metric_p = function (name) {
    return(name !== "source");
  };

  var fst = function (xs) {
    return(xs[0]);
  };

  var snd = function (xs) {
    return(xs[1]);
  };

  var datapoint_timestamp = fst;

  var datapoint_value = snd;

  var foldl = function (xs, z, f) {
    for (var k in xs) {
      if (xs.hasOwnProperty(k))
        z = f(xs[k], z);
    }
    return(z);
  };

  var id = function (x) {
    return(x);
  };

  var cons = function (x, xs) {
    xs.push(x);
    return(xs);
  };

  var snoc = function (x, xs) {
    xs.unshift(x);
    return(xs);
  };

  var map = function (xs, f) {
    return(foldl(xs, [], function (x, z) {
      return(snoc(f(x), z));
    }));
  };

  var sum = function (xs) {
    return(foldl(xs, 0, function (x, z) {
      return(x + z);
    }));
  };

  var _group = function (resolution, zero, datapoints) {
    var g = [];
    var r = [];
    var z = undefined;
    var s = 60;

    var process = function (d) {
      g.push(d);
      if (g.length == resolution) {
        r.push([datapoint_timestamp(g[0]), (sum(map(g, snd)) / resolution)]);
        g = [];
      }
      return(g);
    };

    for (var k in datapoints) {
      if (datapoints.hasOwnProperty(k)) {
        var d = datapoints[k];
        var t = datapoint_timestamp(d);
        var v = datapoint_value(d);
        z = (z === undefined ? t : z + s);
        while (z !== t) {
          process([z, zero]);
          z += s;
        }
        process([t, v]);
      }
    }
    console.log(r.length);

    return(r);
  };

  var average = function (resolution, zero) {
    var f = function (json) {
      for (var m in json) {
        if (json.hasOwnProperty(m) && metric_p(m)) {
          json[m] = _group(resolution, zero, json[m]);
        }
      }
      return(json);
    };
    return(f);
  };

  var dot = function (f, g) {
    var h = function () {
      var to_a = Array.prototype.slice
      return(f(g.apply(null, to_a.call(arguments))));
    };
    return(h);
  };

  return({ average: average,
           dot: dot
         });

})();
