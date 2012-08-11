var LEELA;

if (LEELA === undefined) {
    LEELA = {};
}

LEELA.widget = function (root, opts) {
    var options = opts || {};

    var install = function (json) {
        options.selection = { mode : 'x', fps : 30 };
        options.title = options.title || (json.source.hostname + " - " + json.source.service);
        options.subtitle = options.subtitle || "Powered by locaweb";

        options.xaxis = options.xaxis || {};
        options.yaxis = options.yaxis || {};

        options.xaxis.showMinorLabels = true;
        options.xaxis.timeUnit = 'hour';
        //options.xaxis.noTicks = 10;
        options.xaxis.tickFormatter = function(n) {
            var d = new Date(n*1000);
            return d.getHours() + ":" + d.getMinutes();
        };

        options.yaxis.min = 0;

        var container = document.getElementById(root);

        function drawGraph (opts) {
            // Clone the options, so the 'options' variable always keeps intact.
            var o = Flotr._.extend(Flotr._.clone(options), opts);
            return Flotr.draw(container, json, o);
        }

        drawGraph();

        Flotr.EventAdapter.observe(container, 'flotr:select', function (area) {
            graph = drawGraph({
                xaxis: {min:area.x1, max:area.x2},
                yaxis: {min:area.y1, max:area.y2}
            });
        });

        Flotr.EventAdapter.observe(container, 'flotr:click', function () { drawGraph(); });
    };

    return({"install": install});
}
