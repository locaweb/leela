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

        options.xaxis.tickFormatter = options.xaxis.tickFormatter || function(n) {
            var d = new Date(n*1000);
            return ("0" + d.getHours()).slice(-2) + ":" + ("0" + d.getMinutes()).slice(-2);
        };

        function resetZoom () {
            delete options.xaxis.min; delete options.xaxis.max;
            delete options.yaxis.min; delete options.yaxis.max;
        }

        var container = document.getElementById(root);

        Flotr.draw(container, json, options);

        Flotr.EventAdapter.observe(container, 'flotr:select', function (area) {
            options.xaxis.min = area.x1; options.xaxis.max = area.x2;
            options.yaxis.min = area.y1; options.yaxis.max = area.y2;
            Flotr.draw(container, json, options);
        });

        Flotr.EventAdapter.observe(container, 'flotr:click', function () { resetZoom(); Flotr.draw(container, json, options); });
    };

    return({"install": install});
}
