document.addEventListener('DOMContentLoaded', function() {
    const div = document.getElementById('main');
    var app = Elm.Popup.embed(div);

    app.ports.openTab.subscribe(function (url) {
        var creating = browser.tabs.create({
            url: url
        });
    });
});