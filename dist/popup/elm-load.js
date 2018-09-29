document.addEventListener('DOMContentLoaded', function () {
    const mainDiv = document.getElementById('main');
    const app = Elm.Popup.init({
        node: mainDiv,
        flags: 1
    });

    app.ports.openTab.subscribe(function (url) {
        const creating = browser.tabs.create({
            url: url
        });
    });

    app.ports.setAlarm.subscribe(function (data) {

        const sending = browser.runtime.sendMessage({
            action: 'set-alarm',
            data: data
        });

        sending.then(
            (res) => {
                const when = res.data.when;
                app.ports.receiveAlarm.send(when);
            },
            (err) => {
                console.error('Failed to receive setAlarm response');
            }
        );
    });

    app.ports.getAlarm.subscribe(function () {

        const sending = browser.runtime.sendMessage({
            action: 'get-alarm'
        });

        sending.then(
            (res) => {
                const when = res.data.when;
                app.ports.receiveAlarm.send(when);
            },
            (err) => {
                console.error('Failed to receive getAlarm response');
            }
        );
    });

    app.ports.clearAlarm.subscribe(function () {

        const sending = browser.runtime.sendMessage({
            action: 'clear-alarm'
        });

        sending.then(
            (res) => {
                const when = res.data.when;
                app.ports.receiveAlarm.send(when);
            },
            (err) => {
                console.error('Failed to receive clearAlarm response');
            }
        );
    });
});