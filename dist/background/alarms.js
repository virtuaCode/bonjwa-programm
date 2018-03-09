let alarmTitle, alarmMessage;

function handleMessage(request, sender, sendResponse) {
    if (request.action === 'set-alarm') {

        const { title, message, timestamp } = request.data;

        alarmTitle = title;
        alarmMessage = message;

        browser.alarms.create('bonjwa-programm', {
            when: timestamp
        });

        sendResponse({
            data: {
                when: timestamp
            }
        });

    } else if (request.action === 'get-alarm') {
        const alarmPromise = browser.alarms.get('bonjwa-programm').then(alarm => {
            const when = alarm ? alarm.scheduledTime : null;

            return {
                data: {
                    when: when
                }
            }
        });

        return alarmPromise;

    } else if (request.action === 'clear-alarm') {
        const alarmPromise = browser.alarms.clear('bonjwa-programm').then(res => {
            return {
                data: {
                    when: null
                }
            }
        });

        return alarmPromise;
    }
}

function handleAlarm(alarm) {
    if (alarm.name === 'bonjwa-programm') {
        const notify = browser.notifications.create({
            type: "basic",
            iconUrl: browser.extension.getURL("icons/icon48.png"),
            title: alarmTitle,
            message: alarmMessage
        });

        notify.then(id => {
            browser.notifications.onClicked.addListener(clickedId => {
                if (id === clickedId) {
                    browser.tabs.create({
                        url: 'https://www.twitch.tv/bonjwa'
                    });
                }
            })
        });
    }
}

browser.runtime.onMessage.addListener(handleMessage);
browser.alarms.onAlarm.addListener(handleAlarm);