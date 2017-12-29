let globalBroadcasts = [];
let currentDay = new Date();
currentDay.setHours(0, 0, 0, 0);

document.addEventListener('DOMContentLoaded', () => {
  init();
});

function init() {
  refreshDate();
  initEventListeners();
  loadContent();
}

function loadContent() {
  showStatus('LOADING...');
  fetch("https://bnjw.viceair.com/broadcasts", {
    method: 'get'
  }).then(function (response) {
    if (response.status !== 200) {
      console.error("wrong status code", response.status);
      showStatus('Serveranfrage ist fehlgeschlagen!');
      return;
    }

    response.json().then(body => {
      globalBroadcasts = body.data.map(bc => {
        const { id, start, end, topic } = bc;
        return { id, start: new Date(start), end: new Date(end), topic };
      });
      hideStatus();
      refreshBroadcasts();
    }).catch(err => {
      showStatus('Invalide Antwort vom Server!');
      console.error('invalid response body');
    });
  }).catch(function (err) {
    showStatus('Verbindungsaufbau zum Server ist fehlgeschlagen!')
    console.error(err);
  });
}

function initEventListeners() {
  const next = document.getElementById('next');
  const prev = document.getElementById('prev');
  next.addEventListener('click', onClickNext);
  prev.addEventListener('click', onClickPrev);
}

function refreshBroadcasts() {
  const dayStart = new Date(currentDay.getTime());
  const dayEnd = new Date(currentDay.getTime());
  dayEnd.setDate(dayEnd.getDate() + 1);

  const bcs = globalBroadcasts.filter(bc => {
    return dayStart <= bc.start && dayEnd >= bc.end;
  });

  const fragment = buildBroadcastsFragment(bcs);
  const table = document.getElementById('table');

  empty(table);

  table.appendChild(fragment);
}

function buildBroadcastsFragment(broadcasts) {

  const fragment = document.createDocumentFragment();
  const now = new Date();

  for (const broadcast of broadcasts) {

    const live = now >= broadcast.start && now < broadcast.end;

    const row = document.createElement("div");
    row.className = 'row' + (live ? ' live' : '');

    const left = document.createElement("div");
    left.className = 'left';

    const right = document.createElement("div");
    right.className = 'right';

    const time = document.createElement("div");
    time.className = 'time';
    time.innerText = broadcast.start.getHours() + ":00 - " + broadcast.end.getHours() + ":00";

    const topic = document.createElement("div");
    topic.className = 'topic';
    topic.innerText = broadcast.topic;

    left.appendChild(time);
    right.appendChild(topic);

    row.appendChild(left);
    row.appendChild(right);

    fragment.appendChild(row);
  }

  return fragment;
}

function onClickNext() {
  nextDay();
  refreshDate();
  refreshBroadcasts();
}

function onClickPrev() {
  prevDay();
  refreshDate();
  refreshBroadcasts();
}

function nextDay() {
  currentDay.setDate(currentDay.getDate() + 1);
}

function prevDay() {
  currentDay.setDate(currentDay.getDate() - 1);
}

function refreshDate() {
  const day = document.getElementById('day');
  day.innerText = currentDay.getDate() + '.' + (currentDay.getMonth() + 1) + '.' + currentDay.getFullYear();
}

function showStatus(message) {
  const status = document.getElementById('status');
  status.className = 'show';
  status.innerText = message;
}

function hideStatus() {
  const status = document.getElementById('status');
  status.className = '';
}

function empty(e) {
  let last;
  while (last = e.lastChild) e.removeChild(last);
}