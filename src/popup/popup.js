const DAY_NAMES = ['Sonntag', 'Montag', 'Dienstag', 'Mittwoch', 'Donnerstag', 'Freitag', 'Samstag'];
const BROADCASTS_URL = 'https://bnjw.viceair.com/broadcasts'; 
let globalBroadcasts = [];
let currentDate = new Date();
currentDate.setHours(0, 0, 0, 0);

document.addEventListener('DOMContentLoaded', () => {
  init();
});

function init() {
  hideStatus();
  refreshDate();
  initEventListeners();
  loadContent();
}

function loadContent() {
  showStatus('LOADING...');
  fetch(BROADCASTS_URL, {
    method: 'get'
  }).then((response) => {
    if (response.status !== 200) {
      console.error('wrong status code', response.status);
      showStatus('Serveranfrage ist fehlgeschlagen!');
      return;
    }

    response.json().then(body => {
      const broadcasts = body.data.map(bc => {
        const { id, start, end, topic } = bc;
        return { id, start: new Date(start), end: new Date(end), topic };
      });
      hideStatus();
      setBroadcasts(broadcasts);
    }).catch(err => {
      showStatus('Ungültige Antwort vom Server!');
      console.error('invalid response body');
    });
  }).catch((err) => {
    showStatus('Verbindungsaufbau zum Server ist fehlgeschlagen!')
    console.error(err);
  });
}

function initEventListeners() {
  const next = document.getElementById('next');
  const prev = document.getElementById('prev');
  next.addEventListener('click', onClickNext);
  prev.addEventListener('click', onClickPrev);

  document.addEventListener('keydown', (event) => {
    const keyName = event.key;

    if (keyName === 'ArrowRight') {
      return onClickNext();
    } else if (keyName === 'ArrowLeft') {
      return onClickPrev();
    }
  }, false);
}

function setBroadcasts(broadcasts) {
  globalBroadcasts = broadcasts;
  refreshBroadcasts();
}

function refreshBroadcasts() {
  const dayStart = new Date(currentDate.getTime());
  const dayEnd = new Date(currentDate.getTime());
  dayEnd.setDate(dayEnd.getDate() + 1);

  const bcs = globalBroadcasts.filter(bc => {
    return bc.start >= dayStart && bc.end <= dayEnd;
  });

  const fragment = buildBroadcastsFragment(bcs);
  const table = document.getElementById('table');

  empty(table);

  table.appendChild(fragment);
  scrollToActive();
}

function buildBroadcastsFragment(broadcasts) {

  const fragment = document.createDocumentFragment();
  const now = new Date();

  for (const broadcast of broadcasts) {

    const live = now >= broadcast.start && now < broadcast.end;

    const row = document.createElement('div');
    row.className = 'row' + (live ? ' live' : '');

    const left = document.createElement('div');
    left.className = 'left';

    const right = document.createElement('div');
    right.className = 'right';

    const time = document.createElement('div');
    time.className = 'time';
    time.innerText = broadcast.start.getHours() + ':00 - ' + broadcast.end.getHours() + ':00';

    const topic = document.createElement('div');
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

function scrollToActive() {
  const table = document.getElementById('table');
  const live = table.querySelector('.row.live')

  table.scrollTop = 0;

  if (live) {
    const topPos = live.offsetTop;
    table.scrollTop = topPos;
  }
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
  currentDate.setDate(currentDate.getDate() + 1);
}

function prevDay() {
  currentDate.setDate(currentDate.getDate() - 1);
}

function refreshDate() {
  const day = document.getElementById('day');
  day.innerText = DAY_NAMES[currentDate.getDay()] + ', ' + currentDate.getDate() + '.' + (currentDate.getMonth() + 1) + '.' + currentDate.getFullYear();
}

function showStatus(message) {
  const status = document.getElementById('status');
  status.className = '';
  status.innerText = message;
}

function hideStatus() {
  const status = document.getElementById('status');
  status.className = 'hidden';
}

function empty(e) {
  let last;
  while (last = e.lastChild) e.removeChild(last);
}