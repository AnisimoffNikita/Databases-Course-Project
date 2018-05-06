import './main.css';
import { Main } from './Main.elm';
import registerServiceWorker from './registerServiceWorker';

var storedState = localStorage.getItem('model');
var startingState = storedState ? JSON.parse(storedState) : null;

var app = Main.embed(document.getElementById('root'), storedState);

registerServiceWorker();

app.ports.setStorage.subscribe(function(session) {
        localStorage.setItem('model', JSON.stringify(session));
    });