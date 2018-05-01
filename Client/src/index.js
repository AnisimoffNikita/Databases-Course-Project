import './main.css';
import { Main } from './Main.elm';
import registerServiceWorker from './registerServiceWorker';

var storedState = localStorage.getItem('model');
var startingState = storedState ? JSON.parse(storedState) : null;

var app = Main.embed(document.getElementById('root'), startingState);

registerServiceWorker();



app.ports.setStorage.subscribe(function(state) {
        localStorage.setItem('model', JSON.stringify(state));
    });