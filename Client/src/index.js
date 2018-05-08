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

app.ports.fileSelected.subscribe(function (id) {
    var node = document.getElementById(id);
    if (node === null) {
        return;
    }
        // If your file upload field allows multiple files, you might
        // want to consider turning this into a `for` loop.
    var file = node.files[0];
        // FileReader API is event based. Once a file is selected
        // it fires events. We hook into the `onload` event for our reader.

	var imageType = /image.*/;

	if (file.type.match(imageType)) {
        var reader = new FileReader();

        reader.onload = (function(event) {
            // The event carries the `target`. The `target` is the file
            // that was selected. The result is base64 encoded contents of the file.
            var base64encoded = event.target.result;
                // We build up the `ImagePortData` object here that will be passed to our Elm
                // runtime through the `fileContentRead` subscription.
            var portData = {
                contents: base64encoded,
                filename: file.name
            };
                // We call the `fileContentRead` port with the file data
                // which will be sent to our Elm runtime via Subscriptions.
            app.ports.fileContentRead.send(portData);
            });
            // Connect our FileReader with the file that was selected in our `input` node.
        reader.readAsDataURL(file);
    }
 });