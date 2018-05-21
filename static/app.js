function webSocketTest(name) {
    var ws = new WebSocket("ws://localhost:3000");
    ws.onopen = () => {
        ws.send(name);
    };
    ws.onmessage = evt => {
        var m = evt.data;
        log.textContent += (m + '\n');
    };
    ws.onclose = function () {
        log.textContent += ("ws closed" + "\n");
    };
    window.onbeforeunload = evt => {
        socket.close();
    };
    return ws;
}
const connect = document.getElementById("connect");
const input = document.getElementById("input");
const submit = document.getElementById("submit");
const log = document.getElementById("log");
let ws;
connect.addEventListener('click', evt => {
    const name = input.value;
    input.value = "";
    ws = webSocketTest(name);
});
submit.addEventListener('click', evt => {
    const data = input.value;
    input.value = "";
    ws.send(data);
})
