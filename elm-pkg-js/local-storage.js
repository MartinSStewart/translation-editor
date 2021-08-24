// port local_storage_save_to_js : { key : String, value : String } -> Cmd msg
// port local_storage_request_load_to_js : { key : String } -> Cmd msg
// port local_storage_load_from_js : (Json.Decode.Value -> msg) -> Sub msg

exports.init = async function(app) {
    app.ports.local_storage_save_to_js.subscribe(function(data) {
        window.localStorage.setItem(data.key, data.value);
    });

    app.ports.local_storage_request_load_to_js.subscribe(function(data) {
        const value = window.localStorage.getItem(data.key);
        app.ports.local_storage_load_from_js.send({ key : data.key, value : value });
    });
}