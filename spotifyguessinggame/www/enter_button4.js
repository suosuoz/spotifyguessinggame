$(document).keyup(function(event) {
    if ($("#gta_ui_playlist").is(":focus") && (event.key == "Enter")) {
        $("#gta_but_submit_playlist").click();
    }
});