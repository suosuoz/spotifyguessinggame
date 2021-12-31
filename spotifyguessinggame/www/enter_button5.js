$(document).keyup(function(event) {
    if ($("#gts_ui_playlist").is(":focus") && (event.key == "Enter")) {
        $("#gts_but_submit_playlist").click();
    }
});