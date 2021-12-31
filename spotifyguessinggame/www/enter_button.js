$(document).keyup(function(event) {
    if ($("#gts_ui_guess").is(":focus") && (event.key == "Enter")) {
        $("#gts_but_submit_guess").click();
    }
});