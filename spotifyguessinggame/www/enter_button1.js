$(document).keyup(function(event) {
    if ($("#gta_ui_guess").is(":focus") && (event.key == "Enter")) {
        $("#gta_but_submit_guess").click();
    }
});