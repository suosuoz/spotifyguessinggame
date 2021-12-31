$(document).keyup(function(event) {
    if ($("#gta_ui_numsongs").is(":focus") && (event.key == "Enter")) {
        $("#gta_but_submit_numsongs").click();
    }
});