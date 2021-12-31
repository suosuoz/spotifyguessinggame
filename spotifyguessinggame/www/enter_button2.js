$(document).keyup(function(event) {
    if ($("#gts_ui_numsongs").is(":focus") && (event.key == "Enter")) {
        $("#gts_but_submit_numsongs").click();
    }
});