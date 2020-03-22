$(document).keyup(function(event) {
    if ($("#txt11").is(":focus") && (event.key == "Enter")) {
        $("#guessbutton1").click();
    }
});