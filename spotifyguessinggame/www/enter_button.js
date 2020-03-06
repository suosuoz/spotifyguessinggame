$(document).keyup(function(event) {
    if ($("#txt1").is(":focus") && (event.key == "Enter")) {
        $("#guessbutton").click();
    }
});
