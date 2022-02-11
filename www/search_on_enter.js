
$(document).keyup(function(event) {
    if ($("#addr").is(":focus") && (event.key == "Enter")) {
        $("#submit").click();
    }
});
