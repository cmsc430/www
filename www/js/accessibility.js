// Adds the web accessibility link to the bottom of every page

// change to true to mark pages as stale
var is_stale = false;

function AddAccessibilityToMainDiv()
{
    var main_div = document.getElementsByClassName("main")[0];
    var h = document.createElement('div');
    h.setAttribute("class", "accessibility-link");
    h.innerHTML = "<a href=\"https://www.umd.edu/web-accessibility\" title=\"UMD Web Accessibility\">Web Accessibility</a>";
    main_div.insertBefore(h, main_div.lastChild);

    if (is_stale) {
	var g = document.createElement('div');
	g.setAttribute("class", "stale-warning");
	g.innerHTML = "This is NOT the current webpage! See " +
            "<a href=\"http://www.cs.umd.edu/class/\">Class web pages</a> for current listing.";
	main_div.insertBefore(g, main_div.firstChild);
    };
}
AddOnLoad(AddAccessibilityToMainDiv);
