function workbook() {
    var search_exp = $("#workbookq").val();
    $.ajax({
        dataType: "html",
        url: "/workbook/q/?attrs=italian+english&search="+encodeURIComponent(search_exp),
        success: function (content) {
            $("#workbooka").prepend(content);
            $("#workbookq").focus();
        }
    });
}

