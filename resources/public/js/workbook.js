function workbook(prefix) {
    var search_exp = $("#workbookq").val();
    $.ajax({
        dataType: "html",
        url: prefix + "/q/?attrs=italian+english&search="+encodeURIComponent(search_exp),
        success: function (content) {
            $("#workbooka").prepend(content);
            $("#workbookq").focus();
        }
    });
}

