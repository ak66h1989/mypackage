$(function(){
  $("#submitbutton").click(function(e){
    e.preventDefault();
    var btn = $(this).attr("disabled", "disabled");
    var req = ocpu.call("dy", {
      title : $("#mytitle").val()

    }, function(session){
      $("iframe.dy").attr('src', session.getFileURL("dy.html"));

    }).fail(function(text){
      alert("Error: " + req.responseText);
    }).always(function(){
      btn.removeAttr("disabled");
    });

    var req1 = ocpu.call("DT", {


    }, function(){

      $("iframe.DT").attr('src', getFileURL("DT.html"));
    }).fail(function(text){
      alert("Error: " + req1.responseText);
    }).always(function(){
      btn.removeAttr("disabled");
    });


  });
});
