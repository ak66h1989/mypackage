    $(document).ready(function(){
      $("#submitbutton").on("click", function(){


    var req=ocpu.call("dy", {
      title : $("#mytitle").val()

    },function(session){
      $("iframe.dy").attr('src', session.getFileURL("dy.html"));

      }
      );
    });
    });
