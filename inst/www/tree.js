    $(document).ready(function(){
      $("#submitbutton").on("click", function(){

        //disable the button to prevent multiple clicks
        $("#submitbutton").attr("disabled", "disabled");

        //read the value for 'myname'
        var nfield = parseInt($("#nfield").val());
        var dep = $("#dep").val();

        //create the plot area on the plotdiv element
        var req = $("#plotdiv").rplot("tree", {
          y : dep
        })
        //if R returns an error, alert the error message
        req.fail(function(){
          alert("Server error: " + req.responseText);
        });

        //after request complete, re-enable the button
        req.always(function(){
          $("#submitbutton").removeAttr("disabled")
        });
        
      
        
        
        
      });
    });
