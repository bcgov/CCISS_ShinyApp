$("body").on("shown.bs.tab", "a[data-toggle='tab']", function(e) {
  Shiny.setInputValue("active_tab", $(e.target).parent().index());
})

Shiny.addCustomMessageHandler("jsCode",
  function(message) {
    console.log(message)
    eval(message.code);
  }
);