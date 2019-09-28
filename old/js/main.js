$(document).ready(function() {
  hljs.initHighlightingOnLoad()

  if ($("#markdown").length == 1) {
      var editor = new EpicEditor({
          container: "markdown"
      }).load();
  }
})
