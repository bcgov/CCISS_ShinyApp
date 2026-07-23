$(document).ready(function() {

  // Back to top
	$(".back-to-top").on("click", function(e) {
		e.preventDefault();
    $('html,body').animate({ scrollTop: 0 }, 'slow');
	});
});

function findSpan(spanLabel) {
  var spans = $('.leaflet-control-layers-base').find('span');
  $.each(spans, function(index) {
    if ($(this).text().trim() == spanLabel) {
      $(this).prev().click();
    }
  });
}

