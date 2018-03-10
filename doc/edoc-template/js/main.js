$('.nav-flip').on('click', function() {
    $('.entry-app.active').removeClass('active');
    const targetApp = $(this).data('kzApp');
    $('#nav-app-' + targetApp).addClass('active');
});
