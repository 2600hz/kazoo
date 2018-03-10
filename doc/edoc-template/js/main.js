$('.nav-flip').on('click', function() {
    const activeEl = $('.entry-app.active');
    const targetId = $(this).data('targetId');

    if (!targetId) {
        return
    }

    if (activeEl.length !== 0) {
        const activeId = activeEl.attr('id');
        if (activeId && (activeId !== targetId))
        activeEl.removeClass('active')
    }

    $('#' + targetId).toggleClass('active');
});
