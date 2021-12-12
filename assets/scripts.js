(function($) {
    "use strict";
    var toc = function(options) {
        return this.each(function() {
            var root = $('<ol/>').appendTo($(this)), data = root.data(), thisOptions, stack = [root], listTag = 'ol', currentLevel = 0, headingSelectors;
            thisOptions = $.extend({
                content: "body",
                headings: "h1,h2,h3"
            }, {
                content: data.toc || undefined,
                headings: data.tocHeadings || undefined
            }, options);
            headingSelectors = thisOptions.headings.split(",");
            $(thisOptions.content).find(thisOptions.headings).attr("id", function(index, attr) {
                var generateUniqueId = function(text) {
                    if (text.length === 0) {
                        text = "?";
                    }
                    var baseId = text.replace(/\s+/g, "_")
                      , suffix = ""
                      , count = 1;
                    while (document.getElementById(baseId + suffix) !== null) {
                        suffix = "_" + count++;
                    }
                    return baseId + suffix;
                };
                return attr || generateUniqueId($(this).text());
            }).each(function() {
                var elem = $(this)
                  , level = $.map(headingSelectors, function(selector, index) {
                    return elem.is(selector) ? index : undefined;
                })[0];
                if (level > currentLevel) {
                    var parentItem = stack[0].children("li:last")[0];
                    if (parentItem) {
                        stack.unshift($("<" + listTag + "/>").appendTo(parentItem));
                    }
                } else {
                    stack.splice(0, Math.min(currentLevel - level, Math.max(stack.length - 1, 0)));
                }
                $("<li/>").appendTo(stack[0]).append($("<a/>").text(elem.text()).attr("href", "#" + elem.attr("id")));
                currentLevel = level;
            });
            var header = $('<h3>').text('Contents').prependTo($(this));
        });
    }
      , old = $.fn.toc;
    $.fn.toc = toc;
    $.fn.toc.noConflict = function() {
        $.fn.toc = old;
        return this;
    }
    ;
    $(function() {
        toc.call($("[data-toc]"));
    });
}(window.jQuery));
;function userWantsDarkMode() {
    return localStorage.theme === 'dark' || (!('theme'in localStorage) && window.matchMedia('(prefers-color-scheme: dark)').matches);
}
function setTheme() {
    const buttons = document.querySelectorAll(".theme_btn");
    if (userWantsDarkMode()) {
        document.documentElement.classList.add('dark');
        buttons.forEach(function(el) {
            el.src = "/images/light-mode.svg";
        });
    } else {
        document.documentElement.classList.remove('dark');
        buttons.forEach(function(el) {
            el.src = "/images/dark-mode.svg";
        });
    }
}
setTheme();
window.addEventListener('storage', function() {
    setTheme();
});
$(function() {
    setTheme();
    $(".theme_btn").click(function(event) {
        var wasDark = userWantsDarkMode();
        if (wasDark) {
            localStorage.theme = "light";
        } else {
            localStorage.theme = "dark";
        }
        setTheme();
    });
    var mobilenav = $('#mobile-nav');
    $('html').click(function() {
        mobilenav.find('.on').each(function() {
            $(this).removeClass('on').next().hide();
        });
    });
    mobilenav.on('click', '.menu .button', function(e) {
        if (!$(this).hasClass('on')) {
            var width = $(this).width() + 70;
            $(this).addClass('on').next().css({
                width: width
            }).show();
        } else {
            $(this).removeClass('on').next().hide();
        }
    }).on('click', '.search .button', function(e) {
        if (!$(this).hasClass('on')) {
            var width = mobilenav.width() - 51;
            mobilenav.children('.menu').children().eq(0).removeClass('on').next().hide();
            $(this).addClass('on').next().show().css({
                width: width
            }).children().children().eq(0).focus();
        } else {
            $(this).removeClass('on').next().hide().children().children().eq(0).val('');
        }
    }).click(function(e) {
        e.stopPropagation();
    });
    var $nav = $('#main-nav .main');
    var toggleForm = function(button, form) {
        button.click(function() {
            if (form.is(':visible')) {
                form.fadeOut('fast');
            } else {
                form.fadeIn('fast');
                form.find('input').focus();
            }
        });
        form.find('input').keyup(function(e) {
            if (e.keyCode == 27) {
                form.fadeOut('fast');
            }
        });
    };
    toggleForm($('#search_btn'), $('.desk_search'));
    var $footnotes = $('.footnotes > ol > li');
    $('a[href^="#fn:"]').attr('title', 'read footnote').click(function() {
        $footnotes.stop(true, true);
        var note = $(this).attr('href');
        $footnotes.not(note.replace(/:/, "\\:")).css({
            opacity: 0.1
        }).animate({
            opacity: 1.0
        }, 15000, 'linear');
    });
    $('a[href^="#fnref"]').html('<i class="fas fa-level-up-alt"></i>').click(function() {
        $footnotes.stop(true, true);
    });
    var res = $('#toc').toc({
        content: 'div.entry-content',
        headings: 'h1,h2,h3,h4,h5'
    });
    $('article.post h1[id], article.post h2[id], article.post h3[id], article.post h4[id], article.post h5[id]').each(function() {
        var text = $(this).text();
        $(this).empty();
        $(this).append('<span class="hash">#</span>');
        $(this).append('<a href="#' + this.id + '" class="header-link">' + text + '</a>');
    });
});
