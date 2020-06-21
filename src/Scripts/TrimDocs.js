"use strict";

var cheerio = require('cheerio');


exports.loadImpl = function loadImpl (code) {
    return cheerio.load(code);
};

exports.selectImpl = function selectImpl (selector,$) {
    return $(selector);
};

exports.setHtmlImpl = function setHtmlImpl (code,c_) {
    c_.html(code);
};

exports.htmlImpl = function htmlImpl ($) {
    return $.html();
};
