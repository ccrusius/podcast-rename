[![Build Status](https://travis-ci.org/ccrusius/podcast-rename.svg?branch=master)](https://travis-ci.org/ccrusius/podcast-rename)

A Podcast Renaming Application
==============================

Some podcasts have names with useless words in it (like "Podcast") which can't be
removed in some environments (iTunes specifically). Changing the title is a simple
matter replacing the `/rss/channel/title` element in the podcast's RSS feed XML, so
it is simple to do if you can intercept the XML before handling it over to your
podcast application.

This is what the [podcast-rename](http://podcast-rename.appspot.com "podcast-rename")
application did, but it died on me one day. This project is my replacment for
that app, which I have deployed for myself on my own server.

Installing
==========

Install Erlang/OTP. Generate the release by executing

> `./rebar get-deps compile generate`

Copy the generated `rel/podcast_rename` directory to wherever you want. Get
the web server started with `bin/podcast_rename start` from that directory.
By default, the web server listens on port 8080, but that can be changed
by setting the `PORT` variable before starting the application.

Make this server publicly available somewhere, so your iPhone/Android/etc
can reach it from anywhere.

Using
=====

Find the RSS link URL for your podcast. Decide on the new title. On your
podcast application, add a podcast with the URL

> `http://my.host:8080/rename?url=podcast_rss_url&title=new_title`

Make sure to quote the podcast URL properly, for example replacing
`=` by `%3D` and so on.

You're done.
