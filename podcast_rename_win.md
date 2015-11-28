[![Build Status](https://travis-ci.org/ccrusius/podcast-rename.svg?branch=master)](https://travis-ci.org/ccrusius/podcast-rename)

Detailed steps for Windows
==========================
<br>
1) Clone or download the [rebar repository](https://github.com/rebar/rebar)

2) Download [Erlang/OTP](http://www.erlang.org/download.html)

3) Add Erlang/OTP path (eg: C:\Program Files\erl7.1\bin) as Windows Path Environment Variable

4) From command line, move to "rebar-master" folder and run `bootstrap.bat` (*)(**)

5) Copy the generated `rebar` and `rebar.cmd` files to the "podcast-rename-master" folder

5) Clone or download the [podcast rename repository](https://github.com/ccrusius/podcast-rename)

6) From command line, move to "podcast-rename-master" folder and run `rebar get-deps compile generate`

5) From command line, move to ".\rel\podcast_rename\bin" and start the service by typing:

* podcast_rename.cmd install
* podcast_rename.cmd start

Be sure to launch the previous statements as an administrator

6) Test it by  opening a browser session, and entering the url: <IP server address>:8080

As stated the `url` and `title` should be quoted properly. You can use the [following](http://www.w3schools.com/tags/ref_urlencode.asp) to do it.

<sub>(\*) You can add a pause statement to look at the output<br>
(\**) An error of missing `rebar.beam` file may throw, you can ignore it
</sub>
