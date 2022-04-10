# bookmark_manager

A bookmark manager in Prolog.

This is a simple bookmark manager. You can open a specific URL with `lookup(URL)` or choose a random URL with
`randomlookup`.

It saves its data in a Prolog file called `url.pl`. To initialize this file, call `save`. This must also be called
after viewing bookmarks to update the data in the file.

