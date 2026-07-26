# File Cache


Septum maintains files and directory contents in memory to speed
searches.  Anecdotally, this results in ~100 MiB per 1 million
lines of code.

Adding directories causes septum to recursively add every file
which looks like text to the search pool. Looks like covers
popularly known extensions (.txt, .cpp, .rs, etc.) while
ignoring other known binary extensions (.jpg, .png, .zip).

If a file's extensions don't match the built-in filters, then
the first 4 KiB of characters are loaded and the file is
considered text if a null byte is not found.

Normally, directories get added for search, and then every file
is evaluated in turn to decide whether or not it should be loaded.
`add-files` provides a mechanism to add specific files, while not
loading the containing directory.

`add-dirs` is the primary mechanism through which files get added
for search.

`add-files` provides a mechanism for target loads, such as for logfiles,
or otherwise isolated files.

`clear-dirs`
removes all search directories and their contents from the file cache.
Files added directly via `add-files`
are not affected.

`clear-files`
removes files directly added to the file cache.  Files discovered
by recursive directory search are unaffected.

`list-dirs`
lists all directories which recursively get traversed looking
for files to add to the file cache.

`list-files`
List the files of the search list.
Supports an optional 'full' argument, otherwise the number
of printed files is capped.

Septum currently doesn't track updates to files to
loaded directories.

`reload`
provides the means to update all currently loaded files with the
current contents on disk.

`reload`
also provides the counterpart to `unload` which is used to drop
the file cache.

Anecdotally, septum uses ~100 MB per million lines of code loaded
for search. When dealing with extremely large amounts of text this
can interfere with other operations.  Instead of shutting down the
program, instead you can
`unload`
the data set, do whatever operations
you need and then
`reload`
to bring the files back for search.

`stats`
septum maintains all search context in memory within the file cache.
Due to the large amount of text that can be loaded, it can be useful
to examine where and how this storage is used.

# Line Filters

`find-text` TEXT... provides a case-sensitive filter

`find-like` TEXT ... provides a case-insensitive filter

Each space separated text parameter to
& `find-text`
is treated as an additional filter. This supports applying
multiple text filters and then being able to manipulate individual
ones using commands like
& `drop`
and
& `reorder`

`exclude-regex`
provides a regex to exclude.

`find-regex`
provides a regex to search for.



`clear-line-filters`
Removes all line filters, while maintaining currently
excluded file extensions and path filters.



`list-line-filters`
provides a case-insensitive filter.



`reorder`
Sometimes you have the right filters, but want to reorder them
so it's easier to pop specific ones, or to better organize them.


`drop`
drops given filters, or the most recent filter if non given.



`pop`
sometimes the filter you just applied is too restrictive
or you want to try a different approach.
let's you remove the most recently applied line filter.



`test`
provides a mechanism to see why a specific line is getting
matched or excluded from a search.  This command shows how
each filter matches against a line of text.


# Path Filters


All files are considered during the search, unless specific paths
are requested to be found.
& `find-path`
restricts the search to only files which match this filter.


Path filtering occurs at both the path and the extension level.
Paths containing specific elements can be removed, and the results
can also be tailored to only produce results which match specific
file extensions.



All paths are considered search candidates unless a find-path is
provided.  This means
& `exclude-path`
can be used as an axe to more easily shave off entire portions
of the search space.



`clear-exts` Clears extension filters.


`clear-path-filters` Removes all path filters.

`exclude-path` - Provides path elements to exclude from the search.

`list-exts` - Lists extensions to filter by

`list-path-filters`
Lists the currently bound path filters



`only-exts`
Adds extension to the search list



`remove-exts`
Removes extension from the search list

# Results

Lists the Contexts currently matching all filters.

New_Line;
match-contexts        Prints up to max-results results
match-contexts N      Prints the first N results
match-contexts M N    Prints the M ... N results


`match-files`
lists all files which match the current filters.
This command is particularly useful to determine if path filters
would be effective to cull search results.



Due to partial matching of commands, the abbreviated versions
of match commands are often used instead of the full ones.

`set-context-width [COUNT]` - Sets the number of lines form the search neighborhood above and below a search term.

`set-max-results [COUNT]` - 
Sets the maximum results printed before truncating.


`enable-auto-search` - Automatically search when filters are updated.

`enable-line-numbers` - Whether to print line numbers in results.

`enable-line-colors` - Whether to colorize lines with found search terms.

# System

Septum is designed as a interactive search application.
In typical usage, the program remains 'live' in the background
in a separate tmux tab or terminal.

`reload`
is needed to update text files during heavy edits or when
rebasing during the day.

`enable-timing`
enables reporting of time it takes to run commands.

`disable-timing`
disables reporting of time it takes to run commands.
