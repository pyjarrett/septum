with Ada.Text_IO; use Ada.Text_IO;

package body SP.Help.Topics is

    procedure File_Cache is
    begin
        SP.Help.Header ("File Cache", "");

        SP.Help.Block (
            "Septum maintains files and directory contents in memory to speed"
            & " searches.  Anecdotally, this results in ~100 MiB per 1 million"
            & " lines of code."
        );

        SP.Help.Block (
            "Adding directories causes septum to recursively add every file"
            & " which looks like text to the search pool. ""Looks like"" covers "
            & "popularly known extensions (.txt, .cpp, .rs, etc.) while "
            & "ignoring other known binary extensions (.jpg, .png, .zip). "
        );
        SP.Help.Block (
            "If a file's extensions don't match the built-in filters, then "
            & "the first 4 KiB of characters are loaded and the file is "
            & "considered text if a null byte is not found."
        );

        SP.Help.Block (
            "Normally, directories get added for search, and then every file "
            & "is evaluated in turn to decide whether or not it should be loaded."
            & "`add-files` provides a mechanism to add specific files, while not "
            & "loading the containing directory."
        );

        SP.Help.Block (
            SP.Help.Colorize_Command ("add-dirs")
            & " is the primary mechanism through which files get added "
            & "for search."
        );

        SP.Help.Block (
            SP.Help.Colorize_Command ("add-files")
            & " provides a mechanism for target loads, such as for logfiles,"
            & " or otherwise isolated files."
        );

        SP.Help.Block (
            SP.Help.Colorize_Command ("clear-dirs")
            & " removes all search directories and their contents from the file cache."
            & " Files added directly via " & SP.Help.Colorize_Command ("add-files")
            & " are not affected."
        );

        SP.Help.Block (
            SP.Help.Colorize_Command ("clear-files")
            & " removes files directly added to the file cache.  Files discovered "
            & " by recursive directory search are unaffected."
        );

        SP.Help.Block (
            SP.Help.Colorize_Command ("list-dirs")
            & " lists all directories which recursively get traversed looking"
            & " for files to add to the file cache."
        );

        SP.Help.Block (
            SP.Help.Colorize_Command ("list-files")
            & "List the files of the search list."
            & " Supports an optional 'full' argument, otherwise the number"
            & " of printed files is capped."
        );

        SP.Help.Block ("Septum currently doesn't track updates to files to "
            & "loaded directories."
        );
        SP.Help.Block (
            SP.Help.Colorize_Command ("reload")
            & " provides the means to update all currently loaded files with the "
            & "current contents on disk."
        );
        SP.Help.Block (SP.Help.Colorize_Command ("reload")
            & " also provides the counterpart to `unload` which is used to drop "
            & "the file cache."
        );


        SP.Help.Block ("Anecdotally, septum uses ~100 MB per million lines of code loaded "
            & "for search. When dealing with extremely large amounts of text this "
            & "can interfere with other operations.  Instead of shutting down the "
            & "program, instead you can "
            & SP.Help.Colorize_Command ("unload")
            & " the data set, do whatever operations "
            & "you need and then "
            & SP.Help.Colorize_Command ("reload")
            & " to bring the files back for search.");

        SP.Help.Block (
            SP.Help.Colorize_Command ("stats")
            & " septum maintains all search context in memory within the file cache. "
            & "Due to the large amount of text that can be loaded, it can be useful "
            & "to examine where and how this storage is used. "
        );

    end File_Cache;

    ----------------------------------------------------------------------------

    procedure Line_Filters is
    begin
        SP.Help.Header ("Line Filters", "");

        SP.Help.Describe_Command(
            "find-text",
            "TEXT...",
            "provides a case-sensitive search filter."
        );

        SP.Help.Describe_Command (
            "find-like",
            "TEXT...",
            "provides a case-insensitive filter.");

        SP.Help.Block (
            "Each space separated text parameter to "
            & SP.Help.Colorize_Command ("find-text")
            & " is treated as an additional filter. This supports applying "
            & "multiple text filters and then being able to manipulate individual "
            & "ones using commands like "
            & SP.Help.Colorize_Command ("drop")
            & " and "
            & SP.Help.Colorize_Command ("reorder")
            & "."
        );

        SP.Help.Block(
            SP.Help.Colorize_Command ("exclude-regex")
            & " provides a regex to exclude."
        );

        SP.Help.Block(
            SP.Help.Colorize_Command ("find-regex")
            & " provides a regex to search for."
        );

        SP.Help.Block(
            SP.Help.Colorize_Command ("clear-line-filters")
            & " Removes all line filters, while maintaining currently"
            & " excluded file extensions and path filters."
        );

        SP.Help.Block(
            SP.Help.Colorize_Command ("list-line-filters")
            & " provides a case-insensitive filter."
        );

        SP.Help.Block(
            SP.Help.Colorize_Command ("reorder")
            & " Sometimes you have the right filters, but want to reorder them"
            & " so it's easier to pop specific ones, or to better organize them. "
        );

        SP.Help.Block(
            SP.Help.Colorize_Command ("drop")
            & " drops given filters, or the most recent filter if non given."
        );

        SP.Help.Block (
            SP.Help.Colorize_Command ("pop")
            & " sometimes the filter you just applied is too restrictive"
            & " or you want to try a different approach. "
            & " let's you remove the most recently applied line filter."
        );

        SP.Help.Block (
            SP.Help.Colorize_Command ("test")
            & " provides a mechanism to see why a specific line is getting"
            & " matched or excluded from a search.  This command shows how "
            & " each filter matches against a line of text."
        );

    end Line_Filters;

    procedure Path_Filters is
    begin
        SP.Help.Header ("Path Filters");

        SP.Help.Block (
            "All files are considered during the search, unless specific paths"
            & " are requested to be found. "
            & SP.Help.Colorize_Command ("find-path")
            & " restricts the search to only files which match this filter."
        );

        SP.Help.Block (
            "Path filtering occurs at both the path and the extension level."
            & " Paths containing specific elements can be removed, and the results"
            & " can also be tailored to only produce results which match specific"
            & " file extensions."
        );

        SP.Help.Block (
            "All paths are considered search candidates unless a find-path is"
            & " provided.  This means "
            & SP.Help.Colorize_Command ("exclude-path")
            & " can be used as an axe to more easily shave off entire portions"
            & " of the search space."
        );

        SP.Help.Describe_Command(
            "clear-exts",
            "",
            "Clears extension filters."
        );

        SP.Help.Describe_Command(
            "clear-path-filters",
            "",
            "Removes all path filters."
        );

        SP.Help.Describe_Command(
            "exclude-path",
            "",
            "Provides path elements to exclude from the search."
        );


        SP.Help.Describe_Command (
            "list-exts",
            "",
            "Lists extensions to filter by."
        );

        SP.Help.Describe_Command(
            "list-path-filters",
            "",
            "Lists the currently bound path filters."
        );

        SP.Help.Describe_Command(
            "only-exts",
            "",
            "Adds extension to the search list."
        );

        SP.Help.Describe_Command(
            "remove-exts",
            "",
            "Removes extension from the search list."
        );

    end Path_Filters;

    procedure Results is
    begin
        SP.Help.Header ("Results");

        Put_Line ("Lists the Contexts currently matching all filters.");
        New_Line;
        Put_Line ("match-contexts        Prints up to max-results results");
        Put_Line ("match-contexts N      Prints the first N results");
        Put_Line ("match-contexts M N    Prints the M ... N results");

        SP.Help.Block (
            SP.Help.Colorize_Command ("match-files")
            & " lists all files which match the current filters. "
            & "This command is particularly useful to determine if path filters "
            & "would be effective to cull search results. "
        );

        SP.Help.Block (
            "Due to partial matching of commands, the abbreviated versions"
            & " of match commands are often used instead of the full ones."
        );


        SP.Help.Describe_Command (
            "set-context-width",
            "[COUNT]",
            "Sets the number of lines form the search neighborhood above and below a search term."
        );

        SP.Help.Describe_Command (
            "set-max-results",
            "[COUNT]",
            "Sets the maximum results printed before truncating."
        );

        SP.Help.Describe_Command (
            "enable-auto-search",
            "",
            "Automatically search when filters are updated."
        );

        SP.Help.Describe_Command (
            "enable-line-numbers",
            "",
            "Whether to print line numbers in results."
        );

        SP.Help.Describe_Command (
            "enable-line-colors",
            "",
            "Whether to colorize lines with found search terms."
        );
    end Results;

    procedure System is
    begin
        SP.Help.Block (
            "Septum is designed as a interactive search application. "
            & "In typical usage, the program remains 'live' in the background "
            & "in a separate tmux tab or terminal."
        );
        SP.Help.Block (
            SP.Help.Colorize_Command ("reload")
            & " is needed to update text files during heavy edits or when "
            & " rebasing during the day."
        );

        SP.Help.Block (
            SP.Help.Colorize_Command ("enable-timing")
            & " enables reporting of time it takes to run commands."
        );

        SP.Help.Block (
            SP.Help.Colorize_Command ("disable-timing")
            & " disables reporting of time it takes to run commands."
        );

    end System;
end SP.Help.Topics;