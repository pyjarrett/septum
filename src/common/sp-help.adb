SP.Help.Header ("Line Filters");

SP.Help.Block (
    "Septum searches multi-line neighborhoods called contexts, not single isolated lines. You build a search by stacking filters: find filters require terms to appear somewhere in a context, and exclude filters drop any context that still contains an unwanted term. Filters are applied in order, and each new find filter only keeps contexts that also satisfy earlier find filters where their neighborhoods overlap. That lets terms appear in any order and across line breaks, which is hard to express in ordinary line-oriented tools."
);

SP.Help.Block (
    "Because each space-separated argument becomes its own filter entry, you can add several terms at once and later remove or reorder just one of them. When auto-search is off, filter commands only update the filter stack; you run " & Colorize_Command ("match-contexts") & " or " & Colorize_Command ("match-files") & " to see the effect. With auto-search on, each filter change triggers a search and prints result counts."
);

SP.Help.Block (
    Colorize_Command ("find-text") & " adds a case-sensitive substring filter. Every argument is a separate keep filter that contexts must still satisfy after merging."
);

SP.Help.Block (
    Colorize_Command ("find-like") & " works like " & Colorize_Command ("find-text") & " but matches case-insensitively, which is often enough when casing varies across the codebase."
);

SP.Help.Block (
    Colorize_Command ("exclude-text") & " adds a case-sensitive exclusion. Any context whose line range still contains a matching line is discarded entirely, even if that line is only in the surrounding neighborhood of another match."
);

SP.Help.Block (
    Colorize_Command ("exclude-like") & " is the case-insensitive form of " & Colorize_Command ("exclude-text") & ". Use it to peel away overloaded names, generated helpers, or other contexts that share a term but are not what you want."
);

SP.Help.Block (
    Colorize_Command ("find-regex") & " keeps contexts that match a regular expression. Invalid patterns fail that filter rather than being accepted silently."
);

SP.Help.Block (
    Colorize_Command ("exclude-regex") & " excludes contexts that match a regular expression, using the same full-context exclusion rule as the text excludes."
);

SP.Help.Block (
    Colorize_Command ("list-line-filters") & " prints the current line-filter stack in order, with indices matching what " & Colorize_Command ("drop") & " and " & Colorize_Command ("reorder") & " expect."
);

SP.Help.Block (
    Colorize_Command ("pop") & " removes only the most recently applied line filter. It is the quick undo when the last term was too aggressive or mistyped, as in the common fix-up cycle of apply, inspect counts, then " & Colorize_Command ("pop") & "."
);

SP.Help.Block (
    Colorize_Command ("drop") & " removes filters by 1-based index. With no arguments it behaves like " & Colorize_Command ("pop") & ". With several indices it drops those entries, processing from high to low so remaining indices stay valid."
);

SP.Help.Block (
    Colorize_Command ("reorder") & " rebuilds the stack from a full list of existing indices. You must list every current index exactly once; use " & Colorize_Command ("drop") & " to delete filters and " & Colorize_Command ("reorder") & " only to rearrange what remains so later " & Colorize_Command ("pop") & " operations hit the filter you intend."
);

SP.Help.Block (
    Colorize_Command ("clear-line-filters") & " removes every line filter at once. Path filters and extension filters are left unchanged, so you can start a new content search over the same file subset."
);

SP.Help.Block (
    Colorize_Command ("test") & " evaluates each current line filter against a sample line you provide. For every filter it shows whether that line would MATCH a keep filter, EXCLUDE, or neither, then summarizes whether the line would be MATCHED, EXCLUDED, or IGNORED. Use it when the stack is large and a result is mysterious."
);

SP.Help.Header ("File Cache");

SP.Help.Block (
    "Septum loads candidate text into an in-memory file cache and runs searches against that cache rather than rereading the disk for every query. Anecdotally this uses about 100 MB per million lines and can scan on the order of millions of lines per second on a modern laptop, which is why interactive refinement stays responsive on large trees."
);

SP.Help.Block (
    "What you load and what you search are related but not identical. Directories and files define the cache. Path and extension filters decide which cached paths participate in the next match. Reloading refreshes content; it does not by itself change your filter stack."
);

SP.Help.Block (
    Colorize_Command ("add-dirs") & " is the main way to grow the cache. It walks each directory recursively and loads files that look like text: common source extensions are accepted, known binary extensions are skipped, and unknown types are accepted only if the first 4 KiB contain no null byte. Load progress and failures are reported as each path is processed."
);

SP.Help.Block (
    Colorize_Command ("add-files") & " adds individual paths without treating their parent directory as a recursive search root. Prefer it for log files, single dumps, or a handful of targets you do not want to pull an entire tree for."
);

SP.Help.Block (
    Colorize_Command ("list-dirs") & " prints the directory roots currently registered for recursive loading."
);

SP.Help.Block (
    Colorize_Command ("list-files") & " prints paths currently in the search pool. By default output is capped so huge trees stay readable; pass a positive number to choose the cap, or full to print every path."
);

SP.Help.Block (
    Colorize_Command ("clear-dirs") & " removes all recursive roots and the files discovered through them. Files that entered the cache only via " & Colorize_Command ("add-files") & " remain."
);

SP.Help.Block (
    Colorize_Command ("clear-files") & " removes only those directly added files. Files discovered under " & Colorize_Command ("add-dirs") & " stay loaded."
);

SP.Help.Block (
    Colorize_Command ("stats") & " reports how many files, lines, and characters the cache currently holds. Check it after large loads or before " & Colorize_Command ("unload") & " when memory pressure is a concern."
);

SP.Help.Block (
    "The cache does not watch the filesystem. After edits, rebases, or generated-file updates, run " & Colorize_Command ("reload") & " to reread every currently loaded path from disk while keeping the same roots and direct-file list."
);

SP.Help.Block (
    Colorize_Command ("unload") & " empties the in-memory text without exiting the session. Use it when a huge load is contending with other work, then " & Colorize_Command ("reload") & " when you need search again. Unloading does not forget your filter stack or settings."
);

SP.Help.Header ("Path Filters");

SP.Help.Block (
    "Path filters shrink the set of cached files considered for matching. They do not unload files from memory; they only decide eligibility at search time. Extension filters work the same way and can be combined with path fragment filters."
);

SP.Help.Block (
    "With no find-path filters active, every cached file whose extension is allowed is a candidate, and " & Colorize_Command ("exclude-path") & " is enough to carve out noise such as dependency or build trees. Once any " & Colorize_Command ("find-path") & " filter exists, the default flips: a file must match at least one find-path filter to stay in, then exclude-path filters can still drop it. Matching is against the full path string as stored in the cache, so directory names, separators, and file names all participate as ordinary text fragments."
);

SP.Help.Block (
    Colorize_Command ("find-path") & " keeps only paths containing the given fragments. Each argument becomes its own path keep filter. After the first find-path, files outside those fragments stop appearing in " & Colorize_Command ("match-contexts") & " and " & Colorize_Command ("match-files") & " even though they remain cached."
);

SP.Help.Block (
    Colorize_Command ("exclude-path") & " drops paths containing the given fragments. A single well-chosen segment often removes an entire subtree. When both keep and exclude path filters match, exclusion wins for that path."
);

SP.Help.Block (
    Colorize_Command ("list-path-filters") & " prints the path-filter stack currently in effect."
);

SP.Help.Block (
    Colorize_Command ("clear-path-filters") & " removes all path keep and exclude filters so every extension-eligible cached file is a candidate again."
);

SP.Help.Block (
    Colorize_Command ("only-exts") & " restricts candidates to files with the listed extensions. Extensions are compared without inventing a leading dot if you omit it the same way the path is stored; list what " & Colorize_Command ("list-exts") & " shows after you set them. Until you call " & Colorize_Command ("only-exts") & ", extension filtering is open and all extensions are eligible."
);

SP.Help.Block (
    Colorize_Command ("remove-exts") & " removes individual extensions from the allow-list built by " & Colorize_Command ("only-exts") & "."
);

SP.Help.Block (
    Colorize_Command ("list-exts") & " shows the current extension allow-list. An empty list means no extension restriction is active."
);

SP.Help.Block (
    Colorize_Command ("clear-exts") & " clears the extension allow-list entirely, restoring the default that every extension may participate."
);

SP.Help.Block (
    "A practical workflow is to load broadly with " & Colorize_Command ("add-dirs") & ", confirm size with " & Colorize_Command ("stats") & " and " & Colorize_Command ("list-files") & ", then use " & Colorize_Command ("exclude-path") & " or " & Colorize_Command ("find-path") & " and " & Colorize_Command ("only-exts") & " until " & Colorize_Command ("match-files") & " shows a plausible set before refining line filters."
);

SP.Help.Header ("Results");

SP.Help.Block (
    "A result is a context: a contiguous line range in one file that contains the merged matches of your find filters and none of your exclude hits. Context width controls how many lines above and below each matching line are pulled into that neighborhood. The default width is several lines so nearby declarations stay visible; width 0 collapses each hit toward a single line and approximates an interactive grep. Omitting the argument to " & Colorize_Command ("set-context-width") & " removes the width restriction so neighborhoods can grow to the whole file when needed."
);

SP.Help.Block (
    "Multiple find filters do not require all terms on one line. Each keep filter produces candidate neighborhoods; only overlapping neighborhoods are merged into a smaller shared window that still contains every required match. Overlapping or nested contexts are then deduplicated, preferring the larger enclosing range so the list stays readable. Lines that actually matched a keep filter are marked in the interactive display with an arrow prefix."
);

SP.Help.Block (
    Colorize_Command ("match-contexts") & " runs the full search and prints contexts. With no arguments it prints up to the current max-results limit. One number N prints the first N contexts. Two numbers M and N print the inclusive slice from M through N, which is useful when paging through a long hit list. After printing, the interactive UI reports Matching contexts and Matching files totals so you can watch the search tighten as filters change."
);

SP.Help.Block (
    Colorize_Command ("match-files") & " lists each distinct file that still has at least one matching context, without printing the line bodies. Use it to judge whether path or extension filters should be applied before diving into large context dumps."
);

SP.Help.Block (
    "Command names resolve by unique prefix, so short forms such as match-c often expand to " & Colorize_Command ("match-contexts") & " after a Resolved to message."
);

SP.Help.Block (
    Colorize_Command ("set-context-width") & " sets the neighborhood size in lines above and below each match. Larger widths find terms that are farther apart but also make exclusions more powerful, because any excluded line inside the wider window kills the whole context."
);

SP.Help.Block (
    Colorize_Command ("set-max-results") & " caps how many contexts are printed before the rest are omitted. The totals still reflect the full match set, so you can keep output short while watching counts fall. Omit the argument to remove the print cap."
);

SP.Help.Block (
    Colorize_Command ("enable-auto-search") & " reruns a context search after filter-changing commands and prints results using the current max-results setting. That matches the interactive style in the README, where each find or exclude immediately shows new Matching contexts counts."
);

SP.Help.Block (
    Colorize_Command ("disable-auto-search") & " restores manual mode, where you call " & Colorize_Command ("match-contexts") & " or " & Colorize_Command ("match-files") & " when you want output. Manual mode is quieter when building long filter stacks or running scripts."
);

SP.Help.Block (
    Colorize_Command ("enable-line-numbers") & " prefixes each printed line with its 1-based file line number."
);

SP.Help.Block (
    Colorize_Command ("disable-line-numbers") & " hides those numbers for denser output."
);

SP.Help.Block (
    Colorize_Command ("enable-line-colors") & " colorizes lines that contain matches when the terminal supports it."
);

SP.Help.Block (
    Colorize_Command ("disable-line-colors") & " prints match lines without that color emphasis. Match arrows still mark hit lines either way."
);

SP.Help.Header ("System");

SP.Help.Block (
    "Septum is meant to stay open in a terminal or tmux tab while you iterate. On startup it runs command scripts from the project-local " & Colorize_Command (".septum/config") & " and from the global septum config directory when present, unless you opt out with shell flags such as --no-config. Create a starter local config with septum init. In config and script files, blank lines and lines whose first non-empty character starts a # comment are ignored, so you can document shared setups."
);

SP.Help.Block (
    "Partial command matching applies everywhere: type a unique prefix and Septum resolves it to the full command name before running. Ambiguous prefixes are rejected rather than guessed. That same resolution is what lets abbreviated match and filter commands feel short in daily use."
);

SP.Help.Block (
    Colorize_Command ("help") & " with no arguments prints a one-line summary of every command. " & Colorize_Command ("help") & " with a command name prints that command's longer description."
);

SP.Help.Block (
    Colorize_Command ("run") & " reads one or more script files and executes each non-comment line as if typed interactively. Nested " & Colorize_Command ("run") & " of a file already on the script stack is refused to prevent recursion loops. From outside the REPL, septum run can drive the same scripts for batch jobs and pipelines; tool-oriented modes can emit structured results for match commands."
);

SP.Help.Block (
    Colorize_Command ("source") & " remains as a deprecated alias for " & Colorize_Command ("run") & " so older configs keep working. Prefer " & Colorize_Command ("run") & " in new scripts."
);

SP.Help.Block (
    Colorize_Command ("enable-timing") & " prints how long each executed command took, which helps when comparing large cache loads or heavy match runs."
);

SP.Help.Block (
    Colorize_Command ("disable-timing") & " stops those duration lines."
);

SP.Help.Block (
    Colorize_Command ("quit") & " and " & Colorize_Command ("exit") & " end the interactive session. They do not write the cache or filters back to disk; put durable defaults in config files via " & Colorize_Command ("run") & " lines such as " & Colorize_Command ("enable-auto-search") & ", " & Colorize_Command ("set-max-results") & ", and " & Colorize_Command ("add-dirs") & " if you want them every launch."
);

