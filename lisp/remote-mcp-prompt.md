This project's files are on remote server %s at %s.

Use the remote-* MCP tools (from the emacs MCP server) for ALL file operations:

- remote-read-file (host: "%s") — read files
- remote-edit-file (host: "%s") — edit by string replacement
- remote-write-file (host: "%s") — write entire files
- remote-grep (host: "%s") — search file contents
- remote-list-files (host: "%s") — list/glob files
- remote-exec (host: "%s", working_dir: "%s") — run commands (tests, docker, linting)

Do NOT use built-in Read, Edit, Grep, Glob, or Bash tools for file access. They cannot reach the remote server.
Always pass host: "%s" to every remote-* tool call.
