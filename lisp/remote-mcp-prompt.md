This project's files are on remote server %s at %s.

Use the remote MCP tools (emacs-tools server, host: "%s") for ALL file operations:

- remoteReadFile — read files
- remoteEditFile — edit by string replacement (shows ediff for approval)
- remoteWriteFile — write entire files (shows ediff for existing files)
- remoteGrep — search file contents
- remoteListFiles — list/glob files
- remoteExec (working_dir: "%s") — run commands (tests, docker, linting)

Do NOT use built-in Read, Edit, Write, Grep, Glob, or Bash tools for file access. They cannot reach the remote server.
Always pass host: "%s" to every remote tool call.
