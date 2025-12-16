cpsc411-pub
=======
<p align="left">
  <a href="https://github.com/cpsc411/cpsc411-pub/actions?query=workflow%3A%22CI%22"><img alt="GitHub Actions status" src="https://github.com/cpsc411/cpsc411-pub/workflows/CI/badge.svg"></a>
</p>

This collection defines the public support code for the UBC's Computer Science
411 (CPSC 411), "Introduction to Compiler Construction".
This code is meant distributed to students as they work through the assignments
described by the `cpsc411-book` package.

## Installation
From the `cpsc411-lib` directory, run `raco pkg install`, or run
`raco pkg install https://github.com/cpsc411/cpsc411-pub.git?path=cpsc411-lib#2025w2`.

If you get strange errors referencing `cpsc411-pub`, you may have run the
command from the wrong directory.
Try doing `raco pkg remove cpsc411-pub` and running the command from the
directory as described above.
