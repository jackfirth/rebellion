# rebellion [![CI Status][ci-status-badge]][ci-status] [![Documentation][docs-badge]][docs]

Rebellion is a set of infrastructure libraries for Racketeers to build new languages, new frameworks, and new tools with. It is installable as a single package with `raco pkg install --auto rebellion` and provides dozens of modules to aid general-purpose programming. Most of these modules are grouped into the following collections:

- [`rebellion/base`][rebellion-base] - Relatively simple utility modules used in the APIs of other Rebellion modules.
- [`rebellion/collection`][rebellion-collection] - Collection types including records, tables, multidicts, and association lists.
- [`rebellion/type`][rebellion-type] - Libraries for dynamically creating new data types using structs.
- [`rebellion/binary`][rebellion-binary] - Libraries for working with bits, bytes, and binary data, including the encoding and decoding of binary data into other forms.

[ci-status]: https://github.com/jackfirth/rebellion/actions
[ci-status-badge]: https://github.com/jackfirth/rebellion/workflows/CI/badge.svg
[docs]: http://docs.racket-lang.org/rebellion/index.html
[docs-badge]: https://img.shields.io/badge/docs-published-blue.svg
[rebellion-base]: https://docs.racket-lang.org/rebellion/Base_Libraries.html
[rebellion-binary]: https://docs.racket-lang.org/rebellion/Binary_Data.html
[rebellion-collection]: https://docs.racket-lang.org/rebellion/Collections.html
[rebellion-type]: https://docs.racket-lang.org/rebellion/Data_Types.html
