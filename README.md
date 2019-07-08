# rebellion [![CircleCI](https://circleci.com/gh/jackfirth/rebellion.svg?style=svg)](https://circleci.com/gh/jackfirth/rebellion) [![Documentation](https://img.shields.io/badge/docs-published-blue.svg)](http://docs.racket-lang.org/rebellion/index.html)

Rebellion is a set of infrastructure libraries for Racketeers to build new languages, new frameworks, and new tools with. It is installable as a single package with `raco pkg install --auto rebellion` and provides dozens of modules to aid general-purpose programming. Most of these modules are grouped into the following collections:

- [`rebellion/base`][base] - Relatively simple utility modules used in the APIs of other Rebellion modules.
- [`rebellion/collection`][collection] - Collection types including records, tables, multidicts, and association lists.
- [`rebellion/type`][type] - Libraries for dynamically creating new data types using structs.
- [`rebellion/binary`][binary] - Libraries for working with bits, bytes, and binary data, including the encoding and decoding of binary data into other forms.

[base]: https://docs.racket-lang.org/rebellion/Base_Libraries.html
[collection]: https://docs.racket-lang.org/rebellion/Collections.html
[type]: https://docs.racket-lang.org/rebellion/Data_Types.html
[binary]: https://docs.racket-lang.org/rebellion/Binary_Data.html
