GniehTuring v3
==============

Introduction
------------

GniehTuring provides a complete toolchain for writing and executing [Turing machines](http://en.wikipedia.org/wiki/Turing_machine). This toolchain contains the following components:
- a language called _Turing Machine Description Language_ (TMDL), that allows people to write algorithms in the Turing machine formalism,
- a bytecode called _Turing Bytecode_ (TBC), that represents basic operations needed to execute Turing machines and link them dynamically,
- a compiler _gtc_ from TMDL to TBC to compile written Turing machines to the corresponding (possibly) optimized bytecode,
- a TBC interpreter _gtvm_ to load and execute Turing machines.

TMDL is a language based on the original formalism proposed by Alan Turing in his article from 1936. The semantics is also based on the concepts presented in his article.
It is a complete re-implementation of the old project located [here](http://gturing.n7mm.org/), written in [Scala](http://www.scala-lang.org).

Goals
-----

GniehTuring mainly aims to be an educative toolchain that allow people to learn more about:
- Turing machines,
- programming language development,
- compiler construction,
- interpreter and virtual machines.

It sounded more interesting to create a new kind of programming language for this purpose than yet another object-oriented language.
Most of the features provided by the toolchain are inspired by real-world languages/compilers/virtual machines features (static linking, modules, garbage collection, ...) which may be interesting for people wanting to learn these concepts.

More concepts may be added, everything is still open.

If you are interested in participating to this project and/or have any idea, do not hesitate to contact me and/or send pull requests.

Organization
------------

The project is divided in different modules as follows:
- **parent/**: contains the parent project used to compile/build/release the entire toolchain,
- **compiler/**: contains the compiler,
- **vm/**: contains the virtual machine,
- **bytecode**: contains a library used for reading/writing bytecode (used by both compiler and virtual machine),
- **util/**: contains some utility classes possibly used by all the other modules.

Building GniehTuring
--------------------

Clone the project with
``git clone git://github.com/gnieh/gniehturing.git``

GniehTuring is built using [Apache Maven](http://maven.apache.org/). 
To build GniehTuring, go to the _parent_ module and run:
``mvn package``
This will build the project, test it and package it.
To build a specific component, go to the component directory and run
``mvn package``

License
-------

This project is published under the [GPL v3](http://www.gnu.org/licenses/gpl-3.0.txt) license.
