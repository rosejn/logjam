# logjam

An experimental, task based logging library.

Log messages are sent to a named channel.  These channels are organized into a hierarchy,
and channels can be connected to various output mechanisms such as the console or a file.

## Usage

    (require '[logjam.core :as log])

    ; Create a log channel named :foo
    (log/channel :foo)

    ; Create another log channel :bar, that is a child of :foo.
    ; All messages sent to bar will be forward up to :foo also.
    (log/channel :bar :foo)

    ; Output channel :foo to the console
    (log/console :foo)

    ; Output channel :bar to a file
    (log/file :bar "example.log")
    
    ; Send some log messages
    (log/to :foo "log message...")
    (log/to :foo "log message..." 1 2 3)
    (log/to :bar "another log message...")
    
    (defn foo 
      [a b] 
      (+ a b))
    
    ; spy can be used to log the execution of expressions
    (log/spy :foo (foo 1 2))

## Installation

Add this to your project.clj:

    [logjam "0.1.0-SNAPSHOT"]

## License

Copyright (C) 2010 Jeff Rose

Distributed under the Eclipse Public License, the same as Clojure.
