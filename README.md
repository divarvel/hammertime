#Hammertime

Hammertime is a simple time-tracker.

Please notice that hammertime is more a pet project than a real one. Its main
goal is to allow me to write some "Real World" haskell.

## Install

With cabal

    cabal configure
    cabal build
    cabal install

## Usage

Start tracking a task

    hammertime start project_name task_name [tags*]

Starting a new task automatically closes the previous.

Stop tracking a task

    hammertime stop

Visualize tasks

    to be done

## Roadmap

* More Tests
* Create text reports
* Create SVG reports
* Use more robust data structures
