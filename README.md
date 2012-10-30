#Hammertime

Hammertime is a simple time-tracker.

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

* Create text reports
* Create SVG reports
* Use more robust data structures
