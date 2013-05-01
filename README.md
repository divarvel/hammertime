[![Build status](https://travis-ci.org/divarvel/hammertime.png)](https://travis-ci.org/divarvel/hammertime)

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

###Start tracking a task

    hammertime start project_name task_name [tags*]

Starting a new task automatically closes the previous.

###Stop tracking a task

    hammertime stop

###Visualize tasks

Show current tasks

    hammertime current

Show all tasks done today

    hammertime report day

Show all tasks of a particular project done this month

    hammertime report month --project=<project name>

Show the time spent on a particular activity, this week

    hammertime report week --activity=<activity name> --type=totaltime

To see all the available options

    hammertime report --help

## Roadmap

* Polish the existing reports
* Use a templating engine to ease the creation of reports
* Create SVG reports
* Create CSV reports
