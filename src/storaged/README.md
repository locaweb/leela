# STORAGED : LEELA TIME-SERIES STORAGE SERVICE #

This daemon provides a durable storage for leela time-series. It is a
stateless service making it relatively easy to deploy.

## PACKING ##

    $ lein with-profile triggers jar     ; C* trigger
    $ lein with-profile storaged uberjar ; storaged daemon

## TESTING ##

    $ lein with-profile storaged test

Notice this requires that the trigger is properly installed on your C*
instances. Without it a great deal of tests will fail.

## RUNNING ##

    $ java -jar ./target/storaged-VERSION-standalone.jar
