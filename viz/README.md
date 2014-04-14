# Nlpviz

This project should grow to visualize various NLP tools.  For example,
Mark H in his experimentation with polytrees would like to visualize
polytrees.  Presently Nlpviz only visualizes dependencies and so it's
largely a wrapper for Whatswrong (https://code.google.com/p/whatswrong/).

The current functionality was taken from Nlpweb.  It's difficult to add
a tool to Nlpweb because it's an old project and it requires setting up
and configuring a server with your NLP tool.  This doesn't work for
frequent experimentation.

There are a multitude of NLP formats out there.  Ideally we would
standardize somewhat.  I would rather Nlpviz not turn into a tool
that takes in every format out there.  Rather, I'd rather have a
separate tool NlpCanonicalize that converts formats into what we
adopt as canonical.

This tool can either be used from a webpage or used programatically
via POST requests.

## Running

This project uses sbt as the build system.  sbt can also be used to run
Nlpviz.

    $ sbt compile
    $ sbt run

Now you should have a HTTP server running at http://localhost:8080.  To
change the port, edit `src/main/resources/application.conf`.

## Future support

* Polytrees
* SRL frames
