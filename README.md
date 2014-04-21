# AITK

This contains our basic stack of NLP tools.  We have general interfaces on each
tool so we have a clear definition of what each tool does and so we can change
the underlying implementation of a tool.

## Project Layout

1.  tools/core: this project contains the tool interfaces and NLP representations.
2.  tools/impl: an implementation of an NLP tool
3.  webapp: a web application for running tools and visualizing serialized
    representations.
