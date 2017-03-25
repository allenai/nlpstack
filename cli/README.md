Command-line to run any of the CLIs:

```
sbt "cli/runMain <mainClassName> --input <input-file-path> -- output <output-file-path>
```

For e.g., to run the `Stanford POS Tagger`:

```
sbt "cli/runMain org.allenai.nlpstack.cli.StanfordPostaggerMain  --input /Users/sumithrab/nlpstack/posTaggerIp.txt --output /Users/sumithrab/nlpstack/posTaggerOp.txt"
```

The input file is generally a text file with one sentence per line. To run a segmenter, you can give it any block of text.


You can also alternately do the following:

```
sbt "cli/run --input /Users/sumithrab/nlpstack/posTaggerIp.txt --output /Users/sumithrab/nlpstack/posTaggerOp.txt"
```

which will show you all entry points under the `cli` subproject for you to choose from:

```
Multiple main classes detected, select one to run:

 [1] org.allenai.nlpstack.cli.FactorieParserMain

 [2] org.allenai.nlpstack.cli.FactoriePostaggerMain

 [3] org.allenai.nlpstack.cli.FactorieSegmenterMain

 [4] org.allenai.nlpstack.cli.FactorieTokenizerMain

 [5] org.allenai.nlpstack.cli.KnowitallArgumentHeadExtractorMain

 [6] org.allenai.nlpstack.cli.KnowitallRelationHeadExtractorMain

 [7] org.allenai.nlpstack.cli.MorphaStemmerMain

 [8] org.allenai.nlpstack.cli.OpenNlpChunkerMain

 [9] org.allenai.nlpstack.cli.PennTokenizerMain

 [10] org.allenai.nlpstack.cli.PolytreeParserMain

 [11] org.allenai.nlpstack.cli.StanfordPostaggerMain

 [12] org.allenai.nlpstack.cli.StanfordSegmenterMain

 [13] org.allenai.nlpstack.cli.StanfordTokenizerMain

 [14] org.allenai.nlpstack.cli.WhitespaceTokenizerMain

Enter number: 
```

Note, for e.g., that this shows that there is also a Factorie POS Tagger. If you would like to run that, you can type 2 at the above prompt, and hit enter.



You can also run a specific CLI listed above with the `--help` flag to find out how to run it, for e.g.:

```
sbt "cli/runMain org.allenai.nlpstack.cli.StanfordPostaggerMain  --help"
```

This will give you the following usage info:

```
[info] Usage: postagger [options]

[info] 

[info]   --server

[info]         run as a server

[info]   --port <value>

[info]         port to run the server on

[info]   --input <value>

[info]         file to input from

[info]   --output <value>

[info]         file to output to

[info]   --rawInput <value>

[info]         raw input to process

[info]   --parallel

[info]         parallel execution

[info]   --help

[info]         print this usage text
```
