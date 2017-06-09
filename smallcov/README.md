# A README

smallcov is a small utility that determines which tests execute certain methods.

smallcov is really intended for use with ManyBugs. In theory, it's general, but I've only really tested it 
on ManyBugs scenarios, so YMMV on other applications.

The usual use case is that you provide smallcov information on how to compile your program and run tests.
You can also specify the function names in question.  HOWEVER.  The standard use case involves specifying the location
of diff files between two versions.  smallcov parses the diff files to determine which lines it modifies, 
parses the input files, and then figures out which functions contain the modified lines.

