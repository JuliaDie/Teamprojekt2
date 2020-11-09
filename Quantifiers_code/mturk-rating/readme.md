## How to use submiterator mturk tools

To post the HIT, first setup the config file.
Give this config file a unique label as its name: `[LABEL].config`.

    {
    "rewriteProperties":"yes",
    "liveHIT":"no",
    "title":"a title to show to turkers",
    "description":"a description to show to turkers",
    "experimentURL":"https://www.stanford.edu/~you/path/to/experiment.html",
    "keywords":"language research stanford fun cognitive science university explanations",
    "USonly?":"yes",
    "minPercentPreviousHITsApproved":"95",
    "frameheight":"650",
    "reward":"0.00",
    "numberofassignments":"1",
    "assignmentduration":"1800",
    "hitlifetime":"2592000",
    "autoapprovaldelay":"60000",
    "conditions":"cond"
    }


Then run the following commands in the terminal:

    
python submiterator.py posthit [LABEL]

And then when you want to get the results:

    
python submiterator.py getresults [LABEL]

This will create a `[LABEL].results` file.
It will also create a `[LABEL]_anonymized.results` file which will have not have worker ids.

If you want a long-form table of data and your data has a `trials` variable (a list of JSON objects), run the following:

    python submiterator.py reformat [LABEL]

This will create a bunch of .tsv files with data from your experiment.

##  How to make this even cooler

N.B. This will only work on unix.

If you want, you can make `submiterator` a system-wide command, so you can just type (for example):

	submiterator posthit example
    submiterator getresults example
    submiterator reformat example

To do this, save the Submiterator repo somewhere where it won't move, copy-paste and run the following command:

	chmod u+x submiterator.py

Then make a directory called "bin" in your home folder and make sym-links to the Submiterator file:

	cd ~
	mkdir bin
	cd bin
	ln -s [PATH_TO_SUBMITERATOR_DIRECTORY]/submiterator.py submiterator

Then open up or create the file `.bash_profile` or `.bashrc` in your home directory and add the following line:

	PATH=$PATH:~/bin

Then once you open up a new terminal, you should be able to use the submiterator command as above.