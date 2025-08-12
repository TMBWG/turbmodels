The equation files were all named "temp*.tex", because they were not intended to be
posted directly to the TMR site.  Instead, only the resulting png files that ended up in
the directory "temp*" were copied over into other specifically named directories.

For example, the file temp1.tex contains latex equations and the directory
temp1 contains the resulting png files that were created.  These png files have also
been copied elsewhere, and uploaded to the TMR site.  The PROCESS for creating
the png files has changed several times.  The latest method is:

make4ht -d directoryname temp1.tex

(see README file one level up from here).

Ultimately, it is important to save the temp*.tex files in the git repository, even
though they are not posted to the TMR website.  However, the temp* directories are
not important, since their png file contents have been copied elsewhere.

The naming convention of the LaTeX files are as follows:

temp1 - temp16*.tex - equations for the SA model; sometimes there is an additional name
                     included (qcr2020, LRe) to help identify it

Other turbulence models typically have an additional name included, that identifies it.
For example, temp1kkl.tex is for the KKL model.

Not everything is logical; I apologize for that.  In any case, all latex equations are also
included in the html files where the equation appears, as alt="...".

Note that occasionally, a TMR page might use an equation that was created for a different 
page, because it is identical.
