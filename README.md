Dependencies
============

On yum like linux you can run something like this:

    $ yum install ruby calibre rubygems ruby-devel rubygem-ruby-debug
    $ gem install rdiscount

On deb like linux you can run something like this:

    $ apt-get install ruby calibre rubygems ruby-devel rubygem-ruby-debug
    $ gem install rdiscount

On Mac OS X:

    $ install calibre app
    $ check ruby and rubygems
    $ easy_install pygments
    $ port install pandoc texlive +full texlive-xetex
    $ gem install rdiscount

Usage
=====

Build e-book for amazon kindle for russian languages

 	$ makeebooks ru
or

 	$ FORMAT=mobi makeebooks ru

Build e-book in 'epub' format for russian only

 	$ FORMAT=epub makeebooks ru

Build pdf for russian only

    $  makepdfs ru
