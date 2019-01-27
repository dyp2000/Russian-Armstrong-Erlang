Что нового?
===========
`2016-02-18` Книга в GitBook [здесь](https://www.gitbook.com/book/dyp2000/russian-armstrong-erlang/details)

`2014-01-13` Книга одним файлом PDF [здесь](https://github.com/dyp2000/Russian-Armstrong-Erlang/raw/master/pdf/fullbook.pdf).

`2013-12-30` Форматирование глав с 1 по 11. Объединение книги в один файл MD и PDF

`2013-11-05` Добавлена глава 14.

Вопрос
======

А зачем README.md на английском языке, если книга на русском языке?


Dependencies
============

On yum like linux you can run something like this:

    $ yum install ruby calibre rubygems ruby-devel rubygem-ruby-debug redhat-rpm-config
    $ gem install rdiscount

On deb like linux you can run something like this:

    $ apt-get install ruby calibre rubygems ruby-dev rake pandoc texlive-xetex libmaruku-ruby 
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

 	$ ./makeebooks ru
or

 	$ FORMAT=mobi ./makeebooks ru

Build e-book in 'epub' format for russian only

 	$ FORMAT=epub ./makeebooks ru

Build pdf for russian only

    $  ./makepdfs ru
