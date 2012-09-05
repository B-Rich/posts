Today I finally succeeded at getting a fully local version of Hoogle running
on my machine, with filesystem links for all packages that I have installed,
and remote links for those I don't.  Since this was definitely a non-trivial
exercise, I wanted to capture the knowledge here for anyone else trying to do
the same.

First, let me mention that I'm using OS X 10.7.4, GHC 7.4.2 (64-bit), and
Hoogle 4.2.13.  I had to install Hoogle from source in order to apply the
following patch:

    --- a/src/CmdLine/All.hs
    +++ b/src/CmdLine/All.hs
    @@ -74,6 +74,7 @@ guessLocal = do
         lib <- getLibDir
         let xs = [takeDirectory (takeDirectory lib) </> "doc" {- Windows, installed with Cabal -}  ] ++
                  [takeDirectory (takeDirectory ghc) </> "doc/html/libraries" | Just ghc <- [ghc] {- Windows, installed by GHC -} ] ++
    +             [takeDirectory (takeDirectory ghc) </> "share/doc/ghc/html/libraries" | Just ghc <- [ghc] {- Mac OS X, installed by GHC -} ] ++
                  [home </> ".cabal/share/doc" {- Linux -} ]
         filterM doesDirectoryExist xs

This allows Hoogle's `data` command find my GHC library documentation in
`$GHCROOT/share/doc/ghc/html/libraries`.  Otherwise, none of the standard
libraries show up as local links in the search results.  Then I built and
installed Hoogle:

    cabal configure
    cabal install

The next step was to enable Haddock Documentation for all packages I locally
install with `cabal`.  This required editing `~/.cabal/config` and making sure
the following line was present:

    documentation: True

While you're at, go ahead and enable library profiling too, so you have
profiling libs available the next time you want to hunt down a space leak:

    library-profiling: True

Unfortunately these two are not the default, so if you're adding them now you'll
have to rebuild every package in your local repository:

    cabal install world --reinstall --force-reinstalls

This could take awhile -- and may not complete successfully.  I had more luck at
wiping my old state and starting over:

    cp ~/.cabal/config ~/.cabal/world /tmp
    rm ~/.cabal ~/.ghc
    cd <path to cabal-install source tree>
    sh bootstrap.sh
    cp /tmp/config /tmp/world ~/.cabal
    cabal update
    cabal install world

I also edited `bootstrap.sh` to make sure that the libraries installed by this
process also haddocumentation and profiling libs available.  Make sure you add
the following line to the top of `bootstrap.sh`:

    EXTRA_CONFIGURE_OPTS=--enable-library-profiling

And these lines right after the `Setup build` invocation:

    ./Setup haddock ${VERBOSE} \
      || die "Haddocking the ${PKG} package failed"

Even after all this you may need to intervene manually, if some of your
packages require special options to build.  For example, I always need `-f
have-quartz-gtk` to build `gtk`, but it seems Cabal doesn't remember this in
my world file, and so `gtk` breaks every time anything tries to rebuild it.

Back to Hoogle. By now you should have two things: a `hoogle` binary, and a
fset of local documentation in `~/.cabal/share/doc`.  Make sure that you do,
before going any further.  Then, download and generate all the necessary
Hoogle data, with local annotations where possible:

    hoogle data -l -r all

If you only want a subset of hoogle, drop the `all` keyword.  This process
takes a long time, so head out and get some coffee!  Once this is finished --
and after any time you run this command to reflect newly install new packages
-- ensure that your `default.hoo` database is also up-to-date.  Here's how I
did that:

    cd ~/.cabal/share/hoogle-4.2.13/databases
    mv default.hoo default.hoo-prev
    hoogle combine *.hoo

On my system this merged 3377 database, took an *exceedingly* long time, and
used almost 7 gigabytes of RAM.  It is a much longer process than the `data`
command above.  But once it's done you can now run:

    sudo hoogle server --local

And voila!  You should be able to query Hoogle and browse documentation fully
offline, as long as you've installed the related packages.  Even further, you
can add this to your `.ghci` file:

    :def h \x -> return $ ":!hoogle -c -n 10 \"" ++ x ++ "\""
    :def doc \x -> return $ ":!hoogle --info \"" ++ x ++ "\""

And be able to type `:h a -> a` to query the top ten hits in Hoogle, fully
syntax-highlighted (if your terminal supports it), and `:doc head` to read the
documentation for the first match on `head`.