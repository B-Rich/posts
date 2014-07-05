#title Using Nix on the Mac

I've recently started using Nix on the Mac, and I wanted to share some of my
experiences and the things that I found useful.

One of the most powerful aspects of mix is that you can sandbox pretty much
any project.  For example when developing letter I have a default.txt file
in my ledger source directory and all I need to do is run next shell and it
will put me into a shell that has access to all of the dependencies for
building letter.  However, when I'm not a nutshell, none of those
dependencies are visible to me.  This way, I can use dependencies with
slightly different versions for other projects, but it won't in any way
affect my ability to work on letter at any time.
