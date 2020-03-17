GHC and Cabal need a partition mounted with `wxallowed` and a memory limit of
4096M (or even `infinity` in */etc/login.conf*, see
[Setting Up a OpenBSD System for Building GHC](https://gitlab.haskell.org/ghc/ghc/wikis/building/preparation/openbsd).

A (once) working snipped would be

	staff:\
	        :datasize=infinity:\
	        :datasize-cur=infinity:\
	        :datasize-max=infinity:\
	        :maxproc-max=512:\
	        :maxproc-cur=256:\
	        :ignorenologin:\
	        :requirehome@:\
	        :tc=default:

Don't forget to logout and in again.
