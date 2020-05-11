# Use spaces and align
Use spaces and align visually to avoid headaches.

Classic `Markdown.pl` and Pandoc treat nested block elements differently.

Classic `Markdown.pl` expects nested blocks to be aligned by 4 spaces or one tab.
Although one space seems to suffice.

In contract with Pandoc nested block elements have to be aligned with the first
non-space character after the list marker. Using tabs the alignment seems to
use a tabstop set to 4.

#### Examples

1. nested block with single space

 This is a subparagraph for `Markdown.pl` but not for `pandoc`.

1. nested block aligned with spaces

   This is a subparagraph for `Markdown.pl` and for `pandoc`.

An interesting edge case are ordered list, with start number greater 100. This
illustrates, that Pandoc’s alignment criteria uses tabstop set to 4.

1.	"large" list with tabs

	This is a subparagraph for `Markdown.pl` and for `pandoc`.

100.	"large" list with tabs

	This is a subparagraph for `Markdown.pl` but a code block outside the list
	for `pandoc`, because the 100. the tabs are not aligned.

If we indent the block elements with one more tab, we see that the alignment
work for 100.

1.	"large" list with tabs

		This is a code block for `pandoc` and for `Markdown.pl`.

100.	"large" list with tabs

		This is a subparagraph for `pandoc` but a code block for `Markdown.pl`.

If we try to indent the list by two spaces, it gets even more confusing:

  1.	"large" list with tabs

		This is a subparagraph for `pandoc` but a code block for `Markdown.pl`.

  100.	"large" list with tabs

		This is a subparagraph for `pandoc` but a code block for `Markdown.pl`.

Aligning the list with spaces works for both, is intuitive and independent of tabstop.

  1.   "large" with aligned spaces

       This is a subparagraph for `Markdown.pl` and for `pandoc`.

  100. "large" with aligned spaces

       This is a subparagraph for `Markdown.pl` and for `pandoc`.

Is is also safe and intuitive to indent the list by two spaces.

**but** the're still issues with code blocks.

  100. "large" with aligned spaces

        This is a code block for `Markdown.pl` but not for `pandoc`.

  101. "large" with aligned spaces

           This is a code block for `Markdown.pl` and for `pandoc`. `Markdown.pl` is indented to much.

