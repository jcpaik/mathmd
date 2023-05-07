# mathmd

This is an opinionated and niche program that transpiles an Obsidian vault to a single LaTeX file or an `mdbook` book. It aims to:
- allow markdown to include LaTeX theorem/figure environment and references
- and provide WYSIWYG experience in the Obsidian markdown editor.

The code is mainly for myself to transpile a [draft](https://github.com/jcpaik/sofa-vault) of mine in Obsidian,
so it is immature and not recommended for any general use as of now.

# Motivation

LaTeX takes too much mental space for drafting by requiring users to adhere to complicated syntax with no immediate feedback on how the result will look like.
- [Typst](https://github.com/typst/typst) is a promising project, but as academy standard expects LaTeX files right now I can't use it immediately for my needs.
- [Obsidian](https://obsidian.md/) is a nice markdown WYSIWYG editor that still giving the full control to users to edit the source file as-is even in WYSIWYG mode. But it does not support theorem environments natively.

With this, I'm using Pandoc to transpile an Obsidian vault to a LaTeX project.

# Markdown Spec

This software uses a subset of Markdown to represent environments in LaTeX often used for math papers.
They are designed look like the LaTeX rendering even in vanilla Obsidian and other markdown editors.

The definitions are not precise and subject to change anytime.

> __Definition [theorem-environment].__ A _theorem environment_ is a [block quote](https://spec.commonmark.org/0.30/#block-quotes) that contains the following elements. ^def-theorem-environment
> 
> - Right after the block quote marker, the text ` __EnvType [EnvName].__  ` (including the whitespace at the end) follows. 
> 	- `EnvType` should be one of the followings in the table. 
> 	- `EnvName` can be any name, but it only consists of lowercase alphabets, dash `-`, and should start with an alphabet.
> - Then any contents can be inside the block quote.
> - It should have exactly one [block identifier](https://help.obsidian.md/Linking+notes+and+files/Internal+links#Link+to+a+block+in+a+note) somewhere inside the block quote which is exactly `^EnvAbbr-EnvName`.
> 	- `EnvAbbr` should correspond to `EnvType` as the following table.
> 	- The identifier should be positioned at the very end of a line with no trailing spaces.

| EnvType    | EnvAbbr  |
|------------|----------|
| Theorem    | thm      |
| Lemma      | lem      |
| Definition | def      |
| Corollary  | cor      |
| Remark     | rem      |
| Figure     | fig      |

_Proof._ A _proof environment_ is simply a series of paragraphs, with the first letters of the first paragraph starting with `_Proof._`, and the last string of the last paragraph ending with the symbol `□` (&#9633). □

> __Figure [sample].__ This is a sample figure. A figure environment should end with a single paragraph of image. ^fig-sample
> 
> ![70%](images/sample.jpeg)

To refer to a theorem or figure environment, we use Obsidian's way to refer a block identifier like [[#^def-theorem-environment]] or [[#^fig-sample]].
We only use double brackets `[[]]` for referencing environments.

# Obsidian Plugins

For the ease of use, I use the following Obsidian plugins.
- [Latex Suite](https://github.com/artisticat1/obsidian-latex-suite) to type environments easily. 
  - Simply typing `> thm` expands to a `Theorem` environment. 
  - Then by typing the name of the environment name, and pressing tab, it autopopulates the name.
- [Copy Block Link](https://github.com/mgmeyers/obsidian-copy-block-link) to copy the reference of a theorem easily.
