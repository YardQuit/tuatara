;;;; lisp/modes/more-languages-markup.lisp --- The markup, documents, style sheets and templates Helix knows, in major modes named as Emacs's.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; Each language is parsed with the grammar and coloured, indented and
;;;; selected by the rules Helix uses for it (syntax/), in a major mode named as
;;;; the one Emacs or its best-known package edits it with. A file type another
;;;; mode already claims stays that mode's.
;;;;
;;;; Written by tools/languages/generate.py: change that, not this file.

(in-package #:tuatara.defaults)

(define-major-mode xml-mode text-mode
  "XML documents."
  :lighter "XML"
  :block-comment ("<!--" "-->")
  :extensions ("ascx" "atom" "axaml" "axml" "bpmn" "checkstyle" "cpt" "csl" "dita" "ditamap" "dtml" "fods" "fodt" "fxml" "gir" "glif" "gml" "gpx" "iml" "isml" "itermcolors" "jmx" "kml" "launch" "menu" "mobileconfig" "mpd" "musicxml" "mxml" "ncx" "nuspec" "opml" "osc" "osm" "plist" "policy" "pt" "publishsettings" "pubxml" "rbxlx" "rbxmx" "resx" "rng" "rss" "shproj" "smil" "storyboard" "sublime-snippet" "svg" "terminal" "tld" "tmx" "ui" "vcxproj" "wixproj" "wsdl" "wxi" "wxs" "xaml" "xbl" "xib" "xlf" "xliff" "xml" "xmp" "xoml" "xpdl" "xrc" "xsd" "xsl" "xul")
  :patterns ("/[^/]*\\.csproj\\.user$" "/[^/]*\\.pubxml\\.user$" "/[^/]*\\.vbproj\\.user$" "/[^/]*\\.vcxproj\\.filters$" "/[^/]*\\.tm[Ll]anguage$" "/[^/]*\\.tm[Pp]references$" "/[^/]*\\.tm[Tt]heme$"))

(define-language xml
  "XML documents, parsed with tree-sitter-xml."
  :grammar "xml" :modes (xml-mode) :indent 2 :queries "xml" :injection-names "xml" :definitions :textobjects)

(define-major-mode markdown-mode text-mode
  "Markdown documents."
  :lighter "Markdown"
  :block-comment ("<!--" "-->")
  :extensions ("md" "livemd" "markdown" "mdx" "mkd" "mkdn" "mdwn" "mdown" "markdn" "mdtxt" "mdtext" "workbook")
  :file-names ("PULLREQ_EDITMSG"))

(define-language markdown
  "Markdown documents, parsed with tree-sitter-markdown."
  :grammar "markdown" :modes (markdown-mode) :indent 2 :queries "markdown" :injection-names "md|markdown")

(define-language markdown-inline
  "The text inside Markdown's blocks: emphasis, code spans, links, parsed with tree-sitter-markdown-inline."
  :grammar "markdown_inline" :modes nil :indent 4 :queries "markdown.inline" :injection-names "markdown\\.inline")

(define-major-mode html-mode text-mode
  "HTML documents."
  :lighter "HTML"
  :block-comment ("<!--" "-->")
  :extensions ("html" "htm" "shtml" "xhtml" "xht" "jsp" "asp" "aspx" "jshtm" "volt" "rhtml" "cshtml"))

(define-language html
  "HTML documents, parsed with tree-sitter-html."
  :grammar "html" :modes (html-mode) :indent 2 :queries "html" :injection-names "html" :definitions :textobjects)

(define-major-mode css-mode prog-mode
  "Cascading Style Sheets."
  :lighter "CSS"
  :block-comment ("/*" "*/")
  :extensions ("css"))

(define-language css
  "Cascading Style Sheets, parsed with tree-sitter-css."
  :grammar "css" :modes (css-mode) :indent 2 :queries "css" :injection-names "css" :definitions :textobjects)

(define-major-mode scss-mode prog-mode
  "Sass style sheets in the SCSS syntax."
  :lighter "SCSS"
  :block-comment ("/*" "*/")
  :extensions ("scss"))

(define-language scss
  "Sass style sheets in the SCSS syntax, parsed with tree-sitter-scss."
  :grammar "scss" :modes (scss-mode) :indent 2 :queries "scss" :injection-names "scss")

(define-major-mode latex-mode text-mode
  "LaTeX documents."
  :lighter "LaTeX"
  :comment "%"
  :extensions ("tex" "dtx" "ins" "sty" "cls" "Rd" "bbx" "cbx"))

(define-language latex
  "LaTeX documents, parsed with tree-sitter-latex."
  :grammar "latex" :modes (latex-mode) :indent 4 :queries "latex" :injection-names "tex" :definitions :textobjects)

(define-major-mode bibtex-mode text-mode
  "BibTeX bibliographies."
  :lighter "BibTeX"
  :comment "%"
  :extensions ("bib"))

(define-language bibtex
  "BibTeX bibliographies, parsed with tree-sitter-bibtex."
  :grammar "bibtex" :modes (bibtex-mode) :indent 4 :queries "bibtex" :injection-names "bib")

(define-major-mode vue-mode html-mode
  "Vue single-file components."
  :lighter "vue"
  :block-comment ("<!--" "-->")
  :extensions ("vue"))

(define-language vue
  "Vue single-file components, parsed with tree-sitter-vue."
  :grammar "vue" :modes (vue-mode) :indent 2 :queries "vue" :injection-names "vue" :definitions :textobjects)

(define-major-mode svelte-mode prog-mode
  "Svelte components."
  :lighter "Svelte"
  :comment "//"
  :extensions ("svelte"))

(define-language svelte
  "Svelte components, parsed with tree-sitter-svelte."
  :grammar "svelte" :modes (svelte-mode) :indent 2 :queries "svelte" :injection-names "svelte" :definitions :textobjects)

(define-major-mode graphviz-dot-mode prog-mode
  "Graphviz graphs in the DOT language."
  :lighter "Dot"
  :comment "//"
  :extensions ("dot"))

(define-language dot
  "Graphviz graphs in the DOT language, parsed with tree-sitter-dot."
  :grammar "dot" :modes (graphviz-dot-mode) :indent 4 :queries "dot" :injection-names "dot")

(define-language comment
  "Comments in code: the TODO, FIXME and NOTE marks in them, parsed with tree-sitter-comment."
  :grammar "comment" :modes nil :indent 4 :queries "comment" :injection-names "comment")

(define-language jsdoc
  "JSDoc comments in JavaScript and TypeScript, parsed with tree-sitter-jsdoc."
  :grammar "jsdoc" :modes nil :indent 2 :queries "jsdoc" :injection-names "jsdoc")

(define-major-mode astro-mode html-mode
  "Astro components."
  :lighter "Astro"
  :block-comment ("<!--" "-->")
  :extensions ("astro"))

(define-language astro
  "Astro components, parsed with tree-sitter-astro."
  :grammar "astro" :modes (astro-mode) :indent 2 :queries "astro" :injection-names "astro")

(define-major-mode rst-mode text-mode
  "reStructuredText documents."
  :lighter "ReST"
  :comment ".."
  :extensions ("rst"))

(define-language rst
  "reStructuredText documents, parsed with tree-sitter-rst."
  :grammar "rst" :modes (rst-mode) :indent 4 :queries "rst")

(define-major-mode typst-mode text-mode
  "Typst documents."
  :lighter "Typst"
  :comment "//"
  :extensions ("typst" "typ"))

(define-language typst
  "Typst documents, parsed with tree-sitter-typst."
  :grammar "typst" :modes (typst-mode) :indent 2 :queries "typst" :injection-names "typ(st)?" :definitions :textobjects)

(define-major-mode djot-mode text-mode
  "Djot documents."
  :lighter "Djot"
  :block-comment ("{%" "%}")
  :extensions ("dj" "djot"))

(define-major-mode mermaid-mode prog-mode
  "Mermaid diagrams."
  :lighter "Mermaid"
  :comment "%%"
  :extensions ("mermaid" "mmd"))

(define-language mermaid
  "Mermaid diagrams, parsed with tree-sitter-mermaid."
  :grammar "mermaid" :modes (mermaid-mode) :indent 4 :queries "mermaid" :injection-names "mermaid")

(define-major-mode gemini-mode text-mode
  "Gemini documents (gemtext)."
  :lighter "Gemini"
  :extensions ("gmi"))

(define-language gemini
  "Gemini documents (gemtext), parsed with tree-sitter-gemini."
  :grammar "gemini" :modes (gemini-mode) :indent 4 :queries "gemini")

(define-major-mode mediawiki-mode text-mode
  "MediaWiki pages."
  :lighter "MediaWiki"
  :block-comment ("<!--" "-->")
  :extensions ("wikimedia" "mediawiki" "wikitext"))

(define-language wikitext
  "MediaWiki pages, parsed with tree-sitter-wikitext."
  :grammar "wikitext" :modes (mediawiki-mode) :indent 2 :queries "wikitext")

(define-major-mode jinja2-mode prog-mode
  "Jinja templates."
  :lighter "Jinja2"
  :block-comment ("{#" "#}")
  :extensions ("jinja" "jinja2" "j2"))

(define-language jinja
  "Jinja templates, parsed with tree-sitter-jinja."
  :grammar "jinja" :modes (jinja2-mode) :indent 2 :queries "jinja" :injection-names "jinja")

(define-major-mode twig-mode prog-mode
  "Twig templates."
  :lighter "Twig"
  :block-comment ("{#" "#}")
  :extensions ("twig"))

(define-language twig
  "Twig templates, parsed with tree-sitter-twig."
  :grammar "twig" :modes (twig-mode) :indent 2 :queries "twig" :injection-names "twig")

(define-major-mode less-css-mode prog-mode
  "Less style sheets."
  :lighter "Less"
  :block-comment ("/*" "*/")
  :extensions ("less"))

(define-language less
  "Less style sheets, parsed with tree-sitter-less."
  :grammar "less" :modes (less-css-mode) :indent 2 :queries "less" :injection-names "less")

(define-major-mode pug-mode prog-mode
  "Pug templates for HTML."
  :lighter "Pug"
  :comment "//"
  :extensions ("pug"))

(define-language pug
  "Pug templates for HTML, parsed with tree-sitter-pug."
  :grammar "pug" :modes (pug-mode) :indent 2 :queries "pug")

(define-major-mode heex-mode prog-mode
  "HEEx: the HTML templates of Phoenix."
  :lighter "HEEx"
  :block-comment ("<!--" "-->")
  :extensions ("heex"))

(define-language heex
  "HEEx: the HTML templates of Phoenix, parsed with tree-sitter-heex."
  :grammar "heex" :modes (heex-mode) :indent 2 :queries "heex" :injection-names "heex" :definitions :textobjects)

(define-major-mode templ-mode prog-mode
  "templ components: HTML in Go."
  :lighter "templ"
  :comment "//"
  :extensions ("templ"))

(define-language templ
  "templ components: HTML in Go, parsed with tree-sitter-templ."
  :grammar "templ" :modes (templ-mode) :indent 2 :queries "templ")

(define-language markdown-rustdoc
  "Rust's documentation comments, written in Markdown, parsed with tree-sitter-markdown."
  :grammar "markdown" :modes nil :indent 2 :queries "markdown-rustdoc" :injection-names "markdown-rustdoc")

(define-language htmldjango
  "Django templates in HTML, parsed with tree-sitter-htmldjango."
  :grammar "htmldjango" :modes nil :indent 4 :queries "htmldjango" :injection-names "htmldjango")

(define-language edoc
  "EDoc: the documentation in Erlang's comments, parsed with tree-sitter-edoc."
  :grammar "edoc" :modes nil :indent 4 :queries "edoc" :injection-names "edoc")

(define-major-mode blade-mode prog-mode
  "Blade: Laravel's templates."
  :lighter "Blade"
  :extensions ("blade")
  :patterns ("/[^/]*\\.blade\\.php$"))

(define-language blade
  "Blade: Laravel's templates, parsed with tree-sitter-blade."
  :grammar "blade" :modes (blade-mode) :indent 4 :queries "blade" :injection-names "blade" :definitions :textobjects)

(define-major-mode blueprint-mode prog-mode
  "Blueprint: a markup for GTK's user interfaces."
  :lighter "Blueprint"
  :comment "//"
  :extensions ("blp"))

(define-language blueprint
  "Blueprint: a markup for GTK's user interfaces, parsed with tree-sitter-blueprint."
  :grammar "blueprint" :modes (blueprint-mode) :indent 4 :queries "blueprint" :injection-names "blueprint")

(define-major-mode dtd-mode prog-mode
  "XML document type definitions."
  :lighter "DTD"
  :block-comment ("<!--" "-->")
  :extensions ("dtd" "ent"))

(define-language dtd
  "XML document type definitions, parsed with tree-sitter-dtd."
  :grammar "dtd" :modes (dtd-mode) :indent 2 :queries "dtd" :injection-names "dtd")

(define-major-mode eex-mode prog-mode
  "EEx: templates of embedded Elixir."
  :lighter "EEx"
  :block-comment ("<!--" "-->")
  :extensions ("eex"))

(define-language eex
  "EEx: templates of embedded Elixir, parsed with tree-sitter-eex."
  :grammar "eex" :modes (eex-mode) :indent 2 :queries "eex" :injection-names "eex")

(define-major-mode ejs-mode prog-mode
  "EJS: templates of embedded JavaScript."
  :lighter "EJS"
  :block-comment ("<!--" "-->")
  :extensions ("ejs"))

(define-language ejs
  "EJS: templates of embedded JavaScript, parsed with tree-sitter-embedded-template."
  :grammar "embedded-template" :modes (ejs-mode) :indent 2 :queries "ejs" :injection-names "ejs")

(define-major-mode erb-mode prog-mode
  "ERB: templates of embedded Ruby."
  :lighter "ERB"
  :block-comment ("<!--" "-->")
  :extensions ("erb"))

(define-language erb
  "ERB: templates of embedded Ruby, parsed with tree-sitter-embedded-template."
  :grammar "embedded-template" :modes (erb-mode) :indent 2 :queries "erb" :injection-names "erb")

(define-major-mode feature-mode text-mode
  "Gherkin: the feature files of Cucumber."
  :lighter "Feature"
  :comment "#"
  :extensions ("feature"))

(define-language gherkin
  "Gherkin: the feature files of Cucumber, parsed with tree-sitter-gherkin."
  :grammar "gherkin" :modes (feature-mode) :indent 2 :queries "gherkin")

(define-major-mode glimmer-mode prog-mode
  "Glimmer templates, Ember's."
  :lighter "Glimmer"
  :block-comment ("{{!" "}}")
  :patterns ("/(?:app|addon)/(?:components|templates)/[^/]*\\.hbs$"))

(define-language glimmer
  "Glimmer templates, Ember's, parsed with tree-sitter-glimmer."
  :grammar "glimmer" :modes (glimmer-mode) :indent 4 :queries "glimmer" :injection-names "hbs")

(define-major-mode go-template-mode prog-mode
  "Go's templates."
  :lighter "Go-Template"
  :comment "//"
  :extensions ("gotmpl"))

(define-language gotmpl
  "Go's templates, parsed with tree-sitter-gotmpl."
  :grammar "gotmpl" :modes (go-template-mode) :indent 1 :queries "gotmpl" :injection-names "gotmpl")

(define-major-mode eml-mode text-mode
  "Email messages saved as files (.eml): their headers and body."
  :lighter "Mail"
  :extensions ("eml"))

(define-language mail
  "Email messages saved as files (.eml): their headers and body, parsed with tree-sitter-mail."
  :grammar "mail" :modes (eml-mode) :indent 4 :queries "mail" :injection-names "mail|eml|email" :definitions :textobjects)

(define-major-mode markdoc-mode text-mode
  "Markdoc documents."
  :lighter "Markdoc"
  :block-comment ("<!--" "-->")
  :extensions ("mdoc"))

(define-language markdoc
  "Markdoc documents, parsed with tree-sitter-markdoc."
  :grammar "markdoc" :modes (markdoc-mode) :indent 4 :queries "markdoc")

(define-major-mode penrose-mode prog-mode
  "Penrose diagrams: their substance, style and domain."
  :lighter "Penrose"
  :comment "--"
  :extensions ("substance" "style" "domain"))

(define-language penrose
  "Penrose diagrams: their substance, style and domain, parsed with tree-sitter-penrose."
  :grammar "penrose" :modes (penrose-mode) :indent 2 :queries "penrose" :definitions :textobjects)

(define-major-mode pod-mode text-mode
  "Perl's documentation (POD)."
  :lighter "POD"
  :extensions ("pod"))

(define-language pod
  "Perl's documentation (POD), parsed with tree-sitter-pod."
  :grammar "pod" :modes (pod-mode) :indent 4 :queries "pod" :injection-names "pod")

(define-major-mode quarto-mode text-mode
  "Quarto documents: Markdown with code to run."
  :lighter "Quarto"
  :block-comment ("<!--" "-->")
  :extensions ("qmd"))

(define-language quarto
  "Quarto documents: Markdown with code to run, parsed with tree-sitter-markdown."
  :grammar "markdown" :modes (quarto-mode) :indent 2 :queries "quarto" :injection-names "qmd")

(define-major-mode poly-markdown+r-mode text-mode
  "R Markdown documents: Markdown with R code."
  :lighter "PM-Rmd"
  :block-comment ("<!--" "-->")
  :extensions ("rmd" "Rmd"))

(define-language rmarkdown
  "R Markdown documents: Markdown with R code, parsed with tree-sitter-markdown."
  :grammar "markdown" :modes (poly-markdown+r-mode) :indent 2 :queries "rmarkdown" :injection-names "(r|R)md")

(define-major-mode strictdoc-mode text-mode
  "StrictDoc documents of requirements."
  :lighter "StrictDoc"
  :comment ".."
  :extensions ("sdoc" "sgra"))

(define-language strictdoc
  "StrictDoc documents of requirements, parsed with tree-sitter-strictdoc."
  :grammar "strictdoc" :modes (strictdoc-mode) :indent 4 :queries "strictdoc" :injection-names "strictdoc")

(define-major-mode tera-mode prog-mode
  "Tera templates."
  :lighter "Tera"
  :block-comment ("{#" "#}")
  :extensions ("tera"))

(define-language tera
  "Tera templates, parsed with tree-sitter-tera."
  :grammar "tera" :modes (tera-mode) :indent 4 :queries "tera")

(define-major-mode vento-mode prog-mode
  "Vento templates."
  :lighter "Vento"
  :block-comment ("{{#" "#}}")
  :extensions ("vto"))

(define-language vento
  "Vento templates, parsed with tree-sitter-vento."
  :grammar "vento" :modes (vento-mode) :indent 4 :queries "vento")
