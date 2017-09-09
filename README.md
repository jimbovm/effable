# effable
A 21st-century document preparation system.

Effable aims to define extensible high-level syntaxes for document forms common in academic writing (such as articles and textbooks) allowing the single-source compilation of documents into a variety of syntaxes and thus easily portable to a variety of media, including the web, eBooks, and print. Currently Effable is able to generate HTML5 for web and print and ePUB eBooks. Support for OpenDocument Text and Docbook is planned.

Generating XML-like syntax from Lisp is nothing new and interesting as such. Effable instead aims to be a front-end for standardised, modern technologies for document structure, styling, graphics and mathematics rendering. Using vector graphics in textbooks, including weird and wonderful characters and custom styling in academic articles and typesetting mathematics for the web are all solved problems that are, sadly, often unnecessarily difficult in practice.

Effable tries to put all these solutions at authors’ fingertips, and thanks to its basis in Common Lisp is inherently flexible and extensible.

A central design goal of Effable is semantic awareness. As much machine-readable metadata as possible should be automatically generated without any onerous duplication of effort by the user. A doctrine of “semantic umami”, modelled after the term syntactic sugar, is intended to make as much document structure and metadata as possible /inherent/ in the Effable format itself.

Components of Effable, such as the mathseml package which makes writing MathML easy using a high-level Lisp S-expression syntax, are usable by themselves.

At present Effable is quite usable, but its syntax is not especially stable and subject to change.
