;;; html-helper-mode.el --- Major mode for composing html files.

;; Author: Nelson Minar <nelson@santafe.edu>
;; Maintainer: Nelson Minar <nelson@santafe.edu>
;; Created: 01 Feb 1994
;; $Id: html-helper-mode.el,v 2.19.1.1 1998/08/06 18:53:03 nelson Exp $
;; Keywords: HTML major-mode

;; LCD Archive Entry:
;; html-helper-mode|Nelson Minar|nelson@santafe.edu|
;; Major mode for editing HTML.|
;; 16-Mar-94|Version 2.?|ftp://ftp.reed.edu/pub/src/html-helper-mode.tar.Z

;; Copyright (C) 1994 Nelson Minar
;; Copyright (C) 1995 Nelson Minar and Ulrik Dickow

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Massachusettes Ave,
;; Cambridge, MA 02139, USA.

;;; Commentary:
;;{{{ 

;; Installation:
;;   Add this line in your .emacs:
;;     (autoload 'html-helper-mode "html-helper-mode" "Yay HTML" t)
;;   To invoke html-helper-mode automatically on .html files, do this:
;;     (setq auto-mode-alist (cons '("\\.html$" . html-helper-mode) auto-mode-alist))
;;
;;   This mode requires another lisp file, tempo.el. This can be
;;     retrieved from ftp://ftp.lysator.liu.se/pub/emacs/tempo.el
;;   Xemacs users need to have auc-menu installed.
;;   Emacs 18 users need to have auc-menu and add-hook installed.
;;   If your OS has broken 14 character filenames
;;      this mode will also work with the name "html-mode.el".

;; Configuration:
;;   see the "user variables" section, or the documentation on configuration
;;   in http://www.santafe.edu/~nelson/tools/. There are variables you want to
;;   configure, particularly html-helper-address-string and
;;   html-helper-use-expert-menu

;; Description:
;;   html-helper-mode makes it easier to write HTML documents. This mode
;;   handles inserting HTML codes in a variety of ways (keybindings, menus,
;;   completion in the buffer). It also supports indentation, timestamps,
;;   skeletons for new documents, hilit19 patterns, and a variety of other
;;   things. For the full skinny, see the HTML documentation that comes
;;   with the package or is at http://www.santafe.edu/~nelson/tools/

;; Thank yous:
;;   David Kågedal <davidk@lysator.liu.se> for the tempo code which
;;     forms the core of the HTML insertion, as well as the HTML+ tags.
;;   Marc Hedlund <march@europa.com> for general encouragement and
;;     many helpful suggestions, especially with HTML/2.0 compliance
;;     and form design.
;;   Ulrik Dickow <dickow@nbi.dk> for the font-lock code
;;   Denis Howe <dbh@doc.ic.ac.uk> for writing browse-url.
;;   Magnus Homann <d0asta@dtek.chalmers.se> and Jamshid Afshar
;;     <jamshid@ses.com> for timestamp suggestions.
;;   Everyone who sent me a version of menus (16 in all!)
;;   Marc Andreessen <marca@mcom.com> for writing the original html-mode

;; The newest version of html-helper-mode should always be available from
;;   http://www.santafe.edu/~nelson/tools/

;; This code was writting using folding.el, a wonderful folding editor
;; minor mode for emacs. That's what the strange {{{ comments are for.

;;}}}

;;; Code:

(defconst html-helper-mode-version (substring "$Revision: 2.19.1.1 $" 11 15))
;;{{{ user variables

;; Set this to be whatever signature you want on the bottom of your pages.
(defvar html-helper-address-string ""
  "*The default author string of each file.")

;; Features; these are all good to have on. (see also tempo.el)

(defvar html-helper-use-expert-menu nil
  "*If not nil, then use the full HTML menu.")

(defvar html-helper-do-write-file-hooks t
  "*If not nil, then modify `local-write-file-hooks' to do timestamps.")

(defvar html-helper-build-new-buffer t
  "*If not nil, then insert `html-helper-new-buffer-strings' for new buffers.")

;; variables to configure (these defaults are reasonable.)

(defvar html-helper-htmldtd-version "<!DOCTYPE HTML PUBLIC \"-//IETF//DTD HTML//EN\">\n"
  "*Version of HTML DTD you're using.")

(defvar html-helper-user-menu nil
  "*Extra items to put in the HTML expert menu.
The value of this symbol is appended to the beginning of the expert
menu that is handed off to easymenu for definition. It should be a
list of vectors or lists which themselves are vectors (for submenus).")

(defvar html-helper-basic-offset 2
  "*Basic indentation size used for list indentation")

(defvar html-helper-item-continue-indent 4
  "*Indentation of lines that follow a <li> item.
Default is 4, the length of things like \"<li>\" and \"<dd>\".")

(defvar html-helper-never-indent nil
  "*If not nil, the indentation code for html-helper is turned off.")

;; hooks (see also tempo.el)

(defvar html-helper-mode-hook nil
  "*Hook run when html-helper-mode is started.")

(defvar html-helper-load-hook nil
  "*Hook run when html-helper-mode is loaded.")

(defvar html-helper-timestamp-hook 'html-helper-default-insert-timestamp
  "*Hook called for timestamp insertion.
Override this for your own timestamp styles.")

;; strings you might want to change

(defvar html-helper-new-buffer-template
  '(html-helper-htmldtd-version
    "<html> <head>\n"
    "<title>" p "</title>\n</head>\n\n"
    "<body>\n"
    "<h1>" p "</h1>\n\n"
    p
    "\n\n<hr>\n"
    "<address>" html-helper-address-string "</address>\n"
    html-helper-timestamp-start
    html-helper-timestamp-end
    "\n</body> </html>\n")
  "*Template for new buffers.
Inserted by `html-helper-insert-new-buffer-strings' if
`html-helper-build-new-buffer' is set to t")

(defvar html-helper-timestamp-start "<!-- hhmts start -->\n"
  "*Start delimiter for timestamps.
Everything between `html-helper-timestamp-start' and
`html-helper-timestamp-end' will be deleted and replaced with the output
of the functions `html-helper-timestamp-hook' if
`html-helper-do-write-file-hooks' is t")

(defvar html-helper-timestamp-end "<!-- hhmts end -->"
  "*End delimiter for timestamps.
Everything between `html-helper-timestamp-start' and
`html-helper-timestamp-end' will be deleted and replaced with the output
of the function `html-helper-insert-timestamp' if
`html-helper-do-write-file-hooks' is t")

;; control over what types of tags to load. By default, we load all the
;; ones we know of.

(defvar html-helper-types-to-install
  '(anchor list header logical phys textel entity image head form)
  "*List of tag types to install when html-helper-mode is first loaded.
If you want to not install some type of tag, override this variable.
Order is significant: menus go in this order.")

;; emacs18 detection.

(defvar html-helper-emacs18
  (and (boundp 'emacs-version)
       (or (and (boundp 'epoch::version) epoch::version)
           (string-lessp emacs-version "19")))
  "I'll do minimal emacs18 support, grumble.")

;;}}} end of user variables

(require 'tempo)			;essential part of html-helper-mode
(condition-case nil			;menu support, standard in emacs19
    (require 'auc-menu)			;add-on for XEmacs. *why* does this
  (error (require 'easymenu)))		;package have to have two names?

;;{{{ html-helper-mode-syntax-table and html-helper-mode-abbrev-table

;; emacs doesn't seem to be able to really handle SGML like syntax. In
;; particular, comments are a loss.
;; We do try this, though: give < and > matching semantics

(defvar html-helper-mode-syntax-table nil
  "Syntax table for html-helper.")

(if html-helper-mode-syntax-table
    ()
  (setq html-helper-mode-syntax-table (make-syntax-table text-mode-syntax-table))
  (modify-syntax-entry ?<  "(>  " html-helper-mode-syntax-table)
  (modify-syntax-entry ?>  ")<  " html-helper-mode-syntax-table)
  (modify-syntax-entry ?\" ".   " html-helper-mode-syntax-table)
  (modify-syntax-entry ?\\ ".   " html-helper-mode-syntax-table)
  (modify-syntax-entry ?'  "w   " html-helper-mode-syntax-table))

(defvar html-helper-mode-abbrev-table nil
  "Abbrev table used while in html-helper-mode.")
(define-abbrev-table 'html-helper-mode-abbrev-table ())

;;}}}
;;{{{ type based keymap and menu variable and function setup

;; Our basic keymap.
(defvar html-helper-mode-map (make-sparse-keymap)
  "Keymap for html-helper")
(defvar html-helper-mode-menu nil
  "Menu for html-helper. Clobbered and rebuilt by `html-helper-install-menu'")

;; html-helper-mode has a concept of "type" of tags. Each type is a
;; list of tags that all go together in one keymap and one menu.
;; Types can be added to the system after html-helper has been loaded,
;; briefly by doing html-helper-add-type-to-alist, then
;; html-helper-install-type, then html-helper-add-tag (for each tag)
;; then html-helper-rebuild-menu. See the mode documentation for more detail.

(defconst html-helper-type-alist nil
  "Alist: type of tag -> keymap, keybinding, menu, menu string.
Add to this with `html-helper-add-type-to-alist'.")

;;{{{ accessor functions for html-helper-type-alist

(defun html-helper-keymap-for (type)
  "Accessor function for alist: for type, return keymap or nil"
  (nth 0 (cdr-safe (assq type html-helper-type-alist))))

(defun html-helper-key-for (type)
  "Accessor function for alist: for type, return keybinding or nil"
  (nth 1 (cdr-safe (assq type html-helper-type-alist))))

(defun html-helper-menu-for (type)
  "Accessor function for alist: for type, return menu or nil"
  (nth 2 (cdr-safe (assq type html-helper-type-alist))))

(defun html-helper-menu-string-for (type)
  "Accessor function for alist: for type, return menustring or nil"
  (nth 3 (cdr-safe (assq type html-helper-type-alist))))

(defun html-helper-normalized-menu-for (type)
  "Helper function for building menus from submenus: add on string to menu."
  (cons (html-helper-menu-string-for type)
	(eval (html-helper-menu-for type))))

;;}}}

(defun html-helper-add-type-to-alist (type)
  "Add a type specification to the alist.
The spec goes (type . (keymap-symbol keyprefix menu-symbol menu-string)).
See code for an example."
  (setq html-helper-type-alist (cons type html-helper-type-alist)))

;; Here are the types provided by html-helper-mode.
(mapcar 'html-helper-add-type-to-alist
  '((entity  . (nil nil html-helper-entity-menu "Insert Character Entities"))
    (textel  . (nil nil html-helper-textel-menu "Insert Text Elements"))
    (head    . (html-helper-head-map "\C-c\C-b" html-helper-head-menu "Insert Structural Elements"))
    (header  . (html-helper-header-map "\C-c\C-t" html-helper-header-menu "Insert Headers"))
    (anchor  . (html-helper-anchor-map "\C-c\C-a" html-helper-anchor-menu "Insert Hyperlinks"))
    (logical . (html-helper-logical-map "\C-c\C-s" html-helper-logical-menu "Insert Logical Styles"))
    (phys    . (html-helper-phys-map "\C-c\C-p" html-helper-phys-menu "Insert Physical Styles"))
    (list    . (html-helper-list-map "\C-c\C-l" html-helper-list-menu "Insert List Elements"))
    (form    . (html-helper-form-map "\C-c\C-f" html-helper-form-menu "Insert Form Elements"))
    (image   . (html-helper-image-map "\C-c\C-i" html-helper-image-menu "Insert Inlined Images"))))

;; Once html-helper-mode is aware of a type, it can then install the
;; type: arrange for keybindings, menus, etc.

(defconst html-helper-installed-types nil
  "The types that have been installed (used when building menus).
There is no support for removing a type once it has been installed.")

(defun html-helper-install-type (type)
  "Install a new tag type: add it to the keymap, menu structures, etc.
For this to work, the type must first have been added to the list of types
with html-helper-add-type-to-alist."
  (setq html-helper-installed-types (cons type html-helper-installed-types))
  (let ((keymap (html-helper-keymap-for type))
	(key (html-helper-key-for type))
	(menu (html-helper-menu-for type))
	(menu-string (html-helper-menu-string-for type)))
    (and key
	 (progn
	   (set keymap nil)
	   (define-prefix-command keymap)
	   (if html-helper-emacs18
	       (progn
		 (set keymap (make-sparse-keymap))
		 (define-key html-helper-mode-map key (eval keymap)))
	     (define-key html-helper-mode-map key keymap))))
    (and menu
	 (progn
	   (set menu nil)))))

;; install the default types.
(mapcar 'html-helper-install-type html-helper-types-to-install)

;; special mode keys
(mapcar
 (function (lambda (l) (define-key html-helper-mode-map (car l) (nth 1 l))))
 '(("\M-\C-f" tempo-forward-mark)
   ("\M-\C-b" tempo-backward-mark)
   ("\M-\t"   tempo-complete-tag)))

;; Extra commands that HTML helper supports that aren't insertions
(defvar html-helper-mode-functions-map nil
  "Keymap for extra HTML mode functions")
(define-prefix-command 'html-helper-mode-functions-map)
(define-key html-helper-mode-map "\C-c\C-z"
  'html-helper-mode-functions-map)
(define-key html-helper-mode-functions-map "t"
  'html-helper-insert-timestamp-delimiter-at-point)
 
;; indentation keys - only rebind these if the user wants indentation
(if html-helper-never-indent
    ()
  (define-key html-helper-mode-map "\t" 'html-helper-indent-command)
  (define-key html-helper-mode-map "\C-m" 'newline-and-indent))

;; browse url stuff
(if (fboundp 'browse-url-of-file)
    (define-key html-helper-mode-functions-map "v" 'browse-url-of-file))
(if (and (boundp 'browse-url-browser-function) (fboundp browse-url-browser-function))
    (define-key html-helper-mode-functions-map "u" browse-url-browser-function))

;;}}}

;;{{{ html-helper-add-tag function for building basic tags

(defvar html-helper-tempo-tags nil
  "List of tags used in completion.")

;; this while loop is awfully Cish
;; isn't there an emacs lisp function to do this?
(defun html-helper-string-to-symbol (input-string)
  "Given a string, downcase it and replace spaces with -.
We use this to turn menu entries into good symbols for functions.
It's not entirely successful, but fortunately emacs lisp is forgiving."
  (let* ((s (copy-sequence input-string))
	 (l (1- (length s))))
    (while (> l 0)
      (if (char-equal (aref s l) ?\ )
	  (aset s l ?\-))
      (setq l (1- l)))
    (concat "html-" (downcase s))))


(defun html-helper-add-tag (l)
  "Add a new tag to html-helper-mode.
Builds a tempo-template for the tag and puts it into the
appropriate keymap if a key is requested. Format:
`(html-helper-add-tag '(type keybinding completion-tag menu-name template doc)'"
  (let* ((type (car l))
	 (keymap (html-helper-keymap-for type))
	 (menu (html-helper-menu-for type))
	 (key (nth 1 l))
	 (completer (nth 2 l))
	 (name (nth 3 l))
	 (tag (nth 4 l))
	 (doc (nth 5 l))
	 (command (tempo-define-template (html-helper-string-to-symbol name)
					 tag completer doc
					 'html-helper-tempo-tags)))

    (if (null (memq type html-helper-installed-types))    ;type loaded?
	t                                                 ;no, do nothing.
      (if (stringp key)			                  ;bind key somewhere?
	  (if keymap			                  ;special keymap?
	      (define-key (eval keymap) key command)      ;t:   bind to prefix
	    (define-key html-helper-mode-map key command));nil: bind to global
	t)
      (if menu				                  ;is there a menu?
	  (set menu			                  ;good, cons it in
	       (cons (vector name command t) (eval menu))))
      )))

;; for backwards compatability
(fset 'html-helper-add-cookie 'html-helper-add-tag)

;;}}}

;;{{{ most of the HTML tags

;; These tags are an attempt to be HTML/2.0 compliant, with the exception
;; of container <p>, <li>, <dd>, <dt> (we adopt 3.0 behaviour).
;; For reference see <URL:http://www.w3.org/hypertext/WWW/MarkUp/MarkUp.html>

;; order here is significant: within a tag type, menus and mode help
;; go in the reverse order of what you see here. Sorry about that, it's
;; not easy to fix.

(mapcar
 'html-helper-add-tag
 '(
   ;;entities
   (entity  "\C-c#"   "&#"              "Ascii Code"      ("&#" (r "Ascii: ") ";"))
   (entity  "\C-c\""  "&quot;"          "Quotation mark"  ("&quot;"))
   (entity  "\C-c$"   "&reg;"           "Registered"      ("&reg;"))
   (entity  "\C-c@"   "&copy;"          "Copyright"       ("&copy;"))
   (entity  "\C-c-"   "&shy;"           "Soft Hyphen"     ("&shy;"))
   (entity  "\C-c "   "&nbsp;"		"Nonbreaking Space"  ("&nbsp;"))
   (entity  "\C-c&"   "&amp;"		"Ampersand"	  ("&amp;"))
   (entity  "\C-c>"   "&gt;"	  	"Greater Than"       ("&gt;"))
   (entity  "\C-c<"   "&lt;"		"Less Than"	  ("&lt;"))
   
   ;; logical styles
   (logical "b"       "<blockquote>"	"Blockquote"     	  ("<blockquote>" (r "Quote: ") "</blockquote>"))
   (logical "c"       "<code>"		"Code"           	  ("<code>" (r "Code: ") "</code>"))
   (logical "x"       "<samp>"		"Sample"         	  ("<samp>" (r "Sample code") "</samp>"))
   (logical "r"       "<cite>"		"Citation"       	  ("<cite>" (r "Citation: ") "</cite>"))
   (logical "k"       "<kbd>"		"Keyboard Input"       	  ("<kbd>" (r "Keyboard: ") "</kbd>"))
   (logical "v"       "<var>"		"Variable"       	  ("<var>" (r "Variable: ") "</var>"))
   (logical "d"       "<dfn>"		"Definition"     	  ("<dfn>" (r "Definition: ") "</dfn>"))
   (logical "a"	      "<address>"	"Address"		  ("<address>" r "</address>"))
   (logical "e"       "<em>"		"Emphasized"     	  ("<em>" (r "Text: ") "</em>"))
   (logical "s"       "<strong>"	"Strong"         	  ("<strong>" (r "Text: ") "</strong>"))
   (logical "p"       "<pre>"		"Preformatted"   	  ("<pre>" (r "Text: ") "</pre>"))

   ;;physical styles
   (phys    "s"       "<strike>"	"Strikethru"         ("<strike>" (r "Text: ") "</strike>"))
   (phys    "u"       "<u>"		"Underline"          ("<u>" (r "Text: ") "</u>"))
   (phys    "i"       "<i>"		"Italic"             ("<i>" (r "Text: ") "</i>"))
   (phys    "b"	      "<b>"    		"Bold"               ("<b>" (r "Text: ") "</b>"))
   (phys    "f"       "<tt>"		"Fixed"              ("<tt>" (r "Text: ") "</tt>"))

   ;;headers
   (header  "6"       "<h6>"		"Header 6"       	  ("<h6>" (r "Header: ") "</h6>"))
   (header  "5"       "<h5>"		"Header 5"       	  ("<h5>" (r "Header: ") "</h5>"))
   (header  "4"       "<h4>"		"Header 4"       	  ("<h4>" (r "Header: ") "</h4>"))
   (header  "3"       "<h3>"		"Header 3"       	  ("<h3>" (r "Header: ") "</h3>"))
   (header  "2"       "<h2>"		"Header 2"       	  ("<h2>" (r "Header: ") "</h2>"))
   (header  "1"	      "<h1>"     	"Header 1"       	  ("<h1>" (r "Header: ") "</h1>"))

   ;; forms
   (form    "o"       "<option>"        "Option"                 (& "<option>" > ))
   (form    "v"       "<option value"   "Option with Value"      (& "<option value=\"" (r "Value: ") "\">" >))
   (form    "s"       "<select"		"Selections"	          ("<select name=\"" (p "Name: ") "\">\n<option>" > "\n</select>")"<select")
   (form    "z"	      "<input"		"Reset Form"    	  ("<input type=\"RESET\" value=\"" (p "Reset button text: ") "\">"))
   (form    "b"	      "<input"		"Submit Form"   	  ("<input type=\"SUBMIT\" value=\"" (p "Submit button text: ") "\">"))
   (form    "i"	      "<input"		"Image Field"   	  ("<input type=\"IMAGE\" name=\"" (p "Name: ") "\" src=\"" (p "Image URL: ") "\">"))
   (form    "h"       "<input"          "Hidden Field"            ("<input type=\"HIDDEN\" name=\"" (p "Name: ") "\" value=\"" (p "Value: ") "\">"))
   (form    "p"	      "<textarea"	"Text Area"	  ("<textarea name=\"" (p "Name: ") "\" rows=\"" (p "Rows: ") "\" cols=\"" (p "Columns: ") "\">" r "</textarea>"))
   (form    "c"	      "<input"		"Checkbox"   	          ("<input type=\"CHECKBOX\" name=\"" (p "Name: ") "\">"))
   (form    "r"	      "<input"		"Radiobutton"   	  ("<input type=\"RADIO\" name=\"" (p "Name: ") "\">"))
   (form    "t"	      "<input"		"Text Field"	          ("<input type=\"TEXT\" name=\"" (p "Name: ") "\" size=\"" (p "Size: ") "\">"))
   (form    "f"	      "<form"           "Form"		          ("<form action=\"" (p "Action: ") "\" method=\"" (p "Method: ") "\">\n</form>\n"))

   ;;lists
   (list    "t"       "<dt>"            "Definition Item"         (& "<dt>" > (p "Term: ") "\n<dd>" > (r "Definition: ")))
   (list    "l"       "<li>"            "List Item"               (& "<li>" > (r "Item: ")))
   (list    "r"	      "<dir>"		"DirectoryList"      	  (& "<dir>" > "\n<li>" > (r "Item: ") "\n</dir>" >))
   (list    "m"	      "<menu>"		"Menu List"		  (& "<menu>" > "\n<li>" > (r "Item: ") "\n</menu>" >))
   (list    "o"	      "<ol>"		"Ordered List"   	  (& "<ol>" > "\n<li>" > (r "Item: ") "\n</ol>" >))
   (list    "d"	      "<dl>"		"Definition List" 	  (& "<dl>" > "\n<dt>" > (p "Term: ") "\n<dd>" > (r "Definition: ") "\n</dl>" >))
   (list    "u"	      "<ul>"		"Unordered List" 	  (& "<ul>" > "\n<li>" > (r "Item: ") "\n</ul>" >))

   ;;anchors
   (anchor  "n"	      "<a name="	"Link Target"	  ("<a name=\"" (p "Anchor name: ") "\">" (r "Anchor text: ") "</a>"))
   (anchor  "l"	      "<a href="        "Hyperlink"          	  ("<a href=\"" (p "URL: ") "\">" (r "Anchor text: ") "</a>"))                

   ;;graphics
   (image   "a"       nil               "Aligned Image"	  ("<img align=\"" (r "Alignment: ") "\" src=\"" (r "Image URL: ") "\">"))
   (image   "i"       "<img src="	"Image"		  ("<img src=\"" (r "Image URL: ") "\">"))
   (image   "e"       "<img align="     "Aligned Image With Alt. Text"	  ("<img align=\"" (r "Alignment: ") "\" src=\"" (r "Image URL: ") "\" alt=\"" (r "Text URL: ") "\">"))
   (image   "t"       "<img alt="	"Image With Alternate Text"	  ("<img alt=\"" (r "Text URL: ") "\" src=\"" (r "Image URL: ") "\">"))

   ;;text elements
   (textel  "\C-c="    nil		"Horizontal Line"	  (& "<hr>\n"))
   (textel  "\C-c\C-m" nil		"Line Break"		  ("<br>\n"))
   (textel  "\e\C-m"  nil		"Paragraph"	  ("<p>\n"))

   ;;head elements
   (head    "H"       "<head>"          "Head"            ("<head>\n" "</head>\n"))
   (head    "B"       "<body>"          "Body"            ("<body>\n" "</body>\n"))
   (head    "i"	      "<isindex>"	"Isindex"         ("<isindex>\n"))
   (head    "n"	      "<nextid>"	"Nextid"          ("<nextid>\n"))
   (head    "h"       "<meta http-equiv=" "HTTP Equivalent" ("<meta http-equiv=\"" (p "Equivalent: ") "\" content=\"" (r "Content: ") "\">\n"))
   (head    "m"       "<meta name="     "Meta Name"       ("<meta name=\"" (p "Name: ") "\" content=\"" (r "Content: ") "\">\n"))
   (head    "l"	      "<link"		"Link"            ("<link href=\"" p "\">"))
   (head    "b"       "<base"		"Base"            ("<base href=\"" r "\">"))
   (head    "t"	      "<title>"		"Title"           ("<title>" (r "Document title: ") "</title>"))
   ))

;;}}}
;;{{{ html-helper-smart-insert-item

;; there are two different kinds of items in HTML - those in regular
;; lists <li> and those in dictionaries <dt>..<dd>
;; This command will insert the appropriate one depending on context.

(defun html-helper-smart-insert-item (&optional arg)
  "Insert a new item, either in a regular list or a dictionary."
  (interactive "*P")
  (let ((case-fold-search t))
    (if
        (save-excursion
          (re-search-backward "<li>\\|<dt>\\|<ul>\\|<ol>\\|<dd>\\|<menu>\\|<dir>\\|<dl>" nil t)
          (looking-at "<dt>\\|<dl>\\|<dd>"))
        (tempo-template-html-definition-item arg)
      (tempo-template-html-list-item arg))))

;; special keybindings in the prefix maps (not in the list of tags)
(and (boundp 'html-helper-list-map)
     (define-key html-helper-list-map "i" 'html-helper-smart-insert-item))

;; and, special menu bindings
(and (boundp 'html-helper-list-menu)
     (setq html-helper-list-menu
	   (cons '["List Item" html-helper-smart-insert-item t] html-helper-list-menu)))

;;}}}
;;{{{ menu support

;; menus are built for easymenu. html-helper-add-tag builds
;; submenus based on tag type, the expert menu code lumps them
;; together into one list and calls easy-menu-define

(defvar html-helper-novice-menu
  '("HTML"
    ["Insert Paragraph" tempo-template-html-paragraph t]
    ["Insert Hyperlink" tempo-template-html-hyperlink t]
    ["Insert Big Header" tempo-template-html-header-2 t]
    ["Insert Unordered List" tempo-template-html-unordered-list t]
    ["Insert List Item" html-helper-smart-insert-item t]
    ["Insert Inlined Image" tempo-template-html-image-with-alternate-text t]
    ["Turn on Expert Menu" html-helper-toggle-expert-menu t])
  "Menu for novices, only installed if `html-helper-use-expert-menu is nil'")

(defun html-helper-menu nil
  "Return the proper menu. Looks at `html-helper-use-expert-menu'"
  (if html-helper-use-expert-menu
      (html-helper-expert-menu)
    html-helper-novice-menu))

(defun html-helper-rebuild-menu nil
  "Rebuild and install the HTML menu (using `easy-menu-define').
If `html-helper-use-expert-menu' is nil, then just use a novice menu."
  (let ((menu (html-helper-menu)))
    (easy-menu-remove menu)
    (easy-menu-define html-helper-mode-menu-symbol
		      html-helper-mode-map "HTML menus" menu)
    (easy-menu-add menu html-helper-mode-map)))

(defun html-helper-toggle-expert-menu (&optional arg)
  "Toggle full HTML menus. Optional arg acts like minor-mode args."
  (interactive "P")
  (setq html-helper-use-expert-menu
	(if (null arg) (not html-helper-use-expert-menu)
	  (> (prefix-numeric-value arg) 0)))
  (html-helper-rebuild-menu))

;; If browse-url loaded, add this in the novice menu.
(if (fboundp 'browse-url-of-file)
    (setq html-helper-novice-menu
	  (append html-helper-novice-menu 
		  (list ["Load This Buffer in Browser" browse-url-of-file t]))))

;; Expert menus: consed up out of html-helper-installed-types
(defun html-helper-expert-menu ()
  "This menu is based on the current value of `html-helper-installed-types'.
This function can be called again, it redoes the entire menu."
  ;; first, reset this so we can call this again and again.
  (setq html-helper-mode-menu nil)
  
  ;; Cons in the toggle of the menu
  (setq html-helper-mode-menu
	(cons '["Turn on Novice Menu"
		html-helper-toggle-expert-menu t]
	      html-helper-mode-menu))

  ;; Now add in user-provided menu stuff
  (setq html-helper-mode-menu
	(append html-helper-user-menu html-helper-mode-menu))

  ;; Now cons in the browse-url functions
  (if (fboundp 'browse-url-of-file)
    (setq html-helper-mode-menu
	  (cons '["Load this Buffer in Browser" browse-url-of-file t]
		html-helper-mode-menu)))
  (if (and (boundp 'browse-url-browser-function) (fboundp browse-url-browser-function))
    (setq html-helper-mode-menu
	  (cons (vector "Browse URL at point" browse-url-browser-function t)
		html-helper-mode-menu)))

  ;; cons in the timestamp delimiters
  (setq html-helper-mode-menu
	(cons '["Insert Timestamp Delimiter"
		html-helper-insert-timestamp-delimiter-at-point t]
	      html-helper-mode-menu))
  
  ;; now cons up the main menu out of the submenus
  (mapcar
   (function (lambda (type)
	       (setq html-helper-mode-menu
		     (cons (html-helper-normalized-menu-for type)
			   html-helper-mode-menu))))
	  html-helper-installed-types)

  ;; now tack on our name
  (setq html-helper-mode-menu (cons "HTML" html-helper-mode-menu))
  html-helper-mode-menu)

(html-helper-rebuild-menu)

;;}}}

;;{{{ context guessing

;; guess where we are in indented lists based on the last list token.
;; it would be much better to try to match </ul> to <ul>, and </ol> to <ol>
;; etc, but that is pretty unwieldy and slow.
;; Note, we make select/option look like a list structure too, so indentation
;; works. This is a bit weird, but it's ok.

(defvar html-helper-any-list-item-start "<li>\\|<dt>\\|<dd>\\|<option\\|<th>\\|<td>")
(defvar html-helper-any-list-item-end "</li>\\|</dt>\\|</dd>\\|</th>\\|</td>")
(defvar html-helper-any-list-start "<dl>\\|<ul>\\|<ol>\\|<menu>\\|<dir>\\|<select\\|<table\\|<tr>")
(defvar html-helper-any-list-end "</dl>\\|</ul>\\|</ol>\\|</menu>\\|</dir>\\|</select>\\|</table>\\|</tr>")
(defvar html-helper-any-list
  (format "\\(%s\\)\\|\\(%s\\)\\|\\(%s\\)\\|\\(%s\\)"
	  html-helper-any-list-start
	  html-helper-any-list-end
	  html-helper-any-list-item-start
	  html-helper-any-list-item-end))

(defvar html-helper-indentation-list
  (format "\\(%s\\)\\|\\(%s\\)\\|\\(%s\\)"
	  html-helper-any-list-start
	  html-helper-any-list-end
	  html-helper-any-list-item-start))
(defvar html-helper-search-limit 2000 "limit on how far back we search")

(defun html-helper-context-symbol ()
  "Return the symbol the last match (against `html-helper-any-list') found."
  (cond ((match-beginning 1) 'list-start)
	((match-beginning 2) 'list-end)
	((match-beginning 3) 'item-start)
	((match-beginning 4) 'item-end)
	(t 'error)))

(defun html-helper-guess-prev-context ()
  "Figure out the last list-type tag before point relevant to indentation.
Returns 'item-start if the last list tag is a list item start
        'start      if the last list tag is the start of a list
        'end        if the last list tag is the end of a list.
Ignores list item ends, because those aren't reliable for indentation."
  (save-excursion
    (let* ((lim (max (point-min) (- (point) html-helper-search-limit)))
	   (context (if (re-search-backward html-helper-indentation-list lim t)
			(html-helper-context-symbol)
		      nil)))
      (cons context (current-indentation)))))

(defun html-helper-print-prev-context ()
  (interactive)
  (message "%s" (html-helper-guess-prev-context)))

;;}}}
;;{{{ indentation

(defvar html-helper-print-indent-info nil
  "If t, indent will print out information as a message.")

(defun html-helper-indent-command ()
  "Command for indenting HTML to the appropriate column.
Calls `html-helper-indent' which tries to examine how many levels down
in nested lists we are and does the appropriate indentation.'
See also `html-helper-basic-offset', `html-helper-item-continue-indent',
and `html-helper-never-indent'."
  (interactive)
  (html-helper-indent))

;; some ideas borrowed from cc-mode.el.
;; Basic logic:
;;   if this line is some sort of list token, indent according to prev context:
;;     if previous context was a list-end or item-start, use its indentation
;;     if previous context was a list start, indent forward basic-offset
;;     ignore previous list-ends, their indentation is unreliable.
;;     then if this is some sort of list-item, do special case fixups:
;;       if this is a item start or end and previous was a list end, go back
;;           item-continue-indent (the </ul> is probably indented for an <li>
;;       if this is a list end and previous was a list end, go back
;;           item-continue-indent (the </ul> is probably indented for an <li>
;;       if this is a list end and prev *not* a list end, go back basic-offset
;;   else if this line is not a list item, and previous line is a item-start
;;     indent continue-indent, because this is part of the item


(defun html-helper-indent ()
  "Indentation workhorse function."
  (if html-helper-never-indent
      ()
    (let ((m (point-marker))
	  (bol (progn (beginning-of-line) (point))))

      ;; unindent the line
      (delete-region (point) (progn (back-to-indentation) (point)))

      (let* ((where (html-helper-guess-prev-context))
	     (prev-context (car where))
	     (this-context nil)
	     (previ (cdr where))
	     (newi (cond 
		    ((eq prev-context 'list-end) previ)
		    ((eq prev-context 'item-start) previ)
		    ((eq prev-context 'list-start) (+ previ html-helper-basic-offset))
		    (t previ))))

	;; newi is set to the basic indentation, now adjust indentation
	;; based on what the current line is.
	(if (looking-at html-helper-any-list)
	    (progn
	      (setq this-context (html-helper-context-symbol))
	      (cond
	       ;; item start or end and last line was a list-end: go backwards
	       ((and
		 (or (eq this-context 'item-start) (eq this-context 'item-end))
		 (eq prev-context 'list-end))
		(setq newi (- newi html-helper-item-continue-indent)))
	       
	       ;; end of list and last line was an end: go backwards twice
	       ((and (eq this-context 'list-end) (eq prev-context 'list-end))
		(setq newi (- newi html-helper-item-continue-indent html-helper-basic-offset)))
	       
	       ;; Any other end of list? Indent negative
	       ((and (eq this-context 'list-end))
		(setq newi (- newi html-helper-basic-offset)))
	       
	       ;; start of list and last line beginning of item, go forwards
	       ((and (eq this-context 'list-start) (eq prev-context 'item-start))
		(setq newi (+ newi html-helper-item-continue-indent)))))
	  
	  ;; default: no special case, indent forward for text
	  (cond
	   ;; last line an item? Beginning of continued item - go forward
	   ((eq prev-context 'item-start)
	    (setq newi (+ newi html-helper-item-continue-indent)))))

	(if html-helper-print-indent-info
	    (message "Last Context: %s, This Context: %s, Previous: %s New: %s" prev-context this-context previ newi))
	
	;; just in case
	(if (< newi 0)
	    (setq newi 0))
	(indent-to newi newi)
	
	;; adjust point to where it was before, or at start of indentation
	(goto-char (marker-position m))
	(if (< (current-column) (current-indentation))
	    (back-to-indentation))))))

;;}}}
;;{{{ completion finder for tempo

(defvar html-helper-completion-finder
  (if html-helper-emacs18
      'html-helper-emacs18-completion-finder
    "\\(\\(<\\|&\\).*\\)\\=")
  "Passed to tempo-use-tag-list, used to find tags to complete.")

;; The regexp finds everything between the last < or & and point,
;; which is good enough to match the tags HTML might complete.
;; emacs18 doesn't have the \= for regexps, though, so we do something
;; more hackish.

(defun html-helper-emacs18-completion-finder ()
  "Unfortunately emacs18 doesn't support \\= in regexps, so we do this hack.
If you have problems with it, maybe you should upgrade to emacs19 :-)"
  (let* ((where nil)
         (s (buffer-substring
             (point)
             (setq where (save-excursion
                           (re-search-backward "<\\|&" (min (point-min) 100) t)
                           (point))))))
    (cons s where)))

;;}}}

;;{{{ timestamps

(defun html-helper-update-timestamp ()
  "Basic function for updating timestamps.
It finds the timestamp in the buffer by looking for
`html-helper-timestamp-start', deletes all text up to
`html-helper-timestamp-end', and runs `html-helper-timestamp-hook' which
will should insert an appropriate timestamp in the buffer."
  (save-excursion
    (goto-char (point-max))
    (if (not (search-backward html-helper-timestamp-start nil t))
	(message "timestamp delimiter start was not found")
      (let ((ts-start (+ (point) (length html-helper-timestamp-start)))
	    (ts-end (if (search-forward html-helper-timestamp-end nil t)
			(- (point) (length html-helper-timestamp-end))
		      nil)))
	(if (not ts-end)
	    (message "timestamp delimiter end was not found. Type C-c C-t to insert one.")
	  (delete-region ts-start ts-end)
	  (goto-char ts-start)
	  (run-hooks 'html-helper-timestamp-hook)))))
  nil)

(defun html-helper-default-insert-timestamp ()
  "Default timestamp insertion function."
  (let ((time (current-time-string)))
    (insert "Last modified: "
	    (substring time 0 20)
	    (nth 1 (current-time-zone))
	    " "
	    (substring time -4)
	    "\n")))

(defun html-helper-insert-timestamp-delimiter-at-point ()
  "Simple function that inserts timestamp delimiters at point.
Useful for adding timestamps to existing buffers."
  (interactive)
  (insert html-helper-timestamp-start)
  (insert html-helper-timestamp-end))

;;}}}
;;{{{ html-helper-insert-new-buffer-strings

(tempo-define-template "html-skeleton" html-helper-new-buffer-template
		       nil
		       "Insert a skeleton for a HTML document")

(defun html-helper-insert-new-buffer-strings ()
  "Insert `html-helper-new-buffer-strings'."
  (tempo-template-html-skeleton))

;;}}}

;;{{{ html-helper-mode

(defun html-helper-mode ()
  "Mode for editing HTML documents.
For more documentation and the newest version, 
see http://www.santafe.edu/~nelson/tools/

The main function html-helper-mode provides is a menu and keybindings
for the HTML tags one inserts when writing HTML documents. Selecting
the menu item or typing the key sequence for a command inserts the
corresponding tag and places point in the right place. If a prefix
argument is supplied, the tags is instead wrapped around the region.
Alternately, one can type in part of the tag and complete it with M-TAB.

There is also code for indentation, timestamps, skeletons for new
documents, and lots of other neat features.

\\{html-helper-mode-map}
Written by nelson@santafe.edu, http://www.santafe.edu/~nelson/
"
  (interactive)
  (kill-all-local-variables)

  (use-local-map html-helper-mode-map)
  (setq local-abbrev-table html-helper-mode-abbrev-table)
  (set-syntax-table html-helper-mode-syntax-table)

  (setq mode-name "HTML helper")
  (setq major-mode 'html-helper-mode)

  (make-local-variable 'comment-start)
  (make-local-variable 'comment-end)
  (make-local-variable 'comment-column)
  (make-local-variable 'comment-start-skip)
  (make-local-variable 'indent-line-function)

  ;; font-lock setup for various emacsen: XEmacs, Emacs 19.29+, Emacs <19.29.
  ;; By Ulrik Dickow <dickow@nbi.dk>.  (Last update: 05-Sep-1995).
  (cond	((string-match "XEmacs\\|Lucid" (emacs-version)) ; XEmacs/Lucid
	 (put major-mode 'font-lock-keywords-case-fold-search t))
	;; XEmacs (19.13, at least) guesses the rest correctly.
	;; If any older XEmacsen don't, then tell me.
	;;
	((string-lessp "19.28.89" emacs-version) ; Emacs 19.29 and later
	 (make-local-variable 'font-lock-defaults)
	 (setq font-lock-defaults '(html-helper-font-lock-keywords t t)))
	;;
	(t ; Emacs 19.28 and older
	 (make-local-variable 'font-lock-keywords-case-fold-search)
	 (make-local-variable 'font-lock-keywords)
	 (make-local-variable 'font-lock-no-comments)
	 (setq font-lock-keywords-case-fold-search t)
	 (setq font-lock-keywords html-helper-font-lock-keywords)
	 (setq font-lock-no-comments t)))

  (setq comment-start "<!-- "
	comment-end " -->"
	comment-start-skip "<!--[ \t]*"
	comment-column 0
	indent-line-function 'html-helper-indent)

  (tempo-use-tag-list 'html-helper-tempo-tags html-helper-completion-finder)
  
  (if html-helper-do-write-file-hooks
      (add-hook 'local-write-file-hooks 'html-helper-update-timestamp))

  (if (and html-helper-build-new-buffer (zerop (buffer-size)))
      (html-helper-insert-new-buffer-strings))

  (easy-menu-add (html-helper-menu) html-helper-mode-map)

  (run-hooks 'text-mode-hook)
  (run-hooks 'html-mode-hook)
  (run-hooks 'html-helper-mode-hook))

;;}}}

;;{{{ patterns for font-lock

;; By Ulrik Dickow <dickow@nbi.dk>.
;;
;; Originally aimed at Emacs 19.29.  Later on disabled syntactic fontification
;; and reordered regexps completely, to be compatible with XEmacs (it doesn't
;; understand OVERRIDE=`keep').
;;
;; We make an effort on handling nested tags intelligently.

;; font-lock compatibility with XEmacs/Lucid and older Emacsen (<19.29).
;;
(if (string-match "XEmacs\\|Lucid" (emacs-version))
    ;; XEmacs/Lucid
    ;; Make needed faces if the user hasn't already done so.
    ;; Respect X resources (`make-face' uses them when they exist).
    (let ((change-it
	   (function (lambda (face)
		       (or (if (fboundp 'facep)
			       (facep face)
			     (memq face (face-list)))
			   (make-face face))
		       (not (face-differs-from-default-p face))))))
      (if (funcall change-it 'html-helper-bold-face)
	  (copy-face 'bold 'html-helper-bold-face))
      (if (funcall change-it 'html-helper-italic-face)
	  (copy-face 'italic 'html-helper-italic-face))
      (if (funcall change-it 'html-helper-underline-face)
	  (set-face-underline-p 'html-helper-underline-face t))
      (if (funcall change-it 'font-lock-variable-name-face)
	  (set-face-foreground 'font-lock-variable-name-face "salmon"))
      (if (funcall change-it 'font-lock-reference-face)
	  (set-face-foreground 'font-lock-reference-face "violet")))
  ;; Emacs (any version)
  ;;
  ;; Note that Emacs evaluates the face entries in `font-lock-keywords',
  ;; while XEmacs doesn't.  So XEmacs doesn't use the following *variables*,
  ;; but instead the faces with the same names as the variables.
  (defvar html-helper-bold-face 'bold
    "Face used as bold.  Typically `bold'.")
  (defvar html-helper-italic-face 'italic
    "Face used as italic.  Typically `italic'.")
  (defvar html-helper-underline-face 'underline
    "Face used as underline.  Typically `underline'.")
  ;;
  (if (string-lessp "19.28.89" emacs-version)
      () ; Emacs 19.29 and later
    ;; Emacs 19.28 and older
    ;; Define face variables that don't exist until Emacs 19.29.
    (defvar font-lock-variable-name-face 'font-lock-doc-string-face
      "Face to use for variable names -- and some HTML keywords.")
    (defvar font-lock-reference-face 'underline ; Ugly at line breaks
      "Face to use for references -- including HTML hyperlink texts.")))

(defvar html-helper-font-lock-keywords
  (let (;; Titles and H1's, like function defs.
	;;   We allow for HTML 3.0 attributes, like `<h1 align=center>'.
	(tword "\\(h1\\|title\\)\\([ \t\n]+[^>]+\\)?")
	;; Names of tags to boldify.
	(bword "\\(b\\|h[2-4]\\|strong\\)\\([ \t\n]+[^>]+\\)?")
	;; Names of tags to italify.
	(iword "\\(address\\|cite\\|em\\|i\\|var\\)\\([ \t\n]+[^>]+\\)?")
	;; Regexp to match shortest sequence that surely isn't a bold end.
	;; We simplify a bit by extending "</strong>" to "</str.*".
	;; Do similarly for non-italic and non-title ends.
	(not-bend (concat "\\([^<]\\|<\\([^/]\\|/\\([^bhs]\\|"
			  "b[^>]\\|"
			  "h\\([^2-4]\\|[2-4][^>]\\)\\|"
			  "s\\([^t]\\|t[^r]\\)\\)\\)\\)"))
	(not-iend (concat "\\([^<]\\|<\\([^/]\\|/\\([^aceiv]\\|"
			  "a\\([^d]\\|d[^d]\\)\\|"
			  "c\\([^i]\\|i[^t]\\)\\|"
			  "e\\([^m]\\|m[^>]\\)\\|"
			  "i[^>]\\|"
			  "v\\([^a]\\|a[^r]\\)\\)\\)\\)"))
	(not-tend (concat "\\([^<]\\|<\\([^/]\\|/\\([^ht]\\|"
			  "h[^1]\\|t\\([^i]\\|i[^t]\\)\\)\\)\\)")))
    (list ; Avoid use of `keep', since XEmacs will treat it the same as `t'.
     ;; First fontify the text of a HREF anchor.  It may be overridden later.
     ;; Anchors in headings will be made bold, for instance.
     '("<a\\s-+href[^>]*>\\([^>]+\\)</a>"
       1 font-lock-reference-face t)
     ;; Tag pairs like <b>...</b> etc.
     ;; Cunning repeated fontification to handle common cases of overlap.
     ;; Bold complex --- possibly with arbitrary other non-bold stuff inside.
     (list (concat "<" bword ">\\(" not-bend "*\\)</\\1>")
	   3 'html-helper-bold-face t)
     ;; Italic complex --- possibly with arbitrary non-italic kept inside.
     (list (concat "<" iword ">\\(" not-iend "*\\)</\\1>")
	   3 'html-helper-italic-face t)
     ;; Bold simple --- first fontify bold regions with no tags inside.
     (list (concat "<" bword ">\\("  "[^<]"  "*\\)</\\1>")
	   3 'html-helper-bold-face t)
     ;; Any tag, general rule, just after bold/italic stuff.
     '("\\(<[^>]*>\\)" 1 font-lock-type-face t)
     ;; Titles and level 1 headings (anchors do sometimes appear in h1's)
     (list (concat "<" tword ">\\(" not-tend "*\\)</\\1>")
	   3 'font-lock-function-name-face t)
     ;; Underline is rarely used. Only handle it when no tags inside.
     '("<u>\\([^<]*\\)</u>" 1 html-helper-underline-face t)
     ;; Forms, anchors & images (also fontify strings inside)
     '("<\\(form\\|i\\(mg\\|nput\\)\\)\\>[^>]*>"
       0 font-lock-variable-name-face t)
     '("</a>" 0 font-lock-keyword-face t)
     '("<a\\b[^>]*>" 0 font-lock-keyword-face t)
     '("=[ \t\n]*\\(\"[^\"]+\"\\)" 1 font-lock-string-face t)
     ;; Large-scale structure keywords (like "program" in Fortran).
     ;;   "<html>" "</html>" "<body>" "</body>" "<head>" "</head>" "</form>"
     '("</?\\(body\\|form\\|h\\(ead\\|tml\\)\\)>"
       0 font-lock-variable-name-face t)
     ;; HTML special characters
     '("&[^;\n]*;" 0 font-lock-string-face t)
     ;; SGML things like <!DOCTYPE ...> with possible <!ENTITY...> inside.
     '("<![a-z]+\\>[^<>]*\\(<[^>]*>[^<>]*\\)*>"
       0 font-lock-comment-face t)
     ;; Comment declarations according to the HTML 2.0 spec at
     ;; <URL:http://www.w3.org/pub/WWW/MarkUp/html-spec/html-spec_3.html>.
     ;; Usually `<!-- ... -->', but also e.g the single, complete declaration
     ;; `<!--c1--  -- c2 -- -->c3 (still comment) ----c4- c4--   >'.
     ;; Note that e.g. Netscape 3.01 Gold doesn't fully live up to the spec.
     '("<!\\(--\\([^-]\\|-[^-]\\)*--\\s-*\\)*>" 0 font-lock-comment-face t)))
    "Additional expressions to highlight in HTML helper mode.")

;;}}}
;;{{{ patterns for hilit19

;; Define some useful highlighting patterns for the hilit19 package.
;; These will activate only if hilit19 has already been loaded.
;; Thanks to <dickow@nbi.dk> for some pattern suggestions

(if (featurep 'hilit19)
    (hilit-set-mode-patterns
     'html-helper-mode
     '(("<!--" "-->" comment)
       ("<![a-z]+\\>[^<>]*\\(<[^>]*>[^<>]*\\)*>" nil comment) ;<!DOCTYPE ...>
       ("<title>" "</title>" defun)
       ("<h[1-6]>" "</h[1-6]>" bold) ;only colour inside tag
       ("<a\\b" ">" define)
       ("</a>" nil define)
       ("<img\\b" ">" include)
       ("<option\\|</?select\\|<input\\|</?form\\|</?textarea" ">" include)
       ;; First <i> highlighting just handles unnested tags, then do nesting
       ("<i>[^<]*</i>" nil italic)
       ("<b>" "</b>" bold)
       ("<i>" "</i>" italic)
       ("<u>" "</u>" underline)
       ("&[^;\n]*;" nil string)
       ("<" ">" keyword))
     nil 'case-insensitive)
  nil)

;;}}}

(provide 'html-helper-mode)
(provide 'html-mode)			;for 14 character filename
(run-hooks 'html-load-hook)
(run-hooks 'html-helper-load-hook)

;;; html-helper-mode.el ends here
