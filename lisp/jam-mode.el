;;; jam-mode.el --- mode for editing Jam rules files  -*- lexical-binding: t; -*-

; SPDX-License-Identifier: MIT
; SPDX-FileCopyrightText: 2023 Chris Roberts

;;; Commentary:

; This mode currently only provides basic syntax highlighting.
; TODO indentation support

;;; Code:

; Jam built-in keywords
(defvar jam-mode-keywords
  '("actions" "bind" "break" "case" "continue" "default" "else" "existing" "for" "if" "ignore" "in" "include" "jumptoeof"
    "local" "maxline" "on" "piecemeal" "quietly" "return" "rule" "switch" "together" "updated" "while"))

; Jam built-in variables
(defvar jam-mode-constants
  '("JAM_TARGETS" "JAMDATE" "JAMJOBS" "JAMSHELL" "JAMUNAME" "JAMVERSION" "MAC" "MINGW" "NT" "OS" "OS2" "OSPLAT" "OSVER"
    "UNIX" "VMS"))

; Jam built-in rules/actions
(defvar jam-mode-functions
  '("Always" "ALWAYS" "Depends" "DEPENDS" "echo" "Echo" "ECHO" "Glob" "GLOB" "Includes" "INCLUDES" "Leaves" "LEAVES"
    "LOCATE" "Match" "MATCH" "NoCare" "NOCARE" "NOTIME" "NotFile" "NOTFILE" "NoUpdate" "NOUPDATE" "SEARCH" "Temporary"
    "TEMPORARY"))

; Jam built-in rules/actions (warnings)
(defvar jam-mode-warnings
  '("exit" "Exit" "EXIT"))

; Jambase rules/actions
(defvar jam-mode-jambase-functions
  '("Archive" "As" "Bulk" "C++" "Cc" "Chgrp" "Chmod" "Chmod1" "Chown" "Clean" "Depends" "File" "Fortran" "GenFile"
    "GenFile1" "HardLink" "HdrRule" "Install" "InstallBin" "InstallFile" "InstallInto" "InstallLib" "InstallMan"
    "InstallShell" "Lex" "Library" "LibraryFromObjects" "Link" "LinkLibraries" "LocalClean" "LocalDepends" "Main"
    "MainFromObjects" "MakeLocate" "MkDir" "MkDir1" "Object" "ObjectC++Flags" "ObjectCcFlags" "ObjectDefines"
    "ObjectHdrs" "Objects" "Ranlib" "RmTemps" "Setuid" "SubDir" "SubDirC++Flags" "SubDirCcFlags" "SubDirHdrs"
    "SubInclude" "SubRules" "Shell" "Undefines" "UserObject" "Yacc" "Yacc1" "FGrist" "FGristFiles" "FGristSourceFiles"
    "FIsPrefix" "FReverse" "FSubDir" "FStripCommon" "FRelPath" "FAppendSuffix" "FQuote" "FDefines" "FIncludes" "FDirName"
    "BULK" "FILE" "HDRRULE" "INSTALL" "INSTALLBIN" "INSTALLLIB" "INSTALLMAN" "LIBRARY" "LIBS" "LINK" "MAIN" "SETUID"
    "SHELL" "UNDEFINES" "addDirName" "makeCommon" "makeDirName" "makeGrist" "makeGristedName" "makeRelPath"
    "makeString" "makeSubDir" "makeSuffixed"))

; Jambase targets
(defvar jam-mode-jambase-targets
  '("all" "clean" "dirs" "exe" "files" "first" "install" "lib" "obj" "shell" "uninstall"))

; Jambase variables
(defvar jam-mode-jambase-variables
  '("ALL_LOCATE_TARGET" "AR" "ARFLAGS" "AS" "ASFLAGS" "AWK" "BCCROOT" "BINDIR" "C++" "C++FLAGS" "CC" "CCFLAGS" "CHMOD"
    "CP" "CW" "CWGUSI" "CWMAC" "CWMSL" "DOT" "DOTDOT" "EXEMODE" "FILEMODE" "FORTRAN" "FORTRANFLAGS" "GROUP" "HDRGRIST"
    "HDRPATTERN" "HDRRULE" "HDRS" "HDRSCAN" "HDRSEARCH" "INSTALL" "INSTALLGRIST" "JAMFILE" "JAMRULES" "JAMBASEDATE"
    "LEX" "LIBDIR" "LINK" "LINKFLAGS" "LINKLIBS" "LOCATE_SOURCE" "LOCATE_TARGET" "LN" "MACINC" "MANDIR" "MKDIR" "MODE"
    "MSLIB" "MSLINK" "MSIMPLIB" "MSRC" "MSVC" "MSVCNT" "MV" "NEEDLIBS" "NOARSCAN" "OSFULL" "OPTIM" "OWNER" "RANLIB"
    "RCP" "RELOCATE" "RM" "RMDIR" "RSH" "RUNVMS" "SEARCH_SOURCE" "SED" "SHELLHEADER" "SHELLMODE" "SLASH" "SLASHINC"
    "SOURCE_GRIST" "STDHDRS" "STDLIBPATH" "SUBDIR" "SUBDIRASFLAGS" "SUBDIRC++FLAGS" "SUBDIRCCFLAGS" "SUBDIRHDRS"
    "SUBDIRRULES" "subdirreset" "SUBDIR_TOKENS" "SUFEXE" "SUFLIB" "SUFOBJ" "UNDEFFLAG" "UNDEFS" "WATCOM" "YACC"
    "YACCGEN" "YACCFLAGS" "YACCFILES"))

; Jam variable references $(foo)
(defvar jam-mode-variable-reference
  "\\$(\\([^ :\\[()\t\r\n]+\\)[)\\[:]")

; Basic imenu support
(defvar jam-imenu-generic-expression
  '(("Actions" "^actions\\s-+\\([A-Za-z0-9_]+\\)" 1)
    ("Rules" "^rule\\s-+\\([A-Za-z0-9_]+\\)" 1)))

(defvar jam-mode-font-lock-defaults
  `((
     ( ,(regexp-opt jam-mode-keywords 'words) . font-lock-keyword-face)
     ( ,(regexp-opt jam-mode-constants 'words) . font-lock-constant-face)
     ( ,(regexp-opt jam-mode-functions 'words) . font-lock-builtin-face)
     ( ,(regexp-opt jam-mode-warnings 'words) . font-lock-warning-face)
     ( ,(regexp-opt jam-mode-jambase-functions 'words) . font-lock-function-name-face)
     ( ,(regexp-opt jam-mode-jambase-targets 'words) . font-lock-builtin-face)
     ( ,(regexp-opt jam-mode-jambase-variables 'words) . font-lock-function-name-face)
     ( ,(symbol-value 'jam-mode-variable-reference) . font-lock-variable-name-face)
     )))

(defvar jam-mode-tab-width 4 "Width of a tab.")

(defvar jam-mode-tab-indent t "Use tabs for indentation.")

(defvar jam-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?\# "<" table)
    (modify-syntax-entry ?\n ">#" table)
    (modify-syntax-entry ?_ "w" table)
    (modify-syntax-entry ?. "w" table)
    (modify-syntax-entry ?/ "w" table)
    (modify-syntax-entry ?+ "w" table)
  table)
  "Syntax table for Jam mode.")

;;;###autoload
(define-derived-mode jam-mode prog-mode "Jam"
  "Jam mode is a major mode for editing Jam rules files."

  (setq-local font-lock-defaults jam-mode-font-lock-defaults)

  (when jam-mode-tab-width
    (setq-local tab-width jam-mode-tab-width))

  (when jam-mode-tab-indent
    (setq-local indent-tabs-mode t))

  (setq-local comment-start "# ")
  (setq-local comment-end "")
  (setq-local comment-start-skip "#+[\t ]*")

  (setq-local imenu-generic-expression jam-imenu-generic-expression)

  (imenu-add-to-menubar "Jam")

  (set-syntax-table jam-mode-syntax-table)
)

;;;###autoload
(add-to-list 'auto-mode-alist '("\\(Jamfile\\|Jamrules\\|Jambase\\)\\'" . jam-mode))

(provide 'jam-mode)

;;; jam-mode.el ends here
