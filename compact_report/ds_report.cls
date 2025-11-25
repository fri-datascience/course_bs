%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% FRI Data Science_report LaTeX Template
% Version 1.0 (28/1/2020)
%
% Jure Demšar (jure.demsar@fri.uni-lj.si)
%
% Based on MicromouseSymp article template by:
% Mathias Legrand (legrand.mathias@gmail.com)
% With extensive modifications by:
% Antonio Valente (antonio.luis.valente@gmail.com)
%
% License:
% CC BY-NC-SA 3.0 (http://creativecommons.org/licenses/by-nc-sa/3.0/)
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\NeedsTeXFormat{LaTeX2e}
\ProvidesClass{ds_report}[28/01/2020, v1.0]
\RequirePackage{ifthen}
\RequirePackage{calc}
\AtEndOfClass{\RequirePackage{microtype}}
\DeclareOption*{\PassOptionsToClass{\CurrentOption}{article}}
%% To choose the whether there is one or more authors
\DeclareOption{oneauthor}{\gdef\@authornum{author}}
\DeclareOption{moreauthors}{\gdef\@authornum{authors}}
\ProcessOptions*
\LoadClass{article}
\RequirePackage{ifpdf}      % Needed to pick between latex and pdflatex
\RequirePackage{flushend}   % Needed to align columns in the last page
\RequirePackage[pscoord]{eso-pic}%

%----------------------------------------------------------------------
%	FONTS
%----------------------------------------------------------------------

\RequirePackage{times}      % Loads the Times-Roman Fonts
\RequirePackage{mathptmx}   % Loads the Times-Roman Math Fonts

%----------------------------------------------------------------------
%	VARIOUS USEFUL PACKAGES
%----------------------------------------------------------------------

\RequirePackage[utf8]{inputenc}
\RequirePackage{amsmath,amsfonts,amssymb}
\RequirePackage{graphicx,xcolor}
\RequirePackage{booktabs}
\RequirePackage[OT1]{fontenc}
\RequirePackage{tikz} % For \foreach used for Orcid icon

%%%% URL
\RequirePackage{url}
\urlstyle{same}
\g@addto@macro{\UrlBreaks}{\UrlOrds}

%% To have the possibility to change the urlcolor
\newcommand{\changeurlcolor}[1]{\hypersetup{urlcolor=#1}}

%----------------------------------------------------------------------
%	MARGINS
%----------------------------------------------------------------------

\RequirePackage[left=0.8cm,%
right=0.8cm,%
top=0.8cm,%
bottom=0.8cm,%
headheight=9pt,%
letterpaper]{geometry}%

%-------*--------------------------------------------------------------------------------
%	COLUMNS
%----------------------------------------------------------------------------------------

\setlength{\columnsep}{0.4cm} % Distance between the two columns of text

%----------------------------------------------------------------------------------------
%	COLORS
%----------------------------------------------------------------------------------------

\definecolor{color1}{RGB}{70,70,70} % Color of the article title and sections
\definecolor{color2}{RGB}{200,200,200} % Color of the boxes behind the abstract and headings

%----------------------------------------------------------------------------------------
%	HYPERLINKS
%----------------------------------------------------------------------------------------

\usepackage{hyperref} % Required for hyperlinks
\hypersetup{hidelinks,colorlinks,breaklinks=true,urlcolor=color1,citecolor=color1,linkcolor=color1,bookmarksopen=false,pdftitle={Title},pdfauthor={Author}}

%----------------------------------------------------------------------
%	CODE
%----------------------------------------------------------------------

\usepackage{listings}
\lstset{frame=lines}
\lstset{basicstyle=\footnotesize\ttfamily,breaklines=true}

%----------------------------------------------------------------------
%	FIGURES AND TABLES CAPTIONS
%----------------------------------------------------------------------

\RequirePackage[labelfont={bf,sf,small},%
labelsep=period,%
justification=raggedright]{caption}
\setlength{\abovecaptionskip}{0pt}
\setlength{\belowcaptionskip}{0pt}

%----------------------------------------------------------------------
%	PAGE HEADER
%----------------------------------------------------------------------

\RequirePackage{fancyhdr}  % Needed to define custom headers/footers
\pagestyle{fancy}  % Enables the custom headers/footers
% Headers
\lhead{}%
\chead{}%
\rhead{}%
% Footers
\lfoot{}% \cright
\cfoot{}%
\rfoot{}%

\renewcommand{\headrulewidth}{0pt}% % No header rule
\renewcommand{\footrulewidth}{0pt}% % No footer rule

%----------------------------------------------------------------------
%	SECTION/SUBSECTION/PARAGRAPH SET-UP
%----------------------------------------------------------------------

\RequirePackage[explicit]{titlesec}
\titleformat{\section}
  {\color{color1}\large\sffamily\bfseries}
  {}
  {0em}
  {\colorbox{color2}{\parbox{\dimexpr\linewidth-2\fboxsep\relax}{\centering\arabic{section}. #1}}}
  []
\titleformat{name=\section,numberless}
  {\color{color1}\large\sffamily\bfseries}
  {}
  {0em}
  {\colorbox{color2}{\parbox{\dimexpr\linewidth-2\fboxsep\relax}{\centering#1}}}
  []
\titleformat{\subsection}
  {\color{color1}\sffamily\bfseries}
  {\thesubsection}
  {0.5em}
  {#1}
  []
\titleformat{\subsubsection}
  {\sffamily\small\bfseries}
  {\thesubsubsection}
  {0.5em}
  {#1}
  []
\titleformat{\paragraph}[runin]
  {\sffamily\small\bfseries}
  {}
  {0em}
  {#1}
\titlespacing*{\section}{0pc}{3ex \@plus4pt \@minus3pt}{5pt}
\titlespacing*{\subsection}{0pc}{2.5ex \@plus3pt \@minus2pt}{0pt}
\titlespacing*{\subsubsection}{0pc}{2ex \@plus2.5pt \@minus1.5pt}{0pt}
\titlespacing*{\paragraph}{0pc}{1.5ex \@plus2pt \@minus1pt}{10pt}

%----------------------------------------------------------------------
%	LIST CONTROL
%----------------------------------------------------------------------

\RequirePackage{enumitem}

%----------------------------------------------------------------------
%	ABSTRACT+AUTHOR FRAME
%----------------------------------------------------------------------

\newcommand{\Authors}[1]{\def\@Authors{#1}}
\newcommand{\HWInfo}[1]{\def\@HWInfo{#1}}

% ---------------------------------------------------------------------

\renewcommand{\@maketitle}{%
\twocolumn[{%
\thispagestyle{empty}%
{\includegraphics[scale=0.5]{logo}}%
\vskip-36pt%
{\raggedleft\small\sffamily\bfseries\@HWInfo\par}%
{\raggedleft\small\sffamily\bfseries\@Authors\par}%
\vskip24pt%
}]%
}

%----------------------------------------------------------------------
%	REFERENCES
%----------------------------------------------------------------------

% Remove brackets from numbering in List of References
\renewcommand{\@biblabel}[1]{\bfseries\color{color1}\textsuperscript{[#1]}}
%\setlength{\bibitemsep}{0cm}
\let\oldbibliography\thebibliography
\renewcommand{\thebibliography}[1]{%
\addcontentsline{toc}{section}{\refname}%
\oldbibliography{#1}%
\setlength\itemsep{0pt}}%
