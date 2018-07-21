/*
\documentclass{book}[9pt]
%\oddsidemargin  0.5in
%\evensidemargin 0.0in
%\textwidth      7in
%\headheight     0.0in
%\topmargin      0.0in
%\textheight     8in

\usepackage[matrix,curve,arrow,tips,frame]{xy}
\usepackage{bar}
\usepackage{epsfig}
\usepackage{verbatim}
\usepackage{makeidx}         % allows index generation
\usepackage{multicol}        % used for the two-column index

\newenvironment{code}%
{\small \verbatim}%
{\endverbatim \large}

\newtheorem{theorem}{Theorem}
\newtheorem{prop}{Proposition}
\newtheorem{axiom}{Axiom}
\newtheorem{definition}{Definition}

\makeindex

\begin{document}

\author{Randall A. Helzerman}
\title{The 36 Chambers of SHRDLU}
\maketitle

\frontmatter

\tableofcontents

\mainmatter

\chapter{Introductory Justifications}

This chapter consists of a bunch of sales jobs--basically its purpose
is to convince you that two propositions are true:
\begin{enumerate}
\item This book is worth spending your {\em money} on to {\em buy} it.
\item This book is worth spending your {\em time} on to {\em understand} it.
\end{enumerate}
\noindent I'll approach this by trying to answer some questions you may be
bringing to this text.

\section{Why arn't you covering language learning/statistical grammars?}

A book on learning languages would be very interesting, but I'm unaware of
any book which covers that topic in any computationally interesting sense.
All books and research efforts in natural language learning (so-called)
today really arn't focused on learning language as children learn language.  
Rather, they are focused on a very different problem--the induction of
statistical parsers from corpora consisting of parse trees for sentences.

Why do I consider that an uninteresting problem?  Well, first of all, for the
past 13 years or so, that's what everybody has been doing and the results are
rather megre.  Its not at all obvious that this line of reasearch is going
anywhere.

Second, even if it {\em could} be convincingly pulled off, it {\em still}
would shed no light on how children actually learn language, or on how adults
actually process language, because nobody learns languages from corpora of
parse trees.  Its a problem invented by academics for academics.

And third, even if you are more interested in saving money than you are in
solving interesting problems, there really is no economic justification for
it.  The promise is that instead of needing a highly-skilled (and therefore
expensive) natural language engineer to manually create grammars, why not just
have a program automatically learn it from parsed corpora?  Sounds great, but
lets not forget that it takes just as highly-skilled (and therefore just as
expensive) natural langauge engineers to produce parsed corpora.  Its actually
far more expensive to create an extensive corpus and ensure that it is error
free than it is to write a wide coverage grammar for a language.  Microsoft,
for example, has extremely wide-coverage grammars for all the world's major
languages, for use in Microsoft Word's grammar checker--all lovingly hand
crafted.

Syntactic theory is actually the best understood aspect of natural language
processing--and it certainly isn't what is holding us back.  One motivation
for writing this book is to try to bring the attention back to actual, not
artifactual, problems.

\chapter{Running, Fixing, and Enhancing this Program}

\section{SWI Prolog}

\subsection{Declarations, etc}


\begin{code}
*/

:- op(870,xfy,[=>]).
:- op(880,xfy,[<=>]).
:-op(900,fx,not).

:-discontiguous(np/6).
:-discontiguous(defunkify/3).
:-discontiguous(case/2).
:-discontiguous(case/2).
:-discontiguous(funky/1).
:-discontiguous(kind/3).
:-discontiguous(word/1).
:-discontiguous(mood/3).
:-discontiguous(process_logical_form/2).
:-discontiguous(sense/3).
:-discontiguous(sense/4).
:-discontiguous(voice/3).
:-discontiguous(sense/5).
:-discontiguous(sense/6).
:-discontiguous(subcat/3).
:-discontiguous(subcat/4).
:-discontiguous(grammatical_number/3).
:-discontiguous(grammatical_person/3).
:-discontiguous(gender/2).
:-discontiguous(category/2).
:-discontiguous(category/3).
:-discontiguous(category/4).
:-discontiguous(behavior/3).
:-discontiguous(verb_base/2).
:-discontiguous(verb_form/5).
:-discontiguous(nc_transform/3).
:-discontiguous(prove/3).
:-discontiguous(quantifier/3).
:-discontiguous(root_word/2).
:-discontiguous(s/3).
:-discontiguous(conjugation/2).
:-discontiguous(thing_range/3).
:-discontiguous(thing_super/2).

:-dynamic(axiom/1).
:-dynamic(dial/3).

/*
\end{code}

\subsection{Not predicate}

\begin{code}
*/

not F --> F, !, { fail }.
not _ --> [].


/*
\end{code}


\chapter{The First Chamber: Prolog}

\begin{code}
*/

get_sentence(S) :-
        snarf_line(L1),
        chunks(_Dummy,S,L1,_L2).

/*
\end{code}

\chapter{The Second Chamber: Tokenization and Parsing}

The tokenizer is a list-to-list transducer which takes the raw input list and
converts it into something which is easier to write a parser for.  Because it
is a list transducer, we use difference lists in order to support
constant-time concatenation.

A limitation of the tokenizer is that it assumes ascii input.  It would have
to be reworked if we were to support Unicode.

The tokenizer supplies two high-level predicates: snarf\_line/1 and chunks/4.
The predicate snarf\_line/1 just keeps reading characters until an end line
has been reached.  The predicate chunks/4 takes a list of characters (such as
that supplied by snarf\_line/1) and breaks it into "chunks".  Chunks
correspond roughly to words and punctuation, with caveats as noted below.
Generally, we follow the lead of the PENN Treebank project \cite{marcus93} on
tokenization.

\section{Getting a line of input from stdin}

\begin{code}
*/

snarf_line(L) :-
        %  Get a character.
        get0(C),
        
        % process it.
        ( (C = 10) ->
            L = []
        ;
            snarf_line(Cs),
            L = [C|Cs]
        ).

/*
\end{code}

\section{Chunks}

Now, we break up the input list into "chunks":

\begin{code}
*/

chunk([A|L]) -->
        optional_white_space,
        apostrophe(A),
        chunk(L).

chunk(W) -->
        optional_white_space,
        word(W).

chunk(W) -->
        optional_white_space,
        numeral(W).

chunk([W]) -->
        optional_white_space,
        non_chunking_punctuation(W).

/*
\end{code}

\subsection{Whitespace}

Chunks are seperated by whitespace, so we have to specify what counts as
whitespace:

\begin{code}
*/

white_space([H|T]) -->
        white_space_char(H),
        (
          white_space(T)
        ;
          end_white_space(T)
        ).

white_space_char(H) -->
        (
          space_char(H)
        ;
          tab_char(H)
        ).

space_char(32) --> [32].
tab_char(9)    --> [9].

end_white_space([]) --> not white_space_char(_H).

/*
\end{code}

We use this trick for optional white space:

\begin{code}
*/

optional_white_space --> white_space(_S),!.
optional_white_space -->[].

/*
\end{code}

\subsection{Recognizing Letters}

\begin{code}
*/

letter(H) -->
        (
          lower_case_letter(H)
        ;
          upper_case_letter(H)
        ;
          underscore(H)
        ).


lower_case_letter(H) -->
        [H],
        { H >= 97  },
        { H =< 122 }.

upper_case_letter(H) -->
        [H],
        { H >= 65 },
        { H =< 90 }.

underscore(95) --> [95].

/*
\end{code}

\subsection{Words}

The next step is to group segments of letters into words.

\begin{code}
*/

end_word([]) --> not letter(_H).
word([H|T]) -->
        letter(H),
        ( word(T)
        ; end_word(T)
        ).


/*
\end{code}

\subsection{Numerals}

In the same way, we can group digits and numbers:

\begin{code}
*/

digit(H) -->
        [H],
        { H >= 48 },
        { H =< 57 }.

end_numeral([]) --> not digit(_H).
numeral([H|T]) -->
        digit(H),
        (
          numeral(T)
        ;
          end_numeral(T)
        ).

/*
\end{code}

\subsection{Punctuation}

Words can also be seperated by punctuation:

\begin{code}
*/

punctuation(C) -->
        apostrophe(C)
    ;
        non_chunking_punctuation(C).


non_chunking_punctuation(C) -->
        comma(C)
    ;
        semicolon(C)
    ;
        period(C)
    ;
        exclamation_mark(C)
    ;
        question_mark(C)
    ;
        hyphen(C).

comma(44) --> [44].

semicolon(59) --> [59].

apostrophe(39) --> [39].

period(46) --> [46].

exclamation_mark(33) --> [33].

question_mark(63) --> [63].

hyphen(45) --> [45].


/*
\end{code}

\subsection{Defunkification}

So-called "funky" words like "gonna" replaced by  "gon na"

\begin{code}
*/

funky(gunna).
funky(woulda).
funky(coulda).
funky(shoulda).
funky(something).
funky(somethings).

defunkify(gunna,[gun,na|X],X).
defunkify(woulda,[would,a|X],X).
defunkify(coulda,[could,a|X],X).
defunkify(shoulda,[should,a|X],X).
defunkify(something,[some,thing|X],X).
defunkify(somethings,[some,things|X],X).

/*      
\end{code}

\noindent The reason we break things up like this is explained on the Penn
treebank web page, "This tokenization allows us to analyze each component
separately, so (for example) "I" can be in the subject Noun Phrase while "'m"
is the head of the main verb phrase."

The following arn't funky in the sense of the Penn treebank, but since we
already have the machinerty there, it is convenient to treat them as funky.

\begin{code}
*/

funky(table).
defunkify(table,[w_table|X],X).

/*
\end{code}

\subsection{Putting the chunks together}

The following is really interesting...because a single chunk can induce more
than one word in the resulting tokenized stream, we need to use append.  To
make that fast, we need to use difference lists.  This makes the code much
harder to understand...

\begin{code}
*/


chunks(_H,[]) --> not chunk(_W).
chunks(H,T1) -->
        optional_white_space,
        chunk(H1),
        { name(H2,H1) },
        { ( funky(H2) ->
              defunkify(H2,T1,T2)
          ;
              T1 = [H2|T2]
          )
        },
        
        chunks(H,T2).
/*
\end{code}


\chapter{The Third Chamber: Grammars for Natural Language}

\section{Skeleton Grammar of English}

\begin{code}

s --> np, vp.

np --> det, noun.

det --> [a].

noun --> [block].

vp --> verb, adjective.

verb --> [is].

adjective --> [red].
adjective --> [green].

\end{code}

\section{Reusing parts of the grammar}

\begin{code}

s(assertion) --> 
    np, 
    vp([],[]).

s(yn_question) --> 
    verb([],[],W),
    np, 
    vp([gap(W)],[]).

np --> det, noun.

det --> [a].

noun --> [block].

vp(P1,P2) --> verb(P1,P2,_), adjective.


verb([],[],is) --> [is].
verb([gap(W)],[],W) --> [W].


adjective --> [red].
adjective --> [green].

\end{code}

\chapter{The Fourth Chamber: Enabling Elementary Dialog}
\label{language_game_0}
In this chapter, we'll  create a very small language--small enough that we can
{\em completely} implement it so that we can:
\begin{enumerate}
\item Parse an input sentence which is an assertion or query.
\item If its an assertion compile it to Horn clauses.
\item If it is a query, compile it to a query against a horn clause database.
\item Take the results of any such query and express them in English.
\end{enumerate}
\noindent In particular, we want to be able to have the following
conversation with the computer:
\begin{verbatim}
a block is blue.
ok.

is a block blue?
yes.

is a block red?
no.

a block is red.
ok.

is a block red?
yes.
\end{verbatim}
\noindent This is, no doubt, not the most scintilating of conversations, but
the important point is that we cover {\em every} part of NLP in that we have
solutions--vestigial and elementary solutions, to be sure--but solutions
nontheless to all phases of a NLP read-eval-print loop.  This will be a secure
foundation we can use to elaborate upon.

\section{Grammar}

\begin{code}

sentence(Mood,L) -> s(Mood,L).

s(assertion,L) --> 
    np(X^Q^L),
    vp([],[], X^Q),
    ['.'].

s(yn_question,L) --> 
    verb([],[],W),
    np(X^Q^L), 
    vp([gap(W)],[], X^Q),
    ['?'].

np(X^Q^L) --> det(X^R^Q^L), noun(X^R).

det(X^R^Q^exists(X,and(R,Q)))--> [a].

noun(X^block(X)) --> [block].

vp(P1,P2,L) --> verb(P1,P2,_), adjective(L).


verb([],[],is) --> [is].
verb([gap(W)],[],W) --> [].


adjective(X^red(X)) --> [red].
adjective(X^green(X)) --> [green].

\end{code}

\section{Compiling to Horn Clauses}

\subsection{Skolemization}

\begin{code}
*/

gensym(Root, Atom) :-
        name(Root,Name1),
        get_num(Root,Num),
        name(Num,Name2),
        append(Name1,Name2,Name),
        name(Atom,Name).

get_num(Root,Num1) :-
        retract(current_num(Root,Num)),!,
        Num1 is Num + 1,
        asserta(current_num(Root,Num1)).

get_num(Root,1) :- asserta(current_num(Root,1)).

skolemise(X) :- gensym(sk,X).

/*
\end{code}

\subsubsection{Nute-Covington Transform}

\begin{code}

nc_transform(F,Cs) :- nc_transform(F,Cs,[]).

nc_transform(exists(X,R), H,T) :- !,
        skolemise(X),
        nc_transform(R,H,T).

nc_transform(and(R,S), H,T) :-!,
        nc_transform(R, H,I),
        nc_transform(S, I,T).

nc_transform(F, [axiom(F)|T],T).

\end{code}

\section{Compling for query}

\subsection{Lloyd-Topor Transform}

\begin{code}

lt_transform(exists(_X,R), R1) :- !,
        lt_transform(R,R1).

lt_transform(and(P,Q), (P1,Q1)) :- !,
        lt_transform(P,P1),
        lt_transform(Q,Q1).

lt_transform(F,F).

\end{code}

\section{Dialog}

\begin{code}

dialog :-
        % remove previous resuls
        retractall(axiom(_)),
        
        read_eval_loop.

read_eval_loop :-
        get_sentence(S),
        ( S=[bye,'.'] ->
            true
        ;
            process_sentence(S),
            read_eval_loop
        ).

process_sentence(S) :-
        % parse the sentence
        ( sentence(Mood,L,S,[]) ->
             % see how to process it
             process_logical_form(Mood,L)
        ;
             write('couldn\'t understand this:'),
             writeln(S)
        ).


% if this is an assertion, run the nute-covington transform and assert
process_logical_form(assertion,L) :-
    nc_transform(L,Fs),
    assert_all(Fs).

assert_all([]).
    
assert_all([F|Fs]) :-
    assert(F),
    assert_all(Fs).

% if this is a query, run the lloyd-topor transform and query
process_logical_form(yn_question,L) :-
    lt_transform(L,Q),
    
    (  prove(Q) ->
        writeln('yes')
     ; 
        writeln('no')
    ).

\end{code}

\section{Theorem Prover}

\begin{code}

prove((A,B)) :-
    prove(A),
    prove(B).

prove(A) :- axiom(A).

\end{code}

\chapter{The Fifth Chamber: Existential Quantification}

We already know how to handle simple existential
quantification, for sentences lilke:
\begin{verbatim}
A block is green.
\end{verbatim}
\noindent But there are many other ways in which
exitential quantification is expressed in English.
The goal of this chapter is to explain how to make
a program which can engage in a conversation like
the following.  We should be able to input sentences like:
\begin{verbatim}
There is a block.
The block is green.
There is another block.
That block is red.
\end{verbatim}
\noindent and then the computer shoulde be able to answer questions like:
\begin{verbatim}
Is there a block?
yes.

Is there a blue block?
no.

What is there?
There is a red block.
There is a blue block.
\end{verbatim}

\section{Problems to Solve}

How should we handle senteces like:
\begin{verbatim}
There is a block.
\end{verbatim}
\noindent  We might get a clue by considering a sentence like:
\begin{verbatim}
A block is there.
\end{verbatim}
\noindent Which sounds like a verbal pointing, or {\em ostension}.  In other
words, if you say ``A block is {\em there},'' you are saying, of a particular
spatio-temporal region, that it is a block.  For the present, we'll
treat such verbal ostensions as adverbs modifying the verb ``to be.''

\section{Grammar}

\begin{code}
*/

sentence(Mood,L) -> s(Mood,L).

s(assertion,L) --> 
    np(X^Q^L),
    vp([],[], X^Q),
    ['.'].

s(yn_question,L) --> 
    verb([],[],W),
    np(X^Q^L), 
    vp([gap(W)],[], X^Q),
    ['?'].

np(X^Q^L) --> det(X^R^Q^L), noun(X^R).

det(X^R^Q^exists(X,and(R,Q)))--> [a].

noun(X^block(X)) --> [block].

vp(P1,P2,L) --> verb(P1,P2,_), adjective(L).


verb([],[],is) --> [is].
verb([gap(W)],[],W) --> [].


adjective(X^red(X)) --> [red].
adjective(X^green(X)) --> [green].

/*
\end{code}

\section{Compiling to Horn Clauses}

\subsubsection{Nute-Covington Transform}

\begin{code}

nc_transform(F,Cs) :- nc_transform(F,Cs,[]).

nc_transform(exists(X,R), H,T) :- !,
        skolemise(X),
        nc_transform(R,H,T).

nc_transform(and(R,S), H,T) :-!,
        nc_transform(R, H,I),
        nc_transform(S, I,T).

nc_transform(F, [axiom(F)|T],T).

\end{code}

\section{Compling for query}

\subsection{Lloyd-Topor Transform}

\begin{code}

lt_transform(exists(_X,R), R1) :- !,
        lt_transform(R,R1).

lt_transform(and(P,Q), (P1,Q1)) :- !,
        lt_transform(P,P1),
        lt_transform(Q,Q1).

lt_transform(F,F).

\end{code}

\section{Dialog}

\begin{code}

dialog :-
        % remove previous resuls
        retractall(axiom(_)),
        
        read_eval_loop.

read_eval_loop :-
        get_sentence(S),
        ( S=[bye,'.'] ->
            true
        ;
            process_sentence(S),
            read_eval_loop
        ).

process_sentence(S) :-
        % parse the sentence
        ( sentence(Mood,L,S,[]) ->
             % see how to process it
             process_logical_form(Mood,L)
        ;
             write('couldn\'t understand this:'),
             writeln(S)
        ).


% if this is an assertion, run the nute-covington transform and assert
process_logical_form(assertion,L) :-
    nc_transform(L,Fs),
    assert_all(Fs).

assert_all([]).
    
assert_all([F|Fs]) :-
    assert(F),
    assert_all(Fs).

% if this is a query, run the lloyd-topor transform and query
process_logical_form(yn_question,L) :-
    lt_transform(L,Q),
    
    (  prove(Q) ->
        writeln('yes')
     ; 
        writeln('no')
    ).

\end{code}

\section{Theorem Prover}

\begin{code}

prove((A,B)) :-
    prove(A),
    prove(B).

prove(A) :- axiom(A).

\end{code}


\backmatter

\addcontentsline{toc}{chapter}{Bibliography}
\begin{thebibliography}{1}
\bibitem{del8}
\newblock The Fracas Consortium, Robin Cooper, et al.
\newblock Describing the Approaches
\newblock The Fracas Project deliverable D8
\newblock Available online, just google for it...
\newblock December 1994
\bibitem{del9}
\newblock The Fracas Consortium, Robin Cooper, et al.
\newblock The State of the Art in Computational Semantics:
\newblock Evaluating the Descriptive Capabilities of Semantic Theories
\newblock The Fracas Project deliverable D9
\newblock Available online, just google for it...
\newblock December 1994
\bibitem{keller88}
\newblock William Ralph (Bill) Keller.
\newblock Nested Cooper storage: The proper treatment of
lquantification in ordinary noun phrases.
\newblock In U. Reyle and C. Rohrer, editors
\newblock Natural Language parsing and Linguistic Theories
\newblock pages 432-445
\newblock Reidel, Dordrecht, 1988.
\bibitem{larson85}
\newblock Larson, R.K. (1985).  On the syntax of disjunction scope.
\newblock Natural Language and Linguistic Theory
\newblock 3:217, 265
\bibitem{nasslli2003}
\newblock Patrick Blackburn and Johan Bos
\newblock Computational Semantics for Natural Language
\newblock Course Notes for NASSLLI 2003
\newblock Indiana University, June 17-21, 2003
\newblock Available on the web, just google for it....
\bibitem{marcus93}
\newblock Mitchell P. Marcus,  Beatrice Santorini and Mary ann Marciniewicz.
\newblock Building a large annotated corpus of English: the Penn Treebank.
\newblock in Computational Linguistics, vol. 19, 1993.
\bibitem{Parsons90}
\newblock Terrence Parsons
\newblock Events in the semantics of English
\newblock MIT press, Cambridge, Ma.
\newblock 1990
\bibitem{pereira87}
\newblock Fernando C.N. Pereira and Stuart M. Shieber
\newblock Prolog and Natural Language analysis
\newblock CSLI lecture notes no. 10
\newblock Stanford, CA
\newblock 1987
\end{thebibliography}

\end{document}

*/
