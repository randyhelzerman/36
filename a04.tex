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



\begin{code}
*/

:-use_module(library(lists)).
:-use_module(library(ugraphs)).
%:-use_module(library(swi)).

:-discontiguous(clause/1).
:-discontiguous(reduce/3).
:-discontiguous(category/3).
:-discontiguous(combine_sem/6).
:-discontiguous(process_logical_form/2).

:-dynamic(clause/1).
:-dynamic(axiom/1).

/*
\end{code}

\tableofcontents

\mainmatter

\chapter{Introduction}

SHRDLU was an amazing program for natural language understanding,
written by Dr. Terry Winograd.  SHRDLU's expertise was limited to
conversations about the ``blocks world'', which is a restricted domain
consisting of a tabletop with a bunch of colored blocks on it.  The
blocks have various shapes, and can be stacked on top of each other or
arrainged in various ways, subject to a rudimentary set of physical
laws.

The user could have conversations with SHRDLU about this domain,
asking it questions about the blocks on the table and the
configurations that they were in, and instructing it to create towers
by stacking blocks on top of each other.

This is an example conversation which the users could have with 
SHRDLU:
\begin{verbatim}
PERSON: Is there a large block behind a pyramid?
SHRDLU: Yes, three of them.  A large red one, a large green 
        cube, and the blue one.
PERSON: Put a small one onto the green cube which supports 
        a pyramid.
SHRDLU: OK
PERSON: Does the shortest thing the tallest pyramid's support 
        supports support anythying green?
SHRDLU: Yes, the green pyramid.
\end{verbatim}
\noindent In addition to text input-output, SHRDLU had
three-dimensional graphic output which displayed the state of the
blocks world and animated the moving of blocks according to the
commands of the user.

But the most amazing thing about SHRDLU was that it was written over
30 years ago!  The Computing power of children's playthings have
literally millions of times more processing power than the PDP-6 which
hosted SHRDLU.  But today there are no examples of natural language
processing programs remotely appraching SHRDLU's ability to understand
complex sentences.  What happened?

This book is an exploration of of what we've learned about natural
language processing since SHRDLU.  The method of exploration is to
reimplement SHRDLU from scratch, using the latest syntactic and
semantic tricks we have learned in the intervening 36 years.

\section{Why all these chambers??}

The title of this book is an allusion to the Shaw Brothers film ``The
36th Chamber of Shaolin,'' which is indubitably one of the greatest
martial arts film of all time.  A young man enters the Shaolin Temple
to learn Kung-Fu.  As he learns Kung-fu, he progresses through 36
chambers, each chamber containing increasingly difficult physical and
mental hardships and skills he must master.  The entire process bears
more than a passing resemblance to grad school.

But the real inspiration behind this chambered approach is from a
suggestion by Fred Brooks in his book ``The Mythical Man Month'' in
which he suggests that we change the metaphor from ``architecting'' a
program to ``growing'' a program.  Just as a living organism starts
small but is nevertheless a complete living organism and grows bigger,
Similarly, we should try as soon as possible to form a working
computer program, and ``grow'' it by making each of the subsystems
incrementally more and more powerful.  An excellent example of how
effective this approach can be in the context of natural language
processing is Patrick Blackburn and Johan Bos's book ``Natural
Language Understanding''.  The start with a small but complete system
called ``CURT'' and then they grow it, and the sucession of ever
cleverer CURTs gives the reader a smooth on-ramp which greatly eases
the burden of understanding.

This book follows a similer pattern.  We get a very small, but
complete system working as quickly as possible, and then each
suceeding chamber illustrates how to enhance the functionality of one
of the subsystems, or how to integrate a new functionality into an
already working system.

\section{Why are your programs half-baked?}

\begin{quote}
{\em There is a crack in everything \\ that's how the light gets in
  \\ } -- Leonard Cohen.
\end{quote}

This book was written primarily for didactic purposes.  The emphasis
is on understandability.  Some of the routines are simplistic, and
don't handle all of the corner cases which a full, robust system would
have to handle.  Most of the time, this is as a set-up for the next
chamber--experiencing the painful limitations of one program are the
best motivation for the next chamber.  But sometimes, I rest content
with sub-optimal solution, or a solution which doesn't handle all of
the corner cases.  If you find a routine like this, congratulations!
Pat yourself on the back, and write the correct version in your
program.


\section{Why are you using Prolog instead of (insert favorite language here)}

Prolog is, these days, a very arcane, indeed archaic, language.
Although it had good P.R. during the 80's, it never made the leap from
niche to mainstream--why not?  There are many reasons (see
\cite{teaching_prolog_book} for heroic attempts to overcome some of
these drawbacks) but the can be summarized like this: It is said that
a good programming language should have two virtues:
\begin{enumerate}
\item It should mke simple tasks easy.
\item It should make difficult tasks possible.
\end{enumerate}
\noindent What really prevented prolog from ever being a mainstream
programming language, I'm afraid, is that sorely lacks the first virtue.
So why use it?  Well, prolog fairs much better on the second virtue, and
in addition, it has a third virtue uniquely its own, which most other 
programing language {\em don't} have:
\begin{enumerate}
\item It makes (otherwise) impossible tasks possible.
\end{enumerate}

In this reguard, I think it would be interesting to consider the
example of the book ``Paradigms of AI Programming'' by Peter Norvig,
which is, in the opinion of this author, the best book on programming
ever written.  In his book, Norvig describes in detail several natural
language processing programs, with varying capabilities.  When looking
at these programs, an interesting pattern emerges: any program which
needs more than the most basic powers of analyzing the structure of
sentences is written in prolog.  Norvig's preferred programming
languge is LISP, but on the way to natural language processing, Norvig
finds it useful to {\em first} implement prolog in LISP and {\em then}
implement his natural language processing programs on top of prolog.

This is by no means an isolated case.  Carl Hewitt's invention of
planner, for example, or (even more pertinently) Terry Winograd's
SHRDLU.  Because both of these authors emphasized the {\em doing} part
of understanding language (e.g. to understand ``the block'' go and
{\em find} a block in the database) it might seem strange to cite
these as examples of using prolog as an abstraction layer.  However,
Hewitt has emphasized that prolog is really a version of planner, and
therefore SHRDLU (which was also written in a version of planner
called MICROPLANNER) very much exemplifies this strategy of using a
prolog-esque layer of abstraction on top of LISP.

This of course, is obvious only in hindsight. The whole LISP/prolog
dichotomy is a false dichotomy.  No matter what programming language
you use, you will find it useful to structure your programs such that
they have many levels of abstraction on top of assembly language, and
one of these levels of abstraction is gunna look a lot like prolog.
Since this is inevitable anyways, we might as well start not from
assembly language, or even LISP, but from prolog.  We can thereby help
ourselves to over 30 years worth of prolog research, which has
produced very fast and efficient prolog compilers, as well as
libraries, programming techniques and methodologies, and a wealth of
practical know-how in constructing and maintaining programs.  Well
begun is half done.

\section{Why don't you make the programs downloadable?}

Typing programs in from the printed page has a venerable history.
This book is written to help you learn.  If you are a novice prolog
programmer or linguist, typing in programs is a great way to fix the
concepts in your mind.  If you are NOT a novice, you probably already
have several NLP systems which you've already written lying around--in
which case you won't want to use the programs in this book as, is,
you'll want to adapt them to your own programing
language/environment/infrastructure.  In neither case will you benefit
at all by just downloading and running the program.


\chapter{The First Chamber: Prolog}

\section{Variables}

Sometimes, there is an object you you want to talk about.  But you don't exactly
know {\em which} object it is.   But even though its exact identity is unknown to you,
you do know a few facts about it.  A good example of this is the game of 20
questions:
\begin{verbatim}
Is it smaller than a breadbox?  yes
Is it red?                       no
Is it round?                    yes
\end{verbatim}
\noindent The game is to use what you {\em do} know about the unknown
object to find out {\em which} object it is.  In algebra class, you
learned another way to do this: use a letter in place of a number:
\begin{displaymath}
2*X=4
\end{displaymath}
\noindent We don't know exactly {\em which} number $X$ is, but we do
know that if you add 2 to it you get 4.  Prolog uses variables much
the same way that algebra does.  The general gameplan is to using a
variable to find out as many facts as you can about the unknown object.
If you know enough {\em other} facts about it, you can figure out which
object you {\em are} talking about.

Syntactically, a variable in prolog is:
\begin{enumerate}
\item an  upper-case letter
\item optionally followed by any number of upper or lower case letters,
or the underscore character '\_'. 
\end{enumerate}
\noindent for example:
\begin{verbatim}
X  Y  Distance  This_is_a_variable
\end{verbatim}

In algebra, you could only talk about numbers using variables.  Prolog
is a bit more flexible.  The kinds of things we can talk about in
prolog are {\em atoms} and {\em terms}.

The most basic things we can talk about are atoms.  Atoms can be
numbers (as in algebra), but they can also be strings of characters of
this form:
\begin{enumerate}
\item a lower case letter
\item optionally followed by any number of upper or lower case letters,
or the underscore character '\_'.
\end{enumerate}
\noindent for example, here are some atoms:
\begin{verbatim}
abraham  issac jacob
my_left_foot
\end{verbatim}
\noindent here are some items which are not atoms
\begin{verbatim}
f(x)
a+b
``this is not an atom''
\end{verbatim}
\noindent those are all {\em terms}.  A term is a way of creating a
new object out of atoms (kind of like in chemestry how molecules are
made out of atoms).  For example, we want to represent a burrito, made
out of a tortilla, beans, shredded chease, and salsa:
\begin{verbatim}
burrito(tortilla,beans,shreadded_cheese,salsa).
\end{verbatim}
\noindent a term has a {\em functor} (in the above case, its ``burrito'') and
{\em arguments} which appear in parenthesis after the functor.

So, to reherse: prolog lets you name things with variables, and the
things which are named by the variables in prolog are atoms and terms.
To see how we can assign a name to a term, lets go to the command
line.

\section{Read-Eval-Print Loop}

After you launch prolog, the command line prompt looks like this:
\begin{verbatim}
?-
\end{verbatim}
\noindent At the prompt, you can give an atom a name like this:
\begin{verbatim}
?-  X = 5.
\end{verbatim}
\noindent and prolog prints out:
\begin{verbatim}
X=5
?-
\end{verbatim}
\noindent We could also do a term:
\begin{verbatim}
?- X = burrito(tortilla,beans,shreadded_cheese,salsa).

X = burrito(tortilla,beans,shreadded_cheese,salsa).
?-
\end{verbatim}

\section{Unification}

Try typing in:
\begin{verbatim}
?-  X = 1+1.
\end{verbatim}
\noindent You might be surprised that you get:
\begin{verbatim}
X = 1+1
?- 
\end{verbatim}
\noindent why didn't prolog do the addition?  The answer is that
``1+1'' is a term.  The ``+'' is a functor (much like the functor
"burrito" above), but instead of being written as ``+(1,1)'' in front
of its arguments, like the ``burrito'' functor was, it was written in
between them.  The ``+'' operator has what is called {\em infix}
behavior.  There several other operators which can be used in an infix
way like this (the other arithmetic operators being obvious examples).

But for our present purposes, this means that ``1+1'' is not the same
term as "2".  In fact, they are different categories, ``1+1'' being a
term, and ``2'' being an atom.  This brings us to the "=" sign in
prolog.  In prolog, two things are ``='' to each other when:
\begin{enumerate}
\item They are same term or atom, e.g. ``1+1 = 1+1''.
\item They can be made to {\em be} the same term by assigning some
 values to variables. e.g. ``X = 1+1''.
\end{enumerate}
\noindent Given the above, this behaviour shouldn't be surprising to you:
\begin{verbatim}
?- 1+1=X.

X = 1+1
?- 
\end{verbatim}
\noindent ``='' is completely symmetric: it doesn't matter what side
of the = sign the variable is on.  But ``='' is even more flexible
than this.  The variables which we are assigning can be inside of a
term:
\begin{verbatim}
?- 1+1=X.

X = 1+1
?- 
\end{verbatim}
\noindent What prolog has done here is find {\em what value} needs to
be assigned to the variable ``X'' in order to make ``1+X'' the same
term as ``1+1''.  Obviously, the answer is ``X=1'' as prolog prints
out.

The sort of atom-and-term equality which prolog usese is called {\em
unification}.  Two terms are said to {\em unify} if they are already
the same term, or they can be made into the same term by instantiating
any variables they contain.

\section{Relations}

To use prolog, you need to learn to use a new language, whimically
called ``Prolog-ese'' (sometimes more derisively revered to as
``Komputerdeutch'').  The easiest way to learn it is to see some
examples.  We'll use a biblical example, from the King James Version,
of course.
\begin{quote}
Mathew 1:14-16 Eleazar begat Matthan, and Matthan begat Jacob, and
Jacob begat Joseph, the husband of Mary, of whome was born Jesus
\end{quote}
To translate ``Eleazar begat Matthan'' into prolog-ese, we first ask
the question: what is the relationship between Eleazar and Mathan?
Typically, this relationship is expressed by the verb and associated
words, in this case simply ``begat''.  We then indicate that
``Abraham'' and ``Jacob'' stand in this relationship to each other by
putting them in parenthesis behind the relationship, like this.  Open
up the editor and type in these names:
\begin{verbatim}
begat(eleazar,matthan).
\end{verbatim}
\noindent Notice, we can't capitalize the names, because strings
beginning with capital letters are variables.  Continuing the rest of
the mini-geneology:
\begin{verbatim}
begat(matthan,jacob).
begat(jacob,joseph).
\end{verbatim}
\noindent now save and compile the file, and go to the command line.
At the command line, we can now ask questions.  First, lets ask some
yes/no questions.  ``Is Eleazar the father of Matthan?'' translates
to:
\begin{verbatim}
?- begat(eleazar,matthan).

Yes
?- 
\end{verbatim}
\noindent We can also ask Wh-questions, like ``Who is the father of
matthan?''  Note, this fits the pattern mentioned above.  We know
something about X, namely, that X is the father of Matthan.  So we can
use the fact that X is the father of Matthan to find out exactly who X
is:
\begin{verbatim}
?- begat(X,matthan).

X = eleazar

Yes
\end{verbatim}
Here is wisdom: until now, we've only asked questions which have one
answer.  But most questions have many answers.  Here is one such
question: ``Who is the father of whom?''  There are many fathers and
many sons, so this will question will have many answers.  How to
phrase it?  We use two variables.
\begin{verbatim}
?- begat(X,Y).

X = eleazar,
Y = matthan
\end{verbatim}
\noindent Prolog returns you {\em one} of the solutions.  But it
indicates to you that there may be more.  Until now,we've just pressed return.
But instead of pressing return, press ``;'':
\begin{verbatim}
?- begat(X,Y).

X = eleazar,
Y = matthan ;

X = matthan,
Y = jacob 
\end{verbatim}
\noindent and another solution appears.  Pressing ``;'' again will
cause the last solution to be displayed.  This ability of prolog to
explore {\em all possible} answers to a question is a big key to its
power.

\subsection{Some cautions}

To illustrate a pitfall when extracting relations from English prose, 
lets go over to the {\em other} geneology of Jesus which is found in
the New Testament. 
\begin{quote}
Luke 3:23-24 
Joseph, which was the son of Heli, which was the son of
Matthat, which was the son of Levi \ . \ . \ .
\end{quote}
\noindent So first of all, we notice that English has several
different ways to express the same concept.  Matthew uses ``begat''
and Luke uses ``was the son of''.  When you are translating English
sentences into prolog, there is one thing you have to watch out for.
Typically, what we do is pick one, and then stick with it.

But there's another pitfall here.  Suppose we want to add the
information found in Luke here to the information found in Matthew.
So far we have:
\begin{verbatim}
% from Matthew
begat(eleazar,matthan).
begat(matthan,jacob).
begat(jacob,joseph).

% from Luke
begat(heli,joseph).
begat(matthat,heli).
begat(levi,matthat).
\end{verbatim}
\noindent Compile and consult the file, and now lets ask who the
father of Joseph is?
\begin{verbatim}
?- begat(X,joseph).

X = jacob ;

X = heli
\end{verbatim}
\noindent We get two different fathers for Joseph!  Now notice
carefully, because this is probably the biggest cause of prolog bugs:
In our minds, we have all kinds of knowledge about the ``begat'' relationship
between father and son.  One of those bits of knowledge is that a son
only has one father.  But, of course, prolog doesn't know that--prolog
only knows what we've told it.  And we've told prolog here that Joseph
has two fathers, Jacob and Heli.

So what to do?  Well, lets take our clue from Joseph's grandfather.
Matthew says that Joseph's grandfather was Matthan, Luke says that he
was Matthat.  These look like just a minor spelling variation, so
we'll assume that they both are the same name of the same person, just
spelled wrong.  But of course, to prolog, matthan $\neq$ matthat.  So
what we do is kind of the same thing we did for the ``father of''
relation--we just pick a name and stick with it.  It doesn't matter
which name we pick, but here's the thing--in prolog, everythign you
talk about has to have one and only name.  This is called the 
{\em unique names assumption}.  Prolog will {\em always} assume that
Matthan and Matthat are two different things.  So don't confuse
prolog--just pick one name for each thing you want to talk about and
stick with it.  

But there's another problem here.  ``Heli'' doesn't really look like
an alternate spelling for ``Jacob''.  Probably if you saw those two
different names, you'd do what prolog does, and assume they were
talking about two different people.  So this illustrates another
difficulty when translating English into Prolog.   Most of the knowledge
we have is self-contradictory.  But how to handle it?  We have all kinds of choices here.  
\begin{enumerate}
\item We can either say that Jacob actally had two names 
(he was known as ``Heli'' to his poker buddies).
\item We can say that there was a scribal error here--somebody just
  goofed while copying.  ``Jacob'' is the correct name.  (Or we could
  say that ``Heli'' is the correct name and ``Jacob'' is the mistake.)
\item We can say that we were wrong about the ``father''
  relation--scripture actually teaches us that a son can have two
  fathers, or ocassionally, 0 fathers due to a virgin birth.
\end{enumerate}
\noindent The key here is that there's really nothing inherent in the
translation process which is going to help us choose which way to go.
Which one we choose depends upon our purposes and desires.  The more
different alternatives you can think of, the more likely it will be
that you can fulfill your purposes and desires.  This is one of the
key differences between a mediocre and a top-flight programmer: a
topflight programmer will always think of more possilbe ways to
reconcile contradictory information and requirements.

For the present, we'll assume that there has been a scribal error,
Luke goofed on the name of father of Jacob.  Once we assume this, we
can confirm our theory by noticing that Luke also misspeled ``Matthan'',
and so we'll just stick with the Geneology in Matthew for the present.

The key points to remember:
\begin{enumerate}
\item Each relation must have one unique name.
\item Each thing we talk about must have one unique name.
\item The more possible ways you can think to resolve contrdictory
  information, the better programmer (or theologen) you will be.
\end{enumerate}

\section{The Grandfather Clause}

In our program, we've only talked about the relationship of
fatherhood.  What if we wanted to know about grandparents?  In fact,
there is enough information implicitly in the database to find out who
the grandparent of, say, Joseph is.  On the command line, with a
little thought, we can formulate a query which will produce it:
\begin{verbatim}
?- begat(Parent,joseph), begat(Grandparent,Parent).

Parent = jacob,
Grandparent = matthan
\end{verbatim}
\noindent Notice how we've strategically used a shared variable to
link the two ``begat'' queries together.  Both queries share the
variable ``Father'', so when it is instantiated in the first query, it
is used in the secong query to select the father of the father.  Also
notice, suppose we weren't interested in just Joseph's
grandfather--suppose we wanted to be able to find anybody's
grandfather.  We could just replace the hard-coded ``joseph'' with a
variable, and it would work like this:
\begin{verbatim}
?- begat(Parent,Kid), begat(GrandParent,Parent). 
Parent = matthan,
Kid = jacob,
GrandParent = eleazar ;

Parent = jacob,
Kid = joseph,
GrandParent = matthan ;

No
?- 
\end{verbatim}

\section{Rules}

Now we know how to find grandfathers.  There is a problem, however.
Its kind of a bummer to have to keep typing two begats all the time,
so suppose we wanted to modify our original program to have a clause
``grandparent(Kid,Grandparent)'' in it, which was true if
``Grandparent'' were bound to the grandparent of whatever ``Kid'' was
bound to.  There are two ways we could do it.  We could, by hand, type
in all the relations.  But this is tedious, error prone, hard to
update, and worst of all, redundant.  Our database already contains
the information we need, we just need to tell it how to derive
grandparent information from begat information.

To do this, we need what is called a {\em rule}.  Rules are durn near
the last prolog constructs you will need to learn.  Rules have two
parts, a {\em head} and a {\em body}.  Lets talk about the body first.
The body of the rule is just a query, the same kind that you could type
in on the command line.  For our grandfather example, the body would be:
\begin{verbatim}
begat(Parent,Kid), begat(GrandParent,Parent). 
\end{verbatim}
\noindent  The {\em head} of the rule is what you would like to name this query. 
For our example, we've said we wanted to name it: 
\begin{verbatim}
grandparent(Kid,GrandParent)
\end{verbatim}
\noindent  Now, the head and the body of the rule are connected with what is
called (surprise surprise) the {\em neck}.  Its just the symbol ``:-''.  So the
whole rule will look like:
\begin{verbatim}
grandparent(Kid,GrandParent) :- 
     begat(Parent,Kid), begat(GrandParent,Parent). 
\end{verbatim}
\noindent If you add that rule to your biblical geneology program and
consult the file, then you will be able to use it to find the grandparents
of people just the same way you were able to use ``begat'' to find the parents
of people.

\section{Counting and Recursion}

Suppose there were a father who had three sons:
\begin{verbatim}
begat(father, son1). begat(father, son2). begat(father, son3).
\end{verbatim}
\noindent We can enumerate these three on the command line:
\begin{verbatim}
?- begat(father, Son).
Son = son1 ;
Son = son2 ;
Son = son3 ;
\end{verbatim}
\noindent but how could we count them?  Right now, we can deal with
multiple sons, but we're dealing them at seperate instances in time.
Instead of dealing with one object at three different times, we want
to be able to deal with three objects at the same time.  Prolog
provides a way to do this.  It is called a {\em list}. Syntactically,
lists are easy--just seperate what you want to put in the list by
commans, and enclose the whole thing with square brackets, like this:
\begin{verbatim}
[1,2,3]
\end{verbatim}
\noindent  But how do we create one of these lists?  Prolog provides a nice
built-in function to do just this.  Its called {\em findall/3} and it works
like this:
\begin{verbatim}
?- findall(Son, begat(father,Son), Sons).
Sons = [son1,son2,son3]
\end{verbatim}

The first argument is a variable.  The second argument is a query,
(which we could do on the command line), which contains that variable.
The third argument is a list of all of the values for the variable
which the query succeds on.

\section{Meta Interpreters}

\section{Useful Prolog Warts}

\subsection{Testing For Groundness}

\subsection{Self-Modifying Code}

An example of how to use this is creating a new constant.  This is
used during skolemization.  Most prologs these days have this
predicate build-in, but if yours doesn't it is handy to know how to be
able to add it.

\begin{code}
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
\end{code}

\subsection{Testing whether variables are unified to each other}

In prolog, X == Y is true iff X and Y the same variable, or that one
is an alias of the other.
    
\subsection{Double Negation in order not to bind variables trick}

\chapter{The Second Chamber: Tokenization and Parsing}

The tokenizer is a list-to-list transducer which takes the raw input
list and converts it into something which is easier to write a parser
for.  Because it is a list transducer, we use difference lists in
order to support constant-time concatenation.

A limitation of the tokenizer is that it assumes ascii input.  It
would have to be reworked if we were to support Unicode.

The tokenizer supplies two high-level predicates: snarf\_line/1 and
chunks/4.  The predicate snarf\_line/1 just keeps reading characters
until an end line has been reached.  The predicate chunks/4 takes a
list of characters (such as that supplied by snarf\_line/1) and breaks
it into "chunks".  Chunks correspond roughly to words and punctuation,
with caveats as noted below.  Generally, we follow the lead of the
PENN Treebank project \cite{marcus93} on tokenization.

\section{Getting a line of input from stdin}

\begin{code}
*/
get_sentence(S) :-
        snarf_line(L1),
        chunks(_Dummy,S,L1,_L2).

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

end_white_space([]) --> \+ white_space_char(_H).

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

end_word([]) --> \+ letter(_H).
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

end_numeral([]) --> \+ digit(_H).
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

:-discontiguous(funky/1).
:-discontiguous(defunkify/3).

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

The following is really interesting...because a single chunk can
induce more than one word in the resulting tokenized stream, we need
to use append.  To make that fast, we need to use difference lists.
This makes the code much harder to understand...

\begin{code}
*/

chunks(_H,[]) --> \+ chunk(_W).
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

\begin{quote}
{\em Carve the bird at its joints} -- Julia Child
\end{quote}

A grammar for a natural language is supposed to divide lists of words
into two groups: 
\begin{enumerate}
\item Those which belong to the language
\item And those which do not.  
\end{enumerate}
\noindent But if we don't already have a grammar, how do we know which
strings of words belong to the language, and which don't?

The answer is that we start out with pre-theoretical intuition as to
which lists of words are grammatical and which aren't.  For
example, these lists:

\begin{verbatim}
A block is red.
A frog is green.
The block is on the table.
The frog is on the lillypad.
\end{verbatim}
\noindent seem to be good English, while lists these:
\begin{verbatim}
Green the is. *
block frog lillypad. *
\end{verbatim}
\noindent don't seem to be grammatical.  (It is customary to put a
``*'' after lists of words which arn't grammatical.)

So where does this intuition come from?  Well, we do understand
English, and therefore some lists of words sound ``OK'' to us, and
some sound funny.  If this doesn't sound very scientific, well, its
not, or at least, not yet.  As Alen Perlis says, ``You can't go from
informal to formal by formal means.''  Our initial intuitions about
which lists of words are ``correct'' and which are ``funny'' is the only
thing we have at first, but its enough to get started.  How do we use
them?

If we want to follow the advice of Julia Child, and ``carve the bird
at its joints,'' \footnote{Actually, this was Plato \cite{plato400bc}}
then we have to find the joints of the sentences, the places where the
sentences can be broken down into their natural parts.  What is a natural
part?  Well, a part is something which can be combined with other
parts to make a whole.  So we say that if we break a sentence into
two, we've broken it into two parts if we can re-assemble those parts
to make new grammatical sentences.  For example: suppose we were to
break the following sentences into two parts:

\begin{verbatim}
"A block is red"  =>  "A block" + "is red"
"A frog  is green"  =>  "A frog"  +  "is green"
\end{verbatim}

\noindent we know that we've actually carved these sentences at the
joints, because we can reassemble the pieces to make new
sentences:

\begin{verbatim}
"A frog" + "is red" => "A frog is red"
\end{verbatim}

But of course, just like taking puzzle pieces apart, we can't put
these pieces back together any old way.  For example:
\begin{verbatim}
"is red" + "A frog" + "=> "is red A frog"  *
\end{verbatim}
\noindent yields a non-grammatical list of words.  Again, we're just
relying on our pre-theoretical intuition here.  ``is red a frog''
doesn't sound right, but ``a frog is red'' does sound right.

But lets take a step towards formality now.  Its not enough to say
that a piece of a sentence is a part of a sentence: we must also
indicate somehow how that part could fit together with other parts
to form a whole.  To do this, we will sort the parts of sentences into
{\em categories}.  Because the category is supposed to indicate how
that part fits into the whole, we say that two different parts of
sentences have the same category if we can substitute them in a
grammatical sentence and still yield a grammatical sentence.  For
example: the phrases ``A frog'' and ``The block'' have the same
category, becase we can substitute one for the other in a grammatical
sentence and still yield a grammatical sentence:

\begin{verbatim}
[A frog] is green
[The block] is green
\end{verbatim}
\noindent This means that ``A frog'' and ``The block'' will have the
same category.  Similarly, ``is on the block'' and ``is green'' will
have the same category, because we can substitue them as well:
\begin{verbatim}
A frog [is on the block]
A frog [is green]
\end{verbatim}

\noindent However, ``is on the block'' and ``A frog'' must be of
different categories, because we cannot substitue one for the other
without yielding an ungrammatical sentence:

\begin{verbatim}
[A frog] is green 
[is on the block] is green *
\end{verbatim}
\noindent So the parts of sentences are kind of like puzzle pieces:
they will only fit together in certain ways.

Lets give names to these parts.  We've seen that 
\begin{verbatim}
A frog
The block
\end{verbatim}
\noindent are of the same category.  It is traditional to call this
category ``NP'' which stands for ``noun phrase''. 

\section{Formulating a grammar for blocks world}

We want to talk about the blocks world, so we'll try to parse a few
declarative sentences like this:

\begin{verbatim}
A block is green.
A block is red.
A table is green.
\end{verbatim}
\noindent and we'll want to be able to parse questions too:
\begin{verbatim}
Is a block green?
Is a block red?
\end{verbatim}

So lets see what kinds of categories we need to account for those sentences.
We noticed that we can substitue [A table] for [A block]
in any sentence and get another sentece:
\begin{verbatim}
[A table] is green.
[A block] is green.
[The table] is green.
[The block] is green.
\end{verbatim}
\noindent We've called this category "NP".

We also notice that within "A block" we can substitute ``block'' for ``table'' 
in any sentence and still get a sentence:
\begin{verbatim}
A [table] is green.
A [block] is green.
\end{verbatim}
\noindent Call this category ``n'' for (``noun'').  We'll store this fact like this:
\begin{code}
*/
category(block, n).
category(table, n).
/*
\end{code}


\begin{code}
*/
:-op(400, yfx, \).
:-op(400, yfx, /).
/*
\end{code}

Now we can define the lexicon entries for the other words:

\begin{code}
*/
category(a,np/n).
category(the,np/n).

category(is, (s(d)\np)/(n/n)).
category(is, (s(q)/(n/n))/np).

category(green,n/n).
category(red,n/n).
/*
\end{code}

Then we have the parser:

\begin{code}
parse([S],[], S).

parse(Stack,[Word|Buffer],  Answer) :-
        category(Word, C),
        parse([C |Stack],Buffer, Answer).

parse([Cat2,Cat1|Stack],Buffer, Answer) :-
        reduce(Cat1,Cat2, Cat3),
        parse([Cat3|Stack], Buffer, Answer).

% forward and backward application
reduce(A/B,    B,      A).
reduce(B,      A\B,    A).
\end{code}

\section{Parse Trees}

\begin{code}
*/
parse([S],[], S).

parse(Stack,[Word|Buffer],  Answer) :-
        get_category(Word, C),
        parse([C |Stack],Buffer, Answer).

parse([Cat2,Cat1|Stack],Buffer, Answer) :-
        reduce(Cat1,Cat2, Cat3),
        parse([Cat3|Stack], Buffer, Answer).


get_category(Word, C1) :-
        category(Word,C),
        C1=C:leaf(C,Word).

% forward and backward application
reduce(A/B:N1,    B:N2,    A:node(A,N1,N2)).
reduce(B:N1,    A\B:N2,    A:node(A,N1,N2)).

/*
\end{code}

\subsection{Debugging: Printing out the Parse Trees}

\begin{code}
*/
parse_tree_print(T) :-!,parse_tree_print(T,0).

parse_tree_print(leaf(C,W), H) :-!,
        % now print out this node
        tab(H), write(-----),    nl,
        tab(H), write(C), write(' '), write(W),nl,
        tab(H), write(-----),    nl.
        
parse_tree_print(node(F,L,R),H) :-!,
        % Calculate the height for the children to be prited at.
        H1 is H + 7,
        
        % print out the right branch
        parse_tree_print(R,H1),
        
        % now print out this node
        tab(H), write(-----),    nl,
        tab(H), write(F), write(' '), nl,
        tab(H), write(-----),    nl,
        
        % print out the left branch
        parse_tree_print(L,H1).
/*
\end{code}

\noindent now we can type in something like:
\begin{verbatim}
?- parse([],[is,the,block,green],C:T), parse_tree_print(T).
\end{verbatim}
\noindent and it will print out:
\begin{verbatim}
       -----
       n/n green
       -----
-----
s(q) 
-----
                     -----
                     n block
                     -----
              -----
              np 
              -----
                     -----
                     np/n the
                     -----
       -----
       s(q)/ (n/n) 
       -----
              -----
              s(q)/ (n/n)/np is
              -----
C = s(q),
T = node(s(q), node(s(q)/ (n/n), leaf(s(q)/ (n/n)/np, is), 
node(np, leaf(np/n, the), leaf(n, block))), leaf(n/n, green)) ;
false.
\end{verbatim}

\chapter{The Fourth Chamber: Existential Quantification and Predication}
\label{language_game_0}

In this chapter, we'll create a very small language--small enough that
we can {\em completely} implement it so that we can:

\begin{enumerate}
\item Parse an input sentence which is an assertion or query.
\item If its an assertion compile it to Horn clauses.
\item If it is a query, compile it to a query against a Horn clause database.
\item Take the results of any such query and express them in English.
\end{enumerate}

\noindent In particular, we want to be able to have the following
conversation with the computer.  We want to be able to tell it that
there is a block with a particlar color:

\begin{verbatim}
a block is blue.
ok.
\end{verbatim}
\noindent and then we want to be able to ask about it:
\begin{verbatim}
is a block blue?
yes.
\end{verbatim}
\noindent  Since it only knows about a single block which is
blue, if we ask it about a red block:
\begin{verbatim}
is a block red?
no.
\end{verbatim}
\noindent it will answer "no" when asked about this.  But
we want it to be able to add to its knowledge if we
tell it about another block:
\begin{verbatim}
a block is red.
ok.

is a block red?
yes.
\end{verbatim}

\noindent This is, no doubt, not the most scintilating of
conversations, but the important point is that we cover {\em every}
part of of an NLP dialog with a computer, inasmuch as we have
solutions--vestigial and elementary solutions, to be sure--but
solutions nontheless to all phases of a NLP read-eval-print loop.
This will be a solid foundation we can use to elaborate upon.

\section{Logical form}

The logical form we'll use for the sentence:
\begin{verbatim}
A block is blue
\end{verbatim}
\noindent will be:
\begin{verbatim}
exists(X,and(block(X),blue(X)))
\end{verbatim}
\noindent There are several things to notice about this
representation.  First, we're representing a sentence of logic as a
prolog term.  Second, we're using prolog variables to represent
variables in the logical form.  This kind of muddies the waters,
inasmuch as it is a mixing of meta- and object-levels, but on the
whole it simplifies our task.

\section{Enhancing the Lexicon for Semantics}

First, for each of the words, we give its sense:

\begin{code}
*/
sense(block, n , X^block(X)).

sense(a,np/n, P^Q^X^exists(X,and(P,Q))).
sense(the,np/n, P^Q^X^exists(X,and(P,Q))).

sense(green, n/n, X^green(X)).
sense(red,  n/n, X^blue(X)).

sense(is, (s(d)\np)/(n/n),_).
sense(is, (s(q)/(n/n))/np,_).
/*
\end{code}

\section{Syntax semantics interface}

We walk down the parse tree until we reach a leaf.  The semantics for
that leaf is stored in the sense predicate.  As we recurse back up the
tree, we form the semantics for each non-leaf node by combining the
semantics of the child nodes.  The predicate combine\_sem/3 does this
combining.  Basically right now it has a seperate special case for
each possible pair of children which we could see.  Because our
grammar is small and the number of parse trees is limited, this works,
but if you are thinking that this could never scale, you are right.
We'll make a better way of doing it in a later chamber.

\begin{code}
*/
parse_to_semantics(leaf(C,W), C,L) :-
        sense(W,C,L).

parse_to_semantics(node(C,L,R), C,L1) :-
        parse_to_semantics(L, CL,LL),
        parse_to_semantics(R, CR,RL),
        combine_sem(CL,LL, CR,RL, C,L1).
/*
\end{code}

If the following looks hacked and ad hoc, it is. We essentially just
enumerate (some) of the cases in which these happen to occur, and put in
specific code for each case. Later we'll find a more elegant way to do this.

\begin{code}
*/
combine_sem(s(q)/(n/n)/np,_, np,L, s(q)/(n/n), L).
combine_sem(s(q)/(n/n),L2^X^L1,  (n/n),X^L2, s(q), L1).
combine_sem(np/n,L2^Q^X^L1,  n,X^L2,  np,Q^X^L1).
combine_sem(np, P^X^L1,  s(d)\np,X^P,  s(d),L1).
combine_sem((s(d)\np)/(n/n), _,  (n/n), X^L2, (s(d)\np),X^L2).
/*
\end{code}

\section{Compiling to Horn Clauses}

Notice that the above grammar provides the same logical form for both
moods: "A block is blue" and "is a block blue?" both get translated
into "exists(X,block(X),blue(X))".  The only difference is that the
category of the sentence is s(d) for declarative sentences and s(q)
for questions.

Unfortunately, even though the logical form is the same, {\em how we
use} that logical form is quite different for declarative statements
vs. questions.  For declarative statements, we want to add to our
knowledge base.  For questions, we want to query our knowledge base to
see if the query is true or not.

Therefore, for each of these two ways of handling the logical form,
there will be two different compilers.  One compiler compiles for
assertion into the database, the other compile will compile for query
against the database.


\subsubsection{Compiling for Assertion}

\begin{code}
*/
cnf_transform(F,Cs) :- cnf_transform(F,Cs,[]).

cnf_transform(exists(X,R), H,T) :- !,
        skolemise(X),
        cnf_transform(R,H,T).

cnf_transform(and(R,S), H,T) :-!,
        cnf_transform(R, H,I),
        cnf_transform(S, I,T).

cnf_transform(F, [axiom(F)|T],T).

skolemise(X) :- var(X),!, 
	gensym(sk,X).

skolemise([]).
skolemise([H|T]) :-
	skolemise(H),
	skolemise(T).
/*
\end{code}

\section{Compling for query}

\begin{code}
*/
lt_transform(exists(_X,R), R1) :- !,
        lt_transform(R,R1).

lt_transform(and(P,Q), (P1,Q1)) :- !,
        lt_transform(P,P1),
        lt_transform(Q,Q1).

lt_transform(F,F).
/*
\end{code}

\section{Dialog}

\begin{code}
*/
dialog :-
        % remove previous resuls
        retractall(axiom(_)),
        
        read_eval_loop.

read_eval_loop :-
        get_sentence(S),
        ( S=[bye] ->
            true
        ;
            process_sentence(S),
            read_eval_loop
        ).

process_sentence(S) :-
        % parse the sentence
        (  parse([], S, (s(M):N)) ->
            parse_to_semantics(N,_,L),
            process_logical_form(M,L)
        ;
             write('couldn\'t understand this:'),
             write(S),nl
        ).


% if this is an assertion, compile and assert
process_logical_form(d,L) :-
    cnf_transform(L,Fs),
    assert_all(Fs).

assert_all([]).
    
assert_all([F|Fs]) :-
    assert(F),
    assert_all(Fs).

% if this is a query, run the lloyd-topor transform and query
process_logical_form(q,L) :-
    lt_transform(L,Q),
    
    (  prove(Q) ->
        write('yes'),nl
     ; 
        write('no'),nl
    ).
/*
\end{code}

\section{Theorem Prover}

\begin{code}
*/
prove((A,B)) :-
    prove(A),
    prove(B).

prove(A) :- axiom(A).
/*
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
\newblock Nested Cooper storage: The proper treatment ofquantification in ordinary noun phrases.
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
\bibitem{plato400bc}
\newblock Aristocles, son of Ariston
\newblock {\em Phaedrus}
\newblock Academy, Athens, Greece
\newblock 400 B.C.
\end{thebibliography}

\end{document}
*/
