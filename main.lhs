%% Commands for TeXCount
%TC:macro \cite [option:text,text]
%TC:macro \citep [option:text,text]
%TC:macro \citet [option:text,text]
%TC:envir table 0 1
%TC:envir table* 0 1
%TC:envir tabular [ignore] word
%TC:envir displaymath 0 word
%TC:envir math 0 word
%TC:envir comment 0 0


%% For submission and review of your manuscript please change the
%% command to \documentclass[manuscript, screen, review]{acmart}.
%% When submitting camera ready or to TAPS, please change the command
%% to \documentclass[sigconf]{acmart} or whichever template is required
%% for your publication.
\documentclass[sigplan,screen,review,anonymous]{acmart}

\newcommand{\newcommenter}[3]{%
  \newcommand{#1}[1]{%
    \textcolor{#2}{\small\textsf{[{#3}: {##1}]}}%
  }%
}

\newcommenter{\plr}{magenta}{PLR}

%%%% lhs2Tex (*.lhs) document
\let\Bbbk\undefined
%include polycode.fmt
\long\def\ignore#1{}

%%%% %% Rights management information.  This information is sent to you
%%%% %% when you complete the rights form.  These commands have SAMPLE
%%%% %% values in them; it is your responsibility as an author to replace
%%%% %% the commands and values with those provided to you when you
%%%% %% complete the rights form.
%%%% \setcopyright{acmcopyright}
%%%% \copyrightyear{2018}
%%%% \acmYear{2018}
%%%% \acmDOI{XXXXXXX.XXXXXXX}

%%%% %% Submission ID.
%%%% %% Use this when submitting an article to a sponsored event. You'll
%%%% %% receive a unique submission ID from the organizers
%%%% %% of the event, and this ID should be used as the parameter to this command.
%%%% \acmSubmissionID{123-A56-BU3}

%% The majority of ACM publications use numbered citations and
%% references.  The command \citestyle{authoryear} switches to the
%% "author year" style.
%% If you are preparing content for an event
%% sponsored by ACM SIGGRAPH, you must use the "author year" style of
%% citations and references.
%% Uncommenting the next command will enable that style.
\citestyle{acmauthoryear}

%% end of the preamble, start of the body of the document source.
\begin{document}


%% The "title" command has an optional parameter,
%% allowing the author to define a "short title" to be used in page headers.
\title{An Exceptional Actor System (Functional Pearl)}

%% The "author" command and its associated commands are used to define
%% the authors and their affiliations.
%% Of note is the shared affiliation of the first two authors, and the
%% "authornote" and "authornotemark" commands
%% used to denote shared contribution to the research.
\author{Patrick Redmond}
\orcid{0000-0001-5702-0860}
\author{Lindsey Kuper}
\affiliation{
  \institution{University of California, Santa Cruz}
  \country{USA}
}

%%%% %% By default, the full list of authors will be used in the page
%%%% %% headers. Often, this list is too long, and will overlap
%%%% %% other information printed in the page headers. This command allows
%%%% %% the author to define a more concise list
%%%% %% of authors' names for this purpose.
%%%% \renewcommand{\shortauthors}{Trovato et al.}

%% The abstract is a short summary of the work to be presented in the
%% article.
\begin{abstract}
    The Glasgow Haskell Compiler is well known for its fully featured runtime
    system (RTS) which includes green threads, asynchronous exceptions, and
    recently delimited continuations.
    We present a user accessible actor framework hidden in plain sight within
    the RTS, demonstrate its use on classic examples, and raise questions about
    the expressiveness and subsumption of programming language features.
\end{abstract}

%%%% %%
%%%% %% The code below is generated by the tool at http://dl.acm.org/ccs.cfm.
%%%% %% Please copy and paste the code instead of the example below.
%%%% %%
%%%% \begin{CCSXML}
%%%% <ccs2012>
%%%%  <concept>
%%%%   <concept_id>10010520.10010553.10010562</concept_id>
%%%%   <concept_desc>Computer systems organization~Embedded systems</concept_desc>
%%%%   <concept_significance>500</concept_significance>
%%%%  </concept>
%%%%  <concept>
%%%%   <concept_id>10010520.10010575.10010755</concept_id>
%%%%   <concept_desc>Computer systems organization~Redundancy</concept_desc>
%%%%   <concept_significance>300</concept_significance>
%%%%  </concept>
%%%%  <concept>
%%%%   <concept_id>10010520.10010553.10010554</concept_id>
%%%%   <concept_desc>Computer systems organization~Robotics</concept_desc>
%%%%   <concept_significance>100</concept_significance>
%%%%  </concept>
%%%%  <concept>
%%%%   <concept_id>10003033.10003083.10003095</concept_id>
%%%%   <concept_desc>Networks~Network reliability</concept_desc>
%%%%   <concept_significance>100</concept_significance>
%%%%  </concept>
%%%% </ccs2012>
%%%% \end{CCSXML}
%%%% 
%%%% \ccsdesc[500]{Computer systems organization~Embedded systems}
%%%% \ccsdesc[300]{Computer systems organization~Redundancy}
%%%% \ccsdesc{Computer systems organization~Robotics}
%%%% \ccsdesc[100]{Networks~Network reliability}

%% Keywords. The author(s) should pick words that accurately describe
%% the work being presented. Separate the keywords with commas.
\keywords{
    actor framework,
    asynchronous exceptions,
    runtime system,
    programming languages
}

%%%% \received{20 February 2007}
%%%% \received[revised]{12 March 2009}
%%%% \received[accepted]{5 June 2009}

%% This command processes the author and affiliation and title
%% information and builds the first part of the formatted document.
\maketitle


\section{Introduction}

\plr{TODO}

\section{Background}

\subsection{Exceptions in GHC}

The Glasgow Haskell Compiler (GHC) supports three varieties of exceptions, all
of which may be caught in the IO monad and otherwise cause the program to
terminate.
%
\emph{Imprecise exceptions} arise in pure code from expressions such as
\verb|(div 1 0)| which cannot be reduced further.
%
\emph{Synchronous exceptions} are thrown when side effects in the IO monad
cannot proceed such as \verb|(readFile "\0")|.
%
\emph{Asynchronous exceptions} are thrown by threads distinct from the current
one, or the RTS itself, to communicate conditions requiring the thread to
terminate: thread cancellation, user interrupts, or memory limits.
%
We focus on asynchronous exceptions.

Asynchronous exceptions uniquely allow syntactically-distant parts of a program
to interact.
%
A thread needs only the \verb|ThreadId| of another
to throw a \verb|ThreadKilled| exception to it.
The standard library function \verb|killThread|
is even implemented as \verb|(\x -> throwTo x ThreadKilled)|.\footnote{
    These identifiers are variously defined in \texttt{Control.Concurrent} and
    \texttt{Control.Exception} in \texttt{base-4.15.1.0}.
}
%
There is no permission or capability required to access this powerful feature.

Asynchronous exceptions are peculiar because they aren't constrained to their
stated purpose of ``signaling (or killing) one
thread by another'' \cite{marlow2001async}.
%
A thread may throw any exception to any thread for any reason.
%
Standard exceptions may be reused to extend greetings as in,
\verb|(\x -> throwTo x $ AssertionFailed "hello")|.
%
User defined datatypes may even be thrown as asynchronous exceptions by
declaring an instance of \verb|Exception| \cite{marlow2006extensible}.
%
Given the two lines of declarations below it is possible to greet in
vernacular, \verb|(\x -> throwTo x Hi)|.
%
\begin{spec}
data Greet = Hi | Hello deriving Show
instance Exception Greet
\end{spec}

Asynchronous exceptions may be caught by the receiving thread for
either cleanup or surprisingly, recovery.
%
An example of recovery includes ``inform[ing] the program when memory is
running out [so] it can take remedial action'' \cite{marlow2001async}.
%
The ability to recover from a termination signal seems innocuous, but combined
with the rest, asynchronous exceptions facilitate ``spooky action at a
distance'' in ways one might want to constrain in a functional programming
language.

\subsection{The actor model}

The actor model is a computational paradigm characterized by message passing.
%
\citet{hewitt1973actors} says, ``an actor can be thought of as a kind of
virtual processor that is never "busy" [in the sense that it cannot be sent a
message].''
%
In modern terms, we might say an actor is a green-thread with some state and an
inbox.
%
Upon receipt of a message to its inbox, an actor may perform some actions: send
a message, update state, create a new actor, destroy an actor, or terminate
itself.
%
Having completed that, the actor waits to process the next message in its
inbox.
%
We will approximate this model with Haskell's asynchronous exceptions as the
primary metaphor for message passing.

\citet{armstrong2003} provides additional definition for actors in their list of characteristics of a concurrency oriented programming language (COPL).
%
Every COPL
(1) has processes,
(2) which are strongly isolated,
(3) with a unique hidden identifier,
(4) without shared state,
(5) that communicate via unreliable message passing,
and
(6) can detect when another process halts.
%
Additionally
(5a) message passing is asynchronous so that no stuck recipient may cause a sender to become stuck,
(5b) receiving a response is the only way to know that a prior message was delivered,
and
(5c) messages between two processes obey FIFO ordering.
%
While an actor system within an instance of the RTS cannot satisfy all of these
requirements (e.g. termination of the main thread is not strongly isolated from
the child threads), we will show that ours satisfies many requirements of being COPL with relatively little effort.





\section{Implementation}

We define that an actor is a Haskell thread.
%
An actor thread runs a library-provided main-loop function which mediates
message receipt and calls to a user-defined handler function.
%
Here we describe the minimal abstractions around such threads which realize the
actor model.

From this point forward, all code listings are part of a literate Haskell
file.\footnote{
We use \verb|GHC 9.0.2| and \verb|base-4.15.1.0| and the following imports:

\begin{code}
{-# LANGUAGE NamedFieldPuns #-}
-- Section 3.1, 3.2
import Control.Exception (Exception(..), throwTo, catch, mask_)
import Control.Concurrent (ThreadId, myThreadId, threadDelay)
-- Section 3.3
import Control.Exception (TypeError(..), SomeException)
\end{code}
}
%
Our implementation requires a few definitions from Haskell's \verb|base|
package,
and we simplify our presentation with an extension to enable construction and
pattern-matches using binders named the same as fields.

%%%% \subsection{Simple implementation}
%%%% \label{sec:simple-impl}
%%%% 
%%%% We first reveal a simplified actor framework to communicate the essential
%%%% workings to the reader.
%%%% %
%%%% Section \plr{FIXME} defines more complex implementation we will use for
%%%% case studies in Section \ref{sec:case-studies}.


\subsection{Sending (throwing) messages}

To send a message we will throw an exception to the recipient thread's
identifier.
%
So that the recipient may respond, we define a self-addressed envelope data
type.
%
This envelope and the message content must both be instances of
\verb|Exception| and \verb|Show|.
%
\begin{code}
data Envelope a = Envelope { sender :: ThreadId, message :: a }
    deriving Show

instance Exception a => Exception (Envelope a)
\end{code}
%
With the envelope defined, our send function reads the current thread
identifier, constructs a self-addressed envelope, and throws it to the
specified recipient.
%
\begin{code}
sendStatic :: Exception a => ThreadId -> a -> IO ()
sendStatic recipient message = do
    sender <- myThreadId
    throwTo recipient Envelope{sender, message}
\end{code}


\subsection{Receiving (catching) messages}

Every actor thread runs a library-provided main-loop function to manage message
receipt and processing.
%
The main-loop installs an exception handler to accumulate messages in an inbox
and calls a user-defined handler on each.
%
The user-defined handler encodes actor intentions (or behavior) as a
state-transition that takes a self-addressed envelope as its first argument.
%
\begin{code}
type Handler st msg = Envelope msg -> st -> IO st
\end{code}

Figure \ref{fig:mainloop} defines \verb|mainloop| which takes a \verb|Handler|
and its initial state and does not return.
%
Then \verb|mainloop| masks asynchronous exceptions so they will only be raised
at well-defined points and runs its loop under that mask.

The loop has two pieces of state: that of \verb|Handler| and an inbox of
messages to be processed.
%
The loop body is divided roughly into three cases by an exception
handler and a case-split on the inbox list.
%
(1) If the inbox is empty, sleep for 60 seconds and then recurse on the
unchanged  and empty inbox.
%
(2) If the inbox has a message, call the handler and recurse on the
updated handler-state and remainder of the inbox.
%
(3) If during cases (1) or (2) an \verb|Envelope| exception is received,
recurse on unchanged handler-state and an inbox with the new envelope appended
to the end.

In the normal course of things, an actor will start with an empty inbox and go
to sleep.
%
If a message is received during sleep, the actor will wake (because
\verb|threadDelay| is defined to be \emph{interruptible}) and add the message
to its inbox.
%
On the next loop iteration the actor will process that message and once again
have an empty inbox.
%
Exceptions are masked outside of interruptible actions so that the bookkeeping
of recursing with updated state through the loop is not disrupted.

\begin{figure}
\begin{code}
mainloop :: Exception a => Handler s a -> s -> IO ()
mainloop handler initialState = mask_ $ loop (initialState, [])
  where
    loop (state, inbox) =
        catch
            (case inbox of
                [] -> threadDelay 60000000 >> return (state, inbox)
                x:xs -> (,) <$> handler x state <*> return xs)
            (\e@Envelope{} -> return (state, inbox ++ [e]))
        >>= loop
\end{code}
\caption{Core of the actor framework.}
\label{fig:mainloop}
\end{figure}

\paragraph{Unsafety}

Before moving forward, let us acknowledge that this is \emph{not safe}.
%
An exception may arrive while executing the handler.
%
Despite the exception mask which we have intentionally left in place, if the
handler executes an interruptible action then it will be preempted.
%
In this case the handler's work will be unfinished.
%
Without removing the message currently being processed, the loop
will continue on an inbox extended with the new message.
%
The next iteration will begin by processing the same message that the preempted
iteration was, effecting a double-send.

To avoid the possibility of a double-send, a careful implementor of a
\verb|Handler| might follow the documented recommendations (use software
transactional memory (STM), or avoid interruptible actions, or apply
\verb|uninterruptibleMask|), but recall that message sending is implemented
with \verb|throwTo| which is ``\emph{always} interruptible, even if does not
actually block'' \cite{controlDotException}.
%
Here be dragons.

\paragraph{Aspects of a COPL}

Which requirements to be a COPL does this system display?
%
RTS threads behave as independent process, and although not strongly
isolated and able to share state, they have a unique hidden \verb|ThreadId|.

The implementation as shown encourages communication via reliable synchronous
message passing with FIFO order.
%
By wrapping calls to \verb|sendStatic| with \verb|forkIO|, it becomes reliable
\emph{asynchronous} message passing \emph{possibly without} FIFO order.
%
FIFO can be recovered by message sequence numbers or (albeit, jumping the
shark) use of an outbox-thread per actor.
%
With use of \verb|forkFinally| an actor can reliably inform others of its
termination.\footnote{
	\verb|forkIO| and \verb|forkFinally| are defined in
	\texttt{Control.Concurrent} in \texttt{base-4.15.1.0}.
}

\plr{Digresses slightly from COPL, but still relevant to armstrong.}
Our choice to wrap a user-defined message type in a known envelope type has the
benefit of allowing the actor main-loop to distinguish between messages and
exceptions, allowing the latter to terminate the thread as intended.
%
At the same time this choice runs afoul of the \emph{name distribution problem}
\cite{armstrong2003} by indiscriminately informing all recipients of the sender
process identifier.

\paragraph{Perspective}
We have in only a few lines of code discovered an actor framework within the
RTS which makes no explicit use of channels, references, or locks and imports
just a few names from the default modules.
%
The likelihood of double sends might temper enthusiasm for this discovery, but
despite minor brokenness it is notable that this is possible.




\subsection{Dynamic types}

The actor main-loop in Figure \ref{fig:mainloop} constrains an actor thread to
handle messages of only a single type.
%
An envelope containing the wrong message type will not be caught by the
exception handler and will cause the receiving actor to crash.
%
In this section, we extend the framework to support actors that may receive
messages of different types.
%
We hesitate to identify it as a dynamically-typed actor framework.

Instead of sending an \verb|Envelope| of some application-specific message
type, we convert messages to the ``any type'' in Haskell's the exception
hierarchy, \verb|SomeException|.
%
All inflight messages will be of type \verb|Envelope SomeException|.

\begin{code}
sendDyn :: Exception a => ThreadId -> a -> IO ()
sendDyn recipient = sendStatic recipient . toException
\end{code}

On the receiving side, messages must now be downcast to the \verb|Handler|
message type.
%
This is an opportunity to treat messages of the wrong type specially.
%
We define a \verb|handlerDyn| function to convert any \verb|Handler| to one
that can receive messages produced by \verb|sendDyn|.
%
If the message downcast fails, instead of the recipient crashing, it throws an
exception (not a message) to the sender.\footnote{
    The extensions \texttt{ScopedTypeVariables}, \texttt{TypeApplications}, and
    the function \texttt{Data.Typeable.typeOf} can be used to construct a very
    helpful type-error message for debugging actor programs.
}
%%%% %
%%%% We feel that sending a message which the recipient cannot handle is a bug in
%%%% the sender not the recipient and this changes aligns behavior to align with
%%%% that expectation.
%%%% %
%%%% For brevity we show this change to the framework as a wrapper around a
%%%% \verb|Handler|, \plr{but it is better made as a modification to the
%%%% exception-handler in \verb|mainloop|}.

%%%% \begin{code}
%%%% receiveDyn :: Exception a => Handler s a -> s -> IO ()
%%%% receiveDyn handlerStatic = mainloop handlerDyn
%%%%   where
%%%%     handlerDyn e@Envelope{sender, message} state =
%%%%         case fromException message of
%%%%             Just m -> handlerStatic e{message=m} state
%%%%             Nothing
%%%%                 -> throwTo sender (TypeError "...")
%%%%                 >> return state
%%%% \end{code}

\begin{code}
handlerDyn :: Exception a => Handler s a
    -> Handler s SomeException
handlerDyn handler e@Envelope{sender, message} state =
    case fromException message of
        Just m -> handler e{message=m} state
        Nothing
            -> throwTo sender (TypeError "...")
            >> return state
\end{code}

A close reader will note that these changes haven't directly empowered actor
handler-functions to deal with messages of different types.
%
In fact, actors that wish to receive messages of different types will do so by
performing the downcast from \verb|SomeException| themselves.
%
The \verb|handlerDyn| function is most useful for actors that \emph{do not}
receive messages of different types.
%
The next section will show examples of both.






\section{Examples}
\label{sec:case-studies}

\subsection{Dining philosophers}

\subsection{Santa Clause Problem}

\subsection{Ring leader-election}

\cite{lelann1977distributed} and \cite{chang1979decentralextrema}

\subsubsection{Actor implementation}

\subsubsection{Ring setup}

\subsubsection{Extension by dynamic types}


\begin{code}
main :: IO ()
main = print "hello"
\end{code}







\section{Big Questions}

\plr{TODO}


\section{Conclusion}

\plr{TODO}

%% The acknowledgments section is defined using the "acks" environment
%% (and NOT an unnumbered section). This ensures the proper
%% identification of the section in the article metadata, and the
%% consistent spelling of the heading.
\begin{acks}
To you, for reading this.
Ack the PLV people
\end{acks}

%% The next two lines define the bibliography style to be used, and
%% the bibliography file.
\bibliographystyle{ACM-Reference-Format}
\bibliography{main.bib}

%% If your work has an appendix, this is the place to put it.
\appendix

\section{Appendix}

\plr{TODO}

\end{document}
\endinput
