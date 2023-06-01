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

\usepackage{cleveref}
\usepackage{enumitem} % style lists
\usepackage{caption} % align captions globally
\usepackage{tikz}
\usepackage{pifont}

\newcommand{\newcommenter}[3]{%
  \newcommand{#1}[1]{%
    \textcolor{#2}{\small\textsf{[{#3}: {##1}]}}%
  }%
}

\newcommenter{\plr}{magenta}{PLR}
\newcommenter{\lk}{blue}{LK}

% make numbered lists use parenthesized numerals
\renewcommand{\labelenumi}{(\arabic{enumi})}
% left align captions
\captionsetup{
    singlelinecheck = false
}

%%%% lhs2Tex (*.lhs) document
\let\Bbbk\undefined
%include polycode.fmt
\long\def\ignore#1{}
%%%% deindent comments
\renewcommand\onelinecommentchars{-{}- }
\visiblecomments
%%%% run traces
%options ghci -threaded -rtsopts -with-rtsopts=-N
%%%% prettier formatting
%format <$> = "\mathbin{\langle\$\rangle}"
%format <*> = "\mathbin{\langle*\rangle}"
%format ++  = "\mathbin{+\hspace{-0.2em}+}"
%format >>= = "\mathbin{>\hspace{-0.4em}>\hspace{-0.3em}=}"
%format >>  = "\mathbin{>\hspace{-0.4em}>}"
%format =<< = "\mathbin{=\hspace{-0.3em}<\hspace{-0.4em}<}"
%format  << =                 "\mathbin{<\hspace{-0.4em}<}"
%format ^   = "^"
%format [   = "["
%format {   = "\!\{"
%%%%format ]   = "]"
%%%%format }   = "\}"
%%%%format (,)  = "(,\!)"

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
    The Glasgow Haskell Compiler is known for its fully featured runtime
    system (RTS) which includes lightweight threads, asynchronous exceptions, and --
    recently -- delimited continuations.
    %%\plr{Leaving "lightweight threads" in because it's part of being a
    %%fully-featured runtime. A language with a less fully-featured runtime uses
    %%OS threads.}
    %
    The combination of these features is powerful enough that a programmer may
    complete the same task in many different ways -- some more advisable than
    others.

    We present a user-accessible actor framework hidden in plain sight within
    the RTS and demonstrate it on a classic example from the distributed
    systems literature.
    %
    We then extend both the framework and example to the realm of dynamic
    types.
    %
    Finally, we raise questions about how RTS features intersect and possibly
    subsume one another, and make recommendations about how GHC can
    guide best practice by constraining the use of some features.
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

The most commonly used implementation of Haskell today is the Glasow Haskell
Compiler (GHC) \cite{fausak2022} together with its runtime system (RTS).
%
The RTS is featureful and boasts support for lightweight threads, two kinds of
profiling, transactional memory, asynchronous exceptions, and a slew of other
features.
%
Combined with the \verb|base| package, a programmer can get a lot
done without ever reaching into the extensive set of community packages on
Hackage.

In that spirit,
we noticed that there's nothing really stopping one from
abusing the tools \verb|throwTo| and \verb|catch|
to pass data between threads.
%%\lk{I don't know what ``a game of pass'' is}
%%\plr{say asynchronous exceptions}
%
Any user-defined datatype can be made into an asynchronous exception.
%
Why not implement message-passing algorithms on that substrate?

We pursued this line of thought, and in this paper we present an actor
framework hidden just under the surface of the RTS.
%
This paper is a literate Haskell program.\footnote{
    We use \verb|GHC 9.0.2| and \verb|base-4.15.1.0|.
    %
    The actor framework imports \verb|Control.Exception| and
    \verb|Control.Concurrent|, and we use the extensions \verb|NamedFieldPuns|
    and \verb|DuplicateRecordFields| for convenience of its presentation.
    %
    The example section additionally imports the module \verb|System.Random|
    and uses the extension \verb|ViewPatterns|.
    %
    The appendices have other imports which we don't decribe here.
}
\begin{itemize}[leftmargin=1.5em]
    \item[--] \Cref{sec:background} provides a concise summary of asynchronous
    exceptions in GHC and the actor model of programming.

    \item[--] \Cref{sec:actor-framework} details the implementation of our
    actor framework. We first show how actors receive messages of a single
    type. Next we upgrade the framework to support dynamically typed actors,
    which receive messages of more than one type.

    \item[--] \Cref{sec:ring-impl} shows an implementation of the ring
    leader-election algorithm using actors. Then we extend the actors with an
    additional message type and behavior without changing the original
    implementation.

    \item[--] We reflect on whether this was a good idea in
    \Cref{sec:what-hath-we-wrought}:
    Situating the actor framework and reviewing its performance,
    arguing that asynchronous exceptions might be more constrained,
    \plr{foreshadow ``extended "awkward squad"''; TODO the rest of this sentence
    and asking when it is appropriate to implement ``language level'' features
    in terms of others... wrapping up in \Cref{sec:conclusion}.}
\end{itemize}


% Imports for the haskell program:
% Trying to keep them near the footnote that discusses them.
\ignore{
\begin{code}
{-# LANGUAGE NamedFieldPuns #-}        -- Section 2
{-# LANGUAGE DuplicateRecordFields #-} -- Section 3.2
{-# LANGUAGE ViewPatterns #-}          -- Section 3.3

-- Section 2.1, 2.2
import Control.Exception (Exception(..), throwTo, catch, mask_)
import Control.Concurrent (ThreadId, myThreadId, threadDelay)

import Control.Exception (getMaskingState, MaskingState(..))

-- Section 2.3
import Control.Exception (TypeError(..))

-- Section 3.2
import Control.Exception (SomeException)
import Control.Concurrent (forkIO, killThread)
import System.Random (RandomGen, randomR, getStdRandom)

-- Trace appendix
import System.IO (hSetBuffering, stdout, BufferMode(..))

-- Perf eval appendix
import Control.Exception (assert)
import System.Environment (getArgs)
import qualified Control.Concurrent.Chan as Ch
import qualified Control.Concurrent.MVar as Mv
import qualified Criterion.Main as Cr
\end{code}
} % end ignore










\section{Brief background}
\label{sec:background}

\subsection{Asynchronous exceptions in GHC}

The Glasgow Haskell Compiler (GHC) is unusual in its support for
\emph{asynchronous exceptions}.
%
Unlike synchronous exceptions,
which are thrown as a result of executing code in the current thread,
asynchronous exceptions are thrown by threads distinct from the current one,
or by the RTS itself.
%
They communicate conditions requiring the current thread to
terminate: thread cancellation, user interrupts, or memory limits.
%
We focus exclusively on asynchronous exceptions for the rest of the paper.

Asynchronous exceptions allow syntactically-distant parts of a program
to interact in unexpected ways, much like mutable references.
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
This absence of restrictions means that standard exceptions may be reused for
any purpose, such as to extend greetings:
\verb|(\x -> throwTo x $ AssertionFailed "hello")|.
%
Even
user-defined datatypes may be thrown as asynchronous exceptions by
declaring an empty instance of \verb|Exception| \cite{marlow2006extensible}.
%
For example, with the declarations in \Cref{fig:greet} it is possible to greet
in vernacular: \verb|(\x -> throwTo x Hi)|.

Asynchronous exceptions may be caught by the receiving thread for
either cleanup or, surprisingly, recovery.
%
An example of recovery includes ``inform[ing] the program when memory is
running out [so] it can take remedial action'' \cite{marlow2001async}.
%
The ability to recover from a termination signal seems innocuous, but
it leaves asynchronous exceptions open to being repurposed.

\subsection{The actor model}
\label{sec:actor-model}

The actor model is a computational paradigm characterized by message passing.
%
\citet{hewitt1973actors} says ``an actor can be thought of as a kind of
virtual processor that is never `busy' [in the sense that it cannot be sent a
message].''
%
We flesh out this definition by saying, an actor is a green thread\footnote{
    A \emph{green thread} (also ``lightweight thread'' or ``userspace thread'')
    is a thread not bound to an OS thread but dynamically mapped to a CPU by a
    language-level scheduler.
    %
    A language with only OS threads would likely support actor programming
    poorly, due to the large numbers of actors required.
    %
    The \emph{Akka} framework in Java gets around the lack of green threads in
    the JVM with a framework-level scheduler.
} with some state and an inbox.
%
Upon receipt of a message to its inbox, the actor may perform some actions:
send a message, update state, create a new actor, destroy an actor, or
terminate itself.
%
Having completed that, the actor waits to process the next message in its
inbox.
%
We will approximate this model with Haskell's asynchronous exceptions as the
primary metaphor for message passing.

More concretely, we think of an actor framework as
having the characteristics that \citet{armstrong2003} lists for a
\emph{concurrency-oriented programming language} (COPL).
%
After describing our framework, we will make the case that it has many of the
characteristics of a COPL.
%
Summarized, a COPL
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
requirements (e.g., termination of the main thread is not strongly isolated
from the child threads), we will show that ours satisfies many requirements of
being COPL with relatively little effort.





\begin{figure}
%\raggedright
\begin{spec}
data Greet = Hi | Hello deriving Show
instance Exception Greet
\end{spec}
\caption{
    \verb|Show| and \verb|Exception| instances are all that is required to
    become an asynchronous exception.
    \plr{ensure not near \Cref{fig:envelope-and-intent}}
}
\label{fig:greet}
\end{figure}





\section{Actor framework implementation}
\label{sec:actor-framework}


In our framework, an actor is a Haskell thread running a
provided main-loop function.
%
The main-loop function mediates message receipt and makes calls to a
user-defined intent function.
%
Here we describe the minimal abstractions around such threads which realize the
actor model.


\subsection{Sending (throwing) messages}
\label{sec:sending-throwing}


To send a message we will throw an exception to the recipient's thread
identifier.
%
So that the recipient may respond, we define a self-addressed envelope data
type in \Cref{fig:envelope-and-intent} and declare the required instances.


\Cref{fig:static-impl} defines a send function, \verb|sendStatic|,
which reads the current thread identifier, constructs a self-addressed
envelope, and throws it to the specified recipient.
%
For the purpose of explication in this paper, it also prints a trace.


\subsection{Receiving (catching) messages}
\label{sec:receiving-catching}


An actor is defined by how it behaves in response to messages.
%
A user-defined intent function, with the type \verb|Intent| shown in
\Cref{fig:envelope-and-intent},
encodes behavior as a state transition that takes a self-addressed envelope
argument.


Every actor thread will run a provided main-loop function to manage message
receipt and processing.
%
The main-loop function installs an exception handler to accumulate messages in
an inbox and calls a user-defined intent function on each.
%
\Cref{fig:static-impl} defines a main-loop, \verb|runStatic|, that
takes an \verb|Intent| function and its initial state and does not return.
%
It masks asynchronous exceptions so they will only be raised at well-defined
points and runs its loop under that mask.


The loop in \Cref{fig:static-impl} has two pieces of state: that of the intent
function, and an inbox of messages to be processed.
%
The loop body is divided roughly into three cases by an exception
handler and a case-split on the inbox list.
%
\begin{enumerate}[leftmargin=2em]
    \item If the inbox is empty, sleep for an arbitrary length of time and then
    recurse on the unchanged actor state and the empty inbox.
    
    \item If the inbox has a message, call the intent function and recurse on
    the updated actor state and remainder of the inbox.
    \plr{Ensure this list isn't broken across pages}

    \item If during cases (1) or (2) an \verb|Envelope| exception is received,
    recurse on the unchanged actor state and an inbox with the new envelope
    appended to the end.
\end{enumerate}
%
In the normal course of things, an actor will start with an empty inbox and go
to sleep.
%
If a message is received during sleep, the actor will wake (because
\verb|threadDelay| is defined to be \emph{interruptible}), add the message to
its inbox, and recurse.
%
On the next loop iteration, the actor will process that message and once again
have an empty inbox.
%
Exceptions are masked outside of interruptible actions so that the bookkeeping
of recursing with updated state through the loop is not disrupted.


\paragraph{Unsafety}


Before moving forward, let us acknowledge that this is \emph{not safe}.
%
An exception may arrive while executing the intent function.
%
Despite the exception mask which we have intentionally left in place,\footnote{
    It is best practice to use \texttt{mask} instead of \texttt{mask\_}, and
    ``restore'' the masking state of the context before calling the intent
    function.
    %
    However for our purpose here, using \texttt{mask\_} doesn't change much.

} if the intent function executes an interruptible action, then
it will be preempted.
%
In this case the intent function's work will be unfinished.
%
Without removing the message currently being processed, the loop
will continue on an inbox extended with the new message.
%
The next iteration will begin by processing the same message that the preempted
iteration was, effecting a double-send.


To avoid the possibility of a double-send, a careful implementor of an actor
program might follow the documented recommendations for code in the presence of
asynchronous exceptions:
use software transactional memory (STM),
avoid interruptible actions,
or apply \verb|uninterruptibleMask|.
However recall that message sending is implemented with \verb|throwTo| which is
``\emph{always} interruptible, even if does not actually block''
\cite{controlDotException}.
%
Here be dragons.
%
The best recommendation we can make is for the idempotence of intent
functions.\footnote{
    We have also considered a design in which the intent function returns an
    outbox of messages.
    %
    It is then up to the main-loop to carefully send those messages and deal
    with possible interruption.
    %
    While this might work, we opt for the simpler presentation seen here.
}


\begin{figure}
\raggedright
\begin{code}
data Envelope a = Envelope { sender :: ThreadId, message :: a }
    deriving Show

instance Exception a => Exception (Envelope a)

type Intent st msg = st -> Envelope msg -> IO st
\end{code}
\caption{
    Message values are contained in a self-addressed envelope.
    %
    Actor behavior is encoded as a transition system.
}
\label{fig:envelope-and-intent}
\end{figure}


\begin{figure}
\raggedright
\begin{code}
sendStatic :: Exception a => ThreadId -> a -> IO ()
sendStatic recipient message = do
    sender <- myThreadId
    putStrLn (show sender ++ " send " ++ show message
                          ++ " to " ++ show recipient)
    throwTo recipient Envelope{sender, message}

runStatic :: Exception a => Intent s a -> s -> IO ()
runStatic intent initialState = mask_ $ loop (initialState, [])
  where
    loop (state, inbox) =
        catch
            (case inbox of
                [] -> threadDelay 60000000 {-"\hfill(1)"-}
                    >> return (state, inbox)
                x:xs ->
                    (,{-"\!"-}) <$> intent state x <*> return xs) {-"\hfill(2)"-}
            (\e@Envelope{} ->
                return (state, inbox ++ [e])) {-"\hfill(3)"-}
        >>= loop
\end{code}
\caption{
    Messages sends are implemented by throwing an exception.
    %
    Actor threads enter a main-loop to receive messages.
}
\label{fig:static-impl}
\end{figure}




\subsection{Dynamic types}
\label{sec:dynamic-types}


The actor main-loop in \Cref{fig:static-impl} constrains an actor
thread to handle messages of a single type.
%
An envelope containing the wrong message type will not be caught by the
exception handler, causing the receiving actor to crash.
%
We think the recipient should not crash when another actor sends an incorrect
message.


In this section we correct this issue by extending the framework to support
actors that may receive messages of different types.
%
With this extension our framework could be thought of as dynamically typed in
the sense that a single actor can process multiple message types.
%
This is very similar to the dynamic types support in Haskell's
\verb|Data.Dynamic| module.


Furthermore, any actor may be extended by wrapping it (has-a style) with an
actor that uses a distinct message type and branches on the type of a received
message, delegating to the wrapped actor where desired.\footnote{
    It is not sufficient to wrap a message type in a sum-type and write an
    actor that takes the sum-type as its message.
    %
    Such a wrapper will fail to receive messages sent as the un-wrapped type.
    %
    To correct for this one would need to modify other actors to wrap their
    outgoing messages in the sum-type.
    %
    The dynamic types pattern described in \Cref{sec:dynamic-types}
    generalizes this for all types.
}
%
It is desirable to encapsulate such actor-wrapping in combinators that
generalize the patterns by which an actor is given additional behavior.
%
Our purpose here, though, is not to lean into the utility of a dynamically
typed actor framework, but to point out how little scaffolding is required to
obtain one from the RTS.


\subsubsection{Sending dynamic messages}


Instead of sending an \verb|Envelope|
of some application-specific message type
we convert messages to the ``any type''
in Haskell's exception hierarchy,
\verb|SomeException|.
%
\Cref{fig:dyn-impl} defines a new send function which converts messages before
sending, so that all inflight messages will have the type \verb|Envelope
SomeException|.


\subsubsection{Receiving dynamic messages}
\label{sec:dynamic-recv-loop}


On the receiving side, messages must now be downcast to the \verb|Intent|
function's message type.
%
This is an opportunity to treat messages of the wrong type specially.
%
In \Cref{fig:dyn-impl} we define a new main-loop, \verb|runDyn|,
that lifts any intent function to one that can
receive envelopes containing \verb|SomeException|.
%
If the message downcast fails, instead of the recipient crashing, it performs a
``return to sender.''
%
Specifically, it throws an exception (not an envelope) with a run time
type-error.\footnote{
    The extensions \texttt{ScopedTypeVariables}, \texttt{TypeApplications}, and
    the function \texttt{Data.Typeable.typeOf} can be used to construct a very
    helpful type-error message for debugging actor programs.
}


These changes do not directly empower actor intent functions to
deal with messages of different types.
%
We have only lifted our infrastructure to remove
application-specific type parameters from envelopes.
%
Actors intending to receive messages of different types will do so by
performing the downcast from \verb|SomeException| themselves.
%
Such actors will use an intent function handling messages of type
\verb|SomeException|.
%%%, and will work equally well with \verb|runStatic| or \verb|run|.
%
\Cref{sec:dyn-ring} shows an example of an actor that receives
messages of different types, by extending an actor that doesn't.


\begin{figure}
\raggedright
\begin{code}
send :: Exception a => ThreadId -> a -> IO ()
send recipient = sendStatic recipient . toException

runDyn :: Exception a => Intent s a -> s -> IO ()
runDyn intentStatic = runStatic intent
  where
    intent state e@Envelope{sender, message} =
        case fromException message of
            Just m -> intentStatic state e{message=m}
            Nothing
                -> throwTo sender (TypeError "...")
                >> return state
\end{code}
\caption{
    The dynamically typed framework
    upcasts before sending
    and downcasts before processing.
}
\label{fig:dyn-impl}
\end{figure}




\subsection{Safe initialization}
\label{sec:safe-fork}

%%% This mini section raises an issue that's relevant to the content of
%%% \Cref{fig:static-impl}, but it doesn't fit into the flow of
%%% \Cref{sec:receiving-catching}. Reasons this probably shouldn't be in
%%% \Cref{sec:receiving-catching}:
%%%    * \Cref{fig:run} uses \Cref{fig:dyn-impl} which
%%%      isn't defined until \Cref{sec:dynamic-recv-loop}.
%%%    * \Cref{sec:receiving-catching} has a very tight narrative and needs to
%%%      be simple or we'll lose readers.

When creating an actor thread it is important that no exception arrive before
the actor main-loop (\verb|runStatic| in \Cref{fig:static-impl})
installs its exception handler.
%
If this happened, the execption would cause the newly created thread to die.
%
To avoid this, the fork step prior to entering the actor main-loop must be
masked (this is in addition to the mask within the main-loop).

\Cref{fig:run} defines the main-loop wrapper we will use for examples in
\Cref{sec:ring-impl}.
%
It performs a best-effort check and issues a helpful reminder to mask the
creation of actor threads.\footnote{
    We don't define a wrapper around \texttt{forkIO} to perform this masking
    because actors which perform initialization steps can currently do so
    before calling \texttt{run}. There is an example in \Cref{sec:main2-init}.
}

\begin{figure}
\raggedright
\begin{code}
run :: Exception a => Intent s a -> s -> IO ()
run intent state = do
    ms <- getMaskingState
    case ms of
        MaskedInterruptible -> runDyn intent state
        _ -> error "mask the forking of actor threads"
\end{code}
\caption{Remind users to prevent initialization errors by masking forks.}
\label{fig:run}
\end{figure}




\section{Example: Ring leader-election}
\label{sec:ring-impl}

The problem of \emph{ring leader-election} is to designate one node
among a network of communicating nodes organized in a ring topology.
%
Each node has a unique identity, and identities are totally ordered.
%
Nodes do not know number or identities of the other nodes, except for their
immediate successor ``next'' node.
%
A correct solution will result in exactly one node being designated the leader.
%
We choose to demonstrate a solution to this classic problem in distributed
systems literature because it nicely illustrates concurrent programming,
despite being unnecessary in the context of threads in a process.

\citet{chang1979decentralextrema} describe a solution that begins with every
node sending a message to its successor to nominate itself as the leader.
%
Upon receiving a nomination,
a node forwards the nomination to its successor
if the identity of the nominee is
greater than the identity of the current node.
%
Otherwise the nomination is ignored.
%
We implement and extend that solution below.

\begin{figure}
\include{ring.tex}
\caption{
    In-progress ring leader-election with seven nodes
    (\citeauthor{chang1979decentralextrema}'
    \citeyear{chang1979decentralextrema} solution).
    %
    The node identities are unique and randomly distributed.
    %
    Two nomination chains are shown:
    %
    Node 5 nominated itself and was accepted by nodes 3, 1, and 4; next node 4
    will nominate 5 to node 6 (who will reject it).
    %
    Concurrently, node 6 nominated itself and was accepted by node 2 but
    rejected by node 7.
    %
    For this election to obtain a leader, node 7 must nominate itself.
}
\label{fig:ring-election-visual}
\end{figure}

\subsection{Implementing a leader-election}




Each node begins uninitialized, and later becomes a member of the ring when
it learns the identity of its successor.
%
To represent this we define two contsructors in \Cref{fig:node-types} for node
state type, \verb|Node|.
%
Three messages (also defined in \Cref{fig:node-types} as type, \verb|Msg|) will
be used to run the election:
\begin{itemize}[leftmargin=15mm]
    \item[\verb|Init|] After creating nodes, the main thread initializes
    the ring by informing each node of its successor.
    %
    \item[\verb|Start|] The main thread rapidly instructs every node to start
    the leader election.
    %
    \item[\verb|Nominate|] The nodes carry out the election by sending and
    receiving nominations.
\end{itemize}
%
\begin{figure}
\raggedright
\begin{code}
data Node = Uninitialized | Member {next::ThreadId}

data Msg
    = Init{next::ThreadId}
    | Start
    | Nominate{nominee::ThreadId}
    deriving Show

instance Exception Msg
\end{code}
\caption{
    Election nodes can be in one of two states,
    and they accept three different messages.
}
\label{fig:node-types}
\end{figure}



\subsubsection{Election termination}
The node with the greatest identity that nominates itself will eventually
receive its own nomination after it has circulated the entire ring.
%
That same node will ignore every other nomination.
%
Therefore the algorithm will terminate because node identities being unique
means that only one nomination will circumnavigate the ring.
%
If no nomination makes it all the way around the ring, then the algorithm
terminates without a winner.





\subsubsection{Node-actor behavior}
\label{sec:ring-intent-fun}



The intent function for a node actor will have state of type \verb|Node| and
receive messages of type \verb|Msg|, as defined in \Cref{fig:node-types}.
%
We show its implementation and describe each case here below.
%
\plr{These three cases for the implementation of \verb|node| is the only code
outside of a figure. I think it reads better in-flow of the prose.}
%
\begin{code}
node :: Intent Node Msg
\end{code}
%
When an uninitialized node receives an \verb|Init| message, it becomes a member
of the ring and remembers its successor.
%
\begin{code}
node Uninitialized
  Envelope{message=Init{next}} = do
    return Member{next}
\end{code}
%
When a member of the ring receives a \verb|Start| message,
it nominates itself to its successor in the ring.
%
\begin{code}
node Member{next}
  Envelope{message=Start} = do
    self <- myThreadId
    send next $ Nominate self
    return Member{next}
\end{code}
%
When a member of the ring receives a \verb|Nominate| message, it compares the
nominee to its own identity.
%
If they are equal, then the member wins and the algorithm stops.
%
If the nominee is greater, then the member forwards the nomination to its
successor.
%
\begin{code}
node state@Member{next}
  Envelope{message=Nominate{nominee=nom}} = do
    self <- myThreadId
    case () of
     _  |  self == nom -> putStrLn (show self ++ ": I win")
        |  self <  nom -> send next (Nominate nom)
        |  otherwise   -> putStrLn "Ignored nomination"
    return state
\end{code}
%
\ignore{
\begin{code}
node _ _ = error "node: unhandled"
\end{code}
}



\subsubsection{Election initialization}
\label{sec:main1-init}



The election initialization function\footnote{
    The implementation shown doesn't handle rings of size 0 or 1,
    but we consider that out of scope of the demonstration.
    %
    Also, we don't show thread cleanup here.
}
is implemented in \Cref{fig:ringElection}.
%
It takes the size of the ring
and an unevaluated \verb|IO| action representing node behavior,
and then performs the following steps to start an election:
%
\begin{enumerate}[leftmargin=2em]
    \item Create actors (with asynchronous exceptions masked).

    \item Randomize the order of actor \verb|ThreadId|s.

    \item Inform each actor of the \verb|ThreadId| that follows it in the
    random order (its successor) with an \verb|Init| message.

    \item Send each actor the \verb|Start| message to kick things off.
\end{enumerate}
%
To call the election initialization function, we construct an \verb|IO| action
by passing the node intent function and the initial node state to the actor
main-loop from \Cref{fig:run}:
%%%%\begin{figure}[b]
%
\ignore{
\begin{code}
main1 :: Int -> IO ()
main1 count = do
    ring <-
\end{code}
}
%
\begin{center}
\begin{code}
        ringElection count $ run node Uninitialized
\end{code}
\end{center}
%
\ignore{
\begin{code}
    threadDelay 1000000
    mapM_ killThread ring
\end{code}
}
%
%%%%\caption{Calling convention}
%%%%\label{fig:call-ringElection}
%%%%\end{figure}
An election trace appears in \Cref{fig:main1-trace}.


\begin{figure}[h]
\raggedright
\begin{code}
ringElection :: Int -> IO () -> IO [ThreadId]
ringElection n actor = do
    nodes <- sequence . replicate n . mask_ $ forkIO actor {-"\quad\hfill (1)"-}
    ring <- getStdRandom $ permute nodes {-"\quad\quad\hfill (2)"-}
    mapM_
        (\(t, next) -> send t Init{next}) {-"\quad\quad\hfill (3)"-}
        (zip ring $ tail ring ++ [head ring])
    mapM_ (\t -> send t Start) ring {-"\quad\quad\hfill (4)"-}
    return ring
\end{code}
\caption{Ring leader-election initialization.
    \plr{Make sure this doesn't float into the definition of \verb|node|.}
}
\label{fig:ringElection}
\end{figure}


\begin{figure}
\raggedright
\footnotesize
\perform{beginVerb >> main1 5 >> endVerb}
\normalsize
\caption{A trace of the ring leader-election solution.}
\label{fig:main1-trace}
\end{figure}
\plr{remove extra trace from the end}





\subsection{Extending the leader election}
\label{sec:dyn-ring}

The solution we have shown solves the ring leader-election problem
insofar as a single node concludes that it has won.
%
However, it is also desirable for the other nodes to learn the outcome of the
election.
%
Since it is sometimes necessary to extend a system without modifying the
original, we will show how to extend the original ring leader-election to add a
winner-declaration round.

Since there is no message constructor to inform nodes of the election outcome,
we will define a new message type whose constructor indicates a declaration of
who is the winner.
%
We will extend the existing node intent function by wrapping it with a new
intent function that processes messages of either the old or the new message
types, with distinct behavior for each, leveraging the dynamic types support
from \Cref{sec:dynamic-types}.
%
The new behaviors are:
%
\begin{itemize}[leftmargin=1.5em]
    \item[--] Each node remembers the greatest nominee it has seen.

    \item[--] When the winner self-identifies, they will start an extra round
    declaring themselves winner.

    \item[--] Upon receiving a winner declaration, a node compares the greatest
    nominee it has seen with the declared-winner.
    %
    If they are the same, then the node forwards the declaration to its
    successor.
\end{itemize}

Extended-nodes will store the original node state (\Cref{fig:node-types})
paired with the identity of the greatest nominee they have seen.
%
This new extended-node state is shown in \Cref{fig:exnode-types} as type,
\verb|Exnode|.
%
\plr{I'm trying to give the extended nodes a name that's more distinct from
Node than Node' (nodeprime). I'm hoping that Node vs Exnode is more easy to
keep a handle on.}
%
The new message type (\verb|Winner|, also in \Cref{fig:exnode-types}) has only
one constructor and is used to declare some node the winner.
%
\begin{figure}
\raggedright
\begin{code}
type Exnode = (Node, ThreadId)

data Winner = Winner ThreadId deriving Show

instance Exception Winner
\end{code}
\caption{
    Extended-nodes store node state alongside the greatest nominee
    seen.
    %
    They accept one message in addition to those in \Cref{fig:node-types}.
}
\label{fig:exnode-types}
\end{figure}





\subsubsection{Declaration-round termination}
%
When an extended-node receives a declaration of the winner
that matches their greatest nominee seen,
they have ``learned'' that node is indeed the winner.
%
When the winner receives their own declaration,
\emph{everyone} has learned they are the winner,
and the algorithm terminates.
%
If the winner declaration doesn't make it all the way around the ring,
then the algorithm terminates without confirming a winner.






\subsubsection{Exnode-actor behavior}
\label{sec:ring2-intent-fun}


The intent function for the new actor will have state \verb|Exnode| and
receive messages of type \verb|SomeException|.
%
This will allow it to receive either \verb|Msg| or \verb|Winner| values and
branch on which is received.
%
\begin{code}
exnode :: Intent Exnode SomeException
\end{code}


Recall the implementation of the actor main-loop function,
\verb|runDyn| from \Cref{fig:dyn-impl}.
%
When we apply \verb|exnode| to \verb|runDyn|,
the call to \verb|fromException| in \verb|runDyn|
is inferred to return \verb|Maybe SomeException|,
which succeeds unconditionally.
%
The \verb|exnode| intent function must then perform its own downcasts,
and we enable \verb|ViewPatterns| ease our presentation.
%
There are two main cases,
corresponding to the two message types the actor will handle,
which we explain below.
%
\plr{This paragraph was an aside to explain that \texttt{fromException ::
SomeException -> Maybe a} always succeeds when \texttt{a} is
\texttt{SomeException}. It's a bit out-of-flow, but there's no other place it
belongs in.}


The first case of \verb|exnode|, shown in \Cref{fig:exnode-case-msg}, applies
when an extended-node downcasts the envelope contents to \verb|Msg|.
%
In each of its branches, node state is updated by delegating part of message
handling to the held-node.
%
We annotate the rest of \Cref{fig:exnode-case-msg} as follows:
\plr{I would like to put this list into the caption, but was unable to.}
%
\begin{enumerate}[leftmargin=2em]
    \item Delegate to the held-node by putting the revealed \verb|Msg| back
    into its envelope and passing it through the intent function, \verb|node|,
    from from \Cref{sec:ring-intent-fun}.
    %
    \item If the message is a nomination of the current extended-node, start
    the winner round because the election is over. \plr{If you want to shorten this, remove "extended-"}
    %
    \item Otherwise the election is ongoing so keep track of the greatest
    nominee seen.
\end{enumerate}
%
\begin{figure}
\raggedright
\begin{code}
exnode (n, great)
  e@Envelope{message=fromException -> Just m} = do
    self <- myThreadId
    n'@Member{next} <- node n e{message=m} {-"\quad\quad\hfill (1)"-}
    case m of
        Nominate{nominee} ->
            if self == nominee
            then send next (Winner self) {-"\quad\quad\hfill (2)"-}
                >> return (n', great)
            else return (n', max nominee great) {-"\quad\quad\hfill (3)"-}
        _ -> return (n', great)
\end{code}
\caption{
    When \verb|exnode| receives a \verb|Msg|, it delegates to \verb|node|.
    It may also update the greatest nominee seen or trigger the
    winner-declaration round.
}
\label{fig:exnode-case-msg}
\end{figure}


The second case of \verb|exnode| applies when a node downcasts the envelope
contents to a winner declaration.
%
Its implementation is shown in \Cref{fig:exnode-case-winner}.
%
If the current node is declared winner, the algorithm terminates successfully.
%
If the greatest nominee the current node has seen is declared winner, the node
forwards the declaration to its successor.
%
State is unchanged in each of these branches.
%
\begin{figure}
\raggedright
\begin{code}
exnode state@(Member{next}, great)
  Envelope{message=fromException -> Just m} = do
    self <- myThreadId
    case m of
        Winner w
            | w == self -> putStrLn (show self ++ ": Confirmed")
            | w == great -> send next (Winner w)
            | otherwise -> putStrLn "Unexpected winner"
    return state
\end{code}
\caption{
    When \verb|exnode| receives a \verb|Winner|, it manages the
    winner-declaration round.
}
\label{fig:exnode-case-winner}
\end{figure}


\ignore{
\begin{code}
exnode _ _ = error "exnode: unhandled"
\end{code}
}




\subsubsection{Extended-election initialization}
\label{sec:main2-init}


The extended ring leader-election reuses the
initialization scaffolding from before
(\Cref{fig:ringElection}).
%
The only change is that the \verb|IO| action passed to
\verb|ringElection| initializes the greatest nominee seen
to itself.
%
It is called like this:
%
\ignore{
\begin{code}
main2 :: Int -> IO ()
main2 count = do
    ring <-
\end{code}
}
%
\begin{center}
\begin{code}
        ringElection count $ do
            great <- myThreadId
            run exnode (Uninitialized, great)
\end{code}
\end{center}
%
\ignore{
\begin{code}
    threadDelay 1000000
    mapM_ killThread ring
\end{code}
}
%
A trace of an extended-election is in \Cref{sec:main2-trace}.




\section{What hath we wrought?}
\label{sec:what-hath-we-wrought}

\Cref{fig:static-impl} shows that we have, in only a few lines of
code, discovered an actor framework within the RTS which makes no explicit use
of channels, references, or locks and imports just a few names from default
modules.
%
The support for dynamic types, shown in \Cref{fig:dyn-impl} as separate
definitions, can be folded into \Cref{fig:static-impl} for only a few
additional lines.\footnote{
    Instead of wrapping the intent function, the framework's message downcast
    is performed in the exception handler.
}
%
While a user must remember to
mask asynchronous exceptions when creating an actor thread
(\Cref{fig:run}/\Cref{sec:safe-fork}\plr{which to reference?}),
and endeavor to write an idempotent intent function
(\Cref{sec:receiving-catching}),
we find it compelling that this is possible and shocking that it is so easy.

\subsection{Almost a COPL}

Which requirements to be a COPL (\Cref{sec:actor-model}) does this framework
display?
%
RTS threads behave as independent processes, and although not strongly
isolated and able to share state, they have a unique hidden \verb|ThreadId|.

The implementation as shown encourages communication via \emph{reliable
synchronous message passing with FIFO order}.
%
We call it synchronous because ``\verb|throwTo| does not return until the
exception is received by the target thread''
\cite{controlDotException}.\footnote{
	``Synchronous for me, but not for thee'' might be the most correct
	characterization. Senders may experience GHC's asynchronous exceptions as
	synchronous, but recipients will always perceive them as asynchronous.
}
%
This means that a sender may block if the recipient never reaches an
interruptible point (e.g. its intent function enters an infinite loop in pure
computation).
%
Assuming intent functions terminate, instead the framework will tend to
exhibit the behavior of \emph{reliable asynchronous message passing with FIFO
order} and occasional double-sends.
%
By wrapping calls to the send function with \verb|forkIO|
\cite{marlow2001async}, we obtain \emph{reliable asynchronous message passing
without FIFO order} even in the presence of non-terminating intent
functions.\footnote{
    If thread $T_1$ forks thread $T_2$ to send message $M_2$, and then $T_1$
    forks thread $T_3$ to send message $M_3$, the RTS scheduler may first run
    $T_3$ resulting in $M_3$ reaching the recipient before $M_2$, violating
    FIFO if both messages have the same recipient.
}
%
FIFO can be recovered by message sequence numbers or by (albeit, jumping the
shark) use of an outbox-thread per actor.
%
An actor can reliably inform others of its termination with use of
\verb|forkFinally|.\footnote{
	\verb|forkIO| and \verb|forkFinally| are defined in
	\texttt{Control.Concurrent} in \texttt{base-4.15.1.0}.
}

Our choice to wrap a user-defined message type in a known envelope type has the
benefit of allowing the actor main-loop to distinguish between messages and
exceptions, allowing the latter to terminate the thread as intended.
%
At the same time this choice runs afoul of the \emph{name distribution problem}
\cite{armstrong2003} by indiscriminately informing all recipients of the sender
process identifier.
%
One strategy to hide to an actor's name and restore the lost security isolation is
to wrap calls to the send function with \verb|forkIO|.
%
Another strategy would be to define two constructors for envelope, and elide
the ``sender'' field from one.

By comparing the discussion above with
\citeauthor{armstrong2003}'s
requirements summarized in
\Cref{sec:actor-model},
we claim that this actor framework is \emph{almost}
a COPL.
%
It also meets our informal requirements that actors can send and receive
messages, update state, and spawn or kill other actors (though we have not
shown examples of all of these).
%
With conscientious attention to the termination and idempotence of intent
functions, the framework might be considered practical.





\subsection{Pithy statement about performance}

\plr{TODO}

\plr{
\begin{itemize}
    \item when constrained to 4 capabilities in the threaded RTS, usually chan based implementaton takes 0.3 the time of actor based implementation for up to 2048 nodes
    \item when given 8 capabilities in the threaded RTS, chan based implementation takes up to 0.9 the time of actor based implementation for 4096 to 16384 nodes
\end{itemize}
It's hard to interpret
}
\plr{
    lindsey: this is extra; anything isfine;
    %
    will it generate a pdf?
    %
    put like N next to each other
}





\subsection{Feature subsumption}

\lk{Let's figure out what the point is that we want to make here.}

Can we implement an actor framework with Haskell's threads and asynchronous
exceptions?
%
This is the question that led us to writing this paper.
%
\Cref{fig:static-impl} shows that we very nearly can, and this fact hints
that perhaps asynchronous exceptions are more general than actors.
\lk{It's not just \Cref{fig:static-impl} in isolation.  How about ``Our implementation suggests that we very nearly can.''}
\lk{But, why ``very nearly can'' and not just ``can''?  We should explicit about what's missing if something is missing, instead of using weasel words}

When we discussed this research at an informal gathering, a participant asked
whether algebraic effects could be used to implement asynchronous exceptions.
%
This lead us to a lively discussion:
%
Can you implement them in terms of delimited continuations?
%
What does this mean for the design of exception systems? 
%
What is the relationship to coroutines?
%
\lk{I think the above stuff about the ``gathering'' and the ``lively discussion'' is too colloquial.  We could say something like ``Several questions come to mind:'' and then discuss the questions.  We should be sure to put the PLV group in the acks if this paper gets accepted.}

\plr{
Are Haskell's asynchronous exceptions truly asynchronous, given that a sender
could block?
}

\citet[p.~40]{sussman1975interpreter} say, ``we discovered that the "actors"
and the lambda expressions were identical in implementation.

extended ``awkward squad'' \cite{peytonjones2001tackling}






\section{TODO: Conclusion}
\label{sec:conclusion}


\lk{I think that the informality is fine, but the next two or three paragraphs are going to be hard for a reader to appreciate until the reader has actually read the paper and seen what we did.  Therefore, I suggest saving this kind of stuff for \Cref{sec:what-hath-we-wrought} or \Cref{sec:conclusion}.  For the introduction, let's aim for a short, ``just the facts'' sort of thing.  It's OK for the intro to be only one page, or even less!}

\lk{While informality is fine and good, we shouldn't make unsupported claims like ``it's almost there'' in the name of being informal.  We can keep the informality but support the claims!  E.g., in what way is it ``almost there''?  What would being fully ``there'' look like?  What stops it from being ``there''?}
\plr{move last two to conclusion or wrought?}

The actor framework we present is not an advancement:
%
It is easy to use, but easy to use wrongly.
%
It has acceptable throughput, but is slower than accepted tools (\verb|Chan|
and \verb|STM|).
%
It requires no appreciable dependencies, no explicitly mutable data structures
or references, no effort to achieve synchronization, and very little code, but
is also exceedingly difficult to debug (as are \plr{jmc: cannot parse "as are"} problems with asynchronous
exceptions).

\emph{Should} it have been possible to implement the actor framework we present here?
%
It's \emph{almost} practical.
%
Given that it's \emph{almost} there, and yet emphatically should not be used
in practice, we question why it's possible in the first place, much less so
easy.
%
Like many people, we choose Haskell because it is a tool that typically
prevents ``whole classes of errors,'' and also because it is a joy to use (most
of the time).
%
But in this paper we achieve dynamically typed ``spooky action at a distance''
with frighteningly little effort.
%
Should the user-accessible interface to the asynchronous exception system be
constrained?

\paragraph{An extended ``awkward squad''}

A user of the RTS may soon enjoy software transactional memory, asynchronous
exceptions, delimited continuations, extensible algebraic effects, and more,
all together in the same tub.
%
The water is warm, jump in!
\plr{The list above is better the more powerful the features are; STM doesn't
fit; what are some other powerful GHC features?}
%
Which of these features can be implemented in terms of the other?
%
And should their full power be exposed so that we can do so?
%
Let's explore one example -- actors on exceptions -- and have a think about it.

\lk{So, I know I'm fickle and said something else before, but I now think that these next two subsections should be moved out of the introduction, because the level of detail here seems like too much for an introduction.  They could become a section 2 called "Brief Background" or something, or there could be a background subsection at the start of the existing section 2.}
\plr{brief backround}











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

\subsection{Permute}

In \Cref{sec:ring-impl} we provided the implementation of a ring
leader-election in our actor framework.
%
The implementation used \verb|permute| to randomize the list of
\verb|ThreadId|.
%
Its implementation is as follows:
%
Repeatedly pop a random element from the input and add it to the output.
%

\begin{code}
permute :: RandomGen g => [a] -> g -> ([a], g)
permute pool0 gen0
    = snd
    . foldr pick (pool0, ([], gen0))
    $ replicate (length pool0) ()
  where
    pick () (pool, (output, g)) =
        let (index, g') = randomR (0, length pool - 1) g
            (x, pool') = pop pool index
        in (pool', (x:output, g'))
    pop (x:xs) 0 = (x, xs)
    pop (x:xs) n = (x:) <$> pop xs (n - 1)
    pop [] _ = error "pop empty list"
\end{code}





\subsection{Performance evaluation detail}
\label{sec:perf-eval-detail}


For the (extended) ring leader-election solution we have shown, the time to
termination is:
%
The time necessary for the winner's self-nomination to pass around the ring
once, plus the time for the winner-declaration to pass around the ring once,
at minimum.
%
Termination is detected when a node receives a winner declaration with its own
identity.


We will benchmark time to termination using the \verb|criterion| package.
%
For this, we will need an \verb|IO| action which executes the algorithm, cleans
up its resources, and then returns.
%
We will make an initialiazation actor to launch the algorithm, clean up, and
kill itself when termination is detected.
%
We employ \verb|withAsync| and \verb|waitAsync| to detect when the
initialization actor has died and return from the benchmark.

\paragraph{Difficult}
\plr{
\begin{itemize}
    \item Difficult to communicate between actor-world and functional-world
    because actors do not return values. (Actors do not return!)
    %
    \item Difficult to use message passing for this purpose: A non-actor thread
    (a thread not running an actor mainloop) cannot easily fork a child actor
    and receive a message from that child. Race conditions, I think.
    %
    \item Instead we use a mutable cell for communication.
\end{itemize}
}


First we define a benchmarking-node, which extends \verb|exnode|
(\Cref{sec:ring2-intent-fun}) with additional behavior.
%
When a benchmarking-node detects that it is confirmed as winner, it sends the
winner-declaration message to a designated subscriber.
%
\begin{code}
benchNode :: Mv.MVar ThreadId -> Intent Exnode SomeException
benchNode done state e@Envelope{message} = do
    state' <- exnode state e
    self <- myThreadId
    case fromException message of
        Just (Winner w) | w == self -> Mv.putMVar done w
        _ -> return ()
    return state'
\end{code}






\noindent
We define \verb|benchRing| to be the function which \verb|criterion| will
measure. It will run a single \verb|benchLaunch| actor, send a \verb|Start|
message to it, wait for apoptosis, and output any result.
%
\begin{code}
benchRing :: Int -> IO ()
benchRing n = do

    done <- Mv.newEmptyMVar
    ring <- ringElection n $ do
        great <- myThreadId
        run (benchNode done) (Uninitialized, great)

    w <- Mv.takeMVar done
    mapM_ killThread ring
    assert (w == maximum ring) $
        return ()
\end{code}




\noindent
Finally, we define a benchmark-main which runs \verb|benchRing| for each of
several ring sizes.
%
As a control, it also runs a function that starts up some number of threads
that immediately terminate.
%
Finally, it also compares to an implementation of ring leader-election using
\verb|Control.Concurrent.Chan| (one of the more normal ways to do things in
Haskell).
%
These alternates are shown in \Cref{sec:alt-impls}.
%
\begin{code}
benchMain :: IO ()
benchMain = Cr.defaultMain
    [ Cr.bgroup "fork & kill" $ fmap control counts
    , Cr.bgroup "channel ring" $ fmap channel counts
    , Cr.bgroup "actor ring" $ fmap actor counts
    ]
  where
    counts = [2^n | n <- [2,4..14::Int]]
    control n =
        Cr.bench ("n=" ++ show n) . Cr.nfIO $ benchControl n
    actor n =
        Cr.bench ("n=" ++ show n) . Cr.nfIO $ benchRing n
    channel n =
        Cr.bench ("n=" ++ show n) . Cr.nfIO $ channelRing n
\end{code}

%
When producing benchmarks for this paper, we ran an extra step (available in
our repo) to replace all printlines with \verb|pure ()|.
%
This is necessary because the printlines introduce latency and dramatically
inflate the algorithm runtime.






\subsection{Alternate implementations}
\label{sec:alt-impls}

This section has the source code for alternate implementations compared to the
actor implementation by the benchmark.

\subsubsection{Control}
The experimental control only forks threads and then kills them.
%
It's useful for establishing whether or not, for example, laziness has caused
our non-control implementations to perform no work.
%
The other implementations should take longer than the control because they are
doing more work.
%
\begin{code}
benchControl :: Int -> IO ()
benchControl n = do
    nodes <- sequence . replicate n $ forkIO (return ())
    mapM_ killThread nodes
\end{code}


\subsubsection{Channel-based}
In the channel-based ring leader-election, each node has references to a
send-channel and a receive-channel.
%
We reuse the message types from before via an \verb|Either|.
%
\begin{code}
type ChMsg = Either Msg Winner
type Ch = Ch.Chan ChMsg
\end{code}


It is unnecessary to split the channel-based implementation into a simple node
and an extended node, but we do so it is easier to reference against the
actor-based implementation.
%
This structural similarity hopefully has the added benefit of focusing
benchmark differences onto the communication mechanisms instead of anecdotal
differences.
%
\begin{itemize}
    \item
    In \Cref{fig:chanNode} we implement the main-loop, \texttt{chanNode}.
    %
    The only state maintained is the greatest nominee seen.
    %
    It leaves off with definitions of send-functions in its where-clause.

    \item
    \Cref{fig:chanNodePart} shows \texttt{nodePart}, within the where-clause of
    \texttt{chanNode}, which reimplements the behavior of a ring node from
    \Cref{sec:ring-intent-fun}.
    %
    This part has no state because its successor-channel is given on
    construction; it requires no \verb|Init| message for the same reason.

    \item
    In \Cref{fig:chanNodePrimePart}, still within the where-clause of
    \texttt{chanNode}, \texttt{exnodePart} reimplements the behavior of the
    winner-round node (\Cref{sec:ring2-intent-fun}) and the benchmark-node
    (\Cref{sec:perf-eval-detail}).
    %
    We signal termination by placing the confirmed winner's \texttt{ThreadId}
    into the ``done'' \texttt{MVar}.

    \item
    Finally \Cref{fig:channelRing} initializes the algorithm with a function
    similar to \texttt{ringElection}, but using channels instead of passing in
    \texttt{ThreadId}s.
\end{itemize}

\begin{figure}
\raggedright
\begin{code}
chanNode ::
    Mv.MVar ThreadId -> (Ch, Ch) -> ThreadId -> IO ()
chanNode done chans st = do
    chanNode done chans =<< exnodePart st =<< recv
  where
    recv = Ch.readChan (fst chans)
    sendMsg = Ch.writeChan (snd chans) . Left
    sendWinner = Ch.writeChan (snd chans) . Right
\end{code}
\caption{Main-loop for channel-based implementation of ring leader-election.
Includes \Cref{fig:chanNodePart,fig:chanNodePrimePart} it its where-clause.}
\label{fig:chanNode}
\end{figure}

\begin{figure}
\raggedright
\begin{code}
    nodePart :: Msg -> IO ()
    nodePart Start = do
        self <- myThreadId
        putStrLn (show self ++ ": nominate self")
        sendMsg $ Nominate self
    nodePart Nominate{nominee} = do
        self <- myThreadId
        case () of
         _  | self == nominee -> putStrLn (show self ++ ": I win")
            | self <  nominee
                -> putStrLn (show self ++ ": nominate "
                    ++ show nominee)
                >> sendMsg (Nominate nominee)
            | otherwise       -> putStrLn "Ignored nominee"
\end{code}
\caption{Channel-based reimplementation of \verb|node| defined in the
where-clause of \Cref{fig:chanNode}.}
\label{fig:chanNodePart}
\end{figure}
\ignore{
\begin{code}
    nodePart _ = error "nodePart: unhandled"
\end{code}
}

\begin{figure}
\raggedright
\begin{code}
    exnodePart :: ThreadId -> Either Msg Winner -> IO ThreadId
    exnodePart great (Left m) = do
        nodePart m
        self <- myThreadId
        case m of
            Nominate{nominee} ->
                if self == nominee
                then sendWinner (Winner self)
                    >> return great
                else return $ max nominee great
            _ -> return great
    exnodePart great (Right m) = do
        self <- myThreadId
        case m of
            Winner w
                | w == self
                    -> putStrLn (show self ++ ": Confirmed")
                    >> Mv.putMVar done self
                | w == great -> sendWinner (Winner w)
                | otherwise -> putStrLn "Unexpected winner"
        return great
\end{code}
\caption{Channel-based reimplementation of \verb|exnode| defined in the
where-clause of \Cref{fig:chanNode}}
\label{fig:chanNodePrimePart}
\end{figure}

\begin{figure}
\raggedright
\begin{code}
channelRing :: Int -> IO ()
channelRing n = do
    -- (1) Define a channel-node main function.
    done <- Mv.newEmptyMVar
    let mkNode chans = do
            great <- myThreadId
            chanNode done chans great
    -- (2) Make an in-order ring linked by chans.
    chans <- sequence . replicate n $ Ch.newChan
    let nodeActs = map mkNode
            (zip chans $ tail chans ++ [head chans])
    -- (3) Permute the ring and fork to assign random IDs.
    ringActs <- getStdRandom $ permute nodeActs
    ring <- mapM forkIO ringActs
    -- (4) Start the election.
    mapM_ (\c -> Ch.writeChan c . Left $ Start) chans
    -- (5) Wait for termination and clean up.
    w <- Mv.takeMVar done
    mapM_ killThread ring
    assert (w == maximum ring) $
        return ()
\end{code}
\caption{
    Initialization routine for channel-based reimplementation of ring
    leader-election.
    %
    First define a function to run a channel-node on the ``done'' \texttt{MVar}
    and two provided channels.
    %
    Next construct channels and a ring of un-evaluated nodes \emph{in order}.
    %
    Finally permute the nodes and fork them out of order.
    %
    At this point the nodes are assigned random thread identifiers.
}
\label{fig:channelRing}
\end{figure}




\ignore{
\begin{code}
beginVerb :: IO ()
beginVerb = do
    hSetBuffering stdout LineBuffering
    putStrLn "\\begin{verbatim}"

endVerb :: IO ()
endVerb = putStrLn "\\end{verbatim}"
\end{code}
}

\subsection{Election trace}
\label{sec:main1-trace}

In \Cref{sec:main1-init} we defined \verb|main1| to run a ring
leader-election.
%
Here's an example trace.

\footnotesize
\perform{beginVerb >> putStrLn "> main1 8" >> main1 8 >> endVerb }
\normalsize

\subsection{Dynamic types trace}
\label{sec:main2-trace}

In \Cref{sec:main2-init} we defined \verb|main2| to run a ring
leader-election with a winner declaration round.
%
Here's an example trace.

\footnotesize
\perform{beginVerb >> putStrLn "> main2 8" >> main2 8 >> endVerb }
\normalsize

\subsection{Channel node election trace}
\label{sec:channelRing-trace}

In \Cref{sec:alt-impls} we defined \verb|channelRing| to run a ring
leader-election with a winner declaration round using channels for
communication.
%
Here's an example trace.

\footnotesize
\perform{beginVerb >> putStrLn "> channelRing 8" >> channelRing 8 >> endVerb }
\normalsize

% It's necessary to have a main function, but I'm excluding it from appearing
% in the document.
\ignore{
\begin{code}
main :: IO ()
main = do
    args <- getArgs
    case args of
        [n] -> do
            let count = read n
            putStrLn ("Count: " ++ n)
            beginVerb

            putStrLn "main1"
            main1 count
            putStrLn ""

            putStrLn "main2"
            main2 count
            putStrLn ""

            putStrLn "benchRing"
            benchRing count
            putStrLn ""

            putStrLn "channelRing"
            channelRing count
            putStrLn ""

            endVerb
        _ -> benchMain
\end{code}
}











\end{document}
\endinput
