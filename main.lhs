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

\newcommand{\newcommenter}[3]{%
  \newcommand{#1}[1]{%
    \textcolor{#2}{\small\textsf{[{#3}: {##1}]}}%
  }%
}

\newcommenter{\plr}{magenta}{PLR}
\newcommenter{\lk}{blue}{LK}

\renewcommand{\labelenumi}{(\arabic{enumi})}

%%%% lhs2Tex (*.lhs) document
\let\Bbbk\undefined
%include polycode.fmt
\long\def\ignore#1{}
%%%% deindent comments
\renewcommand\onelinecommentchars{-{}- }
\visiblecomments
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
    The Glasgow Haskell Compiler is well known for its fully featured runtime
    system (RTS) which includes green threads, asynchronous exceptions, and
    recently, delimited continuations.
    \plr{Leaving "green threads" in because it's part of being a fully-featured
    runtime. A language with a less fully-featured runtime uses OS threads.}
    %
    The combination of these features, and forthcoming support for algebraic
    effects, is powerful.
    %
    A programmer may complete the same task in many different ways, some more
    advisable than others.
    \plr{I've tweaked the last sentence to hopefully not seem like a
    non-sequitur and better lead into the following paragraph.}

    We present a user-accessible actor framework hidden in plain sight within
    GHC's RTS and demonstrate it on a classic example.
    %
    We then extend both the framework and example to the realm of dynamic
    types.
    %
    Finally, we raise questions about the ability to express one language
    feature in terms of another, and make recommendations about how GHC can
    guide best practice by constraining the use of some features.
    \plr{Tried making this last more specific.}
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



\section{TODO: Introduction}

\noindent using the tools throwTo and catch

\noindent This paper is written as a literate Haskell program.\footnote{
    We use \verb|GHC 9.0.2| and \verb|base-4.15.1.0|.
    %
    The actor framework imports the modules \verb|Control.Exception| and
    \verb|Control.Concurrent|.
    %
    We enable the extensions \verb|NamedFieldPuns| and
    \verb|DuplicateRecordFields| for the convenience of presenting the
    framework.
    %
    The example additionally imports the module \verb|System.Random| and
    enables the extension \verb|ViewPatterns|.
}

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
import System.IO (hSetBuffering, hPutStrLn, stdout, BufferMode(..))

-- Perf eval appendix
import Control.Exception (assert)
import System.Environment (getArgs)
import qualified Control.Concurrent.Async as A
import qualified Control.Concurrent.Chan as Ch
import qualified Control.Concurrent.MVar as Mv
import qualified Criterion.Main as Cr
\end{code}
} % end ignore

\subsection{Exceptions in GHC}
\plr{Should I re-title this to indicate that the point is that asynchronous
exceptions are weird?}

The Glasgow Haskell Compiler (GHC) supports three varieties of exceptions, all
of which may be caught in the IO monad and otherwise cause the program to
terminate.
%
\plr{I introduce all three kinds of exceptions to draw the contrast that
asynchronous exceptions are weird. The topic sentences of the next three
paragraphs indicate this with words "uniquely", "peculiar", and
"surprisingly".}
\emph{Imprecise exceptions} arise in pure code from expressions such as
\verb|(div 1 0)| which cannot be reduced further.
%
\emph{Synchronous exceptions} are thrown when side effects in the IO monad
cannot proceed, such as \verb|(readFile "\0")|.
%
\emph{Asynchronous exceptions} are thrown by threads \emph{distinct from the
current one}, or by the RTS itself, to communicate conditions requiring the
thread to terminate: thread cancellation, user interrupts, or memory limits.
%
We focus exclusively on asynchronous exceptions for the rest of the paper.

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
User-defined datatypes may even be thrown as asynchronous exceptions by
declaring an instance of \verb|Exception| \cite{marlow2006extensible}.
%
Given the two lines of declarations below, it is possible to greet in
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
virtual processor that is never `busy' [in the sense that it cannot be sent a
message].''
%
We flesh out this definition by saying, an actor is a green thread\footnote{
    A \emph{green thread} is a lightweight thread of control implemented in a
    programming language runtime and dynamically mapped to a CPU by a scheduler
    in the runtime.
    %
    A language using OS threads would likely be too heavyweight to support
    actor programming, due to the large numbers of actors used.
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

More specifically, \plr{More concretely,} we think of an actor framework as
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





\section{Actor framework implementation}

In our actor framework implementation, an actor is a Haskell thread running a
provided main-loop function.
%
The main-loop function mediates message receipt and makes calls to a
user-defined intent function.
%
Here we describe the minimal abstractions around such threads which realize the
actor model.
%
\plr{Would it add to the paper to name the framework "Hakkell"? Or would it
just be distracting. I was thinking maybe the title of this section could be
"Hakkell framework implementation" or maybe there could be a footnote
somewhere.}

\subsection{Sending (throwing) messages}
\label{sec:sending-throwing}

\begin{samepage}
\noindent
To send a message we will throw an exception to the recipient's thread
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
\end{samepage}


\begin{samepage}
\noindent
\Cref{fig:sendStatic} defines a send function which reads the current thread
identifier, constructs a self-addressed envelope, and throws it to the
specified recipient.
%
For the purpose of explication in this paper, it also prints a trace.
%
\begin{figure}[h]
\begin{code}
sendStatic :: Exception a => ThreadId -> a -> IO ()
sendStatic recipient message = do
    sender <- myThreadId
    putStrLn (show sender ++ " send " ++ show message
                          ++ " to " ++ show recipient)
    throwTo recipient Envelope{sender, message}
\end{code}
\caption{Send a message in a self-addressed envelope.}
\label{fig:sendStatic}
\end{figure}
\end{samepage}




\subsection{Receiving (catching) messages}
\label{subsec:receiving-catching}


\begin{samepage}
\noindent
Every actor thread runs a provided main-loop function to manage message receipt
and processing.
%
The main-loop function installs an exception handler to accumulate messages in an inbox
and calls a user-defined intent function on each.
%
The user-defined intent function encodes actor behavior as a state transition
that takes a self-addressed envelope as its second argument.
%
\begin{code}
type Intent st msg = st -> Envelope msg -> IO st
\end{code}
\end{samepage}


\begin{samepage}
\noindent
The main-loop, takes an \verb|Intent| function and its initial actor state and
does not return.
%
It masks asynchronous exceptions so they will only be raised at well-defined
points and runs its loop under that mask.
%
\begin{figure}[h]
\begin{code}
runStatic :: Exception a => Intent s a -> s -> IO ()
runStatic intent initialState = mask_ $ loop (initialState, [])
  where
    loop (state, inbox) =
        catch
            (case inbox of
                [] -> threadDelay 60000000 >> return (state, inbox)
                x:xs -> (,{-"\!"-}) <$> intent state x <*> return xs)
            (\e@Envelope{} -> return (state, inbox ++ [e]))
        >>= loop
\end{code}
\caption{Actor-thread message-receipt main-loop.}
\label{fig:runStatic}
\end{figure}
\end{samepage}


\noindent
The loop has two pieces of state: that of the intent function, and an inbox of
messages to be processed.
%
The loop body is divided roughly into three cases by an exception
handler and a case-split on the inbox list.
%
\begin{enumerate}
    \item If the inbox is empty, sleep for 60 seconds and then recurse on the
    unchanged actor state and the empty inbox.
    %
    We will see length of the sleep is arbitrary.
    
    \item If the inbox has a message, call the intent function and recurse on
    the updated actor state and remainder of the inbox.

    \item If during cases (1) or (2) an \verb|Envelope| exception is received,
    recurse on the unchanged actor state and an inbox with the new envelope
    appended to the end.
\end{enumerate}


\noindent
In the normal course of things, an actor will start with an empty inbox and go
to sleep.
%
If a message is received during sleep, the actor will wake (because
\verb|threadDelay| is defined to be \emph{interruptible}), add the message to
its inbox, and recurse.
%
On the next loop iteration, the actor will process that message and once again
recurse on an empty inbox.
%
Exceptions are masked outside of interruptible actions so that the bookkeeping
of recursing with updated state through the loop is not disrupted.


\paragraph{Unsafety}

Before moving forward, let us acknowledge that this is \emph{not safe}.
%
An exception may arrive while executing the intent function.
%
Despite the exception mask which we have intentionally left in place, if the
intent function executes an interruptible action, then it will be preempted.
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
The best recommendation we can make is for the idempotence of intent functions.




\subsection{Dynamic types}
\label{sec:dynamic-types}

The actor main-loop in \Cref{fig:runStatic} constrains an actor thread
to handle messages of a single type.
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


\begin{samepage}
\noindent
Instead of sending an \verb|Envelope| of some application-specific message
type, we convert messages to the ``any type'' in Haskell's exception
hierarchy, \verb|SomeException|.
%
All inflight messages will have the type \verb|Envelope SomeException|.
%
We define a new send function which converts messages before sending.
%
\begin{figure}[h]
\begin{code}
send :: Exception a => ThreadId -> a -> IO ()
send recipient = sendStatic recipient . toException
\end{code}
\caption{Upcast before sending.}
\label{fig:send}
\end{figure}
\end{samepage}


\subsubsection{Receiving dynamic messages}
\label{sec:dynamic-recv-loop}


\begin{samepage}
\noindent
On the receiving side, messages must now be downcast to the \verb|Intent|
function's message type.
%
This is an opportunity to treat messages of the wrong type specially.
%
We define a new main-loop which lifts any \verb|Intent| function to one that can
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
%
\begin{figure}[h]
\begin{code}
runDyn :: Exception a => Intent s a -> s -> IO ()
runDyn intentStatic = runStatic intent
  where
    intent state e@Envelope{sender, message} =
        case fromException message of
            Just m -> intentStatic state e{message=m}
            Nothing
                -> throwTo sender (TypeError "...")
                >> return state

run :: Exception a => Intent s a -> s -> IO ()
run intent state = do
    ms <- getMaskingState
    case ms of
        MaskedInterruptible -> runDyn intent state
        _ -> error "always apply a mask before forking an actor thread"
\end{code}
\caption{Downcast before processing.}
\label{fig:run}
\end{figure}
\end{samepage}


The changes shown so far haven't directly empowered actor intent functions to
deal with messages of different types, only lifted our infrastructure to remove
application-specific type parameters from envelopes.
%
Actors intending to receive messages of different types will do so by
performing the downcast from \verb|SomeException| themselves.
%
Such actors will use an intent function handling messages of type
\verb|SomeException|.
%, and will work equally well with \verb|runStatic| or \verb|run|.
%
\Cref{sec:dyn-ring} shows an example of an actor that receives
messages of different types, by extending an actor that doesn't.




\section{Example: Ring leader-election}
\label{sec:ring-impl}

The problem of \emph{ring leader-election} is to designate one ``leader'' node
among a network of communicating nodes organized in a ring topology
\cite{lelann1977distributed}.
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
despite being unnecessary in the context threads in a process.

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

\begin{figure}[h]
\lk{To help visualize the algorithm, I think it would be helpful to have a
figure with an illustration of the ring, some sent messages, and the algorithm
in progress, kind of like in the ``message chains'' paper}
\plr{ring with seven nodes; two have message arrows bouncing around the outside
of the ring; one message arrow terminates in an X at a greater node, the other
message arrow originates at the greatest node and shows no sign of stopping}
\caption{Ring election visual TODO}
\label{fig:ring-election-visual}
\end{figure}

\subsection{Implementing a leader-election}

\subsubsection{State and messages}


\begin{samepage}
\noindent
Each node begins uninitialized, and is later made a member of the ring
when it learns the identity of its successor.
%
Therefore our node state type will have two constructors.
%
\begin{code}
data Node = Uninitialized | Member {next::ThreadId}
\end{code}
\end{samepage}


\begin{samepage}
\noindent
The main thread will create multiple node actors and then initialize the ring by
informing each node of its successor.
%
Next, the main thread will rapidly instruct every node actor to start running
the leader election algorithm.
%
Finally, the nodes will carry out the algorithm by forwarding or ignoring
nominations.
%
Accordingly, our message type has three constructors.
%
\begin{code}
data Msg
    = Init{next::ThreadId}
    | Start
    | Nominate{nominee::ThreadId}
    deriving Show
instance Exception Msg
\end{code}
\end{samepage}


\noindent
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


\subsubsection{Actor behavior}
\label{sec:ring-intent-fun}


\begin{samepage}
\noindent
The intent function for a node actor will have state of type \verb|Node|
and pass messages of type \verb|Msg|. We describe each case separately.
%
\begin{code}
node :: Intent Node Msg
\end{code}
\end{samepage}


\begin{samepage}
\noindent
When an uninitialized node receives an \verb|Init| message, it becomes a member
of the ring and remembers its successor.
%
\begin{code}
node Uninitialized
  Envelope{message=Init{next}} = do
    return Member{next}
\end{code}
\end{samepage}


\begin{samepage}
\noindent
When a member of the ring receives a \verb|Start| message, it sends a message
to its successor in the ring to nominate itself.
%
\begin{code}
node state@Member{next}
  Envelope{message=Start} = do
    self <- myThreadId
    send next $ Nominate self
    return state
\end{code}
\end{samepage}


\begin{samepage}
\noindent
%% \Cref{fig:nodeNominate} shows the case that characterizes this
%% algorithm.
%% %
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
  Envelope{message=Nominate{nominee}} = do
    self <- myThreadId
    case () of
     _  |  self == nominee -> putStrLn (show self ++ ": I win")
        |  self <  nominee -> send next (Nominate nominee)
        |  otherwise       -> putStrLn "Ignored nominee"
    return state
\end{code}
%% \begin{figure}[h]
%% \caption{Node behavior upon receiving a nomination.}
%% \label{fig:nodeNominate}
%% \end{figure}
\end{samepage}

\ignore{
\begin{code}
node _ _ = error "node: unhandled"
\end{code}
}


\subsubsection{Initialization}
\label{sec:main1-init}


\begin{samepage}
\noindent
The main thread performs several steps to initialize the algorithm:
%
\begin{enumerate}
    \item Create some number of actor threads.

    \item Randomize the order of the \verb|ThreadId|s in a list.

    \item Inform each thread of the \verb|ThreadId| that follows it in the
    random order (its successor).

    \item Tell every thread to start the algorithm.
\end{enumerate}
%
These tasks are implemented in \verb|ringElection|.\footnote{
    The implementation shown doesn't handle degenerate rings of size 0 or 1,
    but we consider that out of scope of the demonstration.
}
%
\begin{code}
ringElection :: Int -> IO () -> IO [ThreadId]
ringElection n actor = do
    nodes <- sequence . replicate n $ forkIO actor {-"\quad\quad\hfill (1)"-}
    ring <- getStdRandom $ permute nodes {-"\hfill (2)"-}
    mapM_
        (\(self, next) -> send self Init{next}) {-"\hfill (3)"-}
        (zip ring $ tail ring ++ [head ring])
    mapM_ (\t -> send t Start) ring {-"\hfill (4)"-}
    return ring
\end{code}
\end{samepage}


\begin{samepage}
\noindent
Finally, in \verb|main1| we construct an \verb|IO| action representing the
behavior of a node actor by passing the node intent function and the
uninitialized state constructor to the message receipt main-loop.
%
To initiate the election algorithm we pass the \verb|IO| action to
\verb|ringElection| which will fork it to several threads.
%
A trace of \verb|main1| is included in \Cref{sec:main1-trace}.
%
\begin{code}
main1 :: Int -> IO ()
main1 count = do
    ring <- ringElection count $ run node Uninitialized
    return ()
\end{code}
\ignore{
\begin{code}
    threadDelay 1000000
    mapM_ killThread ring
\end{code}
}
\end{samepage}


\subsection{Adding a victory round}
\label{sec:dyn-ring}


The solution we have shown solves the ring leader-election problem
insofar as a single node concludes that it has won.
%
However, it is also desirable for the other nodes to learn the outcome of the
election.
%
Since there is no message constructor to inform nodes of the election outcome,
we will define a new message type whose constructor indicates a declaration of
the winner.


We will extend the existing solution by wrapping it with an intent function
that processes messages of either the old or the new message types, with
distinct behavior for each, leveraging the dynamic types support from
\Cref{sec:dynamic-types}.
%
The new behaviors are:
%
\begin{itemize}
    \item Each node will keep track of the greatest nominee it has seen.

    \item When the winner self-identifies, they will start an extra round
    declaring themselves winner.

    \item Upon receiving a winner declaration, a node compares the greatest
    nominee it has seen with the winner. If they are the same, then the node
    forwards the declaration to its successor.
\end{itemize}


When a node receives a declaration of the winner that they agree with, they
have ``learned'' that node is indeed the winner.
%
When the winner receives their own declaration, everyone has learned they are
the winner and the algorithm terminates.
%
If the winner declaration doesn't make it all the way around the ring, then the
algorithm terminates without confirming a winner.


\subsubsection{State and messages}


\begin{samepage}
\noindent
Each node now pairs the old node state with a \verb|ThreadId| which is the
greatest nominee it has seen.
%
\begin{code}
type Node' = (Node, ThreadId)
\end{code}
\end{samepage}


\begin{samepage}
\noindent
The new message type has only one constructor, and it is used to declare some
node the winner.
%
\begin{code}
data Winner = Winner ThreadId
    deriving Show
instance Exception Winner
\end{code}
\end{samepage}


\subsubsection{Actor behavior}
\label{sec:ring2-intent-fun}


\begin{samepage}
\noindent
The intent function for the new actor will use \verb|Node'| as described, and
we declare its message type to be \verb|SomeException|.
%
This will allow it to receive either \verb|Msg| or \verb|Winner| values and
branch on which is received.
%
\begin{code}
node' :: Intent Node' SomeException
\end{code}
\end{samepage}


\noindent
Recall the implementation of the actor thread main-loop function, \verb|run|,
from \Cref{sec:dynamic-recv-loop}.
%
When we apply \verb|node'| to the main-loop, \verb|run|, its call to
\verb|fromException| will be inferred to return \verb|Maybe SomeException|
which succeeds unconditionally.
%
The \verb|node'| intent function must then perform its own downcasts, and so we
enable \verb|ViewPatterns| ease the presentation.
%
There are two main cases, corresponding to the two message types the actor will
handle.


\begin{samepage}
The first case applies when a node downcasts the envelope contents to
\verb|Msg|.
%
This case tracks the last-seen nominee and triggers the victory round.
%
We annotate it as follows:
%
\begin{enumerate}
    \item Delegate to the held node by putting the revealed \verb|Msg| back
    into its envelope and passing it through the intent function, \verb|node|,
    from \Cref{sec:ring-intent-fun}.
    %
    %%Return that resulting node state in all of the cases below.
    %
    \item If the message is a nomination of the current node, start the winner
    round because the election is over.
    %
    \item Otherwise the election is ongoing so keep track of the greatest
    nominee seen.
    %
    \item For any other \verb|Msg| constructors, only return the updated node
    state.
\end{enumerate}
%
\begin{code}
node' (n, great)
  e@Envelope{message=fromException -> Just m} = do
    self <- myThreadId
    n'@Member{next} <- node n e{message=m} {-"\quad\quad\hfill (1)"-}
    case m of
        Nominate{nominee} ->
            if self == nominee
            then send next (Winner self) {-"\quad\quad\hfill (2)"-}
                >> return (n', great)
            else return (n', max nominee great) {-"\quad\quad\hfill (3)"-}
        _ -> return (n', great) {-"\quad\quad\hfill (4)"-}
\end{code}
\end{samepage}


\begin{samepage}
The second case applies when a node downcasts the envelope contents to a winner
declaration.
%
%% The node compares the declared winner to itself and to the greatest nominee
%% it has seen.
%
If the current node is declared winner, the algorithm terminates successfully.
%
If the greatest nominee the current node has seen is declared winner, the node
forwards the declaration to its successor.
%
Otherwise the algorithm terminates unsuccessfully.
%
State is unchanged in each of these branches.
%
\begin{code}
node' state@(Member{next}, great)
  Envelope{message=fromException -> Just m} = do
    self <- myThreadId
    case m of
        Winner w
            | w == self -> putStrLn (show self ++ ": Confirmed")
            | w == great -> send next (Winner w)
            | otherwise -> putStrLn "Unexpected winner"
    return state
\end{code}
\end{samepage}

\ignore{
\begin{code}
node' _ _ = error "node': unhandled"
\end{code}
}


\subsubsection{Initialization}
\label{sec:main2-init}


\begin{samepage}
\noindent
The extended ring leader-election can reuse the same initialization scaffolding
as before; we only define a \verb|main2| function.
%
As part of the \verb|IO| action passed to \verb|ringElection|, each thread
initializes the greatest nominee seen to itself.
%
A trace of \verb|main2| is in \Cref{sec:main2-trace}.
%
\begin{code}
main2 :: Int -> IO ()
main2 count = do
    ring <- ringElection count $ do
        great <- myThreadId
        run node' (Uninitialized, great)
    return ()
\end{code}
\ignore{
\begin{code}
    threadDelay 1000000
    mapM_ killThread ring
\end{code}
}
\end{samepage}




\section{What hath we wrought?}

\Cref{fig:sendStatic,fig:runStatic} show that we have, in only a few lines of
code, discovered an actor framework within the RTS which makes no explicit use
of channels, references, or locks and imports just a few names from default
modules.
%
The support for dynamic types, shown in \Cref{fig:send,fig:run} as separate
definitions, can be folded into \Cref{fig:sendStatic,fig:runStatic} for only a
few additional lines.
%
While the likelihood of double sends might temper enthusiasm for this
discovery, despite minor brokenness it is notable that this is possible and
shocking that it is so easy.

\subsection{Almost a COPL}

Which requirements to be a COPL does this framework display?
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
One strategy hide to an actor's name and restore the lost security isolation is
to wrap calls to the send function with \verb|forkIO|.
%
Another strategy would be to define two constructors for envelope, and elide
the ``sender'' field from one.

From this discussion in is clear that this actor framework is almost a COPL.
%
With conscientious attention to the termination and idempotence of intent
functions, the framework might be considered practical.





\subsection{Pithy statement about performance}

\plr{TODO} "Almost as good as channels"





\subsection{Feature subsumption}

\lk{Let's figure out what the point is that we want to make here.}

Can we implement an actor framework with Haskell's threads and asynchronous
exceptions?
%
This is the question that led us to writing this paper.
%
\Cref{fig:runStatic} shows that we very nearly can, and this fact hints
that perhaps asynchronous exceptions are more general than actors.
\lk{It's not just \Cref{fig:runStatic} in isolation.  How about ``Our implementation suggests that we very nearly can.''}
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

%
We hope the existence of this accidental actor framework 
, we expect to see it deployed for industrial applications throughout
the Haskell-sphere.

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
\begin{samepage}
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
\end{samepage}




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


\begin{samepage}
First we define a benchmarking-node, which extends \verb|node'|
(\Cref{sec:ring2-intent-fun}) with additional behavior.
%
When a benchmarking-node detects that it is confirmed as winner, it sends the
winner-declaration message to a designated subscriber.
%
\begin{code}
benchNode :: ThreadId -> Intent Node' SomeException
benchNode subscriber state
  e@Envelope{message=
  fromException->Just (Winner w)} = do
    self <- myThreadId
    if w == self
        then send subscriber (Winner w)
        else return ()
    node' state e
benchNode _ state e = node' state e
\end{code}
\end{samepage}


\begin{samepage}
\noindent
Next we define a benchmark-launcher actor which starts the algorithm with
benchmarking-nodes and cleans up when it receives a winner declaration.
%
\begin{code}
benchLaunch :: Int -> Intent (Maybe [ThreadId]) SomeException

benchLaunch count Nothing
  Envelope{message=fromException->Just Start} = do
    launcher <- myThreadId
    ring <- ringElection count $ do
        great <- myThreadId
        run (benchNode launcher) (Uninitialized, great)
    return $ Just ring

benchLaunch _ (Just ring)
  Envelope{message=
  fromException->Just (Winner w)} = do
    mapM_ killThread ring
    assert (w == maximum ring) $
        return Nothing
\end{code}
\end{samepage}

\ignore{
\begin{code}
benchLaunch _ _ _ = error "benchLaunch: unhandled"
\end{code}
}


\begin{samepage}
\noindent
We define \verb|benchRing| to be the function which \verb|criterion| will
measure. It will run a single \verb|benchLaunch| actor, send a \verb|Start|
message to it, wait for apoptosis, and output any result.
%
\begin{code}
benchRing :: Int -> IO ()
benchRing n = do
  mask_ $ do
    A.withAsync
        (run (benchLaunch n) Nothing)
        (\a -> do
            send (A.asyncThreadId a) Start
            A.wait a)
            -- XXX use of waitCatch silences errors in noprint.lhs
    hPutStrLn stdout "after"
\end{code}
\end{samepage}


\begin{samepage}
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
    , Cr.bgroup "actor ring" $ fmap actor counts
    , Cr.bgroup "channel ring" $ fmap channel counts
    ]
  where
    counts = [2^n | n <- [2..11::Int]]
    control n =
        Cr.bench ("n=" ++ show n) . Cr.nfIO $ benchControl n
    actor n =
        Cr.bench ("n=" ++ show n) . Cr.nfIO $ benchRing n
    channel n =
        Cr.bench ("n=" ++ show n) . Cr.nfIO $ channelRing n
\end{code}
\end{samepage}
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
First the control, which only forks threads and then kills them.
%
\begin{code}
benchControl :: Int -> IO ()
benchControl n = do
    nodes <- sequence . replicate n $ forkIO (return ())
    mapM_ killThread nodes
\end{code}


\subsubsection{Channel-based}
Next, a channel-based ring leader-election.
%
Each node will have references to a send-channel and a receive-channel.
%
We reuse the message types via an \verb|Either|.
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
    \texttt{chanNode}, \texttt{node'Part} reimplements the behavior of the
    victory-round node (\Cref{sec:ring2-intent-fun}) and the benchmark-node
    (\Cref{sec:perf-eval-detail}).
    %
    We signal termination by placing the confirmed winner's \texttt{ThreadId}
    into the ``done'' \texttt{MVar}.

    \item
    Finally \Cref{fig:channelRing} initializes the algorithm with a function
    similar to \texttt{ringElection}, but using channels instead of passing in
    \texttt{ThreadId}s.
\end{itemize}

\begin{figure}[h]
\caption{Main-loop for channel-based implementation of ring leader-election.
Includes \Cref{fig:chanNodePart,fig:chanNodePrimePart} it its where-clause.}
\label{fig:chanNode}
\begin{code}
chanNode ::
    Mv.MVar ThreadId -> (Ch, Ch) -> ThreadId -> IO ()
chanNode done chans st = do
    chanNode done chans =<< node'Part st =<< recv
  where
    recv = Ch.readChan (fst chans)
    sendMsg = Ch.writeChan (snd chans) . Left
    sendWinner = Ch.writeChan (snd chans) . Right
\end{code}
\end{figure}

\begin{figure}[h]
\caption{Channel-based reimplementation of \verb|node| defined in the
where-clause of \Cref{fig:chanNode}.}
\label{fig:chanNodePart}
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
                -> putStrLn (show self ++ ": nominate " ++ show nominee)
                >> sendMsg (Nominate nominee)
            | otherwise       -> putStrLn "Ignored nominee"
\end{code}
\end{figure}
\ignore{
\begin{code}
    nodePart _ = error "nodePart: unhandled"
\end{code}
}

\begin{figure}[h]
\caption{Channel-based reimplementation of \verb|node'| defined in the
where-clause of \Cref{fig:chanNode}}
\label{fig:chanNodePrimePart}
\begin{code}
    node'Part :: ThreadId -> Either Msg Winner -> IO ThreadId
    node'Part great (Left m) = do
        nodePart m
        self <- myThreadId
        case m of
            Nominate{nominee} ->
                if self == nominee
                then sendWinner (Winner self)
                    >> return great
                else return $ max nominee great
            _ -> return great
    node'Part great (Right m) = do
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
\end{figure}

\begin{figure}[h]
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

%options ghci -threaded -rtsopts -with-rtsopts=-N

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
