\documentclass{article}
\usepackage{listings}
\usepackage[hidelinks]{hyperref}
\usepackage{microtype}
\usepackage{amsmath}

\title{BigWebThing}
\author{True Ghiassi}
\begin{document}
\maketitle
BigWebThing is a computer system for sharing apps between users. This document contains a user specification and a technical specification.

The user specification just defines what is essential from a user point of view, but doesn't bother about technical issues that the user doesn't care or know about. With regard to performance, this section just assumes that the hardware is so good that performance issues do not exist.

The technical specification is about how to implement the user specification, and is greatly concerned with performance issues of all kinds, such as memory, disk and network usage.

\section{User specification}
It should be possible for a very large number of people to use the system.
\subsection{Hardware support}
The system should work on:
\begin{itemize}
\item Android phones and tablets
\item Apple phones and tablets
\item Apple desktops and laptops
\item Windows desktops and laptops
\item Linux desktops
\item Linux servers
\end{itemize}
\subsection{Mutable data}
The data stored in the system is specified here in relational form, that is as sets of unique tuples.

\subsubsection{File author}
\begin{itemize}
\item the file (bytes)
\item author ID (uint64)
\item creation timestamp (uint32)
\end{itemize}

\subsubsection{Permission for user to view a file}
\begin{itemize}
\item the file (bytes)
\item user ID (uint64)
\item timestamp (uint32)
\end{itemize}

\subsubsection{App}
\begin{itemize}
\item the app file (bytes)
\end{itemize}

\subsubsection{Permission for app to view a file}
\begin{itemize}
\item the file (bytes)
\item the app (bytes)
\item timestamp (uint32)
\end{itemize}

\subsubsection{Alias}
This is an alias for the user ID, chosen by the user.
\begin{itemize}
\item ID of the aliased user (uint64)
\item ID of the user creating the alias (uint64)
\item alias (UTF-8 string)
\item timestamp (uint32)
\end{itemize}

\subsection{Static data}
This data is constant and built into the system.

\begin{itemize}
\item files must be smaller than or equal to 10GB
\item users are not able to view files unless they have permission
\item apps are not able to access files unless they have permission
\item apps are not able to edit the data
\end{itemize}

\section{Technical specification}
\subsection{Public data on single server}
\subsubsection{Users}
Each user has an ID number, a public static key, and a URL. Many users can share a URL.
\begin{itemize}
\item user ID (uint64)
\item URL (UTF-8 string)
\item public static Noise key
\item created timestamp (uint32)
\item account active (bool)
\end{itemize}

\subsection{Public data on each user's server}
\subsubsection{Unread message}
\begin{itemize}
\item sender ID (uint64)
\item recipient ID (uint64)
\item message (bytes)
\item timestamp when sent (uint32)
\end{itemize}
\subsubsection{Read message}
\begin{itemize}
\item sender ID (uint64)
\item recipient ID (uint64)
\item message size (uint32)
\item timestamp when sent (uint32)
\item timestamp when read (uint32)
\end{itemize}

\subsubsection{Payment}
A payment to me.
\begin{itemize}
\item payer ID (uint64)
\item amount in GBP (uint32)
\item timestamp (uint32)
\end{itemize}

\subsection{Data on each user's computer}

\subsubsection{Static Noise key pair}
\begin{itemize}
\item secret key (bytes)
\item public key (bytes)
\end{itemize}

\subsubsection{Noise session seed}
\begin{itemize}
\item session ID (bytes)
\item seed (bytes)
\end{itemize}

\subsubsection{Noise KK1 received}
\begin{itemize}
\item session ID (bytes)
\item KK1 (bytes)
\item sender ID (uint64)
\end{itemize}

\subsubsection{Noise KK2 received}
\begin{itemize}
\item session ID (bytes)
\item KK2 (bytes)
\item sender ID (uint64)
\end{itemize}

\subsubsection{File}
\begin{itemize}
\item file hash (bytes)
\item file (bytes)
\end{itemize}

\subsubsection{App}
\begin{itemize}
\item file hash
\end{itemize}

\subsubsection{File author}
\begin{itemize}
\item file hash (bytes)
\item author ID (uint64)
\end{itemize}

\subsubsection{Permission for app to view a file}
\begin{itemize}
\item app file hash (bytes)
\item file hash (bytes)
\end{itemize}

\subsubsection{Send file}
\begin{itemize}
\item sending ID (uint32)
\item timestamp (uint32)
\item file hash (bytes)
\item recipient ID (uint64)
\end{itemize}

\subsubsection{Error sending file}
\begin{itemize}
\item sending ID (uint32)
\item UTF-8 error message (string)
\end{itemize}

\end{document}
