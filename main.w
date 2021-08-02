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
\item creation timestamp (uint64)
\end{itemize}

\subsubsection{Permission for user to view a file}
\begin{itemize}
\item the file (bytes)
\item user ID (uint64)
\item timestamp (uint64)
\end{itemize}

\subsubsection{App}
\begin{itemize}
\item the app file (bytes)
\end{itemize}

\subsubsection{Permission for app to view a file}
\begin{itemize}
\item the file (bytes)
\item the app (bytes)
\item timestamp (uint64)
\end{itemize}

\subsubsection{Alias}
This is an alias for the user ID, chosen by the user.
\begin{itemize}
\item ID of the aliased user (uint64)
\item ID of the user creating the alias (uint64)
\item alias (UTF-8 string)
\item timestamp (uint64)
\end{itemize}

\subsubsection{Payment}
A payment to me.
\begin{itemize}
\item payer ID (uint64)
\item amount in GBP (uint32)
\item timestamp (uint64)
\end{itemize}

\subsection{Static data}
This data is constant and built into the system.

\begin{itemize}
\item the mutable data can only be modified by inserting new items, not by editing or deleting existing items
\item all timestamps are unique, and give the order that items were added to the database or last modified
\item files must be smaller than or equal to 10GB
\item users are not able to view files unless they have permission
\item apps are not able to access files unless they have permission
\item apps are not able to edit the data
\item A user can only add data if they have made sufficient payments. This is to cover hosting costs and make some profit for me. This condition must be met:
\[
\text{total payments} \geq \text{price} \times \sum ( (\text{age} \times \text{size}) \text{ for each file} )
\]
\end{itemize}

\end{document}
