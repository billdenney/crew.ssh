% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/crew_ssh.R
\name{ssh_launcher_class}
\alias{ssh_launcher_class}
\title{Create an ssh job launcher object}
\description{
Create an ssh job launcher object

Create an ssh job launcher object
}
\section{Super class}{
\code{\link[crew:crew_class_launcher]{crew::crew_class_launcher}} -> \code{ssh_launcher_class}
}
\section{Public fields}{
\if{html}{\out{<div class="r6-fields">}}
\describe{
\item{\code{ssh_host}}{Passed to \code{\link[ssh:ssh]{ssh::ssh_connect()}}}

\item{\code{ssh_keyfile}}{Passed to \code{\link[ssh:ssh]{ssh::ssh_connect()}}}

\item{\code{ssh_passwd}}{Passed to \code{\link[ssh:ssh]{ssh::ssh_connect()}}}

\item{\code{ssh_verbose}}{Passed to \code{\link[ssh:ssh]{ssh::ssh_connect()}}}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-ssh_launcher_class-new}{\code{ssh_launcher_class$new()}}
\item \href{#method-ssh_launcher_class-launch_worker}{\code{ssh_launcher_class$launch_worker()}}
\item \href{#method-ssh_launcher_class-terminate_worker}{\code{ssh_launcher_class$terminate_worker()}}
\item \href{#method-ssh_launcher_class-clone}{\code{ssh_launcher_class$clone()}}
}
}
\if{html}{\out{
<details><summary>Inherited methods</summary>
<ul>
<li><span class="pkg-link" data-pkg="crew" data-topic="crew_class_launcher" data-id="active"><a href='../../crew/html/crew_class_launcher.html#method-crew_class_launcher-active'><code>crew::crew_class_launcher$active()</code></a></span></li>
<li><span class="pkg-link" data-pkg="crew" data-topic="crew_class_launcher" data-id="booting"><a href='../../crew/html/crew_class_launcher.html#method-crew_class_launcher-booting'><code>crew::crew_class_launcher$booting()</code></a></span></li>
<li><span class="pkg-link" data-pkg="crew" data-topic="crew_class_launcher" data-id="call"><a href='../../crew/html/crew_class_launcher.html#method-crew_class_launcher-call'><code>crew::crew_class_launcher$call()</code></a></span></li>
<li><span class="pkg-link" data-pkg="crew" data-topic="crew_class_launcher" data-id="done"><a href='../../crew/html/crew_class_launcher.html#method-crew_class_launcher-done'><code>crew::crew_class_launcher$done()</code></a></span></li>
<li><span class="pkg-link" data-pkg="crew" data-topic="crew_class_launcher" data-id="errors"><a href='../../crew/html/crew_class_launcher.html#method-crew_class_launcher-errors'><code>crew::crew_class_launcher$errors()</code></a></span></li>
<li><span class="pkg-link" data-pkg="crew" data-topic="crew_class_launcher" data-id="forward"><a href='../../crew/html/crew_class_launcher.html#method-crew_class_launcher-forward'><code>crew::crew_class_launcher$forward()</code></a></span></li>
<li><span class="pkg-link" data-pkg="crew" data-topic="crew_class_launcher" data-id="launch"><a href='../../crew/html/crew_class_launcher.html#method-crew_class_launcher-launch'><code>crew::crew_class_launcher$launch()</code></a></span></li>
<li><span class="pkg-link" data-pkg="crew" data-topic="crew_class_launcher" data-id="rotate"><a href='../../crew/html/crew_class_launcher.html#method-crew_class_launcher-rotate'><code>crew::crew_class_launcher$rotate()</code></a></span></li>
<li><span class="pkg-link" data-pkg="crew" data-topic="crew_class_launcher" data-id="scale"><a href='../../crew/html/crew_class_launcher.html#method-crew_class_launcher-scale'><code>crew::crew_class_launcher$scale()</code></a></span></li>
<li><span class="pkg-link" data-pkg="crew" data-topic="crew_class_launcher" data-id="set_name"><a href='../../crew/html/crew_class_launcher.html#method-crew_class_launcher-set_name'><code>crew::crew_class_launcher$set_name()</code></a></span></li>
<li><span class="pkg-link" data-pkg="crew" data-topic="crew_class_launcher" data-id="settings"><a href='../../crew/html/crew_class_launcher.html#method-crew_class_launcher-settings'><code>crew::crew_class_launcher$settings()</code></a></span></li>
<li><span class="pkg-link" data-pkg="crew" data-topic="crew_class_launcher" data-id="start"><a href='../../crew/html/crew_class_launcher.html#method-crew_class_launcher-start'><code>crew::crew_class_launcher$start()</code></a></span></li>
<li><span class="pkg-link" data-pkg="crew" data-topic="crew_class_launcher" data-id="summary"><a href='../../crew/html/crew_class_launcher.html#method-crew_class_launcher-summary'><code>crew::crew_class_launcher$summary()</code></a></span></li>
<li><span class="pkg-link" data-pkg="crew" data-topic="crew_class_launcher" data-id="tally"><a href='../../crew/html/crew_class_launcher.html#method-crew_class_launcher-tally'><code>crew::crew_class_launcher$tally()</code></a></span></li>
<li><span class="pkg-link" data-pkg="crew" data-topic="crew_class_launcher" data-id="terminate"><a href='../../crew/html/crew_class_launcher.html#method-crew_class_launcher-terminate'><code>crew::crew_class_launcher$terminate()</code></a></span></li>
<li><span class="pkg-link" data-pkg="crew" data-topic="crew_class_launcher" data-id="terminate_workers"><a href='../../crew/html/crew_class_launcher.html#method-crew_class_launcher-terminate_workers'><code>crew::crew_class_launcher$terminate_workers()</code></a></span></li>
<li><span class="pkg-link" data-pkg="crew" data-topic="crew_class_launcher" data-id="unlaunched"><a href='../../crew/html/crew_class_launcher.html#method-crew_class_launcher-unlaunched'><code>crew::crew_class_launcher$unlaunched()</code></a></span></li>
<li><span class="pkg-link" data-pkg="crew" data-topic="crew_class_launcher" data-id="validate"><a href='../../crew/html/crew_class_launcher.html#method-crew_class_launcher-validate'><code>crew::crew_class_launcher$validate()</code></a></span></li>
<li><span class="pkg-link" data-pkg="crew" data-topic="crew_class_launcher" data-id="wait"><a href='../../crew/html/crew_class_launcher.html#method-crew_class_launcher-wait'><code>crew::crew_class_launcher$wait()</code></a></span></li>
</ul>
</details>
}}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-ssh_launcher_class-new"></a>}}
\if{latex}{\out{\hypertarget{method-ssh_launcher_class-new}{}}}
\subsection{Method \code{new()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ssh_launcher_class$new(
  ...,
  ssh_host,
  ssh_keyfile = NULL,
  ssh_passwd = "",
  ssh_verbose = FALSE
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{...}}{passed to \code{crew::crew_class_launcher$initialize()}}

\item{\code{ssh_host, ssh_keyfile, ssh_passwd, ssh_verbose}}{Passed to
\code{\link[ssh:ssh]{ssh::ssh_connect()}}}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-ssh_launcher_class-launch_worker"></a>}}
\if{latex}{\out{\hypertarget{method-ssh_launcher_class-launch_worker}{}}}
\subsection{Method \code{launch_worker()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ssh_launcher_class$launch_worker(call, name, launcher, worker, instance)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{call, name, launcher, worker, instance}}{As used with
\code{crew::crew_class_launcher$launch_worker()}}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-ssh_launcher_class-terminate_worker"></a>}}
\if{latex}{\out{\hypertarget{method-ssh_launcher_class-terminate_worker}{}}}
\subsection{Method \code{terminate_worker()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ssh_launcher_class$terminate_worker(handle)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{handle}}{As used with \code{crew::crew_class_launcher$terminate_worker()}}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-ssh_launcher_class-clone"></a>}}
\if{latex}{\out{\hypertarget{method-ssh_launcher_class-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ssh_launcher_class$clone(deep = FALSE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{deep}}{Whether to make a deep clone.}
}
\if{html}{\out{</div>}}
}
}
}
