knit
====

Knit is a tool for making Erlang release upgrades. It also makes Erlang release
packages but is primarily focused on making upgrades. Its similar in spirit
to [rebar's release generation][rebar] or [relx][relx] but focusing much more
specifically on upgrades.

If you're already using rebar's release generation then trying knit should
be straight forward to try as its written to be compatible with the
`reltool.config`.


Releases and Upgrades
---------------------

This is the obligatory background on Erlang releases and upgrades. An Erlang
release is (generally) a self-contained tarball that includes the entire
Erlang VM bundled with the standard library and a copy of all the code
required to run an Erlang program. You can read more about releases at
[LYSE][LYSE-releases] or in [Erlang's documentation][erlang-releases].

While releases are full of awesome, the fun doesn't really start until you
get to working with upgrades. Everyone talks about Erlang's hot code loading
and some have even ventured into the land of `l/1` and `nl/1` for loading
individual modules into a live VM. Release upgrades are hot code loading
on steroids. They provide a repeatable (mostly) scripted approach to apply
code loading primitives onto a running system. It can be a bit daunting at
first but is really quite neat once you learn the fundamentals.

A release upgrade comprises of two basic components: a copy of the new
compiled application code and some metadata about the new release, most
notably of which is called a `relup`. The `relup` is simply a linear set
of instructions for applying hot code loading primitives. The process of
applying an upgrade is, in a nutshell, "unpack a tarball and run the relup".

While there are obviously a lot of details surrounding the various bits of
this process the most important part to remember is that its a fundamentally
simple and straightforward process. Interested readers are encouraged to
check out the [LYSE docs on relups][LYSE-relups] as the
[Erlang docs on relups][erlang-relups] are less than stellar.

One final major concept to be aware of is that of appups. While the idea of
the relup is simple, the details involved in generating it are numerous and
intertwined. The general idea for generating a relup is that each Erlang
application that changes between releases will have an appup. These appups
are then "compiled" by systools and merged into a single relup.

Historically the process of generating these appups has been rather painful
even when using the various release tools. Knits entire motivation for
existence is to improve the process around handling these appup files while
making sure the rest of the release/upgrade generation is as painless as
possible. For a comparison with rebar and relx see the section at the bottom
of this README for an (admittedly biased) explanation.

For more information on appups see this [LYSE section][LYSE-appups], the
[Erlang docs on appups][erlang-appups], or the
[Erlang appup cookbook][erlang-appup-cookbook]. To read more about how
knit helps with generating appups see the section below called
"How Knit Helps with Appups".

Finally, a quick aside on downgrades. While Erlang supports the notion of
downgrades between releases, knit does not yet have support. In general it
borrows from rebar's theory that you should just roll forward to a new
version if something is broken. Its possible that knit will eventually
support downgrades but its not currently on the agenda.


[rebar]: https://github.com/rebar/rebar
[relx]: https://github.com/erlware/relx
[LYSE-releases]: http://learnyousomeerlang.com/release-is-the-word
[erlang-releases]: http://www.erlang.org/doc/design_principles/release_handling.html
[LYSE-relups]: http://learnyousomeerlang.com/relups
[erlang-relup]: http://www.erlang.org/doc/man/relup.html
[LYSE-appups]: http://learnyousomeerlang.com/relups#appup-files
[erlang-appups]: http://www.erlang.org/doc/man/appup.html
[erlang-appup-cookbook]: http://www.erlang.org/doc/design_principles/appup_cookbook.html


An Outline of Release and Upgrade Generation
--------------------------------------------

Before we get too far its helpful to have a general idea on the steps involved
in generating release and upgrade tarballs. In a few steps it looks something
like this:

1. Compile all application code (knit relies on rebar for this step)
2. Generate a release based on `reltool.config`
3. Process overlay instructions from `reltool.config`
4. Generate the release tarball
5. Locate previous release versions to upgrade from
6. Expand each previous release
7. For each old release find which Erlang applications have changed
8. Generate an appup for each changed application
9. Compile all appups into the relup
10. Package the upgrade tarball

Most tools are pretty good at handling steps 1-4. Everything after that has
historically not been the most well documented or easiest to use. Knit tries
to make the rest of those steps as painless as possible. See the section
"How Knit Helps with Appups" below for a much more detailed description of
these steps as well as knit is actually doing that's new.


Building Knit
-------------

This should be as simple as:

    $ git clone https://github.com/cloudant/knit.git
    $ cd knit
    $ make
    $ cp knit /usr/local/bin

The output of the build is a single executable `knit` which is exactly like
the `rebar` executable (a self-contained executable zip file).


Running Knit
------------

This should be as simple as:

    $ cd ~/path/to/my/project/
    $ knit

Assuming you have either a `reltool.config` or `rel/reltool.config` in your
project `knit` will automatically find it and do its thing. Assuming this
is your first time running knit you should expect to see `rel/$name/` and a
new `rel/releases/$name-$vsn.tar.gz` file. `rel/$name` is based on the
configured boot release (you can read more on that in the section on
`reltool.config`).


Running Knit Again
------------------

Once you create a release, if you edit some code and update your application
version you can create another release like such:

    $ knit

Fairly complicated stuff. Though if you watch closely you'll notice that
knit also builds an upgrade package as well. Assuming this all went off
without a hitch you should find that `rel/upgrades/$name-$newvsn.tar.gz` now
exists.

At this point if you were to go and edit more code and update your version
once again you could generate a third release tarball and second
upgrade tarball like such:

    $ knit

The intrepid observer will note that this time knit will only generate
a single `rel/upgrades/$name-$newnewvsn.tar.gz` but that it was also looking
at the original `rel/releases/$name-$vsn.tar.gz`. This is because the single
upgrade tarball will apply to both `$name-$vsn.tar.gz` *and*
`$name-$newvsn.tar.gz`. Neat.


Knit's Command Line Interface
-----------------------------

The `knit` command is fairly basic. The help output looks like such:

    Usage: knit [-h] [-V] [-v <verbose>] [-d] [-q] [-l <log_file>]
                [-r <reltool>] [var ...]

      -h, --help            Show this help message and exit.
      -V, --version         Show version information and exit.
      -v, --verbose         Set verbose output. Can be repeated for increased 
                            verbosity.
      -d, --debug           Set debug verbosity logging.
      -q, --quiet           Disable all output to the console.
      -l, --log_file        Log all output to the specified file. Unaffected 
                            when using quiet.
      -r, --reltool_config  Path to your reltool.config
      var                   Set variables using the name=value syntax

The only real parameter of immediate interest is `-r/--reltool_config`. If
you have a reltool configuration file named something other than
`reltool.config` or `rel/reltool.config` you'll need to specify the name
with this flag.


Setting Config Values from the Command Line
-------------------------------------------

All of the configuration variables are listed below but its important to note
that any of these can be overridden via knit's command line variable syntax.
It looks similar to rebar's variables with a bit of extra magic.

A basic example looks something like such:

    $ knit name1=value1 name2=value2 ...

The extra sauce here is that you can have your values parsed as Erlang strings
which can be helpful in some specific circumstances that you'll know if you
need once you get there. For the most part the most common settings will work
without this as they're written internally to check for string values.

The format for Erlang statements as values looks like:

    $ knit name1=!!value1

Where `value1` is a valid Erlang statement. More specifically:

    $ knit 'name1=!!{this, is, 1.0, "a tuple"}'

Notice that there's no trialing `.` to terminate the statement as that's
automatically appended. Also note the use of quotes so that the shell
passes the expression as a single command line argument.


A Quick Primer on reltool.config
--------------------------------

The [erlang docs][erlang-reltool] on `reltool.config`  are a bit opaque at
first but hopefully this quick primer will explain enough to help the
unfamiliar make sense of things.

A `reltool.config` file is a standard "list of Erlang terms" format that is
used for things like `*.app` files. A simplistic `rel/reltool.config` looks
something like such:

    {sys, [
        {lib_dirs, ["../apps", "../deps"]},
        {rel, "dbcore", git, [
            kernel,
            stdlib,
            my_cool_app
        ]},
        {rel, "start_clean", "", [kernel, stdlib]},
        {boot_rel, "dbcore"},
        {profile, embedded},
        {excl_sys_filters, ["^bin/.*", "^erts.*/bin/(dialyzer|typer)"]},
        {excl_archive_filters, [".*"]},
    
        {app, my_cool_app, [{incl_cond, include}]}
    ]}.

There are a few major things to note about this right away. First is that
the `{sys, [option()]}` structure. This is important as this term is passed
directly to `reltool:start_server/1`.

The second thing to notice is the format of the release descriptions:

    {rel, Name, Version, [Application]}

While we generally talk about a release as a single tarball, under the
covers a single tarball can actually include multiple releases. While that
seems a bit crazy there is actually a common use case for this as
demonstrated by `{rel, "start_clean", "", [kernel, stdlib]}` entry. This
"release" is used by system scripts or `remsh` when connecting to running
nodes.

Given that there are multiple releases we need to tell reltool which one is
the default release to boot. This is the aptly named `boot_rel` option. Knit
uses the value for `boot_rel` as the name for all generated files so its
important that you pick something you'd want to see in tarball file names.

The `{profile, embedded}` setting dictates how many extraneous files are
included by default in the release tarball. The possible values for this
setting are `development`, `standalone`, and `embedded` which include the
most, fewer, and fewest files in the resulting release.

Lastly the `{app, my_cool_app, [{incl_cond, include}]}` tells reltool to
include `my_cool_app` in the generated release along with all of the
dependencies listed in `my_cool_app.app`.

For now its best to just ignore the `excl_sys_filters` and
`excl_archive_filters`. When you need to learn the details for these options
you'll be far along enough that the man pages make sense.
    
For an extended description of all of the possible `sys` options the write
up by [LYSE][LYSE-releases] is probably the best place to check. There's a
section labeled "Releases with Reltools" towards the bottom that's helpful.

[erlang-reltool]: http://erlang.org/doc/man/reltool.html


Knit Specific Config Options
----------------------------

Along with the `sys` tuple, knit recognizes a number of other options that
can be specified in the same reltool.config file. So there's no confusion
a `reltool.config` with knit configuration values would look like such:

    {knit_option_1, knit_value_1}.
    {knit_option_2, knit_option_2}.
    
    {sys, [
        {lib_dirs, ["../apps", "../deps"]},
        {rel, "dbcore", git, [
            kernel,
            stdlib,
            my_cool_app
        ]},
        {rel, "start_clean", "", [kernel, stdlib]},
        {boot_rel, "dbcore"},
        {profile, embedded},
        {excl_sys_filters, ["^bin/.*", "^erts.*/bin/(dialyzer|typer)"]},
        {excl_archive_filters, [".*"]},
        
        {app, my_cool_app, [{incl_cond, include}]}
    ]}.

It doesn't matter where in the file you place things, its just important to
note that they aren't nested inside the `sys` tuple.    

The current list of recognized congfiguration options:

* `force` - By default knit will not attempt to overwrite any previously
  generated releases or upgrades. If you want to regenerate a release and/or
  upgrade you'll need to remove them by hand first or set `{force, true}` in
  your reltool.config (or `force=true` on the command line). Generally
  speaking your releases and upgrades should be idempotent but during
  development it can be useful to trash generated tarballs while iterating
  on appups.
* `overlay` - This is a list of simple commands to execute once a release
  has been generated so that you can include other resources in your
  releases. It is generally compatible with rebar's implementation of
  overlays. The one difference is how overlay_vars is overridden via
  command line variable. See `extra_overlay_vars` below for more on that
  incompatibility. Overlays are described more thoroughly below.
* `overlay_vars` - A path to a "list of Erlang terms" formatted file that
  contains `{key, value}.` entries used when rendering overlay templates.
* `extra_overlay_vars` - Rebar allows users to load both a vars file from
  `reltool.config` and then load a *second* vars file specified on the
  command line which overrides the first vars file. Knits config system works
  a bit differently so if you want this behavior you need to set the
  `extra_overlay_vars` variable on the command line. This difference is
  described in more detail in the section on overlays.
* `warnings_as_errors` - If warnings are generated by any of the reltool
  or systool commands these will treated as errors and knit will exit.
* `semver` - Takes a `true` or `false` value and is used when comparing
  versions for upgrades. If true, there's a fairly hacky implementation
  of semantic version sorting. The default value is true as the hack will
  fall back to a basic sorting. If set to `false` you can specifically
  disable the semantic version interpretation which then just sorts versions
  based on Erlang's builtin comparison operators for strings.
* `version_regex` - When knit goes to build upgrades it looks in the
  `release_dir` for release tarballs. Any tarball named `$name-$vsn.tar.gz`
  will be attempted to use as a possible old version when generating the
  upgrade. If you want to eliminate some tarballs based on a pattern you can
  use this option to do that. Although really this option should just be
  applied to all filenames and anything it returns will be the version. Or
  some such. I'll add that to my To-Do list.
* `version_min` - If you want to specifically exclude all versions prior to
  some defined version you can use this setting. Any filenames that have been
  found and passed through the `veresion_regex` filter will be compared to
  `version_min` (using the semantic version comparison unless disable though
  when not disabled it should worst case behave as though it were disabled).
  Any version found to be less than `version_min` will not be included in
  the upgrade. This is option is useful once you know that all of your
  production code is newer than some version so that you can limit the amount
  of versions that need to have valid upgrade instructions.
* `upgrade_versions` - Alternatively to using `version_regex` and `version_min`
  you can also just set `upgrade_versions` directly and knit will trust you
  that all of the `$name-$vsn.tar.gz` releases in `release_dir`. Remember
  that if you want to set this from the command line you'll need to use
  something like `'upgrade_versions=!!["0.0.1", "0.0.3"]'`.
* `remove_tmp` - Defaults to true but if you're having a hard time figuring
  out how an upgrade is generated it can be useful to disable this by setting
  `{remove_tmp, false}` or setting `remove_tmp=false` on the command line.
* `target_dir` - By default knit will use the directory where it finds your
  `reltool.config` as the target directory. The target directory contains
  the release and upgrade directories and tmp directories used when building
  tarballs. If you don't want to much up your source checkouts you can use
  this single setting to point somewhere else on disk. A common value would
  be something like `/var/lib/$name/`.
* `release_dir` - If you want to set where release tarballs are written
  without changing `target_dir` you can use this option. By default this
  will be `$target_dir/releases`. Its not uncommon to not set `target_dir`
  and then specify values for `release_dir` and `upgrade_dir` as something
  like `/var/lib/$name/releases/` and `/var/lib/$name/upgrades/` respectively.
* `upgrade_dir` - See above about `release_dir`.
* `tmp_dir` - *This directory will be forcefully wiped each time knit runs.*
  By default it is `$target_dir/.knit_tmp` but you can configure it to be
  anything. It will be removed when knit is finished unless you have disabled
  that with `remove_tmp=false`.
* `root_dir` - An advanced parameter that should probably just be a constant
  in retrospect. You can safely ignore this option included for completeness.


Release Overlays
----------------

Overlays are basically just a list of file operations applied after a release
is generated (but before it is packaged) so that you can do things like
including non-Erlang related files in a release or creating common file trees
used in your production environments. For instance, including `etc` config
directories is one common use.

A simple overlay setting would look something like such:

    {overlay_vars, "my_vars.config"}.
    {overlay, [
        {mkdir, "var/log"},
        {copy, "overlay/bin"},
        {copy, "overlay/etc"},
        {copy, "overlay/share"},
        {copy, "../path/\{\{var_name\}\}/thing", "bin/thing"},
        {template, "overlay/etc/config.ini", "etc/config.ini"}
    ]}.

The basic idea is that variables are loaded from `my_vars.config` and used
while rendering templates and file paths. The rendering is all handled using
[Mustache][mustache] templating. Remember that paths are also rendered which
can be handy.

Its also important to note that source paths are relative to the specified
`reltool.config` while destination paths are relative to `$target_dir/$name/`
which contains the `lib`, `releases`, and `erts-5.y.z` directories.

[mustache]: https://github.com/mojombo/mustache.erl


Overlay Commands
----------------

* `{mkdir, OutDir}` - Create a directory named `OutDir`
* `{copy, Src}` - Translates directly to `{copy, Src, ""}`
* `{copy, Src, Dst}` - Copy a file `Src` to `Dst`
* `{template, Src, Dst}` - Run the file `Src` through a mustache rendering
  using the `overlay_vars` as context. Write result to `Dst`.
* `{template_wildcard, Wildcard, OutDir}` - Similar to the `template` but will
  run the template against all the files matching the shell-like `Wildcard`
  pattern. Ie `overlay/etc/*.ini` to template all `*.ini` files instead of
  requiring a `template` command for each file.
* `{create, Dst, Contents}` - Write the value of `Contents` to `Dst`.
* `{replace, Dst, RegX, Repl}` - Translates directly to
  `{replace, Dst, RegX, Repl, []}`
* `{replace, Dst, RegX, Repl, Opts}` - Read the file `Dst` and execute the
  regular expression `RegX` and replace matches with the value `Repl`. The
  `Opts` value is passed to the call to `re:replace/4`. The value `Repl` is
  rendered before being passed to `re:replace/4`.


How Knit Helps with Appups
--------------------------

As explained above, knit's entire purpose is to make working with appups
bearable and as minimally rage inducing as possible. In general knit does
this by attempting to automatically generate upgrade instructions. The basic
algorithm is borrowed from rebar's appup generation but knit extends this
algorithm considerably so that it can be tweaked by providins small bits of
metadata directly in the affected modules.

Historically there have been roughly three approaches I've seen to working
with appups:

1. Write them by hand
2. Hope that rebar's `generate-appups` gets something close
3. Use the output of rebar's `generate-appups` and then edit by hand

Generally speaking rebar will cover roughly 75% of your general upgrade
use cases. The downfall is that anything beyond a trivial upgrade requires
the user to edit the generated appup to cover the more complicated cases. And
the bit that makes this particularly fun is when your upgrades are generated
on a build machine so you have to check in the edited appup which then leads
to confusion when that breaks the next release (because you forgot to remove
it and it doesn't cover the new release version).

Without any tooling, maintaining appups for all version pairs is mind numbingly
boring. Maintaining appups by hand would be almost doable if there were a tool
that would allow you to have a hybrid "auto generate appups for some version
pairs but let me override this particular version". Originally knit was
conceived as exactly this tool but was then extended when I realized it would
be even easier to just provide a bit of metadata to hopefully cover 99% of
upgrade use cases.

So that's knit in a nutshell: Automatically generate appups while providing
an interface for coercing that generation for complicated situations. First
we'll talk a bit about how that works and then show a number of examples
on common non-trivial use cases.

Knit Module Attributes
----------------------

The way we pass metadata to knit's appup generation algorithm is through
module attributes. This way we store appup related information directly
in the affected modules which means that our upgrade logic will be less
likely to go stale with source changes.

Supported module attributes:

* `knit_priority` - Set a module priority. This is an integer which defaults
  to 0. Added and changed modules are sorted according to this priority. Beware
  that relup generation will also affect ordering if you use dependencies.
* `knit_extra` - Set the value of Extra for code_change. This defaults to
  an empty list.
* `knit_depends` - Set module dependencies. This is a list of module names
  as atoms.
* `knit_timeout` - Set an time when suspending processes. This isn't a common
  option.
* `knit_purge` - Set the purge style. Purging affects how processes running
  old code are handled during an upgrade. There is a pre and post merge step
  and both can be either `soft_purge` or `brutal_purge`. `soft_purge` basically
  means to skip the purge step and just wait for Erlang to kill the process
  running old code. `brutal_purge` means that the upgrade process will kill
  any process running old code for this module. The value for this attribute
  should be of the form `soft_purge | brutal_purge | {Pre, Post}`. Setting it
  as an atom is just a shorthand for setting `Pre` and `Post` to the same
  value. Default is `{brutal_purge, brutal_purge}`. If you're uncertain use
  `brutal_purge` as it'll be immediately clear if your code is upgrading
  properly. `{brutal_purge, brutal_purge}` is the default.
* `knit_apply` - Run a function during an upgrade. This lets you run arbitrary
  code to affect how things are managed. In general the most common use case
  for this option is to change supervision trees during an upgrade. This can
  either be an MFA or a `{Phase, MFA}` tuple. There are three phases:
  `first`, `immediate`, `last` which affect when the function is invoked. First
  means before new code is loaded, immediate is just after the current module
  is loaded, and last is after all other modules have been loaded. `Phase` can
  also be `{PhaseName, Priority}` if you want to be specific on the ordering
  of invocation. N.B. that the first/immediate/last phase is on a
  per-application basis. There's no support (yet) for a global ordering. This
  will be added if/when is needed.

A Note on Module Dependencies
-----------------------------

Module dependencies are quite useful but there's a non-obvious effect of
using them buried in systools. Basically, any connected set of module
dependencies will be grouped in the relup and upgraded between the same
suspend/resume calls. Its possible that this can have unintended side effects
during an upgrade.

For instance, a common case might be if you have a `gen_server` behavior
paired with a utility module that you want to upgrade simultaneously. Adding
a `-knit_depends([my_app_util])` would be a straightforward method to
accomplish this. The part to be careful on is if you have a second
`gen_server` that you also want to have depend on `my_app_util`. If you do
this then processes running code from *both* `gen_server` modules will be
suspended simultaneously.

In general its best to try and make sure that your modules can be upgraded
independently but sometimes a simple dependency will save you lots of pain
trying to support multiple live module versions.