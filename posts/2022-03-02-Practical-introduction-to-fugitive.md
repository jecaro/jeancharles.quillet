---

title: Practical introduction to fugitive
toc: true

---

[Fugitive][fugitive], the `git` wrapper so awesome, it should be illegal.
That's at least what states the [github][fugitive] page. And it is indeed
awesome and I'm glad it is not illegal. I do most of my work in vim and
[fugitive] is my go-to wrapper for `git` in vim.

Why would you want to use `git` inside vim in the first place ? The command line
interface is fine.

Well, most of my work consists in software development. I carefully craft my
commits to make them atomic and to keep the code history clean. My workflow
consists in reviewing every changes before staging and committing them. During
the review I often find myself editing the changes. Once there are changes they
need to be at least compiled and sometimes even tested.

With [fugitive] I can do this without quitting vim (who knowns how to do that
anyway ?). Here I'll present most of the important features of [fugitive] and
how I use them.

# Basics

`git status` is available with the command `:Git`. It opens a new window with
all the information returned by `git status`. Quite a few commands can be
started from this window, I will described the most important in the following
sections.

By default the arguments of the `:Git` commands if they are not handled by
[fugitive] itself are sent to the `git` program. For example, one can create
and checkout a new branch with `:Git checkout -b bug-fix-1234` or fetch the
latest changes in the remote with `:Git fetch`.

# Viewing current editing changes

Let's say that I currently edit a file tracked by `git`. I want to see the
changes I made. I can see them in diff mode in a split view with:

- `:Gvdiffsplit` for a vertical split diff view
- `:Ghdiffsplit` for a horizontal split diff view
- `:Gdiffsplit` for a vertical or horizontal split depending on the current
  window ratio

Now I can browse and edit the changes. Every shortcuts from diff mode are of
course available here. I use mostly:

- `do` to obtain the changes from the other window
- `dp` to put the changes to the other window

Some other interesting commands when editing files tracked by `git` are:

- `:Gread` to revert the local changes
- `:Gwrite` to stage the file for the next commit

# Change review and commit

Now I have done a bunch changes I want to commit. I can get the `git status` of
my working directory with `:Git`.

A window appears in an horizontal split showing usually three sections:

- The list of untracked files
- The list of modified files tracked by `git` in the section unstaged
- The list of staged files

I can put the cursor on each of these files and toggle the staged/unstaged
state with the key `-`. Hitting `X` reverts a changed file for the version in
the repository.

Once I'm done I can hit:

- `cc` to commit staged changes
- `ca` to amend the latest commit

Then I'm presented a new buffer where I can write the commit message. Once
saved and closed the commit is added to the `git` database. That's the most
basic workflow, it assumes that I'm happy with the changes I made which is
rarely the case.

Now before committing my changes, I want to review every single hunk to make
sure that they are all relevant for the commit I want to add.

I can put my cursor on an unstaged file and hit:

- `dv` for a vertical split diff view
- `dh` for a horizontal split diff view
- `dd` for a vertical or horizontal split depending on the current window ratio

Then I can review my changes and make the appropriate modifications if needed.

Split views are very powerful. It is even possible to stage individual hunks
editing the version of the file in the index.

For example, let's say I have changed a file. I open a vertical diff. On the
left I can see the version in the repository, on the right I have the version I
have edited. The buffer on the left is editable and represents what it is about
to be staged. Practically it means that being in my version on the right if I
go to the first change, I hit `dp` it will put my changes in the buffer on the
left. When I save this buffer, that change will be staged.

That's great, that means that I don't have to delete all temporary statement in
my code before committing them.

# Browsing logs

Browsing logs can be achieved with the command `:Git log`. It opens a buffer
showing the commits in a reverse chronological order such as the latest
commits are located at the top.

Hitting the `Enter` key on any ref in the log opens the corresponding commit.
The commit buffer shows the commit message, its author and the changes it
contains as a diff. Well, viewing diff is not very helpful, we usually need to
see the changes in the context of a file. It is fortunately possible to do it
with [fugitive].

This is how I like to do it:

- `:set foldmethod=syntax` folds the diff under each modified filename. Great,
  now I can see the list of files changed by the commit. `zo` `zc` open/close
  the diff on each filename. But there is better.
- `O` on a file opens a new tab with a diff view of the changes
- `:tabclose` I'm back to the commit view and I can choose another file to view

# Viewing the commits of a file

A special case of the previous workflow is to look at the log of the current
file. This can be done with `:Git log %`.

# Browsing through the history of a file

Another variation would be to quickly browse the different versions of the
current file. `:0Gllog` does just that by filling the location list with those
versions. Browsing through them is possible with the usual `:lnext`
`:lprevious`. Then call `:lclose` when you are done.

# Resolving conflicts

In case of a conflict during a merge or a rebase operation, [fugitive] can
present a three panes view with:

- on the left the target branch: where you want to merge i.e. HEAD
- in the middle the version to reconcile. This is the one which contains the
  markers you need to get rid of: `=======`, `>>>>>>>` and `<<<<<<<`.
- on the right the branch you want to merge

Just resolve the conflicting hunks one after another, save the file, stage it,
then move on with `:Git merge --continue` or `:Git rebase --continue` depending
on the case.

# Other cool features

## Interactive rebase

A feature I use a lot is `git` interactive rebase. It's a great tool to clean
up the commits in a branch before merging them. That can also be done in
[fugitive].

In a `git` log view, just hit `ri` on any commit to start an interactive rebase
from this commit to the current one. Alternatively you can also start it with
`:Git rebase -i anyref`.

Then you'll be presented the window listing the commits and a short message
describing what you can do with the commit.

## git blame

`git blame` is available in two versions in current [fugitive].

The first one can be triggered with `:Git blame` It opens a window on the left
showing the last editor of the current line. One can blame the previous version
with `~` on any commit or view the commit with the `Enter` key.

One problem with that workflow tho. It is that there is no easy way to get back
to the previous view. Indeed the two windows need to be updated so `CTRL-o` is
not of any help here.

A workaround is to use `:Git blame %`. It opens a buffer with the blame message
in front of each line in a single window. An issue is that the syntax highlight
doesn't work anymore but now it is possible to blame the previous version with
`~` and move in the location list as usual with `CTRL-o` `CTRL-i`.

## Viewing the history of a range

In the same vein as [here](#browsing-through-the-history-of-a-file), it is
possible to load all the versions that has affected a specific range of lines
in a file with: `:[range] Gllog`. This one is incredibly useful to diagnostic a
bug or a regression.

## Getting back to the current editing file

When browsing the `git` history, it's easy to get lost after a while. You can
get back to the current version of the current file any time with `:Gedit`.

## Viewing the commit the current file belongs to

Another handy trick is to reach the last commit that has touched the current
file: `:Gedit !`.

# Conclusion

I hope that these use case descriptions have made the demonstration that there
is no point using `git` command line or reaching for another tool when editing
`git` tracked files in vim.

And it only scratches the surface, feel free to start reading the great `:help
fugitive` and be thankful to [Tim Pope](https://github.com/tpope) for allowing
us not to have to quit vim when in need for `git` !

[fugitive]: https://github.com/tpope/vim-fugitive
