Contributing to Guix-Systole
======================

There are many ways to contribute to Guix-Systole, with varying levels of effort: finding bugs, helping documentation and testing the software.

We encourage a range of Pull Requests, from patches that include passing tests and documentation, all the way down to half-baked ideas that launch discussions.

The PR Process
----------------------------------------------

### How to submit a PR ?

If you are new to Guix-Systole development and you don't have push access to the Guix-Systole repository, here are the steps:

1. [Fork and clone](https://docs.github.com/get-started/quickstart/fork-a-repo) the repository.
2. Create a branch.
3. [Push](https://docs.github.com/get-started/using-git/pushing-commits-to-a-remote-repository) the branch to your GitHub fork.
4. Create a [Pull Request](https://github.com/Guix-Systole/Guix-Systole/pulls).

This corresponds to the `Fork & Pull Model` described in the [GitHub collaborative development](https://docs.github.com/pull-requests/collaborating-with-pull-requests/getting-started/about-collaborative-development-models) documentation.

When submitting a PR, the developers following the project will be notified. That said, to engage specific developers, you can add `Cc: @<username>` comment to notify them of your awesome contributions. Based on the comments posted by the reviewers, you may have to revisit your patches.


### How to efficiently contribute ?

We encourage all developers to:

* add or update tests. There are plenty of existing tests to inspire from in the following repositories:

    - [Guix](https://git.savannah.gnu.org/git/guix.git)
    - [Guix-HPC](https://gitlab.inria.fr/guix-hpc/guix-hpc.git)
    - [Guix-Science](https://codeberg.org/guix-science/guix-science.git)


### How to write commit messages?

Write your commit messages using the standard prefixes and component tags in the format `[Prefix][Component] Commit title`, where:

- **Prefix** is one of the standard prefixes:

  - `BUG:` Fix for runtime crash or incorrect result
  - `COMP:` Compiler error or warning fix
  - `DOC:` Documentation change
  - `ENH:` New functionality
  - `PERF:` Performance improvement
  - `STYLE:` No logic impact (indentation, comments)
  - `WIP:` Work In Progress not ready for merge

- **Component** indicates the related component, for example `services/dicomd-service`, `packages/slicer`, etc.

The body of the message should clearly describe the motivation of the commit (**what**, **why**, and **how**). To ease the task of reviewing commits, the message body should follow these guidelines:

1. **Leave a blank line between the subject and the body.**  
   This helps `git log` and `git rebase` work nicely and allows for smooth generation of release notes.
2. **Try to keep the subject line below 72 characters, ideally 50.**
3. **Capitalize the subject line.**
4. **Do not end the subject line with a period.**
5. **Use the imperative mood in the subject line** (e.g., `[BUG][module-x] Fix spacing not being considered`).
6. **Wrap the body at 80 characters.**
7. **Use semantic line feeds to separate different ideas, improving readability.**
8. **Be concise but thorough:** If significant alternative solutions were available, explain why they were discarded.
9. **Reference related discussions or issues:** If the commit refers to a topic discussed on the project forum or fixes a regression test, provide the link. If it fixes a compiler error, include a minimal verbatim message of the compiler error. If the commit closes an issue, use the [GitHub issue closing keywords](https://docs.github.com/issues/tracking-your-work-with-issues/linking-a-pull-request-to-an-issue).

Keep in mind that significant time is invested in reviewing commits and *pull requests*, so following these guidelines will greatly help the people doing reviews.

These guidelines are largely inspired by Chris Beams's [How to Write a Git Commit Message](https://chris.beams.io/posts/git-commit/) post.

#### Examples:

- **Bad:** `BUG: Check pointer validity before dereferencing`  
  *Implementation detail, self-explanatory by looking at the code.*
- **Good:** `[BUG][packages/slicer]] Fix crash when running Slicer`

- **Bad:** `ENH: More work in some package`  
  *"More work" is too vague;* 
- **Good:** `[ENH][packages/itk] Add gpu support`

- **Bad:** `COMP: Typo in cmake variable`  
  *Implementation detail, self-explanatory.*
- **Good:** `[COMP][package/numpy] Fix compilation error with Numpy`

### How to integrate a PR ?

Getting your contributions integrated is relatively straightforward, here
is the checklist:

* All tests pass
* Consensus is reached. This usually means that at least two reviewers approved
  the changes (or added a `LGTM` comment) and at least one business day passed
  without anyone objecting. `LGTM` is an acronym for _Looks Good to Me_.
* To accommodate developers explicitly asking for more time to test the
  proposed changes, integration time can be delayed by few more days.

* If you do NOT have push access, a Guix-Systole core developer will integrate your PR. 
