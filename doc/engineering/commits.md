# Commits in KAZOO

This document is intended to describe what a good commit to master or release branches looks like and the git commands that are helpful in achieving this.

## General Development Guidelines

When starting new work, create a branch off the relevant parent branch (like `master`, `4.3`, etc). The branch should be named based on the ticketing / support system tracking the work (JIRA board, support system ticket ID, etc) such as `KAZOO-6120`:

    git checkout -b KAZOO-6120 origin/master

Now that you are on a branch, commence development as you normally would.

!!! note It is good practice to commit early and often as you go.

Commit messages on feature/bug branches are mostly for you, the developer, to record notes about what the changes mean as you go. We'll clean them up later into a more publicly consumable format.

### Development complete

Once you reach the point where you think a Pull Request is viable, run the relevant CI checks locally. Typically `make ci-codechecks ci-docs ci-dialyze` is a minimal effort to make.

Now comes the fun! Your commit history is probably littered with messages like `dialyzing` or `hmm does this work?` or `how did this ever work?`. Generally speaking, those aren't going to be too helpful for future readers of the code who might be `git blame`-ing to see why code is the way it is.

Instead, let's `git rebase` the branch, squashing all the commits but the first, and rewriting the commit message to something useful:

1. First, local the commit SHA of the first commit to your branch; this will be used to start the rebase from. In this branch's case, that SHA was `da72d28082`.
2. Start the interactive rebase: `git rebase -i da72d28082^`
  * You should see a list of your commits with a prefixed word `pick`. Leave the first commit (the SHA from step 1) as `pick` and change the others to `squash`. You will then get a screen in your editor with the opportunity to create the commit subject and message, with the commits you're squashing already included. This is your time to inform the reader of this pull request what the purpose of the work is and why you think the changes contained should be merged. Be as clear as possible on the `why`.
3. Now that you have a nice commit message and the code is complete (as far as you know), you are ready to push the branch up and open a pull request: `git push origin KAZOO-6120`

An example commit message:

```
KAZOO-6120: update documentation to establish guidance on git branches and commits

This change provides the reader with some example git commands for managing branches and commits within the KAZOO project. The goal of this document is to help contributors format their pull request commit to better serve future readers of the code.

Additionally, the doc will provide examples of rebasing the branch against the parent branch to ensure upstream changes are incorporated and conflicts handled.

Finally, the doc will include examples of how KAZOO committers can pull down a pull request and merge it to the parent branch locally, effectively emulating Github's squash-and-merge functionality.
```

#### Changes requested

After you've successfully pushed your branch and opened your pull request, you may need to make follow-up changes, either because CI errored on a step or a reviewer requested something. On your local branch, create the change and commit it with a nice commit message related to what prompted the change:

```
Changing log message re: review
```

```
Fixing failing test re: CI
```

This will make review easier to see what has changed since the review/CI process last ran. Once the PR is reviewed and ready for acceptance, you commit history may look like:

```
84a8e54e14 * fixing up review commits
a29967517d * add more steps
e15fb20c63 * KAZOO-6120: document a bit about git and commit hygiene
```

This is okay as the merging will squash this down.

## Merging pull requests locally

Using the Github UI does not allow you to merge PRs in with a verified commit. It is possible to achieve this locally and have Github reflect the merged status, as well as having a signed commit in master (or the relevant feature branch).

0. Fetch upstream: `git fetch origin`
1. Fetch the PR branch: `git checkout -b KAZOO-6120 origin/KAZOO-6120`
2. Rebase the commit against the latest master to be sure it incorporates those changes: `git rebase origin/master`
3. If there is more than one commit on the branch, perform an interactive rebase as above, squashing all but the first commit: `git rebase -i da72d28082^`.
4. Edit the commit message to append the Github pull request ID to the commit subject:
   `KAZOO-6120: document a bit about git and commit hygiene` => `KAZOO-6120: document a bit about git and commit hygiene (#9988)`
5. Push the modified branch back up to Github: `git push -v --force-with-lease origin KAZOO-6120:refs/heads/KAZOO-6120`
   Because the SHA has changed, if you merged the branch into the parent now, Github would not recognize the merge commit and would re-evaluate the branch based on the new parent HEAD. Instead, we force-push the local branch back up, overwriting the existing upstream branch with our modified single-commit branch.
   Depending on how far behind the branch was with the parent, you may delay moving to the next step until a CI pass has completed.
6. Merge the local branch into the parent:
   ```
   git checkout master
   git merge --no-edit KAZOO-6120
   ```
   At this point, your parent branch should be one commit ahead of the upstream parent branch. If you are signing commits (and you should be) this commit will be signed by your key as well.
7. Push the parent branch up: `git push -v origin refs/heads/master:refs/heads/master`
   If you have the Github PR UI open you should see the PR go from `open` to `merged`.
8. Last step is to delete the local and remote feature branches:
   ```
   git branch -d KAZOO-6120
   git push origin :KAZOO-6120
   ```

All cleaned up and a shiny new verified commit appears in Github's commit history for the branch!
