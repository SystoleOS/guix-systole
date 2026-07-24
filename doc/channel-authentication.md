# Channel authentication

Guix channels can be cryptographically authenticated: every commit is
signed by an authorized key, and users pull with a *channel
introduction* (first authorized commit + signer fingerprint) that lets
`guix pull` verify the whole history.  For a medical-imaging OS channel
this is a headline supply-chain property — without it, anyone who can
push (or MITM the transport) can change what every deployment builds.

The repository already carries the two required files:

- `.guix-authorizations` — the set of OpenPGP keys allowed to sign
  commits (currently the fingerprint of Rafael Palomar's key
  `6513 C724 8D7B ECE2 EC1B D34B 7035 0DAD 507F A72F`; edit if the
  channel should be signed by a dedicated project key instead).
- This runbook.

**ACTIVATED 2026-07-24**: introduction commit
`0898d41446ecbd6eddcbf5d78beae63f7e8a4069`, fingerprint
`53A2 D043 E0DC 9249 DF75  ABB6 65D8 96E0 0C10 1DDF` (the ed25519 [SA]
signing subkey -- guix authenticates against the fingerprint of the key
that actually signs, i.e. the SUBKEY, not the primary; both signing
subkeys are authorized in `.guix-authorizations`, the primary key is
published on the `keyring` branch).  `guix git authenticate` passes and
its pre-push/post-merge hooks are installed in maintainers' clones.
The runbook below is kept for reference and for key additions/rotations:

## Activation runbook

1. Decide which key signs the channel.  A personal key works; a
   dedicated project key (kept offline, subkeys for signing) is better
   for bus-factor and rotation.  Update `.guix-authorizations` if the
   choice differs from the current entry.

2. Publish the public key.  Convention is a `keyring` branch holding
   the armored export:

   ```sh
   gpg --export --armor 6513C7248D7BECE2EC1BD34B70350DAD507FA72F > rafael.key
   git checkout --orphan keyring
   git rm -rf . && git add rafael.key && git commit -m "Add rafael's key"
   git push origin keyring
   git checkout main
   ```

3. Enable commit signing for this repository:

   ```sh
   git config user.signingkey 6513C7248D7BECE2EC1BD34B70350DAD507FA72F
   git config commit.gpgsign true
   ```

4. Make the **introduction commit**: the first *signed* commit that
   contains `.guix-authorizations`.  Any signed commit touching the
   file after signing is enabled qualifies:

   ```sh
   git commit --allow-empty -S -m "[ENH][channel] Introduce channel authentication"
   INTRO=$(git rev-parse HEAD)
   gpg --fingerprint 6513C7248D7BECE2EC1BD34B70350DAD507FA72F
   ```

5. Publish the introduction in `README.md` and everywhere the channel
   is configured, e.g.:

   ```scheme
   (channel
    (name 'guix-systole)
    (url "https://github.com/systoleos/guix-systole")
    (branch "main")
    (introduction
     (make-channel-introduction
      "<INTRO commit hash>"
      (openpgp-fingerprint
       "6513 C724 8D7B ECE2 EC1B  D34B 7035 0DAD 507F A72F"))))
   ```

   Update: `channels-lock.scm`, `%guix-systole-channel` in
   `systole/systole/transformations.scm` (it currently installs the
   channel *without* an introduction, unlike the nonguix entry next to
   it), the ivs-infrastructure Cuirass specification, and the docs.

6. From that commit on, **every** commit to `main` must be signed by an
   authorized key, including merge commits.  CI note: GitHub's
   squash/rebase buttons produce commits signed by GitHub's key (not
   authorized) — merge locally and push, or add a CI check that
   `guix git authenticate` passes:

   ```sh
   guix git authenticate $INTRO \
     "6513 C724 8D7B ECE2 EC1B  D34B 7035 0DAD 507F A72F"
   ```

7. Key management: to add a signer, add their fingerprint to
   `.guix-authorizations` in a commit signed by an *already-authorized*
   key (and export their key to the keyring branch).  Removal works the
   same way.  There is no revocation of history — protect the private
   key accordingly.

## Why not activated automatically

Choosing the signing identity and taking on the "every commit signed
forever" commitment is an operator decision; a half-activated channel
(introduction published, then unsigned commits pushed) breaks `guix
pull` for everyone who configured the introduction.  Activate when
ready, then never push unsigned again.
