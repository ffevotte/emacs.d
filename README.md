# My Emacs settings

This is an ever-changing set of Emacs settings. These are more or less grouped in files:

- `init.el`: top-level file, does nothing but include others

- `init-std.el`: standard settings:
  - I think all these settings should appear in any standard configuration (kind of like a starter
    kit);
  - all these settings depend only on standard libraries (standard meaning included in Emacs or in
    the emacs-related debian packages, but nothing locally installed via Emacs' package management
    system).

- `init-extra.el`: these settings fall into two categories:
  - they are more personal and could likely break others' workflows
  - they depend on a non-standard package

- `setup-*.el`: if the configuration of a package takes more than a few lines, it is put in a
  dedicated file.
  
- `host-*.el`: host-specific settings
