# Notifier

A simple tool for creating `libnotify` style notifications when a new rss feed
item has been received.

## Running

Right now the application only supports manual invocation. Automated polling
might be implemented at some point but not yet. The easiest way to
automatically invoke this tool is by adding a systemctl timer.

### Configuring

The application expects to find `~/.config/notifier/config.dhall` file with a format like below:

```
{ feeds = ["https://github.com/MasseR/notifier/commits/master.atom"] }
```
