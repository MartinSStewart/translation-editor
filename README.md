# Translation Editor

This is a webapp [hosted here](https://translation-editor.lamdera.app) that loads the insurello/app code, parses out all the translations, displays them as a list that you can edit, and creates a pull request for any changes you made.

## Getting started

To run this locally, first download the Lamdera binary
```
curl https://static.lamdera.com/bin/osx/lamdera -o /usr/local/bin/lamdera
chmod a+x /usr/local/bin/lamdera
```
and then run `lamdera live` from this projects root folder. More information can be found on [this page](https://dashboard.lamdera.app/docs/download).

When running locally, the app is configured to use this repo https://github.com/MartinSStewart/translation-test. Feel free generate pull requests to it!

## How does Lamdera work?

https://dashboard.lamdera.app/docs