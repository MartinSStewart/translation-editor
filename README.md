# Translation Editor

This is a webapp [hosted here](https://translations-editor.lamdera.app) that loads the insurello/app code, parses out all the translations, displays them as a list that you can edit, and creates a pull request for any changes you made.

If you'd like to have a translator make changes to a specific branch create a url that looks like this: https://translations-editor.lamdera.app/?branch=sign-box

## Getting started

To run this locally, first download the Lamdera binary
```
curl https://static.lamdera.com/bin/osx/lamdera -o /usr/local/bin/lamdera
chmod a+x /usr/local/bin/lamdera
```
and then run `lamdera live` from this projects root folder. More information can be found on [this page](https://dashboard.lamdera.app/docs/download).

On the login screen, you'll need to use a personal access token.
To do that, go to github -> click on your profile icon in the top right -> click settings -> Developer settings -> Personal access token -> Generate new token. In the select scopes field, make sure to check "repo".
Give the token a name and the press "Generate token".
Then finally you can copy the key it generates and paste it into the app.

When running locally, the app is configured to use this repo https://github.com/MartinSStewart/translation-test. Feel free generate pull requests to it!

## How does Lamdera work?

https://dashboard.lamdera.app/docs
