# Translation Editor

This is a webapp [hosted here](https://translations-editor.lamdera.app) that loads the insurello/app code, parses out all the translations, displays them as a list that you can edit, and creates a pull request for any changes you made.

## Typical workflow

If you (the programmer) have a bunch of translations in our Elm code that need translating then do the following:
1. Give the translator a link such as `https://translations-editor.lamdera.app/?branch=<YOUR BRANCH>`. Note the `?branch=<YOUR BRANCH>` part. This sets which branch the translator will make a pull request for.
2. The translator will need a Personal Access Token in order to login (unless they have a github account with access to our organization). You should sign into https://github.com/insurello-customer-service and click on your profile icon in the top right -> click settings -> Developer settings -> Personal access token -> Generate new token. In the select scopes field, make sure to check "repo".
3. Once the translator is done you should get a PR with the translations which you can review and then approve.

## Running locally

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
