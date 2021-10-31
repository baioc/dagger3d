# dagger3d

An [elmish](https://elmish.github.io/) raycaster engine using [Fable](https://fable.io/).

![Untextured](https://user-images.githubusercontent.com/27034173/139572752-a9e844c9-9baf-4426-be5f-440d241d1055.gif)


## Requirements

* [.NET Core SDK](https://www.microsoft.com/net/download/core) 5.0 or higher
* [Node.js](https://nodejs.org), which includes NPM


## Building and running the app

* Restore tools and install dependencies: `npm install`
* Start the compiler in watch mode with a development server: `npm start`. After the first compilation is finished, open [localhost:8080](http://localhost:8080) and (optionally) enable [Redux DevTools](https://chrome.google.com/webstore/detail/redux-devtools/lmhkpmbekcpmknklioeibfkpmmfibljd)
* To make a release build: `npm run build`. This will bundle all the F# code into one minified JavaScript file at `public/bundle.js`
