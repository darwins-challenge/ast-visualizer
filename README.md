# ast-visualizer
An Elm project visualizing moonlander AST

## Getting started
### Node
This project uses [node][] to retrieve [Elm][elm] and automate certain tasks.
Execute the following command to get all the dependencies.

```sh
npm install
```

### Elm
#### Dependencies
Elm can use packages as well. In order to retrieve does run

```sh
npm run install
```

#### Reactor
To aid development Elm supports a rapid development cycle,
[Elm Reactor][elm-reactor]. Elm Reactor is a

> Interactive development tool that makes it easier to develop Elm programs. Key
> features include: 
>
>>    Automatically compile any Elm program
>>    Compatible with any editor
>
> This means you can get a great development experience whether you are using
> Sublime Text, Emacs, vim, or whatever else to edit Elm code. 

To start it run

```sh
npm run reactor
```

and open [http://localhost:8000/src/Main.elm](http://localhost:8000/src/Main.elm)

#### Make
To create a compiled version of all the Elm files run

```sh
npm run make
```

[node]: https://nodejs.org/
[elm]: http://elm-lang.org/
[elm-reactor]: https://github.com/elm-lang/elm-reactor
