# My Portfolio

https://kushibikimashu.github.io/portfolio/

## Installation

`
$ git clone https://github.com/KushibikiMashu/portfolio.git
$ cd portfolio
$ mkdir portfolio && mv src assets portfolio
$ npm i
$ ./node_modules/elm/bin/elm reactor
`

Well done! Just access `http://localhost:8000/index.html`!

### Customize
After editing `src/elm/Main.elm`, remember to enter the command below.
`
$ ./node_modules/elm/bin/elm make src/elm/Main.elm --optimize --output=assets/js/main.js
`

Don't forget to compile when you customize `src/css/customized.css` or `src/js/tailwind.js`.
`
$ ./node_modules/.bin/tailwind build src/css/customized.css -c src/js/tailwind.js -o assets/css/styles.css
`

## Special Thanks 😂

Elm - A delightful language for reliable webapps<br>
https://elm-lang.org/

Tailwind CSS - A Utility-First CSS Framework for Rapid UI Development<br>
https://tailwindcss.com/docs/what-is-tailwind
