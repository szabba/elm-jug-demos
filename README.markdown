# Elm JUG Demos (2018-03-28)

This is a series of demos for the upcoming Zielona Góra Java User Group meeting
about Elm.

To install the dev tools, run
```bash
$ yarn install
```

To install the elm dependencies, run
```bash
$ yarn run elm package install
```

To view the demos, run
```bash
$ yarn run elm reactor
```
and visit [`http://localhost:8000/index.html`](http://localhost:8000/index.html).

# To do

* [x] Write the Stoper example.
* [ ] Write the Values example.
    * Initially, the page contains a button with the question "What is best in
      life?".
    * When the user clicks it, a form opens.
    * The user chooses values to add from a drop-down.
    * The values include: Challenge, Comfort, Community, Freedom, Mastery, Status.
    * The user can remove a selected value.
    * The user gets a slider + input field to set how much they care about each
      of the selected values.
    * If a value is outside the 1-100 range, there is an error message.
    * If the values do not sum up to 100, there is an error message.
    * The user cannot submit the values unless there are no error messages.
    * The user can always discard the changes made to the form and go back to
      previous state.
    * If the user succesfully submits their answer, they get a non-editable
      view of it + a "Are these best in life?" button that returns to the form.
* [ ] Cover the Values example with tests.