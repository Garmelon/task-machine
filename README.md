# task-machine

A TUI client for the [todo.txt](https://github.com/todotxt/todo.txt) format, written in Haskell,
that supports automatically creating new tasks based on template tasks.

For an introduction to the file format, see the [todo.txt readme](https://github.com/todotxt/todo.txt/blob/master/README.md).

## Template tasks

Template tasks allow for automatically creating new tasks on certain dates.

They work by evaluating a formula for the current day.
If the formula evaluates to `true`, a new task gets added to the current day using the template text specified.
If it evaluates to `false`, nothing happens.

When creating a template task, a start date can be specified.
In that case, the formula will be evaluated for every day from the starting date to the current date.

Once all tasks are added, the starting date will be set to one day after the current date automatically,
to avoid evaluating any day twice, even if no starting date was specified initially.

Template tasks are inspired by Ben Crowell's [when](https://github.com/bcrowell/when) calendar program.

### Format

This is the format for template tasks.
The square brackets `[]` indicate an optional part of the syntax.
The curly braces `{}` are part of the syntax.

`# [start-date] {when-formula} [priority] template-text`

- The `#` signals that this line is a template task, not a normal task.

- *Optional*: All days from `start-date` to the current date are tested when creating new tasks.
It should be in the `YYYY-MM-DD` format.
If no `start-date` is specified, it is assumed to be the current date.

- The `when-formula` is evaluated in order to decide whether a new task should be created for a day.

- *Optional*: The `priority` is a regular todo.txt priority, for example `(E)`.

- The `template-text` is pretty much a normal task description.
The only difference is that it supports date expressions.

#### Examples

`# 2018-09-08 {day_of_week == fri} clean the kitchen @home`

`# {d2018-10-20 - today >= 0} (A) daily every day up to (and including) 2018-10-20 with priority A`

`# {day == 20 && month == 10} Tom's birthday ({year - 1978} years old)`
(if Tom was born on 1978-10-20)

### Date expression

***TODO***: Document properly once it's actually implemented

- *date* + - *number* → *date*
- *date* - *date* → *number*
- *number* + - \* / *number* → *number*
- *number* == != > < >= <= *number* → *boolean*
- *boolean* && || == != *boolean* → *boolean*

### When formula

### Template text
