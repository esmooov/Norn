starter-kit
===========

A basic project template using Grunt and Bower.

Getting Started
---------------
 
You'll need `npm` for this tutorial. Install a grunt instance and plugins locally, as specified in `package.json`:

    npm install

You should now be able to build the project and run the test suite:

    npm test

You should see something like the following:

```
Running "clean:tests" (clean) task
Cleaning tmp...OK

Running "purescript:tests" (purescript) task
>> Created file tmp/tests.js.

Running "execute:tests" (execute) task
-> executing tmp/tests.js
The differences of an empty list are empty.
All tests passed
The differences of a single-element list are empty.
All tests passed
The differences of a pair of equal elements are zero.
All tests passed
The diffs function returns Just (...) for a sorted list.
All tests passed
The diffs function returns Nothing for a reverse-sorted list with at least one pair of unequal elements.
All tests passed
-> completed tmp/tests.js (50ms)

>> 1 file and 0 calls executed (60ms)

Running "purescript-make:lib" (purescript-make) task
>> Make was successful.

Done, without errors.
```

If you want to use `grunt` or `bower`, you can find them in `node_modules/.bin/`, or just install them globally.
