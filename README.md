# Cepl.Examples

Examples showing some aspects of the cepl library.

It's tricky to demo CEPL as it doesnt aim to dictate to heavily how you will do things, this means that things such as defining how you define/manage your 'entities' or even how you define a camera are pretty much left up to you.

To that end we have one possible camera implementation in `examples/helpers/camera.lisp`, it this is the one we use in many of the examples.

We also use skitter to manage our events, sdl2 as our host & classimp for model loading (sdl2 and classimp both require external c libraries)

It's a bit all over the place but will be cleaned up eventually.

# NOTE

The `master` branch of this repo should work with the CEPL from quicklisp. If you are have cloned CEPL locally then use the `cepl-master` branch.

This branch policy was decided on 2016-10-26. It may take some time to get everything cleared up. Please file and issue if something doesnt work as expected.
