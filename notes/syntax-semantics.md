---
layout: default
title: Rei Structure and Semantics
---

Rei by default is highly flexible. It is mainly a "runtime language" that includes:

- duck typing, runtime checking of properties
- multiple dispatch / dynamic function overloading, runtime checking of an object's compatibility
- reification of everything in the language. Everything is real
- fail safe semantics. If a method cant be found, then in meta0, nothing really happens
- maximally configurable and extendable through formal means, softly and hardly through views and meta nodes
- highly flexible to optimisation based on formal operational and denotation semantics
- dataflow semantics and declarative semantics. Everything by default "runs..." concurrently

### Denotational Semantics

In rei, everything is real. Everything is of "type" Object in terms of its meta type.

Even a context itself is an object.

We can compose structural objects together to form more complex structural objects.
Then analyse its meaning.

There are ways to transform structural objects and syntax directly as well as meta objects... This is what allows rei to be highly flexible and strong.

