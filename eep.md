---
layout: markdown
title: Erlang Enhancement Process
---
# Erlang Enhancement Process

### What is the EEP?
There are always many discussion threads on [erlang-questions](mailto:erlang-questions@erlang.org) and other places about enhancements, changes and extensions to the Erlang runtime system and the language.

To document the various proposals and the decisions taken we use the Erlang Enhancement Process inspired by the Python Enhancement Process. [EEP 0] contains a listing of all submitted EEPs.

Because Erlang is a programming language with a few million lines of running business critical code in the world, the development process must impose some rigidity and provide resistance against accepting premature changes. Users have Erlang code, linked in drivers written in C, and applications that embed Erlang, so it is important that the inconvenience of upgrading to new versions of Erlang is minimized. Language changes might also make the language more difficult for new users to learn.

To ensure that changes are carefully considered, significant changes must be described in an EEP, short for Erlang Extension Proposal. Each EEP should explain, among other things, why the change is needed, document how it should work, and give an overview how it should be implemented. The EEP author should listen to the community's feedback and edit the EEP as necessary.

Before submitting an EEP; read [EEP 1 - EEP Purpose and Guidelines] that thoroughly explains the purpose of EEPs, their life cycle, and prescribed format. The EEP editor will reject EEPs that do not follow the guidelines. Also check the list of existing [EEPs] before reinventing something.

[EEP 1 - EEP Purpose and Guidelines]: {% link _eeps/eep-0001.md %}
[EEPs]: {% link eeps.html %}
[EEP 0]: {% link eeps.html %}

### EEP Editorial
There is a mailing list for EEPs: eeps (at) erlang (dot) org, see [Mailing Lists](../community/mailinglists). Anyone interested can subscribe to the list. The EEP editor(s) are subscribed to the list, as well as the EEP repository owner. New EEPs should be sent here, as well as EEP updates. Note that as with all [erlang.org](http://www.erlang.org/) mailing lists only subscribers are allowed to post to the list.

### Erlang Extension Proposal repository
The EEP source texts are in a Github GIT version control [repository](http://www.github.com/erlang/eep). Anyone with a Github account can fork the repository, add or change any EEP, inform the [eeps](mailto:eeps@erlang.org) mailing list about the new version, and the origin repository owner will (if approving) pull the commit.