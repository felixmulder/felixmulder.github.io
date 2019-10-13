---
title: Haskell in Production
date: 2019-10-05
published: true
---

# Haskell in Production
During the past year, my team has been building production services using
Haskell. It's been quite a journey. None of us had written production code
using Haskell before. We were all familiar with the language and excited to
get to assess it in production. Because that's really what we've been doing;
seeing if Haskell is a good fit for us in order to deliver quickly and with
great quality.

## tl;dr
Haskell is great for business and great in production. Attracting talent has been
a joy, people are excited to work in Haskell. The libraries we've needed exist
in battle-tested form on Hackage/Stackage. The code we've produced has made it
easy to onboard new folks.

## The longer version
Knowing *exactly* how we wanted to write production services using this
language was not as straightforward.

We tried a lot of different patterns - readers, handlers, MTL, and tagless
final. You've surely heard of some of these, and they all have their own pros
and cons. So, which one should you choose?

That is why I've written this article series, to help you get a good sense of
how production Haskell is written at a company like Klarna and what to avoid
along the road.

## Existing parts in this series

* Part 0: Introduction (this page!)
* Part 1: Making components testable
<!-- * Part 2: Testing your components -->
<!-- * Part 3: Deploying your application -->

This section will be updated as more articles are published.

## Companion repo
For this series you can find the code that goes along with each part in the
[haskell-in-production](https://github.com/felixmulder/haskell-in-production)
repo on GitHub.

## Concerns when writing a production service
We have a couple of concerns that we would have no-matter which language we're
writing our service in.

* Multiple environments and regions
* Testability
* Performance & scalability
* Maintainability, logging, and traceability

When you're coming from object-oriented programming, you usually think of these
things in order to facilitate the above:

* Dependency injection
* Mocking

In functional programming, we have tecnhiques to cover both. Whereas these
frameworks often opt for meta-programming level annotations and reflection - FP
leverages language level features. This allows you to stay purely within the
language.

Below, I'll elaborate some more on the requirements posited above. If you want
to skip ahead to how we'll solve these concerns - please see [part
2](./2019/10/05-Haskell-in-Production.html) of this article series.

### Multiple environments
Klarna provides multiple environments for its services, we have (at least) the
following:

* Staging

  This environment doesn't really have an SLA, but you should expect other
  teams to write end-to-end tests against services that you deploy here.

* Performance

  This environment is used in order to do load testing, or stress tests on your
  service

* Production

  Perhaps obvious, but this is the service that actually serves live traffic.
  Depending on which team you're in you can have very strict requirements in
  your SLA, e.g. 5 nines up-time, latency requirements etc

### Multiple Regions
Some services are served in both the EU and US regions. For services in the US,
we cannot share any data to the EU. For the services that my teams write, we
have certain integrations in the EU - Swedish national bank, the European
Central bank. These of course should not be made available in the US.

### Testability
We need to be able to test different components in isolation. Some of these
components are pure or can be tested in a pure manner - if we mock out certain
dependencies.

### Maintainability
What is different here than for a regular service? Well, for starters it *is*
possible to write some really obscure code using a combination of Haskell and
compiler extensions.

As technical lead for our teams writing Haskell applications, it is my job to
make sure that others are productive. Thus, this article will emphasize
*writing easy to grok, maintainable code.*

## Proposed solution
We will write a configurable, testable service that deploys to a cloud
provider. The application will be shipped in a docker image and deployable
as such.

Next part [Designing Testable
Components](/writing/2019/10/05/Designing-testable-components.html)
