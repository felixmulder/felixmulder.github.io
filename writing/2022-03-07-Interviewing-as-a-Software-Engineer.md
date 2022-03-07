---
title: "Interviewing as a Software Engineer"
date: 2022-03-07
published: false
summary: >
  Interviewing is hard. In different ways for both parties. In this
  post, I describe my thoughts on how to do well in SWE interviews
  both as a candidate and as the hiring party.
---

# Interviewing as a Software Engineer
Interviewing is hard. In different ways for both parties. On the hiring side,
you need to have a good framework for evaluation in place, and make sure that
your engineers understand and apply that effectively. Failure to do so leads to
bias, indecision and ambiguity in decisions. Which isn't great for either
party.

If you're on the hiring side, your interview process is not just a means for
you to hire -- it's also an effective way to advertise for your company's
brand. No matter the outcome of the process, the candidate should be walking
away with a great impression of your engineers, your hiring process, and your
company.

## Intended audience and scope
I'm mostly writing this post for my brother who's graduating soon. It should
be useful for anyone looking to land a job at FAANG or other top-tier tech
companies.

## The interview process at top tier companies
Among the companies I've interviewed and landed offers from, their interview
processes look roughly the same. The process is split between a qualifying and
an on-site portion.[^covid]

#### Qualifying interviews
1. Chat with a recruiter
1. Programming
1. Chat with manager

#### On-site interviews
- Programming
- Debugging
- Systems architecture
* Career

These do differ slightly between companies. E.g. some companies don't have the
manager chat in the qualifying round of interviews, and some have several
programming challenges during the on-site.

## Qualifying interviews
These interviews are intended to assess the candidate's probability of success
for the on-site interviews. In my experience, they're more brief but roughly
equal in difficulty to the on-site interviews.

### Chat with a recruiter
The initial step of the process is a chat with a recruiter. Most times they'll
ask you about your past experiences, and why you're looking for a new
opportunity. They're also looking to see that you would fit into the culture at
the company you're applying to.

This interview is all about making a good impression as a person. Even though you
might not be applying to a new job for a positive reason, you have to play the
game and turn that into a positive, or leave it out:

- 👎 My current role isn't fun

  👍 The challenges your company is facing are exciting
- 👎 I don't like my current boss / company

  👍 I think that your company has great engineering culture
- 👎 I'm not getting that promotion

  👍 I think I can grow well in your organization

Make sure you let them know you'd be excited to work for them! Barring anything
strange popping up, the recruiter screen mostly feels like a sanity check before
kicking off the real process.

### Programming
The programming challenge is different depending on organization. Apart from
Google and Facebook, most companies I've interviewed for have a simple
programming challenge[^cracking-the-coding-interview].

At a former employer, this was a take-home challenge that you solved in an
online editor where you write and run tests. If you passed the take-home
challenge, you'd discuss the solution with an engineer, possibly make some
minor improvements, and then solve an extension to the exercise.

At other places, the coding challenges have all been done in
[CoderPad](https://coderpad.io/). It'll be very much worth your time to try out
the platform. Make sure you're familiar with how to both write code _and tests_
using their editor.

Some places allow you to use your own setup. If you plan on using your own
setup, make sure you're able to both run code, tests, and use a debugger.

When it comes to the type of challenges, I'll give a couple of examples of
things that could pop up[^none]:

- Write a function to get the two largest elements of a list
- Write a caching layer to amortize an expensive operation
- Parsing strings and returning structured data

From the hiring side, when giving problems like these, what do you look for?

#### Structure both in terms of code and approach
Most of these interviews contain some form of problem statement for the
challenge, usually as a short text snippet. These problem statements are
rarely complete **don't jump in straight away**. Just like in real life, you'll
need to **ask questions** in order to figure out the invariants. Maybe you can
even skip some portions that you originally envisioned you had to provide. Note
and ask about edge cases.

When you start implementing the code, think about the function, its arguments and
return value before writing any implementation. E.g. in Java

```java
static List<Color> parseColors(String input) {
  // TODO - implement
  return List.of();
}
```

or Python:

```python
def parse_colors(input: str) -> list[Color]:
  # TODO - implement
  return []
```

that way you can already now scaffold a simple hello-world test. Does the given
prompt contain any examples for expected output? Encode those as tests too!

It's not important that you know the stdlib APIs of your language, but make
sure that you're familiar with common operations. Most importantly you should
be able to write idiomatic and well-structured code.

Once you're done with your initial take, go over it. Did you miss anything?
Should you improve the phrasing anywhere? Are there more edge-cases? Have you
tested all examples?

Don't be afraid to run your code often, this exercise is supposed to show the
interviewer how you work in your day to day. Would you write all the code
before hitting run or compile? Or would you do it piecemeal? Probably the
latter unless you use a very sophisticated editor.

In none of the places where I've interviewed candidates have we "deducted
points" for when the program doesn't compile or give the expected output on the
first try. It _is_, however, quite common that I see folks writing _a lot of code_
without knowing if it'll compile. That puts these candidates at a disadvantage.
They'll have to spend a bunch of time fixing their mistakes before seeing if
the code actually does what it is supposed to.

#### Proficiency with language and tools

#### Communication

### Chat with a manager

## On-site interviews

[^covid]: Naturrally, during COVID-19 both parts of the interview process would
  be remote.

[^cracking-the-coding-interview]: As opposed to Google and Facebook where the
  challenge was indeed a [Cracking the Coding
  Interview](https://www.crackingthecodinginterview.com/) type challenge.

[^none]: None of these are the actual questions I've been asked, or I've given.
  However, the themes of them should be enough for you to gauge the sort of
  challenges.
