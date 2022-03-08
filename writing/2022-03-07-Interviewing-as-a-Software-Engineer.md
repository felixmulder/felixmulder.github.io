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

The qualifying interviews are intended to assess the candidate's probability of
success for the on-site interviews. In my experience, they're more brief but
roughly equal in difficulty to the on-site interviews.

## Interviews
Some of the interviews overlap in their rubric, for instance the programming
interviews are most often judged in similar fashions. I'll try to not repeat
myself too much below.

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

### Communication
This is not an interview itself, but it's a common to all interviews and by far
one of the most important skills you will be judged on as an engineer.

Whether the interview has you programming, sketching a system, or talking to
someone -- you will be judged on how you communicate. This is true both when
the interview is going well and when it is going poorly.

During the practical interviews, you're being judged on how you're approaching
the problem. Make sure you communicate, not just what you're doing, but what
you're thinking about doing next.

If you're stuck when debugging something, say that out loud: "I'm now stuck,
because I don't know X". Then proceed to let the interviewer know how you'll
try to forge ahead.

In all the places I've worked, candidates can receive a hire recommendation
despite bombing an interview, but that's prefaced on knowing why the candidate
did poorly during that phase of the process.

A good candidate doesn't just give great answers when prompted, but also asks
great questions of the interviewers. Some of the greatest interviews I've had,
the candidate has been in the driver's seat from getting their prompt - they're
asking all the questions and driving the problem forward.

Lastly, make sure that you're receptive to feedback and questions. If the
interviewer is hinting about something, hear them out. There's nothing worse
than an interview where the candidate is adamant about their solution. Most
interviews can be at least somewhat collaborative.

### Programming
This section covers both programming in the qualifying and during the on-site.

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
public static List<Color> parseColors(String input) {
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
without knowing if it'll compile. It puts these candidates at a disadvantage.
They'll have to spend a bunch of time fixing their mistakes before seeing if
the code actually does what it is supposed to.

Another consideration is just how testable your code is. Sometimes you'll be
asked to write a function that is side-effectful in nature. It requires access
to a randomness or perhaps reading something from the environment. This makes
testing your solution effectively, a lot harder. Consider creating an internal
function that you can delegate to:

```java
public static User createUser() {
  var uuid = UUID.randomUUID();
  return createuser(uuid);
}

static createUser(UUID uuid) { ... }
```

now you can test the package private one deterministically.

#### Language proficiency
Make sure you're able to fluidly write code in your chosen language. Most
companies would be happy for you to search the web for how to do things, that
is an important part of your real job.

That being said, you should probably not be searching for how to do basic things
in your language. You should know how to iterate through collections, how to use
common data structures like sets, maps, and lists.

Be up-to-date on the language of choice, but don't unnecessarily lean into
advanced features. If you choose Java, do you know about modern additions like
`var`? If you choose JavaScript, do you know how to destructure maps and lists?

You should also know how to test given your language of choice. That means
familiarizing yourself with unit testing frameworks.

### Chat with a manager
This is usually scheduled with the hiring manager, the person who'll likely be
your direct superior. During this interview you'll likely discuss your
experience at previous employers, your greatest accomplishments, biggest
regrets, as well as your future goals. Be ready to answer why you're looking
for a new opportunity.

Your answers don't have to be clever, especially when asked about regrets and
weaknesses. Be genuine. If you made a mistake, show that your learned something
from it. When describing your weaknesses, don't turn them into strengths. Show
that you're aware of them and let the interviewer know how you handle them.[^best-advice]

When discussing your past contributions, be concrete. Sometimes engineers who
do a lot of cross team collaboration and glue work, feel like their work wasn't
as important as the ones who wrote the code and will hand-wave. I say, it is as
important, own it!

This brings us to goals. For junior engineers, I know it's hard to be concrete
here. Here you've got to play the game, the interviewer has probably let you in
on what the team is about, be cognizant of that in your goals. Does the org or
team align with those?

I've sometimes seen people ask about mobility within the company. Sometimes
that's a great selling point, but bear in mind that the hiring manager is most
likely trying to fill a slot on _their_ team.

As an individual contributor, the manager is also looking to see that you're
easy to collaborate with. No matter how senior you are, show that you're
coachable.

### Debugging
I've seen a few versions of this. Spotify had an interesting one, where we
roleplayed that a service was misbehaving and it was up to me to diagnose it.

Others present you with some code where you attempt to diagnose, fix and verify
the issue.

The [Systems architecture](#systems-architecture) section has some overlap with
debugging a service, so I'll focus on debugging code in this section.

#### Approach
Fixing bugs is something every engineer does, even if it wasn't they who
introduced the error. The point of these types of excercises is to see how you
diagnose and fix issues. Sometimes you'll be given a failing test-case which
you can use as an entry point. Sometimes, you'll need to recreate the error
yourself. If you can, capture the issue in a unit test.

How you navigate the codebase is important here. Do you wildly grep and pray?
Or do you have a methodical approach to finding the root cause?

When you've an IDE where you can jump to definition, that's most commonly enough
to navigate the code. If you're using a dynamic language without such support,
grep might be justified.

Try to build an intuition for how the code is structured, and then build a
hypothesis. Once you have an idea, try to confirm it somehow. If it proves
false, abandon it and backtrack to the last place where you had a firm footing.

Don't try to read all of the code. Most likely if you're given a repo, it'll
be too large for you to be able to digest and fix the error within the alotted
time.

Making progress is key, most likely you won't have to solve the bug in order
to pass the interview. Build an intuition for the structure of the code come up
with a hypothesis, confirm or reject, iterate.

#### Proficiency with language and tools
I cannot recommend enough that you're strategic in your choice of language and
setup. I've seen candidates choose a language that they'd like to work in
(Haskell), and failed miserably because they're either not proficient enough in
it - or they're not able to get their setup to work for the interview. Most
commonly, people can't get the debugger to work properly, or the language server
doesn't work for one of the common toolchains (Stack vs Cabal, Maven vs Gradle).

Also, make sure you're able to build and modify sample projects. Clone something
from GitHub and try to build it, change some tests, run them, use the debugger.
These are basic things you should know how to do.

Learn how to set breakpoints, what stepping in, out and over mean, and how to
evaluate expressions.

If your language of choice has several package or build managers (yarn vs npm,
maven vs gradle, stack vs cabal), it's a good idea to be able to do the above
for all of them!

What about debug by print statements? That can be fine, make sure you're
methodical thee as well. Try to print relevant things, not just `"HERE"`!

### Systems architecture

[^covid]: Naturrally, during COVID-19 both parts of the interview process would
  be remote.

[^cracking-the-coding-interview]: As opposed to Google and Facebook where the
  challenge was indeed a [Cracking the Coding
  Interview](https://www.crackingthecodinginterview.com/) type challenge.

[^none]: None of these are the actual questions I've been asked, or I've given.
  However, the themes of them should be enough for you to gauge the sort of
  challenges.

[^best-advice]: One of the best pieces of advice I ever received was not to
  focus on eliminating my weaknesses, but to lean into my strengths.


