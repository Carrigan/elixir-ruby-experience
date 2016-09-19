# Lunch With Elixir

---

## Agenda

- The Elixir Language
- The Phoenix Framework
- My Experience So Far

---

## Elixir

- Elixir is a dynamic, functional language designed for building scalable and maintainable applications.
- Compiled language that runs on the Erlang VM

---

## Erlang

- Developed in the 80's by Ericsson to improve telephony applications.
- Open sourced in 1998.
- Didn't catch on much from its strange syntax:

```erlang
-module(fact).
-export([fac/1]).

fac(0) -> 1;
fac(N) when N > 0, is_integer(N) -> N * fac(N-1).
```

---

## Platform Differences with Ruby

- Ruby has a global interpreter lock, Elixir is built for concurrency.
- Ruby has Objects, Elixir has Actors.
- Let it Crash Attitude
- A lot of great language features.

???

- So in Ruby, objects and data are all mutable. You can say `user.name = "Brian"` and your `user`
  object just changed, and it changed everywhere that references user. This makes concurrency really
  hard since modifying `user` in one thread could cause problems for another thread reading from `user`.
  Because of this, Ruby has what is called a Global Interpreter Lock (GIL) which prevents multiple
  threads running
- In Elixir, on the other hand, data is immutable so it can be passed around processes and you know
  that it won't break. The Erlang VM also provides the idea of a `Process`, which acts somewhat like
  a thread but takes up far fewer system resources.
- Actors are processes that talk to each other through messages. Actors are watched over by a supervisor,
  and if any actor fails the supervisor is able to bring them back online. Compare this to Ruby where
  if my Rails project imports a Gem and that gem errors out, my Rails application is going to be what
  fails.
- Syntax looks very similar, but there will be immediate differences.

---

## Immutability and State

- Imperative languages encourage global state.
- OO languages encourage encapsulated state.
- Erlang encourages that state be its own process.

???

- In the beginning of time, state was held globally. The problem is that unrelated processes can
  modify global state without other consumers having any idea. When there is a bug in these operations,
  they can even overwrite things they shouldn't be acting on. This is why some Toyota cars occasionally
  accelerate out of control and cause accidents.
- OO languages were pretty much invented to encapsulate that state. Instead of a global
  `ACCELERATOR_PRESSED` boolean, we built an `Accelerator` object and gave it a `.pressed` field.
- While much better than Imperative languages, this OO model is still nearly impossible to use in
  a multithreaded environment because functions can have side-effects. If two threads are accessing
  the same data while its being manipulated, bad things can happen.
- But, state is essential to write programs and side-effects are inevitable. Most of the time you
  can pass around immutable objects and be totally fine, but what if you want to keep track of how
  many users are on your site right now? Elixir's answer to this is to create a full process, or server,
  that holds that state.
- This is a really bizarre concept at first, but really its pretty similar to having a bunch of Redis
  or MongoDB databases in your application keeping state. This also eliminates the need for extra
  programs like Redis because it is built into the language.

---

## Error Handling

- Imperative languages handle errors... poorly.
- OO languages handle errors using rescues.
- Erlang watches the world burn and remembers how to rebuild it.

???

- Imperative languages sucked at errors. There was no exception model so when things crash in C, they
  crash really hard. The best way to handle this was to have to home bake error types, but the code
  became quite cumbersome.
- OO languages tried to fix this by implementing error handling by allowing functions to rescue
  any errors thrown in them. This cleaned up a lot of the handling in imperative languages, but gives
  a false confidence. If the API for your Mailgun client can fail by throwing 8 different errors,
  who here honestly writes a catch statement for every one of them?
- Erlang takes a different approach and says that we as programmers are not perfect; instead of
  trying to handle every single failure type, let the current part of the project fail and we can
  rebuild it.

---

## Language Features: Pattern Matching

Ruby

```ruby
def format_name(name)
  if name.nil?
    "Please provide a name!"
  elsif name.class == Array
    "Hello, #{name.join(' ')}"
  else
    "Hello, #{name}!"
  end
end
```

Elixir

```elixir
def format_name(nil), do: "Please provide a name!"
def format_name(names) when is_array(names), do: "Hello, #{Enum.join(names, " ")}"
def format_name("Rob Andrews"), do: "Suh Dude"
def format_name(name), do: "Hello, #{name}!"
```

---

## Language Features: Pipe Operator

Ruby

```ruby
User
  .find(id)
  .update(email: new_email)
  .send_confirmation_email!
```

Elixir

```elixir
User
|> Repo.find(id)
|> User.update_changeset(new_email)
|> Repo.update!()
|> Mailer.send(:confirmation)
```

???

- One side effect of not having objects is that we can't do the nice chains like in JS and Ruby.
- Look at the composability - The Ruby code requires that the `User` class implements all those
  functions, while the Elixir module is explicitly calling functions from 3 different places.

---

## Language Features: Destructuring

Ruby
```ruby
def confirm_password(params)
  return false unless params[:email] && params[:password]
  user = User.find_by(email: params[:email])
  user.check_password(params[:password], "Invalid email/password")    
end
```

Elixir
```elixir
def password_ok?(%{"email" => email, "password" => password}) do
  Repo.get_by(Participant, email: email)
  |> check_password(password, "Invalid email/password")
end

def password_ok?(_), do: false
```

---
class: center, middle

# It's Ruby Productivity for Erlang

???

- Ruby, made for shell scripting, prefers readability and productivity over performance
- Erlang, made to be massively parallel and fault tolerant, is a bit rough around the edges

---
class: center, middle

# But... Rails

???

- So Elixir is performant and has some really cool features, but we are an agency and Rails is really
  productive and prolific.
- When I fell in love with Clojure, this was what kept me from really diving in was that their web
  frameworks required lots of up front work and decisions about things I just wanted to work. I
  remember spending hours trying to find a library to help with database migrations and it required
  lots of manual setup and in the end didn't work with the mapping library I was using.

---

## Phoenix Framework

- A productive web framework that implements the server-side MVC pattern.
- Familiar syntax for the MVC components, migrations, and generators.
- Does more than just REST: comes with primitives for channels, pubsub, and more.

---
class: center

# But Really Freakin' Fast

???

- In my experience, the routing and rendering takes ~500us, everything else is database.
- Database is significantly faster; loads can happen in parallel. I stress tested the voting app once
  and was able to load 5,000 objects, run them through a service, and render the outcome in ~130ms.
- A single digital ocean server with 2 million connections
- Over 200 RPS on a raspberry pi [Source](https://mfeckie.github.io/Profiling-Phoenix-On-Raspberry-Pi/)
  while using only 80 MB of RAM.
- People have reported nginx being the bottleneck.

---

## Differences: Ecto

Ruby
```ruby
user = User.find(1)
user.name = params.require(:name)
if user.save
  render 200, user
else
  render 400, user.errors
end
```

Elixir
```elixir
changeset = Repo.get!(User, 1)
  |> User.name_changeset(params)

cond Repo.update(changeset) do
  {:ok, user} -> render 200, user: user
  {:error, changeset} -> render 400, errors: changeset.errors
end
```

???

- ActiveRecord models in Rails are extremely mutable. In this small snippet that you would likely
  find in a controller, there are two places where user is modified in place.
- The controller is also responsible here for filtering the parameters that can be accepted by the
  model.
- Compare this with the same Elixir controller code. Since we can't make our changes as side effects,
  Ecto has the concept of a `Changeset`, which is essentially a list of changes being applied and
  rules about whether the values supplied are OK.

---

## Differences: Ecto

```elixir
def name_changeset(struct, params) do
  struct
  |> cast(params, [:name])
  |> validate_required([:name])
end
```

???

- The Ecto changeset is responsible for making sure types are correct, casting those values, and
  doing validations. So you have things from Rails' models and controllers in one place. Really what
  we have is a really lightweight Rails service here.

---

## Differences: Testing

Ruby
```ruby
expect(object).to receive(:function).once
```

Elixir
```elixir
# In the config
config :dihi_backend, :auth_module, DihiBackend.MockAuth

# The Mock
defmodule DihiBackend.MockAuth do
  def logout(conn) do
    send self(), :logout
    DihiBackend.Auth.logout(conn)
  end
end

# In your test
assert_received :logout
```

???

Ruby
- Here is a typical mock test in Ruby, which are really nice to testing that APIs are being called
  rather than testing what they do. Under the covers, this works by modifying the class or object
  globally, in place, to have state associated with it about how many times it has been called.
  Thus, we cannot do this in Elixir; and since there is no way to escape that mocks are a global
  state, it is much more in depth.

Elixir
- What you wind up doing is mocking the entire contract in roughly the same way, except instead of
  the object or class keeping track of the calls, the test itself does.
- This allows multiple tests to run at the same time since the object/class does not keep the state.
- Significantly more setup, but you gain testability in parallel.

---

## Differences: Testing

```elixir
describe "GET / when logged in" do
  setup %{conn: conn} do
    admin = Repo.insert!(admin)
    conn = conn |> Auth.sign_in(admin)
    %{:ok, conn: conn, admin: admin}
  end

  test "something" %{conn: conn, admin: admin} do
    assert ...
  end
end
```

???

- One thing that I don't like is that there are no contexts, let statements, or nesting in ExUnit.
- You can set things up like let statements in your setup block, but then you have to reference them
  in every single test case, which is annoying (especially when you have lots of variables).

---

## Differences: Tooling

- Much less mature ecosystem; there is a gem for nearly everything
- Gems like Devise cut developer time on low-value items like password reset and email confirmation.
- "Phoenix is not your app"

???

"Phoenix is not your app" -> You can much easier build umbrella apps, applications with non-http
services, much easier with Phoenix since it is part of the ecosystem.

---
class: center

## The Right Tool for the Job
### The tradeoff: productivity for reliability and speed

???

So the real question is- would I use this on another project. And the answer is 100% absolutely,
given that this is the right tool for the job. There are a ton of benefits in reliability, speed,
and performance that could be gained, but there is a very real tradeoff of less productivity than
Rails.

The truth is that a lot of our apps don't need to serve 10,000 requests per second. They need to
fill all the client's requirements for the lowest amount possible while being fast enough, safe,
and maintainable. Projects that last only a few weeks and are for a very niche crowd would not
benefit at all from this. Projects like WeaveUp however could have benefitted tons from the Elixir
ecosystem because their test suite now takes 20+ minutes to run, their app requires 5 servers, and
they are now somewhat stuck with a monolithic application that would be really hard to refactor out.

---

## I would use Elixir again if...

- You want any form of web sockets or non HTTP protocol (TCP, CoAP, etc).
- Your application is heavy on delayed jobs, Redis, or async calls.
- Your application needs to be distributed in any way.
- Your application has a low hardware cost or high scalability requirement.

---
background-image: url(phoenix.png)
