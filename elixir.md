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
- Strange syntax:

```
-module(fact).
-export([fac/1]).

fac(0) -> 1;
fac(N) when N > 0, is_integer(N) -> N * fac(N-1).
```

---

## Platform Differences with Ruby

- Ruby has a global interpreter lock, Elixir is massively concurrent.
- Ruby has Objects, Elixir has Actors.
- Pattern matching, pipe operator, and guard clauses

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

## Syntax Differences: Pattern Matching

Ruby
```
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
```
def format_name(nil), do: "Please provide a name!"
def format_name(names) when is_array(names), do: "Hello, #{Enum.join(names, " ")}"
def format_name("Rob Andrews"), do: "Suh Dude"
def format_name(name), do: "Hello, #{name}!"
```

---

## Syntax Differences: Pipe Operator

Ruby
```
User
  .find(id)
  .update(email: new_email)
  .send_confirmation_email!
```

Elixir
```
User
|> Repo.find(id)
|> User.update_changeset(new_email)
|> Repo.update!()
|> Mailer.send(:confirmation)
```

???

- One side effect of not having objects is that we can't do
- Look at the composability - The Ruby code requires that the `User` class implements all those
  functions, while the Elixir module is explicitly calling functions from 3 different places.

---

## Syntax Differences: Destructuring

Ruby
```
def confirm_password(email, password)
  user = User.find_by(email: email)
  return false unless params[:email] && params[:password]
  user.check_password(params[:email], params[:password])    
end
```

Elixir
```
def password_ok?(%{"participant" => %{"email" => email, "password" => password}}) do
  Repo.get_by(Participant, email: email)
  |> check_password(password, "Invalid email/password")
end

def password_ok?(_), do: false
```

---

## It's Ruby Syntax and Productivity for Erlang

???

- Ruby, made for shell scripting, prefers readability and productivity over performance
- Erlang, made to be massively parallel and fault tolerant, but a bit rought around the edges

---

## Rails

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
- Does more than just REST: comes with primitives for channels and pubsub.

???

- Over 200 RPS on a raspberry pi [Source](https://mfeckie.github.io/Profiling-Phoenix-On-Raspberry-Pi/)
  while using only 80 MB of RAM.




