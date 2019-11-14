---
title: Database Access and Persistence
---

The
[Database section on the Awesome-cl list](https://github.com/CodyReichert/awesome-cl#database)
is a resource listing popular libraries to work with different kind of
databases. We can group them roughly in four categories:

- wrappers to one database engine (cl-sqlite, postmodern, cl-redis,…),
- interfaces to several DB engines (clsql, sxql,…),
- persistent object databases (bknr.datastore (see chap. 21 of "Common Lisp Recipes"), ubiquitous,…),
- [Object Relational Mappers](https://en.wikipedia.org/wiki/Object-relational_mapping) (Mito),

and other DB-related tools (pgloader).

We'll begin with an overview of Mito. If you must work with an
existing DB, you might want to have a look at cl-dbi and clsql. If you
don't need a SQL database and want automatic persistence of Lisp
objects, you also have a choice of libraries.


# The Mito ORM and SxQL

Mito is in Quicklisp:

~~~lisp
(ql:quickload :mito)
~~~

## Overview

[Mito](https://github.com/fukamachi/mito) is "an ORM for Common Lisp
with migrations, relationships and PostgreSQL support".

- it **supports MySQL, PostgreSQL and SQLite3**,
- when defining a model, it adds an `id` (serial primary key),
  `created_at` and `updated_at` fields by default like Ruby's
  ActiveRecord or Django,
- handles DB **migrations** for the supported backends,
- permits DB **schema versioning**,
- is tested under SBCL and CCL.

As an ORM, it allows to write class definitions, to specify
relationships, and provides functions to query the database.  For
custom queries, it relies on
[SxQL](https://github.com/fukamachi/sxql), an SQL generator that
provides the same interface for several backends.

Working with Mito generally involves these steps:

- connecting to the DB
- writing [CLOS](clos.html) classes to define models
- running migrations to create or alter tables
- creating objects, saving same in the DB,

and iterating.

## Connecting to a DB

Mito provides the function `connect-toplevel` to establish a
connection to RDBMs:

~~~lisp
(mito:connect-toplevel :mysql :database-name "myapp" :username "fukamachi" :password "c0mon-1isp")
~~~

The driver type can be of `:mysql`, `:sqlite3` and `:postgres`.

With sqlite you don't need the username and password:

~~~lisp
(connect-toplevel :sqlite3 :database-name "myapp")
~~~

As usual, you need to create the MySQL or PostgreSQL database beforehand.
Refer to their documentation.

Connecting sets `mito:*connection*` to the new connection and returns it.

Disconnect with `disconnect-toplevel`.

=> you might make good use of a wrapper function:

~~~lisp
(defun connect ()
  "Connect to the DB."
  (connect-toplevel :sqlite3 :database-name "myapp"))
~~~

## Models

### Defining models

In Mito, you can define a class which corresponds to a database table
by specifying `(:metaclass mito:dao-table-class)`:

~~~lisp
(defclass user ()
  ((name :col-type (:varchar 64)
         :initarg :name
         :accessor user-name)
   (email :col-type (or (:varchar 128) :null)
          :initarg :email
          :accessor user-email))
  (:metaclass mito:dao-table-class))
~~~

Note that the class automatically adds some slots: a primary key named `id`
if there's no primary keys, `created_at` and `updated_at` for
recording timestamps. To disable these behaviors, specify `:auto-pk
nil` or `:record-timestamps nil` to the defclass forms.

You can inspect the new class:

~~~lisp
(mito.class:table-column-slots (find-class 'user))
;=> (#<MITO.DAO.COLUMN:DAO-TABLE-COLUMN-CLASS MITO.DAO.MIXIN::ID>
;    #<MITO.DAO.COLUMN:DAO-TABLE-COLUMN-CLASS COMMON-LISP-USER::NAME>
;    #<MITO.DAO.COLUMN:DAO-TABLE-COLUMN-CLASS COMMON-LISP-USER::EMAIL>
;    #<MITO.DAO.COLUMN:DAO-TABLE-COLUMN-CLASS MITO.DAO.MIXIN::CREATED-AT>
;    #<MITO.DAO.COLUMN:DAO-TABLE-COLUMN-CLASS MITO.DAO.MIXIN::UPDATED-AT>)
~~~

The class inherits `mito:dao-class` implicitly.

~~~lisp
(find-class 'user)
;=> #<MITO.DAO.TABLE:DAO-TABLE-CLASS COMMON-LISP-USER::USER>

(c2mop:class-direct-superclasses *)
;=> (#<STANDARD-CLASS MITO.DAO.TABLE:DAO-CLASS>)
~~~

This may be useful when you define methods which can be applied for
all table classes.

For more information on using the Common Lisp Object System, see the
[clos](clos.html) page.


### Creating the tables

After defining the models, you must create the tables:

~~~lisp
(mito:ensure-table-exists 'user)
~~~

So a helper function:

~~~lisp
(defun ensure-tables ()
  (mapcar #'mito:ensure-table-exists '('user 'foo 'bar)))
~~~


See
[Mito's documentation](https://github.com/fukamachi/mito#generating-table-definitions)
for a couple more ways.

When you alter the model you'll need to run a DB migration, see the next section.

### Fields

#### Fields types

Field types are:

`(:varchar <integer>)` ,

`:serial`, `:bigserial`, `:integer`, `:bigint`, `:unsigned`,

`:timestamp`, `:timestamptz`,

`:bytea`,

#### Optional fields

Use `(or <real type> :null)`:

~~~lisp
   (email :col-type (or (:varchar 128) :null)
          :initarg :email
          :accessor user-email))
~~~


#### Field constraints

`:unique-keys` can be used like so:

~~~lisp
(defclass user ()
  ((name :col-type (:varchar 64)
         :initarg :name
         :accessor user-name)
   (email :col-type (:varchar 128)
          :initarg :email
          :accessor user-email))
  (:metaclass mito:dao-table-class)
  (:unique-keys email))
~~~

We already saw `:primary-key`.

You can change the table name with `:table-name`.

### Relationships

You can define a relationship by specifying  a foreign class with `:col-type`:

~~~lisp
(defclass tweet ()
  ((status :col-type :text
           :initarg :status
           :accessor tweet-status)
   ;; This slot refers to USER class
   (user :col-type user
         :initarg :user
         :accessor tweet-user))
  (:metaclass mito:dao-table-class))

(table-definition (find-class 'tweet))
;=> (#<SXQL-STATEMENT: CREATE TABLE tweet (
;        id BIGSERIAL NOT NULL PRIMARY KEY,
;        status TEXT NOT NULL,
;        user_id BIGINT NOT NULL,
;        created_at TIMESTAMP,
;        updated_at TIMESTAMP
;    )>)
~~~

Now you can create or retrieve a `TWEET` by a `USER` object, not a `USER-ID`.

~~~lisp
(defvar *user* (mito:create-dao 'user :name "Eitaro Fukamachi"))
(mito:create-dao 'tweet :user *user*)

(mito:find-dao 'tweet :user *user*)
~~~

Mito doesn't add foreign key constraints for referring tables.

#### One-to-one

A one-to-one relationship is simply represented with a simple foreign
key on a slot (as `:col-type user` in the `tweet` class). Besides, we
can add a unicity constraint, as with `(:unique-keys email)`.

#### One-to-many, many-to-one

The tweet example above shows a one-to-many relationship between a user and
his tweets: a user can write many tweets, and a tweet belongs to only
one user.

The relationship is defined with a foreign key on the "many" side
linking back to the "one" side. Here the `tweet` class defines a
`user` foreign key, so a tweet can only have one user. You didn't need
to edit the `user` class.

A many-to-one relationship is actually the contrary of a one-to-many.
You have to put the foreign key on the appropriate side.

#### Many-to-many

A many-to-many relationship needs an intermediate table, which will be
the "many" side for the two tables it is the intermediary of.

And, thanks to the join table, we can store more information about the relationship.

Let's define a `book` class:

~~~lisp
(defclass book ()
    ((title
       :col-type (:varchar 128)
       :initarg :title
       :accessor title)
     (ean
       :col-type (or (:varchar 128) :null)
       :initarg :ean
       :accessor ean))
    (:metaclass mito:dao-table-class))
~~~

A user can have many books, and a book (as the title, not the physical
copy) is likely to be in many people's library. Here's the
intermediate class:

~~~lisp
(defclass user-books ()
    ((user
      :col-type user
      :initarg :user)
    (book
      :col-type book
      :initarg :book))
    (:metaclass mito:dao-table-class))
~~~

Each time we want to add a book to a user's collection (say in
a `add-book` function), we create a new `user-books` object.

But someone may very well own many copies of one book. This is an
information we can store in the join table:

~~~lisp
(defclass user-books ()
    ((user
      :col-type user
      :initarg :user)
    (book
      :col-type book
      :initarg :book)
    ;; Set the quantity, 1 by default:
    (quantity
      :col-type :integer
      :initarg :quantity
      :initform 1
      :accessor quantity))
    (:metaclass mito:dao-table-class))
~~~


### Inheritance and mixin

A subclass of DAO-CLASS is allowed to be inherited. This may be useful
when you need classes which have similar columns:

~~~lisp
(defclass user ()
  ((name :col-type (:varchar 64)
         :initarg :name
         :accessor user-name)
   (email :col-type (:varchar 128)
          :initarg :email
          :accessor user-email))
  (:metaclass mito:dao-table-class)
  (:unique-keys email))

(defclass temporary-user (user)
  ((registered-at :col-type :timestamp
                  :initarg :registered-at
                  :accessor temporary-user-registered-at))
  (:metaclass mito:dao-table-class))

(mito:table-definition 'temporary-user)
;=> (#<SXQL-STATEMENT: CREATE TABLE temporary_user (
;        id BIGSERIAL NOT NULL PRIMARY KEY,
;        name VARCHAR(64) NOT NULL,
;        email VARCHAR(128) NOT NULL,
;        registered_at TIMESTAMP NOT NULL,
;        created_at TIMESTAMP,
;        updated_at TIMESTAMP,
;        UNIQUE (email)
;    )>)
~~~

If you need a 'template' for tables which doesn't related to any
database tables, you can use `DAO-TABLE-MIXIN`. Below the `has-email`
class will not create a table.

~~~lisp
(defclass has-email ()
  ((email :col-type (:varchar 128)
          :initarg :email
          :accessor object-email))
  (:metaclass mito:dao-table-mixin)
  (:unique-keys email))
;=> #<MITO.DAO.MIXIN:DAO-TABLE-MIXIN COMMON-LISP-USER::HAS-EMAIL>

(defclass user (has-email)
  ((name :col-type (:varchar 64)
         :initarg :name
         :accessor user-name))
  (:metaclass mito:dao-table-class))
;=> #<MITO.DAO.TABLE:DAO-TABLE-CLASS COMMON-LISP-USER::USER>

(mito:table-definition 'user)
;=> (#<SXQL-STATEMENT: CREATE TABLE user (
;       id BIGSERIAL NOT NULL PRIMARY KEY,
;       name VARCHAR(64) NOT NULL,
;       email VARCHAR(128) NOT NULL,
;       created_at TIMESTAMP,
;       updated_at TIMESTAMP,
;       UNIQUE (email)
;   )>)
~~~

See more examples of use in [mito-auth](https://github.com/fukamachi/mito-auth/).


### Troubleshooting

#### "Cannot CHANGE-CLASS objects into CLASS metaobjects."

If you get the following error message:

~~~
Cannot CHANGE-CLASS objects into CLASS metaobjects.
   [Condition of type SB-PCL::METAOBJECT-INITIALIZATION-VIOLATION]
See also:
  The Art of the Metaobject Protocol, CLASS [:initialization]
~~~

it is certainly because you first wrote a class definition and *then*
added the Mito metaclass and tried to evaluate the class definition
again.

If this happens, you must remove the class definition from the current package:

~~~lisp
(setf (find-class 'foo) nil)
~~~

or, with the Slime inspector, click on the class and find the "remove" button.

More info [here](https://stackoverflow.com/questions/38811931/how-to-change-classs-metaclass).

## Migrations

First create the tables if needed:

~~~lisp
(ensure-table-exists 'user)
~~~

then alter the tables, if needed:

~~~lisp
(mito:migrate-table 'user)
~~~

You can check the SQL generated code with `migration-expressions
'class`. For example, we create the `user` table:

~~~lisp
(ensure-table-exists 'user)
;-> ;; CREATE TABLE IF NOT EXISTS "user" (
;       "id" BIGSERIAL NOT NULL PRIMARY KEY,
;       "name" VARCHAR(64) NOT NULL,
;       "email" VARCHAR(128),
;       "created_at" TIMESTAMP,
;       "updated_at" TIMESTAMP
;   ) () [0 rows] | MITO.DAO:ENSURE-TABLE-EXISTS
~~~

There are no changes from the previous user definition:

~~~lisp
(mito:migration-expressions 'user)
;=> NIL
~~~

Now let's add a unique `email` field:

~~~lisp
(defclass user ()
  ((name :col-type (:varchar 64)
         :initarg :name
         :accessor user-name)
   (email :col-type (:varchar 128)
          :initarg :email
          :accessor user-email))
  (:metaclass mito:dao-table-class)
  (:unique-keys email))
~~~

The migration will run the following code:

~~~lisp
(mito:migration-expressions 'user)
;=> (#<SXQL-STATEMENT: ALTER TABLE user ALTER COLUMN email TYPE character varying(128), ALTER COLUMN email SET NOT NULL>
;    #<SXQL-STATEMENT: CREATE UNIQUE INDEX unique_user_email ON user (email)>)
~~~

so let's apply it:

~~~lisp
(mito:migrate-table 'user)
;-> ;; ALTER TABLE "user" ALTER COLUMN "email" TYPE character varying(128), ALTER COLUMN "email" SET NOT NULL () [0 rows] | MITO.MIGRATION.TABLE:MIGRATE-TABLE
;   ;; CREATE UNIQUE INDEX "unique_user_email" ON "user" ("email") () [0 rows] | MITO.MIGRATION.TABLE:MIGRATE-TABLE
;-> (#<SXQL-STATEMENT: ALTER TABLE user ALTER COLUMN email TYPE character varying(128), ALTER COLUMN email SET NOT NULL>
;    #<SXQL-STATEMENT: CREATE UNIQUE INDEX unique_user_email ON user (email)>)
~~~


## Queries

### Creating objects

We can create user objects with the regular `make-instance`:

~~~lisp
(defvar me
  (make-instance 'user :name "Eitaro Fukamachi" :email "e.arrows@gmail.com"))
;=> USER
~~~

To save it in DB, use `insert-dao`:

~~~lisp
(mito:insert-dao me)
;-> ;; INSERT INTO `user` (`name`, `email`, `created_at`, `updated_at`) VALUES (?, ?, ?, ?) ("Eitaro Fukamachi", "e.arrows@gmail.com", "2016-02-04T19:55:16.365543Z", "2016-02-04T19:55:16.365543Z") [0 rows] | MITO.DAO:INSERT-DAO
;=> #<USER {10053C4453}>
~~~

Do the two steps above at once:

~~~lisp
(mito:create-dao 'user :name "Eitaro Fukamachi" :email "e.arrows@gmail.com")
~~~

You should not export the `user` class and create objects outside of
its package (it is good practice anyway to keep all database-related
operations in say a `models` package and file). You should instead use
a helper function:

~~~lisp
(defun make-user (&key name)
  (make-instance 'user :name name))
~~~

### Updating fields

~~~lisp
(setf (slot-value me 'name) "nitro_idiot")
;=> "nitro_idiot"
~~~

and save it:

~~~lisp
(mito:save-dao me)
~~~

### Deleting

~~~lisp
(mito:delete-dao me)
;-> ;; DELETE FROM `user` WHERE (`id` = ?) (1) [0 rows] | MITO.DAO:DELETE-DAO

;; or:
(mito:delete-by-values 'user :id 1)
;-> ;; DELETE FROM `user` WHERE (`id` = ?) (1) [0 rows] | MITO.DAO:DELETE-DAO
~~~

### Get the primary key value

~~~lisp
(mito:object-id me)
;=> 1
~~~

### Count

~~~lisp
(mito:count-dao 'user)
;=> 1
~~~

### Find one

~~~lisp
(mito:find-dao 'user :id 1)
;-> ;; SELECT * FROM `user` WHERE (`id` = ?) LIMIT 1 (1) [1 row] | MITO.DB:RETRIEVE-BY-SQL
;=> #<USER {10077C6073}>
~~~

So here's a possibility of generic helpers to find an object by a given key:

~~~lisp
(defgeneric find-user (key-name key-value)
  (:documentation "Retrieves an user from the data base by one of the unique
keys."))

(defmethod find-user ((key-name (eql :id)) (key-value integer))
  (mito:find-dao 'user key-value))

(defmethod find-user ((key-name (eql :name)) (key-value string))
  (first (mito:select-dao 'user
                          (sxql:where (:= :name key-value)))))
~~~

### Find all

Use the macro `select-dao`.

Get a list of all users:

~~~lisp
(mito:select-dao 'user)
;(#<USER {10077C6073}>)
;#<SXQL-STATEMENT: SELECT * FROM user>
~~~


### Find by relationship

As seen above:

~~~lisp
(mito:find-dao 'tweet :user *user*)
~~~

### Custom queries

It is with `select-dao` that you can write more precise queries by
giving it [SxQL](https://github.com/fukamachi/sxql) statements.

Example:

~~~lisp
(select-dao 'tweet
    (where (:like :status "%Japan%")))
~~~

another:

~~~lisp
(select (:id :name :sex)
  (from (:as :person :p))
  (where (:and (:>= :age 18)
               (:< :age 65)))
  (order-by (:desc :age)))
~~~

You can compose your queries with regular Lisp code:

~~~lisp
(defun find-tweets (&key user)
  (select-dao 'tweet
    (when user
      (where (:= :user user)))
    (order-by :object-created)))
~~~

`select-dao` is a macro that expands to the right thing©.


#### Clauses

See the [SxQL documentation](https://github.com/fukamachi/sxql#sql-clauses).

Examples:

~~~lisp
(select-dao 'foo
  (where (:and (:> :age 20) (:<= :age 65))))
~~~

~~~lisp
(order-by :age (:desc :id))
~~~

~~~lisp
(group-by :sex)
~~~

~~~lisp
(having (:>= (:sum :hoge) 88))
~~~

~~~lisp
(limit 0 10)
~~~

and `join`s, etc.


#### Operators

~~~lisp
:not
:is-null, :not-null
:asc, :desc
:distinct
:=, :!=
:<, :>, :<= :>=
:a<, :a>
:as
:in, :not-in
:like
:and, :or
:+, :-, :* :/ :%
:raw
~~~


## Triggers

Since `insert-dao`, `update-dao` and `delete-dao` are defined as generic
functions, you can define `:before`, `:after` or `:around` methods to those, like regular [method combination](clos.html#qualifiers-and-method-combination).

~~~lisp
(defmethod mito:insert-dao :before ((object user))
  (format t "~&Adding ~S...~%" (user-name object)))

(mito:create-dao 'user :name "Eitaro Fukamachi" :email "e.arrows@gmail.com")
;-> Adding "Eitaro Fukamachi"...
;   ;; INSERT INTO "user" ("name", "email", "created_at", "updated_at") VALUES (?, ?, ?, ?) ("Eitaro Fukamachi", "e.arrows@gmail.com", "2016-02-16 21:13:47", "2016-02-16 21:13:47") [0 rows] | MITO.DAO:INSERT-DAO
;=> #<USER {100835FB33}>
~~~

## Inflation/Deflation

Inflation/Deflation is a function to convert values between Mito and RDBMS.

~~~lisp
(defclass user-report ()
  ((title :col-type (:varchar 100)
          :initarg :title
          :accessor report-title)
   (body :col-type :text
         :initarg :body
         :initform ""
         :accessor report-body)
   (reported-at :col-type :timestamp
                :initarg :reported-at
                :initform (local-time:now)
                :accessor report-reported-at
                :inflate #'local-time:universal-to-timestamp
                :deflate #'local-time:timestamp-to-universal))
  (:metaclass mito:dao-table-class))
~~~

## Eager loading

One of the pains in the neck to use ORMs is the "N+1 query" problem.

~~~lisp
;; BAD EXAMPLE

(use-package '(:mito :sxql))

(defvar *tweets-contain-japan*
  (select-dao 'tweet
    (where (:like :status "%Japan%"))))

;; Getting names of tweeted users.
(mapcar (lambda (tweet)
          (user-name (tweet-user tweet)))
        *tweets-contain-japan*)
~~~

This example sends a query to retrieve a user like "SELECT * FROM user
WHERE id = ?" at each iteration.

To prevent this performance issue, add `includes` to the above query
which only sends a single WHERE IN query instead of N queries:

~~~lisp
;; GOOD EXAMPLE with eager loading

(use-package '(:mito :sxql))

(defvar *tweets-contain-japan*
  (select-dao 'tweet
    (includes 'user)
    (where (:like :status "%Japan%"))))
;-> ;; SELECT * FROM `tweet` WHERE (`status` LIKE ?) ("%Japan%") [3 row] | MITO.DB:RETRIEVE-BY-SQL
;-> ;; SELECT * FROM `user` WHERE (`id` IN (?, ?, ?)) (1, 3, 12) [3 row] | MITO.DB:RETRIEVE-BY-SQL
;=> (#<TWEET {1003513EC3}> #<TWEET {1007BABEF3}> #<TWEET {1007BB9D63}>)

;; No additional SQLs will be executed.
(tweet-user (first *))
;=> #<USER {100361E813}>
~~~

## Schema versioning

~~~
$ ros install mito
$ mito
Usage: mito command [option...]

Commands:
    generate-migrations
    migrate

Options:
    -t, --type DRIVER-TYPE          DBI driver type (one of "mysql", "postgres" or "sqlite3")
    -d, --database DATABASE-NAME    Database name to use
    -u, --username USERNAME         Username for RDBMS
    -p, --password PASSWORD         Password for RDBMS
    -s, --system SYSTEM             ASDF system to load (several -s's allowed)
    -D, --directory DIRECTORY       Directory path to keep migration SQL files (default: "/Users/nitro_idiot/Programs/lib/mito/db/")
    --dry-run                       List SQL expressions to migrate
~~~

## Introspection

Mito provides some functions for introspection.

We can access the information of **columns** with the functions in
`(mito.class.column:...)`:

- `table-column-[class, name, info, not-null-p,...]`
- `primary-key-p`

and likewise for **tables** with `(mito.class.table:...)`.

Given we get a list of slots of our class:

~~~lisp
(ql:quickload "closer-mop")

(closer-mop:class-direct-slots (find-class 'user))
;; (#<MITO.DAO.COLUMN:DAO-TABLE-COLUMN-CLASS NAME>
;;  #<MITO.DAO.COLUMN:DAO-TABLE-COLUMN-CLASS EMAIL>)

(defparameter user-slots *)
~~~

We can answer the following questions:

### What is the type of this column ?

~~~lisp
(mito.class.column:table-column-type (first user-slots))
;; (:VARCHAR 64)
~~~

### Is this column nullable ?

~~~lisp
(mito.class.column:table-column-not-null-p
  (first user-slots))
;; T
(mito.class.column:table-column-not-null-p
  (second user-slots))
;; NIL
~~~


## Testing

We don't want to test DB operations against the production one. We
need to create a temporary DB before each test.

The macro below creates a temporary DB with a random name, creates the
tables, runs the code and connects back to the original DB connection.

~~~lisp
(defpackage my-test.utils
  (:use :cl)
  (:import-from :my.models
                :*db*
                :*db-name*
                :connect
                :ensure-tables-exist
                :migrate-all)
  (:export :with-empty-db))

(in-package my-test.utils)

(defun random-string (length)
  ;; thanks 40ants/hacrm.
  (let ((chars "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789"))
    (coerce (loop repeat length
                  collect (aref chars (random (length chars))))
            'string)))

(defmacro with-empty-db (&body body)
  "Run `body` with a new temporary DB."
  `(let* ((*random-state* (make-random-state t))
          (prefix (concatenate 'string
                               (random-string 8)
                               "/"))
          ;; Save our current DB connection.
          (connection (when (mito.connection:connected-p)
                        mito:*connection*)))
     (uiop:with-temporary-file (:pathname name :prefix prefix)
       ;; Bind our *db-name* to a new name, so as to create a new DB.
       (let* ((*db-name* name))
         ;; Always re-connect to our real DB even in case of error in body.
         (unwind-protect
           (progn
             ;; our functions to connect to the DB, create the tables and run the migrations.
             (connect)
             (ensure-tables-exist)
             (migrate-all)
             ,@body)

           (setf mito:*connection* connection))))))
~~~

Use it like this:

~~~lisp
(prove:subtest "Creation in a temporary DB."
  (with-empty-db
    (let ((user (make-user :name "Cookbook")))
      (save-user user)

      (prove:is (name user)
                "Cookbook"
                "Test username in a temp DB."))))
;; Creation in a temporary DB
;;  CREATE TABLE "user" (
;;       id BIGSERIAL NOT NULL PRIMARY KEY,
;;       name VARCHAR(64) NOT NULL,
;;       email VARCHAR(128) NOT NULL,
;;       created_at TIMESTAMP,
;;       updated_at TIMESTAMP,
;;       UNIQUE (email)
;; ) () [0 rows] | MITO.DB:EXECUTE-SQL
;; ✓ Test username in a temp DB.
~~~

## See also

- [exploring an existing (PostgreSQL) database with postmodern](https://sites.google.com/site/sabraonthehill/postmodern-examples/exploring-a-database)

- [mito-attachment](https://github.com/fukamachi/mito-attachment)
- [mito-auth](https://github.com/fukamachi/mito-auth)
- [can](https://github.com/fukamachi/can/) a role-based access right control library
- an advanced ["defmodel" macro](drafts/defmodel.lisp.html).

<!-- # todo: Generating models for an existing DB -->
