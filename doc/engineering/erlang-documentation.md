# How to Write Erlang Documentation

Having user end documentation is good, having developer's documentation is awesome. Kazoo has a big code base which makes it difficult to remember all good code stuff. New developers want to start coding but don't know where or how. There are so many other arguments why to write an Erlang reference manual.

Kazoo uses [Edoc](http://erlang.org/doc/apps/edoc/chapter.html) to generate a reference of all modules. All comments for exported functions and types are used as description. Before Kazoo 4.2, the code base was not EDoc friendly, an attempt to generate doc is failing. Some part was related to some issues in the comment or code itself, also the fact EDoc is not compatible a multi applications project like Kazoo.

EDoc use comments at top of the module for description of module, an every comments before **exported** function as function's description and functions type declaration/definition (the `spec` tag) for data type descriptions of the function.

YOu can read more about what tags are usable in EDoc at [Edoc](http://erlang.org/doc/apps/edoc/chapter.html) official website.

## Module-level

When writing module-level documentation, we're looking for:

* A very brief description of the functionality of the module as the first sentence in `@doc` tag ended with dor (`.`) followed by empty comment line
* A full description for module
* What module is doing, brief description of use cases, algorithm, usage
* Example of how to use the module
* Common pitfalls or side effect when using the module
* Keep it short, don't go deep explaining everything.
* If module has a general option or have a settings, describe them
* Links to other related module, or sites, article.
* `@author` tag
* `@copyright` tag
* `@deprecated`, `@reference`, `@see` tags if applicate

For more EDoc tags see [EDoc Module tags](http://erlang.org/doc/apps/edoc/chapter.html#id62859)

## Per-functions

When writing docs for functions, we'd like to strive for:

* Same as module, brief description ended with dot(`.`) and empty comment line, followed by more description
* What function is doing
* Explain of the function's argument if necessary
* If function is equivalence to another function call/expression use `@equiv`
* Always write correct function type in `spec` tag
* If function accepts an option as argument, explain each possible options and their possible/default values
* Explain inner function calls or pitfalls, if necessary

## Documentation Style

The purpose of style for writing comments is to have a consistence looks for code and comment and to makes everything easy when writing tooling for building documentation.

The `edocify.escript` in `scripts` directory depends on some of these rules in its regex.

### Comment Leading Character

Conventionally we use triple `%` character for module header comments and separating parts of the source code. For other comments we use double `%`.

For separating section header make sure the is empty line before and after. If a function definition is coming right after separator don't forget to add comment block.

Example of code parting section:

```erlang
%% some code

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec a_function() -> 'ok'.
```

### Line length, Separator Line

We advise to use 80 characters for comment line length (including the leading `%` characters). This is not hard limit but if you can, follow it.

As for module header and function comments use a comment separator to make the comment stand out from other comments. Use `-` for header and function separator and `=` for code section separator. Do not add white space between `%` and `-` or `=`. Total line length should always be 80 character (including leading `%`).

### Source Code in Comment

EDoc is using back-tick `` ` `` for stating in-line code and single quote `'` for ending it. Code, starts with triple back-tick `` ``` `` and ends with triple single quote `'''`.

Refer to EDoc manual for more information.

EDoc parse any HTML tags in comment as it, so if you write a Erlang binary `<<>>` in comment without make it a in-line code or code block EDoc will fail. Same thing may go for similar character like `@`.

### `@doc` Tag

Start brief description in the same as `@doc` tag to avoid adding additional empty paragraph.

Make brief description short and always end it with dot (`.`), otherwise. EDoc considered the first sentence ending with dot followed by empty comment line (a line with only `##` or `###` content) the brief description. Not following this (not ending with dot and empty comment line or having empty line without any character) will result to consider all lines after (line which is not starts with a tag) as brief description which is not looks good in generated documentation.

### `@end` Tag

Since we're adding `%%%----....` to stand out document comments from regular comments, don't forget to use `@end` before the separator otherwise EDoc thinks the separator line is part of the doc and will include it in the description.

### Describing Options

Since HTML tags are allowed in EDoc, HTML description list tag can be used for documenting options, or JSON structures and etc....

```html
%%% <h4>Data options:</h4>
%%% <dl>
%%%   <dt>`action'</dt>
%%%   <dd>How to collect Caller ID: `lists' (`list' has same effect), `static' or `manual'. Default is `manual'.</dd>
%%%
%%%   <dt>`media_id'</dt>
%%%   <dd>ID of the media prompt to play before starting collecting DTMF.</dd>
%%%
%%%   <dt>`id'</dt>
%%%   <dd>ID if the list document to use if action is `list' or `lists'. Required if the action is `list' or `lists'.</dd>
%%%
%%%   <dt>`interdigit'</dt>
%%%   <dd><strong>Optional: </strong>How long to wait for the next DTMF, in milliseconds</dd>
%%% </dl>
```

### Module Header

Always use `%%%---...` to indicate comment block for module header.

Add module header tags like `@copyright`, `@doc`, `@author`, `@end`.

* `@author`: EDoc uses this format for author tags: `@author Richard Carlsson <carlsson.richard@gmail.com> [http://example.net/richardc/]`.
* `@copyright`: Please follow this format for copyright: `@copyright (C) 2010-2018, Copyright Holder`. 2010 is starting copyright year, 2018 is ending year followed by a comma and the copyright holder name (ending year for 2600Hz copyright line only is updated every year by running a script).
* Add module tag immediately after module comment block.

**DO NOTs**

* Do not use `@contributors`, etc... to specify the authors. These tags are not part of the EDoc and will cause EDoc to failed to build the documentation. Use multi author tags if it is needed.
* Do not use any unsupported tags.

```erlang
%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2018, 2600Hz
%%% @doc Start first sentence in the same line as `@doc' ending with dot and empty
%%% comment line.
%%%
%%% Write more doc here.
%%% @author Hesaam Farhang
%%% @end
%%%-----------------------------------------------------------------------------
-module(awesome_module).
```

### Function Comment

Always use `%%---...` to indicate comment block for function comment.

Follow general styles, describe the functionality of functions and probably an example usage.

If function is equivalence to another function call/expression use only`@equiv` before function instead of any comment block.

**Specifications**

Always write function type specifications. EDoc will pretty print in the documentation.

**DO NOTs**

* Do not write the `@spec` tag inside comment, use Erlang native specifications [`-spec`](http://erlang.org/doc/reference_manual/typespec.html#id80676).
* Do not group `spec` together (evil specs), it will breaks EDoc.
* Do not write `spec` header (or any file other than the file contains function definition).
* Do not use `@public` tag (it is not a EDoc tag).
* Do not use any unsupported tags.

```erlang
%%------------------------------------------------------------------------------
%% @doc Start first sentence in the same line as `@doc' ending with dot and empty
%%% comment line {@link crossbar_doc}.
%%
%% For `Options' description see {@link a_function/2}.
%%
%% A link to a type {@link a_module:my_type()}.
%%
%% @see new/2
%% @see another_module:another_func/1
%% @throws {error, unabled_to_forward}
%% @end
%%------------------------------------------------------------------------------
-spec forward(kapps_call:call(), kz_term:proplist()) -> kz_json:object().
```

### Type Comments

Types can be documented too! The comment doesn't have any special tags, just generic tags and macros.

Write the comment immediately after the dot in type specification (you can some white space in between):

```erlang
-type my_list(X) :: [X]. %% A special kind of lists ...

-type another_list(X) :: [X].
%% another_list() is a kind of list... see {@link my_list()}
```

## Edocify Script

Comments in Kazoo source code was not formated correctly to consume by EDoc. Since there was a lot of issues to fix, a [script](https://github.com/2600hz/kazoo/blob/master/scripts/edocify.escript) was written to find the problematic issues in the code using regex and [`ag` The Silver Searcher](https://github.com/ggreer/the_silver_searcher) and fix them.

To avoid repeating the same issues again this script is running as part of CI.

The script may not catch all issues but still it fixes known issues were found during formating comments.

```shell
$ scripts/edocify.escript
Edocify Kazoo...

* running command: separate evil sepc+specs
done
* running command: bump/fix copyright
done
* running command: rename contributors tag to author
done
* running command: remove public/private tag
done
* running command: removing spec from comment
done
* running command: add missing comments block after separator
done
* running command: escape code block in 'resource_exists' function crossbar modules
done
* running command: fix comment blocks with no end
done
* running command: increase separator line (starts with %%) length
done
* running command: increase separator line (starts with %%%) length
done
* running command: move first comment line to the same line as doc tag
done
* running command: remove empty comment line after doc tag
done

Already EDocified! 🎉
```

After running the script you can use `git status` or `git diff` to see the changes.

## State of EDoc script

Just like state of doc for API endpoints, [state of EDoc](https://github.com/2600hz/kazoo/blob/master/scripts/state-of-edoc.escript) will run EDoc on all non-test Kazoo Erlang file to make sure that the EDoc can read/parse and creates documentation. This script runs in CI as well.

If EDoc is failing, usually it says which line of the comment is problematic, so you can fix it.
