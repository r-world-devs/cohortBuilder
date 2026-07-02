# Cohort hooks.

In order to make integration of \`cohortBuilder\` package with other
layers/packages easier, hooks system was introduced.

## Usage

``` r
add_hook(name, method)

get_hook(name)
```

## Arguments

- name:

  Name of the hook. See Details section.

- method:

  Function to be assigned as hook.

## Value

No returned value (\`add_hook\`) or the list of functions
(\`get_hook\`).

## Details

Many
[Cohort](https://r-world-devs.github.io/cohortBuilder/reference/Cohort.md)
methods allow to define \`hook\` parameter. For such method, \`hook\` is
a list containing two values: \`pre\` and \`post\`, storing functions
(hooks) executed before and after the method is run respectively.

Each \`hook\` is a function of two obligatory parameters:

- `public` - Cohort object.

- `private` - Private environment of Cohort object.

When Cohort method, for which hook is defined, allow to pass custom
parameters, the ones should be also available in hook definition (with
some exclusions, see below).

For example \`Cohort\$remove_step\` has three parameters:

- `step_id`

- `run_flow`

- `hook`

By the implementation, the parameters that we should skip are
\`run_flow\` and \`hook\`, so the hook should have three parameters
\`public\`, \`private\` and \`step_id\`.

There are two ways of defining hooks for the specific method. The first
one is to define the method \`hook\` directly as its parameter (while
calling the method).

The second option can be achieved with usage of \`add_hook\` (and
\`get_hook\`) function. The default \`hook\` parameter for each method
is constructed as below:


    remove_step = function(step_id, run_flow = FALSE,
      hook = list(
        pre = get_hook("pre_rm_step_hook"),
        post = get_hook("post_rm_step_hook")
      )
    )

'Pre' hooks are defined with 'pre\_\<method_name\>\_hook' and 'Post'
ones as 'post\_\<method_name\>\_hook'. As a result calling:


    add_hook(
      "pre_remove_step_hook",
      function(public, private, step_id) {...}
    )

will result with specifying a new pre-hook for \`remove_step\` method.

You may add as many hooks as you want. The order of hooks execution is
followed by the order or registering process. If you want to check
currently registered hooks for the specific method, just use:


    get_hook("pre_<method_name>_hook")
