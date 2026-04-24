# Instructions on Testing Library Use
## Testing an entire module
This is the main way to interface with the testing library. You create a test points to a specific module and a suite of tests (more on that below).
```
module_test(args)
args: {
  // The path to the module to test
  module_path: <string>,
  // The path of the suite to test the module against
  suite_path: <string>
}
```

## Defining a test suite
A suite is an object consisting of `tests` and an optional section `skip` for documenting that certain parts of the public interface module are intentionally skipped. If you simply do not include part of the public interface without putting it into the `skip` section it will give a warning.

```
suite = {
  // The tests to run
  tests: {
    <test_key>: <test_definition>,
    ...
  },
  // The parts of the public interface to skip
  [skip]: {
    <test_key>: <boolean>
  }
}
```
`<test_key>` is the name of a part of the exposed public interface.
`<test_definition>` is either a `<function_test>` or a `<class_test>`

## Defining a function test
The test library provides a function `make_function_test` for use in defining a `<function_test>`.

```
make_function_test(test_cases)
test_cases = {
    <test_name>: {
       [config_loader]: <function>,
       args: <list>,
       expected: <any>,
       [context]: <any>,
       [post]: <function>
    },
    ...
}
```
For most cases you will only need to worry about the `args` and `expected` fields.
`config_loader` is a function that is run before executing the test. This can be useful if there is global state that must be setup for the module to behave correctly.
`args` is a list of arguments to test the function
`expected` is what you expect the result of test call to be.
`context` is additional context required for the `post` field.
`post` is a function that is applied to the result of the function under test with `args`. Use this when the tested function returns a function that needs to be applied to something else to get a value.

## Defining a class test
The test library provides a function `make_class_test` for use in defining a `<class_test>`

```
make_class_test(test_cases)
test_cases = {
    <test_name>: {
        [config_loader]: <function>,
        [constructor]: {
            args: <list>,
            [expected]: <any>
        },
        [injected]: {
            <property_name>: <any>,
            ...
        },
        [steps]: <list>
    }
}
```
`config_loader` is a function that is run before executing the test. This can be useful if there is global state that must be setup for the module to behave correctly.
`constructor` is an object that initializes an instance of the class by calling `new test_class(...args)`.
  - `args` is a list of arguments passed to the constructor.
  - `expected` is optional. When provided, it is deeply compared against the constructed instance to verify the initial state produced by the constructor. If `expected` is an error class, the test passes only if the constructor throws an instance of that error.
`injected` is an alternative to `constructor` for cases where you want to test methods against a known instance state without exercising the constructor. The test harness creates an instance via `new test_class()` (no arguments) and then assigns each entry in `injected` onto the instance. Each key in `injected` must already exist as an own property on the default-constructed instance; otherwise the test will fail with a developer error. A test case may define either `constructor` or `injected`, but not both.
`steps` is a list of `class_step` which is defined below:

A `class_step` describes a single interaction with the class instance. Each step must define exactly one of `method`, `get`, or `set`; steps that define zero or more than one of these will throw a developer error.

### Method step
```
class_step = {
    method: <string>,
    args: <list>,
    expected: <any>
}
```
`method` is the method to call on this instance of the class.
`args` is a list of arguments to `method`.
`expected` is the expected return value of calling `method` with `args`. If `expected` is an error class, the test passes only if the method throws an instance of that error.

### Get step
```
class_step = {
    get: <string>,
    expected: <any>
}
```
`get` is the name of the property to read from the instance.
`expected` is the expected value of that property. If `expected` is an error class, the test passes only if reading the property throws an instance of that error.

### Set step
```
class_step = {
    set: <string>,
    value: <any>,
    [expected]: <any>
}
```
`set` is the name of the property to assign on the instance.
`value` is the value to assign to the property.
`expected` is optional. When omitted, the step only performs the assignment. When provided as a value, the property is read after the assignment and deep-compared against `expected`. When provided as an error class, the test passes only if the assignment throws an instance of that error.

The steps occur in order for this test cases's instance of the class and can produce different internal class state if the steps are run in a different order.
## Other exposed functions
The library exposes two other functions. One that is generally useful and one that is specific for this project.

`make_lazy` is meant to make the test harness lazily evaluate a certain function call. The test libarywill only execute the function when it is required for the test to continue running. 
```
make_lazy(args)
args = {
    call: <function>,
    args: <list>
}
```
`call` the function to make lazily evaluated
`args` the arguments to pass into the function

`apply_rule` is a specialized utility used for testing the property-rule libraries for TRAPI property gathering. The property-rule libraries return functions that act on specific structures. When creating tests for property-rule libraries you can use `apply_rule` as the value for the `post` field of a `<function_test>`
