# Contributing guidelines

## Reporting issues
- Please check if there are no similar [open](https://github.com/bearmug/erlang-pure-migrations/issues)
or [recently closed](https://github.com/bearmug/erlang-pure-migrations/issues?q=is%3Aissue+is%3Aclosed+sort%3Aupdated-desc)
issues.
- Feel free to report and discuss new one. You will be suggested with pre-populated template on it.

## Contributing code
You are more than welcome to implement new features of fix existing issues! Here is the check list to make this
process as smooth as possible.
- [ ] ensure you are working onto existing and open issue
- [ ] fork repo to your profile and create feature/bugfix branch
- [ ] as a sanity check, you may want to run `make local` command, if passed - you are good to go with
      development. In case of failure you may want to check development pre-requisites like:
      
    - [ ] Erlang installed, at least 21.0. Here is short [instruction](https://github.com/bearmug/cfg-init/blob/master/cfg-stack-erl.sh) to obtain it.
    - [ ] Docker installed, [instruction](https://github.com/bearmug/cfg-init/blob/master/cfg-tools-docker.sh) to make it happen
    - [ ] Ensure there are no services running at default PostreSQL(5432)/MySQL(3306) ports locally. Those are in use for
    docker-based integration test.
- [ ] produce code change :) 
- [ ] by the same task `make local` ensure that new code is compliant with:
    - [ ] code coverage requirements, related task `make cover` is built-in to the pipeline
    - [ ] code formatting conventions (mostly regular emacs formatting), see `make format` sub-task
    - [ ] there are no dialyzer/linter alerts, related sub-task is `make code-checks`
- [ ] submit your change as a pull-request, code quality checks to be done and reported there automatically   
