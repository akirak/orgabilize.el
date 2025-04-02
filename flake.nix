{
  outputs =
    { ... }:
    {
      elisp-rice = {
        packages = [
          "orgabilize"
          "orgabilize-playwright"
        ];
        # TODO: Configure tests
        # tests = {
        #   buttercup.enable = true;
        # };
      };
    };
}
