{
  outputs =
    { ... }:
    {
      elisp-rice = {
        packages = [ "orgabilize" ];
        # TODO: Configure tests
        # tests = {
        #   buttercup.enable = true;
        # };
      };
    };
}
