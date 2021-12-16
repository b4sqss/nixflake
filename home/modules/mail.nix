{
programs.password-store.enable = true;

programs.gpg.enable = true;
services.gpg-agent = {
  enable = true;
  defaultCacheTtl = 1800;
  enableSshSupport = true;
};


programs.mbsync.enable = true;

home.file.".mbsyncrc".source = ../configs/mbsyncrc;
}
