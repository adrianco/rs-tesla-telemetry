Packaging Instructions
======================

Install Packaging System
------------------------

1. Install Remotes
```
> install.packages("remotes")
trying URL 'https://cran.rstudio.com/bin/macosx/contrib/4.0/remotes_2.2.0.tgz'
Content type 'application/x-gzip' length 385641 bytes (376 KB)
==================================================
downloaded 376 KB


The downloaded binary packages are in
	/var/folders/yz/7k7dhbdn2ybdzxwd18lndmrjvwy8xp/T//RtmpTDAcVF/downloaded_packages
```

2. Install Photon
```
> remotes::install_github("ColumbusCollaboratory/photon")
Downloading GitHub repo ColumbusCollaboratory/photon@HEAD
These packages have more recent versions available.
It is recommended to update all of them.
Which would you like to update?

1: All                             
2: CRAN packages only              
3: None                            
4: vctrs    (0.3.4 -> 0.3.5) [CRAN]
5: pillar   (1.4.6 -> 1.4.7) [CRAN]
6: magrittr (1.5   -> 2.0.1) [CRAN]
7: cli      (2.1.0 -> 2.2.0) [CRAN]

Enter one or more numbers, or an empty line to skip updates:1
vctrs      (0.3.4 -> 0.3.5  ) [CRAN]
pillar     (1.4.6 -> 1.4.7  ) [CRAN]
magrittr   (1.5   -> 2.0.1  ) [CRAN]
cli        (2.1.0 -> 2.2.0  ) [CRAN]
batch      (NA    -> 1.1-5  ) [CRAN]
shinyFiles (NA    -> 0.9.0  ) [CRAN]
miniUI     (NA    -> 0.1.1.1) [CRAN]
Installing 7 packages: vctrs, pillar, magrittr, cli, batch, shinyFiles, miniUI
trying URL 'https://cran.rstudio.com/bin/macosx/contrib/4.0/vctrs_0.3.5.tgz'
Content type 'application/x-gzip' length 1399958 bytes (1.3 MB)
==================================================
downloaded 1.3 MB

trying URL 'https://cran.rstudio.com/bin/macosx/contrib/4.0/pillar_1.4.7.tgz'
Content type 'application/x-gzip' length 176337 bytes (172 KB)
==================================================
downloaded 172 KB

trying URL 'https://cran.rstudio.com/bin/macosx/contrib/4.0/magrittr_2.0.1.tgz'
Content type 'application/x-gzip' length 224854 bytes (219 KB)
==================================================
downloaded 219 KB

trying URL 'https://cran.rstudio.com/bin/macosx/contrib/4.0/cli_2.2.0.tgz'
Content type 'application/x-gzip' length 442114 bytes (431 KB)
==================================================
downloaded 431 KB

trying URL 'https://cran.rstudio.com/bin/macosx/contrib/4.0/batch_1.1-5.tgz'
Content type 'application/x-gzip' length 40494 bytes (39 KB)
==================================================
downloaded 39 KB

trying URL 'https://cran.rstudio.com/bin/macosx/contrib/4.0/shinyFiles_0.9.0.tgz'
Content type 'application/x-gzip' length 403459 bytes (394 KB)
==================================================
downloaded 394 KB

trying URL 'https://cran.rstudio.com/bin/macosx/contrib/4.0/miniUI_0.1.1.1.tgz'
Content type 'application/x-gzip' length 34111 bytes (33 KB)
==================================================
downloaded 33 KB


The downloaded binary packages are in
	/var/folders/yz/7k7dhbdn2ybdzxwd18lndmrjvwy8xp/T//RtmpTDAcVF/downloaded_packages
✓  checking for file ‘/private/var/folders/yz/7k7dhbdn2ybdzxwd18lndmrjvwy8xp/T/RtmpTDAcVF/remotes19eb39d5e5a9/ColumbusCollaboratory-photon-585a4ce/DESCRIPTION’ (361ms)
─  preparing ‘photon’:
✓  checking DESCRIPTION meta-information ...
─  checking for LF line-endings in source and make files and shell scripts
─  checking for empty or unneeded directories
─  building ‘photon_0.0.1.tar.gz’
   Warning: invalid uid value replaced by that for user 'nobody'
   Warning: invalid gid value replaced by that for user 'nobody'
   
* installing *source* package ‘photon’ ...
** using staged installation
** R
** inst
** byte-compile and prepare package for lazy loading
** help
No man pages found in package  ‘photon’ 
*** installing help indices
** building package indices
** testing if installed package can be loaded from temporary location
** testing if installed package can be loaded from final location
** testing if installed package keeps a record of temporary installation path
* DONE (photon)
> 
```

3. Install Homebrew and npm
Visit `https://brew.sh` for instructions

Start a terminal window and install the homebrew package as shown there.

Fix the permission and install npm
```
% sudo chown -R $(whoami) /usr/local/Cellar
% brew install npm
```

4. RStudio setup
Make sure your github directory is not stored on DropBox, as Photon cannot cope with directory paths that contaion '(' or ')' as created by DropBox.

Open photon as a new project. Then select rs-tesla-telemetry and hit the Build button. It will download electron-quick-start but has a directory location bug.

```
> photon:::photon_rstudioaddin()
Loading required package: shiny

Listening on http://127.0.0.1:4303
[1] "Selected directory: "
[1] "Selected directory: /Users/anc/github/rs-tesla-telemetry"
Running Photon
/Users/anc/github/rs-tesla-telemetry/electron-quick-start/R-Portable-Mac
*** R_HOME_DIR ***
/Users/anc/github/rs-tesla-telemetry/electron-quick-start/R-Portable-Mac
WARNING: ignoring environment value of R_HOME

  There is a binary version available but the source version is later:
        binary source needs_compilation
remotes  2.1.1  2.2.0             FALSE

installing the source package ‘remotes’

trying URL 'http://cran.us.r-project.org/src/contrib/remotes_2.2.0.tar.gz'
Content type 'application/x-gzip' length 145553 bytes (142 KB)
==================================================
downloaded 142 KB

/Users/anc/github/rs-tesla-telemetry/electron-quick-start/R-Portable-Mac
*** R_HOME_DIR ***
/Users/anc/github/rs-tesla-telemetry/electron-quick-start/R-Portable-Mac
/Users/anc/github/rs-tesla-telemetry/electron-quick-start/R-Portable-Mac
*** R_HOME_DIR ***
/Users/anc/github/rs-tesla-telemetry/electron-quick-start/R-Portable-Mac
* installing *source* package ‘remotes’ ...
** package ‘remotes’ successfully unpacked and MD5 sums checked
** R
** inst
** byte-compile and prepare package for lazy loading
** help
*** installing help indices
** building package indices
** installing vignettes
** testing if installed package can be loaded
/private/var/folders/yz/7k7dhbdn2ybdzxwd18lndmrjvwy8xp/T/RtmpqBkzW6/R.INSTALL13bec3f41aecd/remotes
*** R_HOME_DIR ***
/private/var/folders/yz/7k7dhbdn2ybdzxwd18lndmrjvwy8xp/T/RtmpqBkzW6/R.INSTALL13bec3f41aecd/remotes
WARNING: ignoring environment value of R_HOME
/Users/anc/github/rs-tesla-telemetry/electron-quick-start/R-Portable-Mac/bin/R: line 258: /private/var/folders/yz/7k7dhbdn2ybdzxwd18lndmrjvwy8xp/T/RtmpqBkzW6/R.INSTALL13bec3f41aecd/remotes/etc/ldpaths: No such file or directory
ERROR: loading failed
* removing ‘/Users/anc/github/rs-tesla-telemetry/electron-quick-start/R-Portable-Mac/library/remotes’

The downloaded source packages are in
	‘/private/var/folders/yz/7k7dhbdn2ybdzxwd18lndmrjvwy8xp/T/RtmpRrbZ6f/downloaded_packages’
Updating HTML index of packages in '.Library'
Making 'packages.html' ... done
Warning message:
In install.packages("remotes", repos = "http://cran.us.r-project.org") :
  installation of package ‘remotes’ had non-zero exit status

Attaching package: ‘magrittr’

The following object is masked from ‘package:purrr’:

    set_names

$cran_packages
[1] "NULL"

$github_packages
[1] "NULL"

$bioc_packages
[1] "NULL"


up to date, audited 149 packages in 1s

4 low severity vulnerabilities

To address all issues, run:
  npm audit fix

Run `npm audit` for details.

> electron-quick-start@1.0.0 package-mac
> electron-packager . --overwrite --platform=darwin --arch=x64 --out=ElectronShinyAppMac

Packaging app for platform darwin x64 using electron v5.0.7
Wrote new app to ElectronShinyAppMac/electron-quick-start-darwin-x64
```



