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
