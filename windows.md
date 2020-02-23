---
title: Setting up Emacs on Windows or Mac
---

Emacs is the preferred Lisp source code editor for most CL developers.

If you want to get going easily, get
[Portacle](https://shinmera.github.io/portacle/), a **portable** and
**multi-platform** CL development environment. You get:

- Emacs25, slightly customized,
- Slime,
- SBCL,
- Quicklisp,
- Git.

You only need to download Portacle and double-click to run it. If you
want to install your environment manually, read on.

In order to help a Lisp beginner set up
an Emacs Common Lisp development environment on a Microsoft Windows
PC, this chapter will lay out step-by-step instructions for installing
all the necessary components for each of the most popular Common Lisp
implementations on Windows:

* **Allegro Common Lisp (ACL):** Franz's own "ELI" will be used in Emacs.
* **CLISP:** ILISP and Emacs Inferior Lisp Mode will be two alternative environments that can be used with CLISP.
* **Corman Common Lisp:** The Emacs Inferior Lisp Mode will be used.
* **LispWorks:** ILISP will be used in Emacs to provide the development environment (Note: the trial version of the product doesn't provide a non-gui Lisp image and this is necessary if the product is to be used with Emacs).

By exactly following the installation instructions, the user will wind up with an Emacs setup that:

* Allows the user to trial the most popular Lisp implementations for Windows.
* Allows the user to try out 3 different techniques (ILISP, ELI, inferior-lisp mode) for establishing a Lisp development environment in Emacs - each with its own sets of pluses and minuses.
* Configures Emacs so that it has keystroke bindings more similar to what a Windows user is accustomed to.
* Configures Emacs with a number of custom configuration settings that are commonly used.


There is also a short appendix describing the sample .emacs file and brief Mac OS X setup instructions:
A. [Sample .emacs file](#Sample .emacs file)
B. [Mac OS X Setup Instructions](#Mac OS X Setup Instructions)

For each installation, an installation directory is suggested. It is recommended that you install in the suggested directory since, if you use the suggested directory names, the provided Emacs start-up file (.emacs) will work "out-of-the-box". Please note the following:

* If you use alternative directory names, you will need to modify the .emacs file (instructions provided in step #8).
* The instructions assume you are using WinZip; however, you can use any alternative utility.
* When you unpack .gz files with WinZip, it will ask you "Should WinZip decompress it to a temporary folder and open it?" - click on "No". When you unzip any files, you should ensure that "All files" radio button is selected and the "Use folder names" check box is ticked.
* There are instructions for downloading all the implementations; however, it is not necessary to do so. You should complete step #1 (Read through step #2 to determine whether you want/need any of the optional Emacs utilities covered in this section. Step #3 is required if you want local access to the Common Lisp documentation. If you don't want/need the documentation, this step is optional as well.) and the relevant parts of #8 & #9 (regardless of which implementations you download) and you will need to download at least one of #4 - #7\. You can download multiple implementations if you want to trial them.


### <a name="Installing Emacs">1\. Installing Emacs</a>

* From a command line, create the following directories (used later):

    <pre>
    md c:\home\
    md c:\home\site\
    md c:\home\docs\
    md c:\home\lisp\
    md c:\home\info\
    md c:\bin\
    </pre>

* Download the precompiled [Emacs](http://www.gnu.org/software/emacs/) binaries for Windows from [http://ftp.gnu.org/pub/gnu/windows/emacs/emacs-26/emacs-26.2-i686.zip](http://ftp.gnu.org/pub/gnu/windows/emacs/emacs-26/emacs-26.2-i686.zip) and use WinZip to unpack the files into the c:\bin directory. This should result in a new directory: c:\bin\emacs-26.2
* Using Run on the Windows Start menu, enter "c:\bin\emacs-26.2\bin\runemacs.exe" to verify that the program works. On the Help menu, there is an Emacs on-line tutorial. If you are unfamiliar with Emacs, you should (at least) complete this tutorial before proceeding any further. The remainder of this chapter's instructions will assume that you have at least basic familiarity with the use of Emacs.
* Download the sample .emacs file from [here](.emacs) and place it in the c:\home directory.


## <a name="Installing additional Emacs utilities">2\. Installing additional Emacs utilities</a>

* OPTIONAL: ILISP is needed if you intend to use CLISP or LispWorks with it. Also, if you want to access the HyperSpec documentation or CLtL2, the code to do this is in the ILISP package. Download the [ILISP](http://sourceforge.net/projects/ilisp/) package from [http://prdownloads.sourceforge.net/ilisp/ilisp-5.12.0.tar.gz?download](http://prdownloads.sourceforge.net/ilisp/ilisp-5.12.0.tar.gz?download) and use WinZip to unpack it into the c:\home\site directory. This should result in a new directory: c:\home\site\ilisp-5.12.0\
    * Edit the EMACSDIR and ILISPDIR variables at the beginning of the icompile.bat file in the ilisp-5.12.0 directory so that they match the settings given below.

      <pre>
      set EMACSDIR=c:\bin\emacs-26.2
      set ILISPDIR=c:\home\site\ilisp
      </pre>

    * From a command line, enter the following commands:

      <pre>
      ren c:\home\site\ilisp-5.12.0 c:\home\site\ilisp
      cd c:\home\site\ilisp
      icompile.bat
      </pre>

* OPTIONAL: If you want Emacs Copy/Cut/Paste keyboard shortcuts to match those of Windows (e.g. - C-c, C-x, C-v), download the [cua.el](http://www.emacswiki.org/cgi-bin/wiki.pl?CuaMode) utility (provides Windows standard key mappings)  from [http://www.eecs.ucf.edu/~leavens/emacs/cua.el](http://www.eecs.ucf.edu/~leavens/emacs/cua.el) and put it into the c:\home\site directory. If you want to use the Emacs defaults for the Copy/Cut/Paste keyboard shortcuts, don't download the cua.el utility.
* OPTIONAL: If you want Emacs to emulate the Windows-style buffer switching (e.g. - C-TAB to move from buffer to buffer), download the [cua-emul.el](http://www.vegetable.demon.co.uk/wence/cua-emul.el) utility from [https://raw.githubusercontent.com/rags/.emacs.d/master/plugins/cua-emul.el](https://raw.githubusercontent.com/rags/.emacs.d/master/plugins/cua-emul.el) and put it into the c:\home\site directory. If you want to use the Emacs defaults for buffer switching, don't download the cua-emul.el utility.
* OPTIONAL: The standard code colorization that is done by Emacs is not to everyone's taste. If you want to use my custom colorization scheme (included in the sample .emacs file) or use one of the standard ones that are included in this optional package (after installation, just press "M-x color-theme-select" to get a listing that you can select from), then download [color-theme.el](https://www.emacswiki.org/emacs/color-theme.el) from [https://www.emacswiki.org/emacs/download/color-theme.el](https://www.emacswiki.org/emacs/download/color-theme.el) and and use WinZip to unpack the files into the c:\home\site directory.
* OPTIONAL: If you want to browse the Lisp documentation files with your default web browser, then skip this step; however, if you want to be able to browse Lisp documentation from within an Emacs buffer, using an Emacs-based web browser, then download the [w3](https://www.emacswiki.org/emacs/w3) browser from [ftp://ftp.xemacs.org/pub/xemacs/emacs-w3/w3.tar.gz](ftp://ftp.xemacs.org/pub/xemacs/emacs-w3/w3.tar.gz) and use WinZip to unpack the files into c:\home\site\w3\. Then, from a command line, cd to the c:\home\site\w3  directory and run the build.bat command.
* OPTIONAL: The [Emacs Code Browser](http://ecb.sourceforge.net/) (ECB) is a useful utility for browsing Lisp code. However, installation is a bit involved and you might want to defer installation until you are more familiar with your Lisp implementation and decide whether it might be useful for your style of development. The sample .emacs file includes setup code for ECB if you do decide to install it. Follow the installation instructions on the ECB site and make certain that the installation directories are all in the C:\home\site\ directory and that they are named ecb, eieio, semantic and speedbar.


## <a name="Installing on-line documentation">3\. Installing on-line documentation</a> (optional)

* OPTIONAL: Download the [Common Lisp Hyperspec](http://www.lispworks.com/documentation/HyperSpec/) reference from: [http://www.lispworks.com/documentation/HyperSpec/HyperSpec-6-0.tar.gz](http://www.lispworks.com/documentation/HyperSpec/HyperSpec-6-0.tar.gz) and use WinZip to unpack the files into the c:\home\docs directory. This should result in a new directory: c:\home\docs\HyperSpec
* OPTIONAL: Download the [Common Lisp the Language](http://www-2.cs.cmu.edu/Groups/AI/html/cltl/cltl2.html) (CLtL2) reference from: [http://www-2.cs.cmu.edu/afs/cs.cmu.edu/project/ai-repository/ai/lang/lisp/doc/cltl/cltl_ht.tgz](http://www-2.cs.cmu.edu/afs/cs.cmu.edu/project/ai-repository/ai/lang/lisp/doc/cltl/cltl_ht.tgz) and use WinZip to unpack the files into the c:\home\docs directory. This should result in a new directory: [c:\home\docs\cltl](file:///c:/home/docs/cltl)
* OPTIONAL: Download the info version of the CL (early ANSI draft) documentation from [ftp://ftp.gnu.org/pub/gnu/gcl/gcl.info.tgz](ftp://ftp.gnu.org/pub/gnu/gcl/gcl.info.tgz) and use WinZip to unpack the files into the c:\home\info directory. You will then need to use WinZip again on each of the "*.gz" files that are in the info directory - unpack each of these into the c:\home\info directory as well. You may then delete the "*.gz" files that remain in the directory. Lastly, add the following line to the "dir" file in the c:\bin\emacs-26.2\info directory:

    <pre>
    * ANSI-CL: (gcl). ANSI Common Lisp Manual.
    </pre>


## <a name="Installing CLISP">4\. Installing CLISP</a> (optional)

* Download the precompiled [CLISP](http://clisp.cons.org/) binaries for Windows from: [CLISP - an ANSI Common Lisp](http://sourceforge.net/project/showfiles.php?group_id=1355)
* Use WinZip to unpack the files into the c:\bin directory This should result in a new directory: c:\bin\clisp-2.31
* Create a file named _clisprc.lisp in the c:\home directory. This file can have any initializations that need to occur when CLISP starts. For the time being, just leave it empty.


## <a name="Installing ACL">5\. Installing ACL</a> (optional)

* Download the trial version of [Franz's ACL](https://franz.com/) for Windows from: [https://franz.com/downloads/](https://franz.com/downloads/)
* (Note: you will need to agree to their license and provide an email address. Franz will send you an email with instructions on how to download the temporary license key. The key works for 60 days and a new one can be downloaded before that one expires).
* Run the downloaded installation program (acl62_trial.exe) and install ACL into the c:\bin\acl-6.2 directory.
* Install the license key as per the instructions that you received from Franz.
* From the Windows Start menu, navigate to the Allegro CL group and startup ACL.
* In the Listener (Debug Window), enter the following:

    <pre><code class="language-lisp">
    (progn
      (build-lisp-image "alisp.dxl" :case-mode :case-insensitive-upper
                        :include-ide nil :restart-app-function nil)
      (sys:copy-file "sys:allegro-ansi.exe" "sys:alisp.exe"))
    </code></pre>

* A non-IDE Lisp image & exe (alisp.exe) will be created in the ACL directory to be used with Emacs. Exit the ACL IDE.
* OPTIONAL: Download the [Franz documentation](https://franz.com/support/documentation/) from [https://franz.com/ftp/pub/acl10.1/acl10.1doc.zip](https://franz.com/ftp/pub/acl10.1/acl10.1doc.zip) and use WinZip to unpack the files into the c:\bin\acl-6.2 directory.


## <a name="Installing LispWorks">6\. Installing LispWorks</a> (optional)

* In order to use LispWorks with ILISP, it is necessary to create a non-gui console version of the LispWorks executable. This is not possible with the free downloadable "Personal" version of LispWorks. It is only possible with the commercial "Professional" and "Enterprise" editions. These can be ordered off the LispWorks web site at [www.lispworks.com](http://www.lispworks.com). The following instructions assume that you have the commercial version of LispWorks. If you don't, skip this section.
* Run the LispWorks installation program and install LispWorks into the c:\bin\lispworks-4.2 directory.
* Create a file called console.lisp in the c:\home\lisp\ directory. This file will be used to create the non-gui console version of the LispWorks executable. The file should have the following lines in it:

    <pre><code class="language-lisp">
    (load-all-patches)
    (save-image "lw42-console" :console t :environment nil)
    (quit)
    </code></pre>

* Go to a dos command line and enter the following commands to create the non-gui console version of the LispWorks executable:

    <pre>
    cd c:\bin\lispworks-4.2
    lispworks-4200 -init c:\home\lisp\console.lisp
    </pre>

 This will result in the lw42-console.exe file being created in the c:\bin\lispworks-4.2 directory.


## <a name="Installing Corman CL">7\. Installing Corman CL</a>

* Download the precompiled binaries for [Corman Lisp](http://www.corman.net/) for Windows from: [http://www.cormanlisp.com/cgi/rgcorman/ccl_download_form.cgi](http://www.cormanlisp.com/cgi/rgcorman/ccl_download_form.cgi)
* Run the downloaded installation program and install Corman CL into the c:\bin\corman-2.5 directory.
* Open the file "c:\bin\corman-2.5\init.lisp", then remove or comment out the following line (line #3):

    <pre><code class="language-lisp">
    (setf (current-directory) *cormanlisp-directory*)
    </code></pre>


## <a name="Configuration steps">8\. Configuration steps</a>

* Setup environmental variables either in the autoexec.bat file (e.g. - Windows/98) or the Control Panel/System (e.g. - Windows/NT/2000/XP):

    * HOME: Should be set to c:/home (Note: use "/" rather than "\". Note also that you should make certain that you don't have both a User and a System HOME variable set up when setting up the HOME environmental variable under Windows/NT/2000/XP. If you do, the User variable will override the System one. Delete one of the two (better to leave the system one if you want multiple login users to use your same emacs configuration) and make certain that it is set to the c:\home directory.).
    * EMACSDIR: Should be set to c:\bin\emacs-26.2
    * PATH: The c:\bin\emacs-26.2\bin directory path should be prepended to the existing PATH to ensure that the Emacs executable can be found (e.g. -- set path=c:\bin\emacs-26.2\bin;%path%).

* If the specified directory locations were used, no changes to the .emacs file should be necessary. If file locations were changed, the directory locations that are specified in the first section of the .emacs file (the "Site-Specific Variables" section) should be modified with the correct directory locations (Note: when specifying directory locations in the .emacs file, use "/" rather than "\" as the directory separator character).
* If not all the Lisp implementations were downloaded, the .emacs file will still work "as is". If you did download multiple installations, you will probably want to change the lisp-used variable (default value is the first lisp implementation in the list of lisp implementations that you installed) to default to the main lisp implementation that you will be using (you don't have to change it, you can still toggle to the other lisp implementations using "C-M-F5").
* Reboot the PC after making the above changes and configuration settings.
* Start Emacs (either by using the Start menu's Gnu Emacs shortcut or by using Run on the Windows Start menu with the command "c:\emacs\bin\runemacs.exe") and do the following:
    * If you installed the cua.el utility:
        * Double-click on the "site" directory to drill down into that directory.
        * Compile the cua.el file by pressing "B" when the cursor is on the file name in the dired window (answer "y" to the "Byte-Compile (y or n)" prompt).
    * If you installed the cua-emul.el utility:
        * Double-click on the "site" directory to drill down into that directory.
        * Compile the cua-emul.el file by pressing "B" when the cursor is on the file name in the dired window (answer "y" to the "Byte-Compile (y or n)" prompt).
    * If you installed the color-theme.el utility:
        * Double-click on the "site" directory to drill down into that directory.
        * Compile the color-theme.el file by pressing "B" when the cursor is on the file name in the dired window (answer "y" to the "Byte-Compile (y or n)" prompt).
    * If you installed CLISP and ILISP, do the following:
        * Press "C-M-F5" until the minibuffer window at the bottom says "lisp-used: :clisp-ilisp" indicating that you have toggled to CLISP mode.
        * Start CLISP by pressing "F5".
        * After starting ILISP/CLISP in Emacs, use "M-x ilisp-compile-inits" to compile the CLISP Lisp initialization files. When finished, press "F4" to close the *clisp-hs* window.
    * If you installed Lispworks and ILISP, do the following:
        * Press "C-M-F5" until the minibuffer window at the bottom says "lisp-used: :lw-ilisp" indicating that you have toggled to LispWorks mode.
        * Start LispWorks by pressing "F5".
        * After starting ILISP/CLISP in Emacs, use "M-x ilisp-compile-inits" to compile the LispWorks Lisp initialization files. When finished, press "F4" to close the window.
* All configuration steps should now be complete - exit Emacs by pressing "C-x C-c" and pressing "y" when you get the "Do you really want to exit Emacs ? (y or n)" prompt in the minibuffer.


## <a name="Testing the environments">9\. Testing the environments</a>

* Start Emacs (either by using the Start menu's Gnu Emacs shortcut or by using Run on the Windows Start menu with the command "c:\emacs\bin\runemacs.exe") and do the following:
    * If you installed CLISP, do the following:
        * Click in the dired window and press "C-M-F5" until the minibuffer window at the bottom says "lisp-used: :clisp-ilisp" indicating that you have toggled to CLISP mode).
        * Test that CLISP runs by pressing "F5" (CLISP should open in the bottom window and the ILISP menu should appear in the Emacs menu bar).
            * Click in the *clisp-hs* window and press "C-h b" (this will show you the keybindings that are in effect for ILISP/CLISP).
            * Click in the *Help* window and scroll through the keybinding list. When finished, press "F4" to close that window.
            * Click in the *clisp-hs* window and press "F4" to close that window.
    * If you installed ACL, do the following:
        * Click in the dired window and press "C-M-F5" until the minibuffer window at the bottom says "lisp-used: :acl-eli" indicating that you have toggled to ACL mode).
        * Test that ACL runs by pressing "F5". The ACL icon will appear in the Windows system tray.
            * A Listener for ACL should also appear in an Emacs *common-lisp* buffer and the ACL menus should appear in the Emacs menu bar.
            * Click in the *common-lisp* window and press "C-h b" (this will show you the keybindings that are in effect for ELI/ACL).
            * Click in the *Help* window and scroll through the keybinding list. When finished, press "F4" to close that window.
            * Click in the *common-lisp* window and select the ACLFile/Exit Allegro CL menu option (this will close down the ACL connection and the ACL application will end).
    * If you installed LispWorks, do the following:
        * Click in the dired window and press "C-M-F5" until the minibuffer window at the bottom says "lisp-used: :lw-ilisp" indicating that you have toggled to LispWorks mode).
        * Test that LispWorks runs by pressing "F5" (LispWorks should open in the bottom window and the ILISP menu should appear in the Emacs menu bar).
            * Click in the *lispworks* window and press "C-h b" (this will show you the keybindings that are in effect for ILISP/LispWorks).
            * Click in the *Help* window and scroll through the keybinding list. When finished, press "F4" to close that window.
            * Click in the *lispworks* window and press "F4" to close that window.
    * If you installed Corman, do the following:
        * Click in the dired window and press "M-C-F5" until the minibuffer window at the bottom says "lisp-used: :corman-inf" indicating that you have toggled to Corman mode).
        * Test that Corman runs by pressing "F5".
            * Click in the *inferior-lisp* window and press "C-h b" (this will show you the keybindings that are in effect for Corman in Inferior Lisp Mode).
            * Click in the *Help* window and scroll through the keybinding list. When finished, press "F4" to close that window.
            * Click in the *inferior-lisp* window and press "F4" to close that window.
    * If you installed the documentation, you can test that the online documentation works by pressing "F1" (for the HyperSpec) or "M-F1" (for CLtL2) when the cursor is on a Lisp keyword (e.g. -- "defun" or "setq"). This should bring up a browser with the documentation for that keyword. See the function key definitions below for accessing other documentation.
    * Exit Emacs by pressing "C-x C-c" and pressing "y" when you get the "Do you really want to exit Emacs ? (y or n)" prompt in the minibuffer.

You should now have a working Windows/Emacs setup that supports a number of different Lisp implementations.



## <a name="Sample .emacs file">A. Sample .emacs file</a>

* Following is a brief explanation of each of the sections of the sample [.emacs](.emacs) file:
    * **Site-Specific Variables:** Contains directory locations for the different Lisp implementations, Lisp documentation and Emacs-specific files. It also sets the default Lisp implementation (Note: if you want to specify a different default Lisp implementation, change the lisp-used variable to one of :clisp-ilisp, :clisp-inf, :acl-eli, :lw-ilisp, :corman-inf).
    * **Initial Code Load:** Loads Emacs files that are needed later on.
    * **System Customizations:** Some standard customizations that make Emacs easier to use.
    * **Programming - Common Lisp:** CL-specific setup.
    * **Programming -  Elisp:** Emacs Lisp-specific setup.
    * **Lisp Key Overrides:** ILISP & ELI have their own key mappings - here are some extra ones:
        * F1 - Brings up the CL Hyperspec (if it was installed) documentation page for the Lisp symbol under the cursor using the default browser.
        * S-F1 - Brings up the CL Hyperspec (if it was installed) documentation page for the Lisp symbol under the cursor using w3 (if it was installed).
        * C-u F1 - Brings up the CL Hyperspec (if it was installed) documentation page for the format character under the cursor using the default browser.
        * C-u S-F1 - Brings up the CL Hyperspec (if it was installed) documentation page for the format character under the cursor using w3 (if it was installed).
        * C-F1 - Brings up the Franz CL (if it was installed) documentation page for the Lisp symbol under the cursor using the default browser.
        * C-S-F1 - Brings up the Franz CL (if it was installed) documentation page for the Lisp symbol under the cursor using w3 (if it was installed).
        * M-F1 - Brings up the CLtL2 (if it was installed) documentation page for the Lisp symbol under the cursor using the default browser.
        * M-S-F1 - Brings up the CLtL2 (if it was installed) documentation page for the Lisp symbol under the cursor using w3 (if it was installed).
        * C-M-F1 - Brings up the info (if it was installed) documentation page for the Lisp symbol under the cursor.
        * F5 - Starts the appropriate Lisp mode for the Lisp implementation that was selected (use C-M-F5 to select the Lisp implementation before pressing F5 to start it).
        * C-M-F5 - Toggles between the different installed Lisp implementations.
        * M-p - For ACL only, in a Lisp listener, retrieves the previous historical entry (this is already the binding in ILISP).
        * M-n - For ACL only, in a Lisp listener, retrieves the next historical entry (this is already the binding in ILISP).
        * C-c d - For ACL only, does a "Do What I Mean" eval/compile (see function documentation for eli-lisp-eval-or-compile-dwim).
        * C-c x - For ACL or the ILISP Lisps, does a "Do What I Mean" eval/compile (see function documentation for copy-eval-dwim-lisp).
        * C-c e - Prompts for a Lisp form to evaluate.
        * C-c ; - Comments out the sexp that the cursor is on (repeated presses comment outward sexp's). (see function documentation for insert-balanced-comments).
        * C-c : - Uncomments comments that were inserted with insert-balanced-comments (see function documentation for remove-balanced-comments).
    * **Standard Key Overrides:** Some convenience key setups. The following key bindings have been added:
        * mouse-2 - Brings up a menu of Lisp symbols when in a Lisp source file.
        * F6 - Switches focus to another window (when multiple windows are in an Emacs frame).
        * F7 - Closes all windows except for the window that currently has focus.
        * C-F7 - If ECB was loaded, toggles between multiple ECB windows and just a single source window.
        * M-F7 - If ECB was loaded, toggles between different ECB window layouts.
        * F12 - Emacs shell
        * C-F12 - Default shell
        * M-F12 - Bash shell (if present on computer)
        * C-c f - Finds the Emacs Lisp function at the cursor location.
        * C-c F - Finds the file whose name is at the cursor location.
        * C-c s - Search forward using the symbol that is currently under the cursor.
        * C-c r - Search backward using the symbol that is currently under the cursor.
        * C-c / - Do a word completion (using hippie-expand).
        * C-c ] - Goto the matching parenthesis of the parenthesis under the cursor.
        * C-c g - Goto a line (prompts for line#).
        * C-c a - Select the entire buffer contents.
    * **MS Windows Customizations:** These settings make Emacs feel more like a Windows application. For Windows machines, they add the following bindings:
        * In dired mode, when you press the "w" key when the cursor is on a file name, the file will be opened with the default application for that file type.
        * When cua.el is installed:
            * C-c - Standard Windows copy.
            * C-x - Standard Windows cut.
            * C-v - Standard Windows paste.
            * C-z - Standard Windows undo.
        * When cua-emul.el is installed:
            * M-F4 - Close frame (closes Emacs if only one Emacs frame is open).
            * C-F4 - Close the current buffer.
            * C-TAB - Switch to next buffer.
            * C-S-TAB - Switch to previous buffer.
    * **Mac OS X Customizations:** Mac OS X specific customizations:
        * Keypad DEL - Deletes character under cursor.
        * C-kp-HOME - Goes to beginning of buffer.
        * C-kp-END - Goes to end of buffer.
    * **Start Directory:** After Emacs starts up, the start directory will be the HOME directory.


## <a name="Mac OS X Setup Instructions">B. Mac OS X Setup Instructions</a>

* At this stage, support for Mac OS X on this page is very limited and (since I do not use Mac OS X on a regular basis) I would welcome someone else providing a separate page of instructions. This section lists the things that are different from the above Windows instructions.
* There are multiple different versions of Emacs (some Aqua-ized, some not) for the Mac and there will eventually be an official FSF Emacs that supports the Aqua GUI. I used [David Caldwell's Carbon build](http://www.porkrind.org/emacs/) since the Mac I tested on had OS X version 10.2\. Other Emacs builds on other versions of Mac OS X may or may not have problems using my .[emacs](.emacs) file.
* All of the places in the above Windows instructions where "c:/home" is mentioned, you should substitute the path to your own home directory (referenced as "~/" in the remainder of these instructions). The .[emacs](.emacs) file is setup in such a way that it is not necessary to set any environmental variables manually.
* After downloading ILISP, you will need to modify the makefile & run make as per the instructions in ILISP.
* The optional cua.el and cua-emul.el utilities caused some issues under Mac OS X; therefore, the .[emacs](.emacs) file ignores them if you're not on Windows. I have not tested the optional ECB utility on Mac OS X but it should work with the settings that are in the .[emacs](.emacs) file so long as all the components are installed in the directories that were specified under the Windows instructions. The optional color-theme.el utility works fine.
* The .[emacs](.emacs) file that is supplied will work unchanged with either a Windows installation or a Mac OS X installation. For a Mac install, the assumed directory locations are listed below.
* The following Lisp installations are supported by the .[emacs](.emacs) file. I am not providing full build instructions for them as was provided for the Windows installation. The things that you need to know to use these installations with the .[emacs](.emacs) file are:
    * CLISP version 2.29: Installed and built with [Fink](http://fink.sourceforge.net/pdb/package.php/clisp). (Note: in order to build CLISP with Fink, you will need the [Apple December 2002 Development Tools](http://developer.apple.com/tools/download/)). The resulting clisp executable should be installed in the /sw/bin/ directory. This is the only source code based install, all the other Lisp installs are binary; therefore, the CLISP installation takes the longest and is the most complicated to do.
    * ACL version 10.1: Installed from [Franz's download page](https://franz.com/downloads/).
    * OpenMCL version 0.13: The [binary distribution](https://ccl.clozure.com/download.html) was downloaded and installed in the ~/bin/ directory. The OpenMCL executable should be at ~/bin/ccl/scripts/openmcl.
    * SBCL version 0.8.2.8: The [binary distribution](http://prdownloads.sourceforge.net/sbcl/sbcl-1.2.11-x86-64-darwin-binary.tar.bz2) was downloaded and unpacked. The files src/runtime/sbcl and output/sbcl.core were moved to the ~/bin/ directory.
    * LispWorks 4.3: I was not able to get a trial version of LispWorks to try out on Mac OS X; however, I have made the changes to the .emacs file that I think will be necessary to support it. The LispWorks executable should be at ~/bin/lispworks-4.3/lw43-console.
