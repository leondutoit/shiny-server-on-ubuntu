
Deploy and start R Shiny Server on Ubuntu 12.04 with Puppet Provisioner
=======================================================================

Why?
----

- Serve your Shiny web apps from Shiny server hosted on your own virtual Ubuntu Linux OS within minutes (with useful data analysis packages in R pre-installed).
- Share your project with others without worrying about dependencies.
- Get a blueprint for deployment to a remote server with Puppet


Background
----------

RStudio recently released the R Shiny Server - a server application
that can be installed on Linux machines. Shiny web applictions written in R can then be served as standalone apps on the web. This is nothing short of brilliant. Have a look at the source and manual installation instructions [here](https://github.com/rstudio/shiny-server).


About this project
------------------

It is common to have your Linux server in the cloud - on AWS EC2 for example. In industrial settings you typically have both staging and production systems. It is also quite normal to change the hosting of your web apps from one node to another. The point is that going through the manual setup each time, and keeping all dependencies up-to-date, will quickly become unworkable.

This may not be a problem for people who are willing to host their apps with [RStudio](http://rstudio.github.com/shiny/tutorial/#deployment-web) but if your company needs its data to be private, or if you want full control of your execution environment having your own Linux box is the best option.

Puppet is a provisioner that makes it easier to manage resources on remote servers. But I am not going to assume you have a remote server - for personal prototyping and development that's overkill.

Instead, the instructions give you a virtual Ubuntu 12.04 Linux machine running on Oracle's Virtual Box. And the great thing is that it installs and starts R Shiny server for you, using Puppet configuration.

After setting this up you can test you shiny apps locally on shiny server after a few commands. You can also re-use the puppet configuration to deploy to your own remote server.


Installation Instructions
------------

Note: on Windows ssh may be slightly problematic - see the Vagrant information link below for hints.

1. Install Oracle Virtual Box: http://download.virtualbox.org/virtualbox/4.2.0/
2. Install Vagrant: http://downloads.vagrantup.com/tags/v1.0.6
3. Install Virtual Box Guest Additions: `vagrant gem install vagrant-vbguest`
4. Checkout this repository: `git clone git://github.com/leondutoit/shiny-server-on-ubuntu.git`
5. Navigate to the repository folder locally: `cd shiny-server-on-ubuntu`
6. Run: `vagrant up`
7. If that did not work try: `vagrant reload` (you will need a good network connection since many resources have to be copied to your machine; this can often be a source of trouble when running vagrant up). When in doubt run: `vagrant destroy; vagrant up`.


Workflow Instructions
---------------------

On your host, in `/shiny-server-on-ubuntu` there are two folders: `/apps` and `/puppet`.
If you place your shiny app folders in `/apps`, they will be served at `localhost:3838/apps/your_app_folder/`.

1. ssh to your virtual machine and start having fun! `vagrant ssh`
2. After `vagrant ssh`, do `cd /vagrant; ls` (Files on your local machine will be visible at /vagrant$ in the VM)
3. To see your apps just make sure you develop them in `/apps` - they will be served without having to set up anything - no reloads, no restarts!
4. The file: usefulpackages.R contains all the add-on packages that are installed on the virtual machine. If you shiny app depends on a package not installed in the R environment on the VM you have to install it manually. You can do like so: `vagrant ssh; sudo R, R > install.packages("whatever", dep=TRUE)`.


Information about Vagrant and Puppet
------------------------------------

- navigate to `/shiny-server-on-ubuntu/puppet` to look at the configuration
- the "Vagrantfile" contains the configuration to set up the VM
- I highly recommend working through this tutorial to get to know [Vagrant](http://docs.vagrantup.com/v1/docs/getting-started/index.html)
- For a reference visit [Puppet](https://puppetlabs.com/)


Other great things about this
-----------------------------

By using a vagrant virtual environment for your Shiny app you can share not just your code and your data, but also your working environment. If you put that into a version controlled repo you go even further by sharing the history of your project too.

This will make collaborative development more transparent and remove barriers caused by runtime dependencies (do you have all the right packages installed to run the app??).




