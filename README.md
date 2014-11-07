
### Intention

This project is intended to provide a base VM that can be used to serve shiny applications locally without having to install shiny-server on your own machine. This can be useful for development puropses. People who find this useful are encouraged to clone the repo and use the VM or fork it and merge their own applications into their fork. Contributions that aim to improve the usefulness of the base VM are if course welcome.

### Setup instructions

1. Download and install Oracle Virtual Box [here](http://download.virtualbox.org/virtualbox/4.2.0/)
2. Download and install Vagrant [here](https://www.vagrantup.com/downloads)
3. Install Virtual Box Guest Additions: `$ vagrant plugin install vagrant-vbguest`
4. Checkout this repository: `$ git clone https://github.com/leondutoit/shiny-server-on-ubuntu.git`
5. Navigate to the repository folder locally: `cd shiny-server-on-ubuntu`
6. Run: `vagrant up`
7. Shiny server is now visible at `localhost:3838` on your machine.

### Workflow

Put your apps in `shiny-server-on-ubuntu/apps` and they will be visible at `localhost:3838/apps/` on your machine.
