# This file is used by vagrant to create the virtual environment

Vagrant::Config.run do |config|

    # Set up the box
    config.vm.box = "precise"

    # If box does not exist fetch it from this url
    config.vm.box_url = "http://files.vagrantup.com/precise64.box"

    # Port forwarding
    config.vm.forward_port 3838, 3838

    # Provisioning
    config.vm.provision :puppet,
    :options => ["--verbose", "--debug"] do |puppet|
        puppet.manifests_path = "puppet/manifests"
        puppet.manifest_file = "r-shiny-server-env.pp"
        puppet.module_path = "puppet/modules"

    end

end