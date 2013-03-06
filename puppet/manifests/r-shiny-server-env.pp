## This file sets up the working environment in the VM
## Installation depends on wget module in /puppet/manifests/wget/

include wget

class install_r_and_packages {

    # Use the relevant Ubuntu box with the cran mirror
    # Install latest version of R
    exec {'prepare-r-install-env':
        provider => shell,
        creates => '/usr/lib/R',
        command => 'echo "deb http://cran.uib.no/bin/linux/ubuntu precise/" >> /etc/apt/sources.list;
            /usr/bin/apt-key adv --keyserver keyserver.ubuntu.com --recv-keys E084DAB9;
            /usr/bin/apt-get update;',
    }

    # Get the core environment
    package {['r-base', 'r-base-core', 'r-recommended', 'r-base-html']:
        ensure => 'present',
        require => Exec[prepare-r-install-env],
    }

    # Install useful packages
    exec {'install-r-packages':
        provider => shell,
        require => Package['r-base'],
        creates => '/tmp/installed-r-packages',
        command => '/usr/bin/R -f /vagrant/usefulpackages.R;
            echo "TRUE" >> /tmp/installed-r-packages;',
    }

}

class install_shiny_server {

    exec {'run-update':
        provider => shell,
        command => '/usr/bin/apt-get update;',
    }

    # Make sure these shiny-server dependencies exist
    package {['software-properties-common', 'python-software-properties', 'python', 'g++', 'make']:
        ensure => 'present',
        require => Exec[run-update],
    }

    # Install shiny server
    exec {'shiny-server-install':
        provider => shell,
        require => [Exec['install-r-packages'], Package['software-properties-common', 'python-software-properties', 'g++']],
        command => '/usr/bin/add-apt-repository ppa:chris-lea/node.js;
            /usr/bin/apt-get update;
            /usr/bin/apt-get -y install nodejs npm;
            /usr/bin/npm install -g shiny-server;',
    }

    # Create shiny system user
    user {'shiny':
        ensure => present,
        system => true,
        shell => '/bin/bash',
        name => 'shiny',
    }

    # Get upstart file
    wget::fetch {'download':
        source => 'https://raw.github.com/rstudio/shiny-server/master/config/upstart/shiny-server.conf',
        destination =>  '/etc/init/shiny-server.conf',
        timeout => 0,
        verbose => false,
    }


    # Create necessary directories
    file {'/var/shiny-server':
        ensure => 'directory',
    }

    file {'/var/shiny-server/www':
        require => File['/var/shiny-server'],
        ensure => 'directory',
    }

    file {'/var/shiny-server/log':
        require => File['/var/shiny-server'],
        ensure => 'directory',
    }

}

class start_shiny {

    # Create symlink for upstart to work
    file {'/etc/init.d/shiny-server':
        ensure => link,
        target => '/lib/init/upstart-job',

    }

    # Start service
    service {'shiny-server':
        ensure => running,
        provider => 'upstart',
        require => File['/etc/init.d/shiny-server'],
    }

}

class copy_examples {

    # Copy examples from R to server
    file {'/var/shiny-server/www/examples':
        ensure  => 'directory',
        source  => '/usr/local/lib/R/site-library/shiny/examples',
        recurse => true,
    }

}

class create_app_symlink {

    # Set up symlink to /vagrant/apps/ or /apps on host machine
    file {'/var/shiny-server/www/apps':
        ensure => link,
        target => '/vagrant/apps/',
    }

}

# Define the sequence of events

stage {'first': before => Stage['main']}
stage {'second': require => Stage['first']}
stage {'third': require => Stage['second']}
stage {'fourth': require => Stage['third']}
stage {'last': require => Stage['fourth']}

# Run it in this order

class {
    'install_r_and_packages': stage => first;
    'install_shiny_server': stage => second;
    'copy_examples': stage => third;
    'create_app_symlink': stage => fourth;
    'start_shiny': stage => last;
}


