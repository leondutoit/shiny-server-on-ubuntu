
Vagrant.configure(2) do |config|

  config.vm.box = "ubuntu/trusty64"
  config.vm.network "forwarded_port", guest: 3838, host: 3838
  config.vm.provider "virtualbox" do |vb|
    vb.customize ["modifyvm", :id, "--memory", "2048"]
    vb.customize ["modifyvm", :id, "--cpus", "2"]
  end

$script = <<BOOTSTRAP
sudo apt-get update
sudo apt-get -y install git gcc
sudo apt-get -y install libxml2-dev libcurl4-openssl-dev libssl0.9.8 libcairo2-dev
sudo apt-get -y install mesa-common-dev
sudo apt-get -y install openjdk-6-jre
sudo apt-get -y install openjdk-6-jdk
sudo add-apt-repository ppa:marutter/rrutter
sudo apt-get update
sudo apt-get -y install r-base r-base-dev
sudo R CMD javareconf
sudo R -e "install.packages('http://cran.r-project.org/src/contrib/Archive/shiny/shiny_0.10.1.tar.gz', repos=NULL, type='source')"
sudo R -e "install.packages('rmarkdown', repos = 'http://cran.rstudio.com/', dep = TRUE)"
sudo R -e "install.packages('devtools', repos = 'http://cran.rstudio.com/', dep = TRUE)"
sudo R -e "install.packages('ggplot2', repos = 'http://cran.rstudio.com/', dep = TRUE)"
sudo R -e "install.packages('fpc', repos = 'http://cran.rstudio.com/', dep = TRUE)"
sudo R -e "install.packages('lsa', repos = 'http://cran.rstudio.com/', dep = TRUE)"
sudo R -e "install.packages('extrafont', repos = 'http://cran.rstudio.com/', dep = TRUE)"
sudo R -e "install.packages('igraph', repos = 'http://cran.rstudio.com/', dep = TRUE)"
sudo R -e "install.packages('reshape2', repos = 'http://cran.rstudio.com/', dep = TRUE)"
sudo R -e "install.packages('wordcloud', repos = 'http://cran.rstudio.com/', dep = TRUE)"
sudo R -e "install.packages('stringr', repos = 'http://cran.rstudio.com/', dep = TRUE)"
sudo R -e "install.packages('jsonlite', repos = 'http://cran.rstudio.com/', dep = TRUE)"
sudo R -e "install.packages('RWeka', repos = 'http://cran.rstudio.com/', dep = TRUE)"
sudo R -e "install.packages('tm', repos = 'http://cran.rstudio.com/', dep = TRUE)"
sudo R -e "install.packages('plyr', repos = 'http://cran.rstudio.com/', dep = TRUE)"
sudo R -e "install.packages('dplyr', repos = 'http://cran.rstudio.com/', dep = TRUE)"
sudo R -e "install.packages('SnowballC', repos = 'http://cran.rstudio.com/', dep = TRUE)"
sudo R -e "install.packages('mclust', repos = 'http://cran.rstudio.com/', dep = TRUE)"
sudo R -e "devtools::install_github('rstudio/shiny-incubator', ref='5a78877229deb60fc00ee21d6f2170c90099443b', dep = TRUE)"
sudo R -e "devtools::install_github('dcurrier/shinythings', ref='83fd649709601b5b6154e75d52b79f4fdfe66fae', dep = TRUE)"
sudo apt-get -y install gdebi-core
wget http://download3.rstudio.org/ubuntu-12.04/x86_64/shiny-server-1.2.3.368-amd64.deb
sudo gdebi shiny-server-1.2.3.368-amd64.deb
sudo dpkg -i *.deb
rm *.deb
sudo ln -s /vagrant/apps /srv/shiny-server
BOOTSTRAP

  config.vm.provision :shell, :inline => $script
end
