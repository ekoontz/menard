# t3a.xlarge | $0.15/hour | 4200 ms/parse
# 
# 
sudo yum -y install java-1.8.0-openjdk git
git clone https://github.com/ekoontz/menard.git
cd menard/
mkdir ~/bin
curl https://raw.githubusercontent.com/technomancy/leiningen/stable/bin/lein > ~/bin/lein
chmod 755 ~/bin/lein
export PATH=$PATH:~/bin
echo "(load \"menard/nederlands\")(take 10 (repeatedly #(->> (-> \"een aardig bedroefd buitengewoon afzonderlijk dik nieuwswaardig ongerust teleurgesteld eigenwijs geheim verrast prachtig slim vrouw probeert mannen te zien\" menard.nederlands/parse time) (map menard.nederlands/syntax-tree) (map println))))" | lein repl
