# Parsing

#               bogomips | price      | ms/parse
# Mac                                 | 1700
# t2.micro                            | 6600
# t3a.xlarge    | 4399.54  | $0.15/hour | 4200
# c4.xlarge     | 5800     | $0.10/hour | 3800
# c5n.xlarge    |          | $0.21/hour | 3500
# c6g.2xlarge   |                         1700
# c6g.12xlarge  |                          726
# c6g.16xlarge               $2.18/hour |  300             
# t4g.2xlarge   |            $0.26/hour | 1152
# a1.4xlarge    |                       | 3300

sudo yum -y install java-1.8.0-openjdk git
git clone https://github.com/ekoontz/menard.git
cd menard/
mkdir ~/bin
curl https://raw.githubusercontent.com/technomancy/leiningen/stable/bin/lein > ~/bin/lein
chmod 755 ~/bin/lein
export PATH=$PATH:~/bin
echo "(load \"menard/nederlands\")(take 10 (repeatedly #(->> (-> \"een aardig bedroefd buitengewoon afzonderlijk dik nieuwswaardig ongerust teleurgesteld eigenwijs geheim verrast prachtig slim vrouw probeert mannen te zien\" menard.nederlands/parse time) (map menard.nederlands/syntax-tree) (map println))))" | lein repl
