# Parsing

#               bogomips | price      | ms/parse
# Mac                                 | 2700
# t2.micro                            | 9000
# t3a.xlarge    | 4399.54  | $0.15/hour | 4200
# c4.xlarge     | 5800     | $0.10/hour | 3800
# c5n.xlarge    |          | $0.21/hour | 3500
# c6gd.medium   |          | $0.04/hour | 8300
# c6gd.large    |          | $0.08/hour | 4600
# c6gd.xlarge   |            $0.15/hour | 2900
# c6gd.2xlarge  |            $0.30/hour | 2166
# c6gd.4xlarge  |            $0.60/hour | 1530
# c6gd.8xlarge  |            $1.20/hour | 1250 
# c6gd.12xlarge |                       | 1070
# c6gd.16xlarge |                       | 1070
# t4g.2xlarge   |            $0.26/hour | 2100
# a1.4xlarge    |                       | 3300

sudo yum -y install java-1.8.0-openjdk git
git clone https://github.com/ekoontz/menard.git
cd menard/
mkdir ~/bin
curl https://raw.githubusercontent.com/technomancy/leiningen/stable/bin/lein > ~/bin/lein
chmod 755 ~/bin/lein
export PATH=$PATH:~/bin
echo "(load \"menard/nederlands\")(take 10 (repeatedly #(->> (-> \"een aardig bedroefd buitengewoon afzonderlijk dik nieuwswaardig ongerust teleurgesteld eigenwijs geheim verrast prachtig slim vrouw probeert mannen te zien\" menard.nederlands/parse time) (map menard.nederlands/syntax-tree) (map println))))" | lein repl
