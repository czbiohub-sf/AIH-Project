#!/bin/bash

for i in `ls cut*`; do cut -f2 $i > $i.o; done; #take the second column in tab file and make a new file with .o extension
paste `ls *.o` > all_counts.tsv #paste together (column after column) all these new files with the second column only
cut -f1 `ls cut*[1234567890] | head -1` > genenames.txt #take the first column of one of the tab files and make a gene names file
paste genenames.txt all_counts.tsv > gene_counts.tsv #make a new file with gene names, followed by the counts from each sample
ls cut*[1234567890] > filenames.txt #make a txt file with all the filenames
tr "\n" "\t" < filenames.txt > filenames2.txt #pipe all the filenames.txt file into the tr command, which will replace enter with tab
cat <(echo -ne '\t') filenames2.txt > filenames.txt # add tab to beginning of filenames file to columns line up
#cat filenames.txt `echo "\n"` gene_counts.tsv > gene_counts_final.tsv
awk '{print}' filenames.txt gene_counts.tsv > aih_full_cutfilt_dashed_gene_counts_final.tsv #print out the filenames, followed by the gene counts in a row after row fashion

#remove all unnecessary files
rm *.o
rm genenames.txt
rm gene_counts.tsv 
rm all_counts.tsv
rm filenames2.txt

