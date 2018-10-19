##rem pandoc --extract-media ./myMediaFolder input.docx -o output.md

pandoc --extract-media ./img-readme SocialDeterminatesofMortality.docx -o readme.md
pandoc --extract-media ./img-modeling2 modeling2.docx -o modeling2.md
pandoc --extract-media ./img-process-chr-files process-chr-files.docx -o process-chr-files.md
pandoc --extract-media ./img-process-irs-files process-irs-files.docx -o process-irs-files.md
pandoc --extract-media ./img-process-wonder-files process-wonder-files.docx -o process-wonder-files.md

perl -pi.bak -e 's/{width="\d+\.\d+in"//g' *.md
perl -pi.bak -e 's/height="\d+\.\d+in"}//g' *.md

git add *.md
git add img-*/*.png

#end of script