$recorder = 1;
$pdf_mode = 1;
$pdflatex = 'pdflatex %O -interaction=nonstopmode -synctex=1 %S';
$clean_ext = 'synctex.gz synctex.gz(busy) bbl nav snm run.xml';

# Use xdg-open to determine standard pdf previewer
$pdf_previewer = 'xdg-open %O %S';
