$recorder = 1;
$pdf_mode = 1;
$pdflatex = 'pdflatex %O -interaction=nonstopmode -synctex=1 %S';
$clean_ext = 'synctex.gz synctex.gz(busy) bbl nav snm run.xml tmpltx ist acn alg glg glo gls acr';

# Use xdg-open to determine standard pdf previewer
$pdf_previewer = 'xdg-open %O %S';

# Custom dependency and function for nomencl package
add_cus_dep( 'nlo', 'nls', 0, 'makenlo2nls' );
sub makenlo2nls {
  system( "makeindex -s nomencl.ist -o \"$_[0].nls\" \"$_[0].nlo\"" );
}
