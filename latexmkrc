$recorder = 1;
$pdf_mode = 1;
$pdflatex = 'pdflatex %O -interaction=nonstopmode -synctex=1 %S';
$clean_ext = 'synctex.gz synctex.gz(busy) bbl nav snm run.xml tmpltx ist';

# Use xdg-open to determine standard pdf previewer
$pdf_previewer = 'xdg-open %O %S';

# Custom dependency and function for nomencl package
add_cus_dep( 'nlo', 'nls', 0, 'makenlo2nls' );
sub makenlo2nls {
  system( "makeindex -s nomencl.ist -o \"$_[0].nls\" \"$_[0].nlo\"" );
}
push @generated_exts, 'nlo', 'nls';

# Custom dependency and function for glossaries package
add_cus_dep('glo', 'gls', 0, 'run_makeglossaries');
add_cus_dep('acn', 'acr', 0, 'run_makeglossaries');
sub run_makeglossaries {
  if ( $silent ) {
    system "makeglossaries -q '$_[0]'";
  }
  else {
    system "makeglossaries '$_[0]'";
  };
}
push @generated_exts, 'glo', 'gls', 'glg';
push @generated_exts, 'acn', 'acr', 'alg';
$clean_ext .= ' %R.ist';
