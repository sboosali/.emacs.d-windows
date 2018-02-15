
/* 


Querying:

nix-env -f "<nixpkgs>" -qaP -A emacsPackagesNg.melpaPackages | grep

Building: 

nix-build emacs.nix

Running:

./result/bin/emacs

Both:

nix-build emacs.nix && ./result/bin/emacs


https://nixos.org/wiki/Emacs_configuration

*/
{ pkgs ? import <nixpkgs> {} }: 


let
  myEmacs = pkgs.emacs25; 
  emacsWithPackages = (pkgs.emacsPackagesNgGen myEmacs).emacsWithPackages; 
in

  emacsWithPackages (epkgs: ([
  
  ]) ++ (with epkgs.melpaPackages; [

 dante
 haskell-mode # haskell-c2hs-mode
 color-theme
# dhall-mode
 magit          # ; Integrate git <C-x g>
 helm
# undo-tree      # ; <C-x u> to show the undo tree
# zoom-frm       # ; increase/decrease font size for all buffers %lt;C-x C-+>
 nix-mode
 projectile
 multi-term
 tabbar
 smooth-scrolling
 centered-cursor-mode
 dash
 s
 projectile
 flx-ido
 evil
 window-purpose
 paredit
use-package
flycheck
#xref

# color-theme

  ]) ++ (with epkgs.melpaStablePackages; [
  
  ]) ++ (with epkgs.orgPackages; [
    org
    
  ]) ++ (with epkgs.elpaPackages; [
    auctex         # ; LaTeX mode
    #vlfi

  ]))
