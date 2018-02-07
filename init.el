;; -----------------------------------------------------------------------------

(package-initialize)

(if (require 'quelpa nil t)
    (quelpa-self-upgrade)
  (with-temp-buffer
    (url-insert-file-contents
     "https://raw.github.com/quelpa/quelpa/master/bootstrap.el")
    (eval-buffer)))

(package-install 'let-alist)

(quelpa '(quelpa-use-package
          :fetcher github
          :repo "quelpa/quelpa-use-package"))

(require 'quelpa-use-package nil t)

;; -----------------------------------------------------------------------------

;; (quelpa '(haskell-mode
;;           :fetcher github
;;           :repo "haskell/haskell-mode"))

;; -----------------------------------------------------------------------------

(setq inhibit-splash-screen t)
(column-number-mode 1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)

(set-frame-font "DejaVu Sans Mono-12" nil t)
(setq default-frame-alist '((font . "DejaVu Sans Mono-12")))

(setq-default fill-column 80)

(setq-default cursor-type 'bar)

;; (load-theme 'zenburn t)

;; -----------------------------------------------------------------------------

(use-package zenburn-theme            :quelpa)
(use-package haskell-mode             :quelpa)
(use-package rainbow-delimiters       :quelpa)
;; (use-package ace-isearch              :quelpa)
(use-package ace-jump-mode            :quelpa)
;; (use-package adjust-parens            :quelpa)
;; (use-package apel                     :quelpa)
;; (use-package auctex                   :quelpa)
(use-package avy                      :quelpa)
(use-package boogie-friends           :quelpa)
(use-package buttercup                :quelpa)
(use-package button-lock              :quelpa)
(use-package cask-mode                :quelpa)
;; (use-package cedet-devel              :quelpa)
(use-package ceylon-mode              :quelpa)
(use-package cl-lib                   :quelpa)
(use-package clojure-mode             :quelpa)
(use-package cmake-font-lock          :quelpa)
(use-package company                  :quelpa)
(use-package company-flow             :quelpa)
(use-package company-ghc              :quelpa)
(use-package company-nixos-options    :quelpa)
(use-package company-racer            :quelpa)
(use-package dash                     :quelpa)
(use-package deferred                 :quelpa)
;; (use-package delight                  :quelpa)
(use-package diminish                 :quelpa)
;; (use-package elfeed                   :quelpa)
(use-package el-get                   :quelpa)
(use-package elm-mode                 :quelpa)
;; (use-package emacs-async              :quelpa)
;; (use-package emacs-racer              :quelpa)
(use-package epl                      :quelpa)
(use-package f                        :quelpa)
(use-package fic-mode                 :quelpa)
(use-package fill-column-indicator    :quelpa)
;; (use-package flim                     :quelpa)
(use-package flx                      :quelpa)
(use-package flycheck                 :quelpa)
(use-package flycheck-ats2            :quelpa)
(use-package flycheck-cask            :quelpa)
;; (use-package flycheck-clangcheck      :quelpa)
(use-package flycheck-color-mode-line :quelpa)
(use-package flycheck-flow            :quelpa)
(use-package flycheck-ghcmod          :quelpa)
(use-package flycheck-haskell         :quelpa)
(use-package flycheck-hdevtools       :quelpa)
(use-package flycheck-liquidhs        :quelpa)
(use-package flycheck-mypy            :quelpa)
;; (use-package flycheck-pmd             :quelpa)
;; (use-package flycheck-protobuf        :quelpa)
(use-package geiser                   :quelpa)
(use-package gitattributes-mode       :quelpa)
(use-package git-auto-commit-mode     :quelpa)
(use-package gitconfig-mode           :quelpa)
(use-package gitignore-mode           :quelpa)
(use-package god-mode                 :quelpa)
(use-package graphviz-dot-mode        :quelpa)
(use-package groovy-mode              :quelpa)
;; (use-package haskell-input-method     :quelpa)
(use-package helm                     :quelpa)
(use-package helm-flx                 :quelpa)
(use-package helm-fuzzier             :quelpa)
(use-package helm-gtags               :quelpa)
(use-package helm-nixos-options       :quelpa)
(use-package helm-swoop               :quelpa)
;; (use-package help-fns+                :quelpa)
(use-package hi2                      :quelpa)
(use-package idris-mode               :quelpa)
(use-package iedit                    :quelpa)
;; (use-package javadoc-help             :quelpa)
(use-package javadoc-lookup           :quelpa)
;; (use-package javaimp                  :quelpa)
(use-package jq-mode                  :quelpa)
;; (use-package k3-mode                  :quelpa)
;; (use-package keep-formation           :quelpa)
(use-package kotlin-mode              :quelpa)
(use-package ledger-mode              :quelpa)
(use-package less-css-mode            :quelpa)
;; (use-package let-alist                :quelpa)
(use-package liquid-types             :quelpa)
(use-package lsp-haskell              :quelpa)
(use-package magit                    :quelpa)
(use-package magithub                 :quelpa)
;; (use-package malabar-mode             :quelpa)
(use-package markdown-mode            :quelpa)
(use-package maude-mode               :quelpa)
(use-package multi-term               :quelpa)
(use-package nix-mode                 :quelpa)
(use-package nixos-options            :quelpa)
;; (use-package nlinum                   :quelpa)
(use-package offlineimap              :quelpa)
;; (use-package org-mode                 :quelpa)
(use-package org-trello               :quelpa)
(use-package package                  :quelpa)
(use-package perspective              :quelpa)
(use-package pkg-info                 :quelpa)
(use-package polymode                 :quelpa)
(use-package popup                    :quelpa)
(use-package popwin                   :quelpa)
(use-package pos-tip                  :quelpa)
(use-package powerline                :quelpa)
(use-package projectile               :quelpa)
(use-package prop-menu                :quelpa)
(use-package protobuf-mode            :quelpa)
(use-package purescript-mode          :quelpa)
(use-package qml-mode                 :quelpa)
(use-package rich-minority            :quelpa)
;; (use-package rudel                    :quelpa)
;; (use-package rustfmt                  :quelpa)
(use-package rust-mode                :quelpa)
;; (use-package rust-racer               :quelpa)
(use-package s                        :quelpa)
(use-package scala-mode               :quelpa)
;; (use-package semi                     :quelpa)
(use-package seq                      :quelpa)
(use-package session                  :quelpa)
(use-package shut-up                  :quelpa)
(use-package smart-mode-line          :quelpa)
(use-package smartparens              :quelpa)
(use-package smex                     :quelpa)
(use-package sr-speedbar              :quelpa)
(use-package textile-mode             :quelpa)
(use-package thrift                   :quelpa)
(use-package toml-mode                :quelpa)
(use-package tramp                    :quelpa)
;; (use-package tuareg-mode              :quelpa)
(use-package twittering-mode          :quelpa)
(use-package undo-tree                :quelpa)
(use-package utop                     :quelpa)
(use-package wakatime-mode            :quelpa)
;; (use-package wanderlust               :quelpa)
(use-package with-editor              :quelpa)
(use-package yaml-mode                :quelpa)
(use-package yasnippet                :quelpa)
(use-package z3-mode                  :quelpa)
(use-package zlc                      :quelpa)

;; -----------------------------------------------------------------------------

;; Autosave into ~/.emacs.d/backups
(setq backup-directory-alist
      `(("." . ,(expand-file-name "~/.emacs.d/backups"))))

;; Save all tempfiles in $TMPDIR/emacs$UID/
(defconst emacs-tmp-dir
  (format "%s/%s%s/" temporary-file-directory "emacs" (user-uid)))
(setq auto-save-file-name-transforms `((".*" ,emacs-tmp-dir t)))
(setq auto-save-list-file-prefix emacs-tmp-dir)

;; -----------------------------------------------------------------------------

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-trello-current-prefix-keybinding "C-c o" nil (org-trello))
 '(package-selected-packages
   (quote
	(quelpa-use-package use-package bind-key quelpa package-build))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
