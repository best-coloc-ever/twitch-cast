'use strict';

var gulp = require('gulp'),
  plumber = require('gulp-plumber'),
  rename = require('gulp-rename');
var autoprefixer = require('gulp-autoprefixer');
var concat = require('gulp-concat');
var uglify = require('gulp-uglify');
var ngmin = require('gulp-ngmin');
var inject = require('gulp-inject');
var flatten = require('gulp-flatten');
var minifycss = require('gulp-minify-css');
var angularFilesort = require('gulp-angular-filesort');
var templateCache = require('gulp-angular-templatecache');
var sass = require('gulp-sass');
var wiredep = require('wiredep');
var gls = require('gulp-live-server');
var exec = require('child_process').exec;

gulp.task('vendor-scripts', function() {

  return gulp.src(wiredep().js)

  .pipe(gulp.dest('dist/vendor'));

});

gulp.task('vendor-css', function() {

  return gulp.src(wiredep().css)

  .pipe(gulp.dest('dist/vendor'));

});

gulp.task('vendor-fonts', function() {
  return gulp.src('./**/*.woff2')
    .pipe(flatten())
    .pipe(gulp.dest('dist/fonts'));

});

gulp.task('img', function() {
  return gulp.src('app/assets/img/**/*')
    .pipe(gulp.dest('dist/img/'))
})

gulp.task('sass', function() {
  return gulp.src(['app/assets/**/*.sass'])
    .pipe(plumber({
      errorHandler: function(error) {
        console.log(error.message);
        this.emit('end');
      }
    }))
    .pipe(sass())
    .pipe(autoprefixer('last 2 versions'))
    .pipe(gulp.dest('dev/assets/'));
});

gulp.task('styles', ['sass'], function() {
  return gulp.src(['dev/assets/**/*.css'])
    .pipe(plumber({
      errorHandler: function(error) {
        console.log(error.message);
        this.emit('end');
      }
    }))
    .pipe(gulp.dest('dist/styles/'))
    // .pipe(rename({
    // suffix: '.min'
    // }))
    .pipe(minifycss())
    .pipe(gulp.dest('dist/styles/'));
});

gulp.task('scripts', ['dev-scripts'], function() {
  return gulp.src(['dev/src/**/*.js', 'dev/src/templates.js'])
    .pipe(angularFilesort())
    .pipe(concat('main.js'))
    .pipe(uglify({
      mangle: false
    }))
    .pipe(gulp.dest('dist/scripts/'));
});

gulp.task('dev-scripts', function() {
  return gulp.src('app/src/**/*.js')
    .pipe(plumber({
      errorHandler: function(error) {
        console.log(error.message);
        this.emit('end');
      }
    }))
    .pipe(gulp.dest('dev/src/'))
});

gulp.task('templates', function() {
  return gulp.src(['app/**/*.html', '!/**/index.html'])
    .pipe(templateCache('templates.js', {
      module: 'TwitchCaster'
    }))
    .pipe(gulp.dest('dev/src/'));
});

gulp.task('html', function() {
  return gulp.src('app/*.html')
    .pipe(gulp.dest('dev/'))
});


gulp.task('dist', [
    'html',
    'templates',
    'scripts',
    'styles',
    'img',
    'vendor-scripts',
    'vendor-css',
    'vendor-fonts'
  ],
  function() {

    return gulp.src('app/index.html')
      .pipe(wiredep.stream({
        fileTypes: {
          html: {
            replace: {
              js: function(filePath) {
                return '<script src="' + 'vendor/' + filePath.split('/')
                  .pop() + '"></script>';
              },
              css: function(filePath) {
                return '<link rel="stylesheet" href="' + 'vendor/' +
                  filePath.split('/').pop() + '"/>';
              }
            }
          }
        }
      }))
      .pipe(inject(
        gulp.src(['dist/scripts/**/*.js'], {
          read: false
        }), {
          addRootSlash: false,
          transform: function(filePath, file, i, length) {
            return '<script src="' + filePath.replace('dist/', '') +
              '"></script>';
          }
        }))



    .pipe(inject(
      gulp.src(['dist/styles/**/*.css'], {
        read: false
      }), {
        addRootSlash: false,
        transform: function(filePath, file, i, length) {
          return '<link rel="stylesheet" href="' + filePath.replace(
            'dist/', '') + '"/>';
        }
      }))

    .pipe(gulp.dest('dist'));
  });


gulp.task('watch', function() {
  gulp.watch(['./app/**/*'], {
    interval: 1000,
    mode: 'poll'
  }, ['dist']);
});
