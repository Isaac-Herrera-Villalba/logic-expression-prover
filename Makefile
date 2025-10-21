# Makefile — Parser + Evaluador + Reporte LaTeX

PYTHON      = python3
SRC_DIR     = src
TMP_DIR     = tmp
OUT_DIR     = output
PARSER      = $(SRC_DIR)/parser.py
PROLOG_MAIN = $(SRC_DIR)/main.pl
INPUT       = queries.txt
TMP_FILE    = $(TMP_DIR)/queries.pl
LATEX_FILE  = $(OUT_DIR)/report.tex
PDF_FILE    = $(OUT_DIR)/report.pdf

.PHONY: all help parse run latex view full clean

all: help

help:
	@echo "Comandos disponibles:"
	@echo "  make parse  -> Ejecuta parser Python (genera $(TMP_FILE))"
	@echo "  make run    -> Ejecuta Prolog (genera $(LATEX_FILE))"
	@echo "  make latex  -> Compila $(LATEX_FILE) a PDF"
	@echo "  make view   -> Abre el PDF con zathura"
	@echo "  make full   -> Ejecuta todo el flujo (parse + run + latex + view)"
	@echo "  make clean  -> Limpia archivos generados (sin borrar carpetas)"

parse:
	@echo "Creando directorio temporal si no existe..."
	mkdir -p $(TMP_DIR)
	mkdir -p $(OUT_DIR)
	@echo "Ejecutando parser..."
	$(PYTHON) $(PARSER) $(INPUT) $(TMP_FILE)

run:
	@echo "Ejecutando evaluador Prolog..."
	swipl -q -s $(PROLOG_MAIN) -g run_all -t halt

latex:
	@echo "Compilando LaTeX..."
	pdflatex -interaction=nonstopmode -output-directory=$(OUT_DIR) $(LATEX_FILE) >/dev/null

view:
	@echo "Abriendo PDF con zathura..."
	zathura $(PDF_FILE) &

full: parse run latex view

clean:
	@echo "Eliminando archivos generados..."
	find $(TMP_DIR) -type f -delete 2>/dev/null || true
	find $(OUT_DIR) -type f -delete 2>/dev/null || true
	@echo "Limpieza completa (directorios conservados)."
############################################

