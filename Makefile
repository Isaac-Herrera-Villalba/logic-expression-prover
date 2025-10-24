# Makefile
# -----------------------------------------------------------------------
# Descripción:
# Este fichero automatiza el proceso de ejecución del proyecto,
# incluyendo el análisis de consultas, evaluación lógica y generación
# del reporte final en PDF mediante LaTeX.
# -----------------------------------------------------------------------

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
	@echo "  make view   -> Abre el PDF con okular"
	@echo "  make full   -> Ejecuta todo el flujo (parse + run + latex + view)"
	@echo "  make clean  -> Limpia directorios generados"

parse:
	@echo "Creando directorios temporales si no existen..."
	mkdir -p $(TMP_DIR)
	mkdir -p $(OUT_DIR)
	@echo "Ejecutando parser..."
	$(PYTHON) $(PARSER) $(INPUT) $(TMP_FILE)

run:
	@echo "Ejecutando evaluador Prolog..."
	swipl -q -s $(PROLOG_MAIN) -g run_all -t halt

latex:
	@echo "Compilando LaTeX..."
	-@pdflatex -interaction=nonstopmode -output-directory=$(OUT_DIR) $(LATEX_FILE) >/dev/null 2>&1 || true


view:
	@echo "Abriendo PDF con okular..."
	okular $(PDF_FILE) &

full: parse run latex view

clean:
	@echo "Eliminando archivos generados..."
	rm -rf $(TMP_DIR)
	rm -rf $(OUT_DIR)

