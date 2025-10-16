# Makefile para el proyecto del cálculo de secuentes (LKP)
# --------------------------------------------------------

PROGRAM_PATH  = src/main.pl
PROGRAM_NAME  = main

# Carpeta de salida
OUTPUT_DIR    = output

# Archivos de salida (se generan automáticamente)
TEX_FILES     = $(OUTPUT_DIR)/proof_*.tex
PDF_FILES     = $(TEX_FILES:.tex=.pdf)

.PHONY: all help run latex view full clean

# --------------------------------------------------------
all: help

help:
	@echo 'Opciones disponibles:'
	@echo '  make run       -> Ejecuta el programa Prolog y genera los .tex en $(OUTPUT_DIR)'
	@echo '  make latex     -> Compila todos los .tex generados a PDF (usa pdflatex de TeX Live)'
	@echo '  make view      -> Abre el último PDF generado'
	@echo '  make full      -> Ejecuta todo el proceso: Prolog -> LaTeX -> abrir PDF'
	@echo '  make clean     -> Elimina archivos generados (.tex, .pdf, .aux, .log, etc.)'

# --------------------------------------------------------
# Ejecuta Prolog y genera los archivos .tex
run:
	@echo 'Ejecutando $(PROGRAM_PATH) con SWI-Prolog...'
	swipl -q -s $(PROGRAM_PATH)

# --------------------------------------------------------
# Compila todos los .tex en output/ a PDF usando TeX Live
latex:
	@echo 'Compilando archivos LaTeX a PDF...'
	@for f in $(OUTPUT_DIR)/*.tex; do \
		echo " -> Compilando $$f ..."; \
		pdflatex -output-directory=$(OUTPUT_DIR) $$f >/dev/null; \
	done
	@echo 'Compilación completa.'

# --------------------------------------------------------
# Abre el último PDF generado
view:
	@last_pdf=$$(ls -t $(OUTPUT_DIR)/*.pdf 2>/dev/null | head -n1); \
	if [ -n "$$last_pdf" ]; then \
		echo "Abriendo $$last_pdf ..."; \
		xdg-open "$$last_pdf" & \
	else \
		echo "No hay PDFs generados aún."; \
	fi

# --------------------------------------------------------
# Ejecuta todo el flujo
full: run latex view

# --------------------------------------------------------
# Limpieza
clean:
	@echo 'Eliminando archivos generados...'
	rm -fv $(OUTPUT_DIR)/*.aux $(OUTPUT_DIR)/*.log $(OUTPUT_DIR)/*.out $(OUTPUT_DIR)/*.toc $(OUTPUT_DIR)/*.pdf $(OUTPUT_DIR)/*.tex 2>/dev/null || true
	rm -fv *.qlf 2>/dev/null || true
	@echo 'Limpieza completada.'

