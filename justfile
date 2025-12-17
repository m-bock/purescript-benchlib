gen-docs:
    PATCHDOWN_FILE_PATH="./docs/tutorial.md" npx spago run -m Patchdown
    npx doctoc --maxlevel 3 docs/tutorial.md

build:
    npx spago build