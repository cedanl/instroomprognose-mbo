# 📊 Instroomprognose MBO Package

Een R package voor de analyse van MBO instroomdata met geautomatiseerde rapportage en data kwaliteitscontroles.

## 🎯 Overzicht

Dit package biedt een complete pipeline voor het analyseren van CAMBO aanmeldingsdata van MBO instellingen. Het genereert gedetailleerde rapporten over instroompatronen, conversieratio's, data kwaliteit en geografische spreiding.

### Belangrijkste functionaliteiten:

-   📥 **Data preparatie** inladen en verrijken met afgeleide variabelen en referentiedata
-   📈 **Instroomanalyse** per instelling met trends en patronen
-   📊 **Data kwaliteit** controles en validatie
-   🗺️ **Geografische analyse** van studentenherkomst
-   📋 **Status transitie** analyse van aanmeldingen tot inschrijvingen
-   🔄 **Meervoudige aanmeldingen** analyse
-   📑 **Geautomatiseerde rapportage** in HTML formaat

## 🚀 Snelstart

### 1. Installatie

Clone het project

```         
git clone https://github.com/cedanl/instroomprognose-mbo
```

Open het project in RStudio Het setup script wordt automatisch gedetecteerd, druk op enter om dit runnen. Dit installeert ondersteunde packages.

### 2. Configuratie

Open configuratie bestand Dit gaat automatisch, run anders:

``` r
open_config()
```

Het `config.yml` bestand bevat alle belangrijke instellingen:

-   **inherits**: `"default"` voor demo data, `"cambo"` voor echte data, die moet je wel zelf toevoegen aan `"data/01_raw"`
-   **prepare**: `true` om data preparatie opnieuw uit te voeren, `false` om bestaande voorbereide data te gebruiken (indien aanwezig)
-   **brin**: Officiële BRIN code van je instelling
-   **school_name**: Naam zoals deze in rapporten moet verschijnen
-   **year**: Analysejaar (2023 of 2024 momenteel)

### 3. Pipeline uitvoeren

Open het hoofdrapport instroomprognose.qmd is als het goed is ook al geopend. Open dit anders handmatig. Loop en stapsgewijs doorheen en run de code blocks of render het bestand.

## 📁 Projectstructuur

```         
instroomprognose-mbo-test/
├── instroomprognose.qmd            # Hoofd rapport wat uitlegt hoe je stapsgewijs analyses genereert
├── config.yml                      # Configuratie instellingen (pas aan voor eigen instelling / voorkeuren)
├── data/                           # Data directories
│   ├── 01_raw/                     # Zet hier ruwe CAMBO data neer (demo-bestanden aanwezig)
│   ├── 02_prepared/                # Bewerkte data
│   └── reference/                  # Referentiebestanden
├── output/                         # Gegenereerde rapporten
├── analysis/                       # Analyse bestanden
│   └── data_preparation.qmd        # Data preparatie workflow op volledig bronbestand
│   ├── instelling_analysis.qmd     # Template voor instellingsrapport
├── R/                              # R functies die worden gebruikt in de analyses
├── utils/                          # Utilities en setup
```

## 📊 Rapportage

Het package genereert verschillende soorten rapporten:

### Hoofd Pipeline Rapport (`instroomprognose.qmd`)

-   Overzicht van gehele pipeline en uitvoer
-   Configuratie validatie
-   Data preparatie controle
-   Trigger om rapport te genereren

### Data preparatie (`analysis/data_preparation.qmd`)

-   Data inladen vanuit `data/01_raw/`
-   Data verrijken met brin en opleidingsvariabelen
-   Data kwaliteit checks en meer afgeleide verrijkingen, zoals nieuw schooljaar variabele maken en datum velden
-   Nieuwe kolommen obv andere aanmeldingen van student op dat moment om indicatie te geven van meervoudige aanmeldingen
-   Opslaan van voorbereide data in `data/02_prepared/`

### Instellingsrapporten (`analysis/instelling_analysis.qmd`)

-   Executive summary
-   Data kwaliteit analyse
-   Instroompatronen en trends
-   Geografische spreiding
-   Aanmelding timing analyse
-   Status transitie overzicht
-   Meervoudige aanmeldingen analyse
-   Conclusies en aanbevelingen

## 🤝 Bijdragen

Contributions zijn welkom! Dit kan door middel van github issues, ceda mailen ([ceda@npuls.nl](mailto:ceda@npuls)) of via pull requests.

Dat laatste is meer geavanceerd, zie daarvoor de volgende tips. Mocht je hier mee aan de gang willen, laat het dan vooral weten dan kijken we mee!

### Development workflow:

1.  Fork het project
2.  Maak een feature branch (`git checkout -b feature/nieuwe-functie`)\
3.  Commit je wijzigingen (`git commit -m 'feat: nieuwe functie toegevoegd'`)
4.  Push naar de branch (`git push origin feature/nieuwe-functie`)
5.  Open een Pull Request

### Code Style Guidelines

-   Gebruik `snake_case` voor functie en variabele namen
-   Documenteer alle functies met roxygen2
-   Gebruik `|>` pipeline operator
-   4 spaties indentatie
-   Plaats imports bovenaan met `@importFrom` tags

### Git Commit Conventies:

-   `feat:` nieuwe functionaliteit
-   `fix:` bug fixes
-   `docs:` documentatie wijzigingen
-   `chore:` herstructuren van code (verplaatsen, uitgebreid refactoren)
-   `test:` toevoegen of wijzigen van tests
-   `style:` voor code / comments aanpassen zonder functionele wijzigingen
-   `remove:` voor het verwijderen van code/bestanden
-   `data:` voor data wijzigingen

## 📄 Licentie

Dit project valt onder de Apache Licentie - zie het [LICENSE](LICENSE) bestand voor details.

## 🙏 Acknowledgments

-   **CEDA Team** voor feedback en ontwikkelingsondersteuning
-   **Inge Meere** van MBO Koppelpunt / CAMBO, voor meedenken over data en duiding behoefte
-   **MBO instellingen** voor eerdere input bij sessies in voorjaar 2025
-   **R Community** voor de geweldige packages en tools
-   **Quarto** voor het uitstekende rapportage framework

------------------------------------------------------------------------
