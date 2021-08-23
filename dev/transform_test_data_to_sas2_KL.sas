
%MACRO label_data(dsn, notes = N);
        %LOCAL tme0 opt0 tme1 o_drive path lbl;
        %PUT;
        %LET tme0 = %sysfunc(time());
        %PUT %nrstr(----- %MACRO) &sysmacroname. [%sysfunc(sum(&tme0.), time11.2)];
        %PUT %str()      &=dsn., &=notes.;
        %PUT;
        %LET opt0 = %sysfunc(getoption(notes)) %sysfunc(getoption(msglevel, keyword));
        OPTIONS %sysfunc(ifc(&notes. = N, NONOTES MSGLEVEL = i, NOTES MSGLEVEL = n));

        %* path to datasets, is also used to store the permanent SAS files *;
        %LET o_drive = \\emea.healthcare.cnb\bhc\Apps\A-C\Biomdata\1_Global_Biostatistics;
        %LET path = &o_drive.\REDS\Peer Groups\Biomarker\BMDI\martini_example_data;

        %* ensure clean environment *;
        PROC DATASETS LIBRARY = work NOWARN NOLIST;
                DELETE &dsn.: / MEMTYPE = data;
        QUIT;

        %* import data *;
        PROC IMPORT DATAFILE = "&path.\&dsn..csv" OUT = work.&dsn. DBMS = csv REPLACE;
            GETNAMES = yes;
        RUN;

        %* import labels *;
        PROC IMPORT DATAFILE = "&path.\&dsn._labels.csv" OUT = work.&dsn._labels DBMS = csv REPLACE;
                GETNAMES = yes;
        RUN;

        %* derive labels *;
        DATA _null_;
                SET work.&dsn._labels END = eof;
                LENGTH lbl $9096;
                RETAIN lbl "";
                lbl = catx(" ", lbl, cats(column, "='", label, "'"));
                IF (eof) THEN CALL symput('lbl', strip(lbl));
        RUN;

        %* save data with labels *;
        LIBNAME out "&path.";
        PROC DATASETS LIBRARY = out NOLIST NOWARN;
                DELETE &dsn. / MEMTYPE = data;
        QUIT;
        DATA out.&dsn.;
                SET work.&dsn.;
                LABEL &lbl.;
        RUN;
        LIBNAME out CLEAR;

        OPTIONS &opt0.;
        %PUT;
        %LET tme1 = %sysfunc(time());
        %PUT %nrstr(----- %MEND) &sysmacroname. [%sysfunc(sum(&tme1.), time11.2), runtime: %sysfunc(sum(&tme1., -&tme0.), 5.2)s];
        %PUT;
%MEND label_data;

%label_data(adsl);
%label_data(advs);
%label_data(admh);
