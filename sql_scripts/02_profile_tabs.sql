USE WAREHOUSE COMPUTE_WH;
USE DATABASE LEGACY;
USE SCHEMA P2_DYNAMICS;
USE ROLE ACCOUNTADMIN;

EXECUTE IMMEDIATE $$
DECLARE
    -- Cursor to iterate over all Staging tables
    c_tables CURSOR FOR
        SELECT table_name
        FROM INFORMATION_SCHEMA.TABLES
        WHERE table_schema = 'P2_DYNAMICS'
          AND table_name LIKE 'STG_%';

    has_created NUMBER;
    has_modified NUMBER;
    col_created STRING;
    col_modified STRING;
    dynamic_table_query STRING;
    v_table_name STRING;
BEGIN
    -- 1. Create the Master Table Focus Profile
    -- DESIGN CONSIDERATION: The schema matches the exact requested taxonomy,
    -- clarifying dates to "oldest/newest" to eliminate ambiguity.
    CREATE OR REPLACE TABLE P2_DYNAMICS.TABLE_HEALTH_PROFILE (
        name_table                  VARCHAR,
        count_rows                  NUMBER,
        count_columns               NUMBER,
        count_outwards_connetions   NUMBER,
        list_tables_pointed         VARCHAR(16777216),
        count_cells                 NUMBER,
        rate_fill_cells             NUMBER(7,4),
        count_active_rows           NUMBER,
        rate_active_rows            NUMBER(7,4),
        oldest_change               TIMESTAMP_NTZ,
        oldest_modified             TIMESTAMP_NTZ,
        newest_change               TIMESTAMP_NTZ,
        newest_modified             TIMESTAMP_NTZ,
        last_modified_years         NUMBER(7,2),
        count_empty_columns         NUMBER,
        data_velocity_per_year      NUMBER(10,2),
        profiled_at                 TIMESTAMP_NTZ DEFAULT CURRENT_TIMESTAMP()
    );

    FOR t IN c_tables DO
        v_table_name := t.table_name;

        -- 2. Safely check for audit columns to prevent compilation crashes
        SELECT COUNT(*) INTO has_created
        FROM INFORMATION_SCHEMA.COLUMNS
        WHERE table_name = :v_table_name AND column_name = 'CREATEDON' AND table_schema = 'P2_DYNAMICS';

        SELECT COUNT(*) INTO has_modified
        FROM INFORMATION_SCHEMA.COLUMNS
        WHERE table_name = :v_table_name AND column_name = 'MODIFIEDON' AND table_schema = 'P2_DYNAMICS';

        -- Apply TRY_TO_TIMESTAMP to cast the raw VARCHARs into valid dates
        IF (has_created > 0) THEN
            col_created := 'TRY_TO_TIMESTAMP(CREATEDON)';
        ELSE
            col_created := 'NULL';
        END IF;

        IF (has_modified > 0) THEN
            col_modified := 'TRY_TO_TIMESTAMP(MODIFIEDON)';
        ELSE
            col_modified := 'NULL';
        END IF;

        -- 3. The Modular Dynamic Query
        -- TRADE-OFF: This query strictly depends on COLUMN_HEALTH_PROFILE existing.
        -- It uses CROSS JOIN to merge the 1-row temporal summary with the 1-row schema summary.
        dynamic_table_query := '
            INSERT INTO P2_DYNAMICS.TABLE_HEALTH_PROFILE (
                name_table, count_rows, count_columns, count_outwards_connetions,
                list_tables_pointed, count_cells, rate_fill_cells, count_active_rows,
                rate_active_rows, oldest_change, oldest_modified, newest_change,
                newest_modified, last_modified_years, count_empty_columns, data_velocity_per_year
            )
            WITH src_stats AS (
                -- Extract raw temporal data directly from the legacy staging table
                SELECT
                    COUNT(*) as total_rows,
                    MIN(' || col_created || ') as min_c,
                    MAX(' || col_created || ') as max_c,
                    MIN(' || col_modified || ') as min_m,
                    MAX(' || col_modified || ') as max_m,
                    COUNT_IF(
                        (' || col_created || ' >= DATEADD(year, -3, CURRENT_TIMESTAMP())) OR
                        (' || col_modified || ' >= DATEADD(year, -3, CURRENT_TIMESTAMP()))
                    ) as active_rows
                FROM P2_DYNAMICS."' || v_table_name || '"
            ),
            meta_stats AS (
                -- Extract schema metadata instantaneously from the previously generated Column Health Profile
                SELECT
                    COUNT(*) as total_cols,
                    COUNT_IF(is_fk = TRUE) as outward_fks,
                    LISTAGG(DISTINCT table_pointed, '','') as pointed_list,
                    SUM(count_non_nulls) as total_non_null_cells,
                    COUNT_IF(count_non_nulls = 0) as empty_cols
                FROM P2_DYNAMICS.COLUMN_HEALTH_PROFILE
                WHERE name_table = ''' || v_table_name || '''
            )
            SELECT
                ''' || v_table_name || ''',
                s.total_rows,
                m.total_cols,
                m.outward_fks,
                m.pointed_list,

                -- Global Matrix Stats
                (s.total_rows * m.total_cols) as count_cells,
                (m.total_non_null_cells / NULLIF(s.total_rows * m.total_cols, 0)) as rate_fill_cells,

                -- Activity Stats
                s.active_rows,
                (s.active_rows / NULLIF(s.total_rows, 0)) as rate_active_rows,

                -- Temporal Footprint
                s.min_c,
                s.min_m,
                s.max_c,
                s.max_m,

                -- Age and Velocity Formulas
                (DATEDIFF(day, s.max_m, CURRENT_TIMESTAMP()) / 365.0) as last_modified_years,
                m.empty_cols,
                (s.total_rows / NULLIF(DATEDIFF(day, s.min_c, s.max_c) / 365.0, 0)) as data_velocity_per_year

            FROM src_stats s CROSS JOIN meta_stats m;
        ';

        EXECUTE IMMEDIATE :dynamic_table_query;

    END FOR;

    RETURN 'Table Focus Profiling completed via Modular Join approach.';
END;
$$;
