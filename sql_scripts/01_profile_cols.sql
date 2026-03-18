USE WAREHOUSE COMPUTE_WH;
USE DATABASE LEGACY;
USE SCHEMA P2_DYNAMICS;
USE ROLE ACCOUNTADMIN;

EXECUTE IMMEDIATE $$
DECLARE
    -- Cursor para procesar todas las tablas de Dynamics
    c_tables CURSOR FOR
        SELECT table_name
        FROM INFORMATION_SCHEMA.TABLES
        WHERE table_schema = 'P2_DYNAMICS'
          AND table_name LIKE 'STG_%';

    col_list STRING;
    dynamic_health_query STRING;
    v_table_name STRING;
BEGIN
    -- 1. Crear la tabla de resultados (Column Health Profile)
    CREATE OR REPLACE TABLE P2_DYNAMICS.COLUMN_HEALTH_PROFILE (
        name_table          VARCHAR,
        name_column         VARCHAR,
        count_rows          NUMBER,
        value_dominant      VARCHAR,
        count_dominant      NUMBER,
        rate_dominant       NUMBER(7,4),
        list_values         VARCHAR(16777216), -- Soporta listas largas
        value_sampled       VARCHAR,
        count_non_nulls     NUMBER,
        rate_fill           NUMBER(7,4),
        is_pk               BOOLEAN,
        is_fk               BOOLEAN,
        table_pointed       VARCHAR,
        should_migrate_col  BOOLEAN,
        profiled_at         TIMESTAMP_NTZ DEFAULT CURRENT_TIMESTAMP()
    );

    FOR t IN c_tables DO
        v_table_name := t.table_name;

        -- 2. Extraer columnas (ignorando si la tabla no tiene ninguna)
        SELECT LISTAGG('"' || column_name || '"', ', ')
        INTO col_list
        FROM INFORMATION_SCHEMA.COLUMNS
        WHERE table_name = :v_table_name AND table_schema = 'P2_DYNAMICS';

        IF (col_list IS NULL OR col_list = '') THEN
            CONTINUE;
        END IF;

        -- 3. Construcción del query usando arquitectura Multi-CTE y escape SQL estándar ('')
        dynamic_health_query := '
            INSERT INTO P2_DYNAMICS.COLUMN_HEALTH_PROFILE (
                name_table, name_column, count_rows, value_dominant, count_dominant,
                rate_dominant, list_values, value_sampled, count_non_nulls,
                rate_fill, is_pk, is_fk, table_pointed, should_migrate_col
            )
            WITH base_data AS (
                -- Lectura única de la tabla
                SELECT * FROM P2_DYNAMICS."' || v_table_name || '"
            ),
            table_stats AS (
                -- Conteo total de filas reales
                SELECT COUNT(*) as total_rows FROM base_data
            ),
            unpivoted AS (
                -- Al hacer UNPIVOT, Snowflake omite automáticamente los NULLs.
                -- Forzamos VARCHAR para unificar los tipos de datos en la agrupación.
                SELECT column_name, column_value::VARCHAR as column_value
                FROM base_data
                UNPIVOT(column_value FOR column_name IN (' || col_list || '))
            ),
            value_frequencies AS (
                -- Frecuencia exacta de cada valor por columna
                SELECT column_name, column_value, COUNT(*) as freq
                FROM unpivoted
                GROUP BY column_name, column_value
            ),
            ranked_values AS (
                -- Función de ventana para encontrar el valor dominante (Moda)
                SELECT
                    column_name,
                    column_value,
                    freq,
                    ROW_NUMBER() OVER(PARTITION BY column_name ORDER BY freq DESC) as rn
                FROM value_frequencies
            ),
            dominant_values AS (
                -- Filtramos para quedarnos solo con el #1
                SELECT column_name, column_value as value_dominant, freq as count_dominant
                FROM ranked_values
                WHERE rn = 1
            ),
            col_stats AS (
                -- Sumarizamos totales a nivel de columna
                SELECT
                    column_name,
                    SUM(freq) as count_non_nulls,
                    COUNT(column_value) as distinct_count,
                    ANY_VALUE(column_value) as value_sampled
                FROM value_frequencies
                GROUP BY column_name
            ),
            small_lists AS (
                -- Ejecutamos LISTAGG solo para columnas con baja cardinalidad (<= 24)
                SELECT
                    f.column_name,
                    LISTAGG(f.column_value, ''|'') WITHIN GROUP (ORDER BY f.freq DESC) as list_values
                FROM value_frequencies f
                JOIN col_stats s ON f.column_name = s.column_name
                WHERE s.distinct_count <= 24
                GROUP BY f.column_name
            )
            SELECT
                ''' || v_table_name || ''' as name_table,
                isc.column_name as name_column,
                t.total_rows as count_rows,

                d.value_dominant,
                COALESCE(d.count_dominant, 0) as count_dominant,
                (COALESCE(d.count_dominant, 0) / NULLIF(s.count_non_nulls, 0)) as rate_dominant,

                l.list_values,
                s.value_sampled,

                COALESCE(s.count_non_nulls, 0) as count_non_nulls,
                (COALESCE(s.count_non_nulls, 0) / NULLIF(t.total_rows, 0)) as rate_fill,

                -- HEURÍSTICA DE LLAVE PRIMARIA (PK)
                -- Es PK si termina en ID, y sus valores únicos son exactamente iguales al total de filas
                CASE
                    WHEN isc.column_name ILIKE ''%id''
                     AND COALESCE(s.distinct_count, 0) = t.total_rows
                     AND COALESCE(s.count_non_nulls, 0) = t.total_rows
                    THEN TRUE
                    ELSE FALSE
                END as is_pk_flag,

                -- HEURÍSTICA DE LLAVE FORÁNEA (FK)
                -- Es FK si termina en ID y no pasó la prueba de Primary Key
                CASE
                    WHEN isc.column_name ILIKE ''%id''
                     AND NOT (
                         isc.column_name ILIKE ''%id''
                         AND COALESCE(s.distinct_count, 0) = t.total_rows
                         AND COALESCE(s.count_non_nulls, 0) = t.total_rows
                     )
                    THEN TRUE
                    ELSE FALSE
                END as is_fk_flag,

                -- INFERENCIA DE TABLA APUNTADA
                -- Si es FK, quita la palabra ID del final e infiere la tabla (Ej. OWNERID -> STG_OWNER)
                CASE
                    WHEN isc.column_name ILIKE ''%id''
                     AND NOT (COALESCE(s.distinct_count, 0) = t.total_rows AND COALESCE(s.count_non_nulls, 0) = t.total_rows)
                    THEN ''STG_'' || REGEXP_REPLACE(UPPER(isc.column_name), ''ID$'', '''')
                    ELSE NULL
                END as table_pointed,

                -- HEURÍSTICA DE MIGRACIÓN (The Entropy Rule)
                CASE
                    -- Siempre migrar llaves (PK/FK)
                    WHEN isc.column_name ILIKE ''%id'' THEN TRUE

                    -- Si tiene menos de 2% de datos: NO MIGRAR
                    WHEN (COALESCE(s.count_non_nulls, 0) / NULLIF(t.total_rows, 0)) < 0.02 THEN FALSE

                    -- Si el valor dominante representa más del 99% de los datos (Baja Entropía): NO MIGRAR
                    WHEN (COALESCE(d.count_dominant, 0) / NULLIF(s.count_non_nulls, 0)) > 0.99 THEN FALSE

                    -- Por defecto, los demás datos de valor se migran
                    ELSE TRUE
                END as should_migrate_col

            FROM INFORMATION_SCHEMA.COLUMNS isc
            CROSS JOIN table_stats t
            LEFT JOIN col_stats s ON isc.column_name = s.column_name
            LEFT JOIN dominant_values d ON isc.column_name = d.column_name
            LEFT JOIN small_lists l ON isc.column_name = l.column_name
            WHERE isc.table_name = ''' || v_table_name || '''
              AND isc.table_schema = ''P2_DYNAMICS'';
        ';

        EXECUTE IMMEDIATE :dynamic_health_query;

    END FOR;

    RETURN 'Perfilado de Salud de Columnas completado exitosamente con análisis de Entropía.';
END;
$$;
