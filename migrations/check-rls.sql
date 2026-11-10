DO $$
DECLARE
  v_problems text;
BEGIN
  SELECT string_agg(problem, '; ') INTO v_problems
  FROM (
    SELECT format('migrations run as %s, which is not BYPASSRLS', current_user) AS problem
    FROM pg_roles AS r
    WHERE r.rolname = current_user AND NOT (r.rolsuper OR r.rolbypassrls)

    UNION ALL
    SELECT 'the role api does not exist'
    WHERE NOT EXISTS (SELECT 1 FROM pg_roles WHERE rolname = 'api')

    UNION ALL
    SELECT 'the role api is SUPERUSER, BYPASSRLS, CREATEROLE or CREATEDB'
    FROM pg_roles AS r
    WHERE r.rolname = 'api' AND (r.rolsuper OR r.rolbypassrls OR r.rolcreaterole OR r.rolcreatedb)

    UNION ALL
    SELECT format('%s lacks ENABLE or FORCE ROW LEVEL SECURITY', c.oid::regclass)
    FROM pg_class AS c
    WHERE c.relnamespace = 'public'::regnamespace
      AND c.relkind IN ('r', 'p')
      AND NOT (c.relrowsecurity AND c.relforcerowsecurity)

    UNION ALL
    SELECT format('api owns %s', c.oid::regclass)
    FROM pg_class AS c
    JOIN pg_roles AS r ON r.rolname = 'api'
    WHERE c.relnamespace = 'public'::regnamespace
      AND c.relkind IN ('r', 'p', 'v', 'm', 'S')
      AND pg_has_role(r.oid, c.relowner, 'USAGE')

    UNION ALL
    SELECT format('api may TRUNCATE, REFERENCES or TRIGGER %s', c.oid::regclass)
    FROM pg_class AS c
    JOIN pg_roles AS r ON r.rolname = 'api'
    WHERE c.relnamespace = 'public'::regnamespace
      AND c.relkind IN ('r', 'p')
      AND has_table_privilege(r.oid, c.oid, 'TRUNCATE, REFERENCES, TRIGGER')

    UNION ALL
    SELECT format('%s is SECURITY DEFINER without SET search_path', p.oid::regprocedure)
    FROM pg_proc AS p
    WHERE p.pronamespace = 'public'::regnamespace
      AND p.prosecdef
      AND NOT EXISTS (SELECT 1 FROM unnest(p.proconfig) AS setting WHERE setting LIKE 'search_path=%')

    UNION ALL
    SELECT format('PUBLIC may execute %s, which is SECURITY DEFINER', p.oid::regprocedure)
    FROM pg_proc AS p
    WHERE p.pronamespace = 'public'::regnamespace
      AND p.prosecdef
      AND has_function_privilege('public', p.oid, 'EXECUTE')
  ) AS problems;

  IF v_problems IS NOT NULL THEN
    RAISE EXCEPTION 'check-rls: %', v_problems;
  END IF;
END;
$$;
