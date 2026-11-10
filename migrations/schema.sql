--
-- PostgreSQL database dump
--

\restrict schema

-- Dumped from database version 18.6 (Postgres.app)
-- Dumped by pg_dump version 18.6 (Postgres.app)

SET statement_timeout = 0;
SET lock_timeout = 0;
SET idle_in_transaction_session_timeout = 0;
SET transaction_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SELECT pg_catalog.set_config('search_path', '', false);
SET check_function_bodies = false;
SET xmloption = content;
SET client_min_messages = warning;
SET row_security = off;

--
-- Name: authenticate(text); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.authenticate(p_token text) RETURNS uuid
    LANGUAGE plpgsql SECURITY DEFINER
    SET search_path TO 'public', 'pg_temp'
    AS $$
DECLARE
  v_user_id uuid;
BEGIN
  SELECT s.user_id INTO v_user_id
  FROM sessions AS s
  WHERE s.token_hash = sha256(convert_to(p_token, 'UTF8')) AND s.expires_at > now();

  PERFORM set_config('app.user_id', COALESCE(v_user_id::text, ''), true);

  RETURN v_user_id;
END;
$$;


--
-- Name: delete_expired(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.delete_expired() RETURNS void
    LANGUAGE sql SECURITY DEFINER
    SET search_path TO 'public', 'pg_temp'
    AS $$
  DELETE FROM magic_links
  WHERE token_hash IN (
    SELECT m.token_hash
    FROM magic_links AS m
    WHERE m.expires_at <= now()
    FOR UPDATE SKIP LOCKED
  );
  DELETE FROM webauthn_challenges
  WHERE challenge IN (
    SELECT c.challenge
    FROM webauthn_challenges AS c
    WHERE c.expires_at <= now()
    FOR UPDATE SKIP LOCKED
  );
  DELETE FROM sessions
  WHERE id IN (
    SELECT s.id
    FROM sessions AS s
    WHERE s.expires_at <= now()
    FOR UPDATE SKIP LOCKED
  );
  DELETE FROM rate_limits
  WHERE key IN (
    SELECT r.key
    FROM rate_limits AS r
    WHERE r.tat <= now()
    FOR UPDATE SKIP LOCKED
  );
$$;


--
-- Name: rate_limit(text, integer, integer); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.rate_limit(p_key text, p_limit integer, p_window_seconds integer) RETURNS TABLE(allowed boolean, remaining integer, retry_after integer)
    LANGUAGE plpgsql SECURITY DEFINER
    SET search_path TO 'public', 'pg_temp'
    AS $$
DECLARE
  v_now timestamptz := clock_timestamp();
  v_interval interval;
  v_burst interval;
  v_tat timestamptz;
BEGIN
  IF p_key IS NULL THEN
    RAISE EXCEPTION 'rate_limit: key must not be null'
      USING ERRCODE = 'invalid_parameter_value';
  END IF;
  IF octet_length(p_key) > 512 THEN
    RAISE EXCEPTION 'rate_limit: key must be at most 512 bytes, got %', octet_length(p_key)
      USING ERRCODE = 'invalid_parameter_value';
  END IF;
  IF p_limit IS NULL OR p_limit < 1 THEN
    RAISE EXCEPTION 'rate_limit: limit must be at least 1, got %', p_limit
      USING ERRCODE = 'invalid_parameter_value';
  END IF;
  IF p_window_seconds IS NULL OR p_window_seconds < 1 THEN
    RAISE EXCEPTION 'rate_limit: window must be at least 1 second, got %', p_window_seconds
      USING ERRCODE = 'invalid_parameter_value';
  END IF;
  IF p_limit > p_window_seconds::bigint * 1000000 THEN
    RAISE EXCEPTION 'rate_limit: limit % exceeds one per microsecond over % s', p_limit, p_window_seconds
      USING ERRCODE = 'invalid_parameter_value';
  END IF;

  v_interval := (p_window_seconds::bigint * 1000000 / p_limit) * interval '1 microsecond';
  v_burst := v_interval * p_limit;

  INSERT INTO rate_limits AS r (key, tat)
  VALUES (p_key, v_now + v_interval)
  ON CONFLICT (key) DO UPDATE
    SET tat = GREATEST(r.tat, v_now) + v_interval
    WHERE GREATEST(r.tat, v_now) + v_interval <= v_now + v_burst
  RETURNING r.tat INTO v_tat;

  IF FOUND THEN
    allowed := true;
    remaining := floor(
      extract(epoch FROM v_now + v_burst - v_tat) / extract(epoch FROM v_interval)
    )::integer;
    retry_after := 0;
  ELSE
    SELECT r.tat INTO v_tat FROM rate_limits AS r WHERE r.key = p_key;
    allowed := false;
    remaining := 0;
    retry_after := GREATEST(
      1,
      ceil(extract(epoch FROM COALESCE(v_tat, v_now) + v_interval - v_burst - v_now))
    )::integer;
  END IF;

  RETURN NEXT;
END;
$$;


SET default_tablespace = '';

SET default_table_access_method = heap;

--
-- Name: magic_links; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.magic_links (
    token_hash bytea NOT NULL,
    email text NOT NULL,
    username text,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    expires_at timestamp with time zone NOT NULL,
    CONSTRAINT magic_links_email_check CHECK (((email = lower(btrim(email))) AND (email <> ''::text))),
    CONSTRAINT magic_links_token_hash_check CHECK ((octet_length(token_hash) = 32)),
    CONSTRAINT magic_links_username_check CHECK (((username = lower(btrim(username))) AND (username <> ''::text)))
);

ALTER TABLE ONLY public.magic_links FORCE ROW LEVEL SECURITY;


--
-- Name: passkeys; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.passkeys (
    credential_id text NOT NULL,
    user_id uuid NOT NULL,
    public_key text NOT NULL,
    sign_count bigint DEFAULT 0 NOT NULL,
    transports text[] DEFAULT '{}'::text[] NOT NULL,
    backup_eligible boolean NOT NULL,
    backed_up boolean NOT NULL,
    name text,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    last_used_at timestamp with time zone,
    CONSTRAINT passkeys_check CHECK ((backup_eligible OR (NOT backed_up))),
    CONSTRAINT passkeys_credential_id_check CHECK ((credential_id ~ '^[A-Za-z0-9_-]+$'::text)),
    CONSTRAINT passkeys_public_key_check CHECK ((public_key ~ '^[A-Za-z0-9_-]+$'::text)),
    CONSTRAINT passkeys_sign_count_check CHECK (((sign_count >= 0) AND (sign_count <= '4294967295'::bigint)))
);

ALTER TABLE ONLY public.passkeys FORCE ROW LEVEL SECURITY;


--
-- Name: rate_limits; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.rate_limits (
    key text NOT NULL,
    tat timestamp with time zone NOT NULL
);

ALTER TABLE ONLY public.rate_limits FORCE ROW LEVEL SECURITY;


--
-- Name: sessions; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.sessions (
    id uuid DEFAULT gen_random_uuid() NOT NULL,
    token_hash bytea NOT NULL,
    user_id uuid NOT NULL,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    expires_at timestamp with time zone NOT NULL,
    CONSTRAINT sessions_token_hash_check CHECK ((octet_length(token_hash) = 32))
);

ALTER TABLE ONLY public.sessions FORCE ROW LEVEL SECURITY;


--
-- Name: users; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.users (
    id uuid DEFAULT gen_random_uuid() NOT NULL,
    email text NOT NULL,
    username text NOT NULL,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    CONSTRAINT users_email_check CHECK (((email = lower(btrim(email))) AND (email <> ''::text))),
    CONSTRAINT users_username_check CHECK (((username = lower(btrim(username))) AND (username <> ''::text)))
);

ALTER TABLE ONLY public.users FORCE ROW LEVEL SECURITY;


--
-- Name: webauthn_challenges; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.webauthn_challenges (
    challenge text NOT NULL,
    ceremony text NOT NULL,
    user_id uuid,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    expires_at timestamp with time zone NOT NULL,
    CONSTRAINT webauthn_challenges_ceremony_check CHECK ((ceremony = ANY (ARRAY['registration'::text, 'authentication'::text]))),
    CONSTRAINT webauthn_challenges_challenge_check CHECK ((challenge ~ '^[A-Za-z0-9_-]+$'::text)),
    CONSTRAINT webauthn_challenges_check CHECK (((ceremony <> 'registration'::text) OR (user_id IS NOT NULL)))
);

ALTER TABLE ONLY public.webauthn_challenges FORCE ROW LEVEL SECURITY;


--
-- Name: magic_links magic_links_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.magic_links
    ADD CONSTRAINT magic_links_pkey PRIMARY KEY (token_hash);


--
-- Name: passkeys passkeys_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.passkeys
    ADD CONSTRAINT passkeys_pkey PRIMARY KEY (credential_id);


--
-- Name: rate_limits rate_limits_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.rate_limits
    ADD CONSTRAINT rate_limits_pkey PRIMARY KEY (key);


--
-- Name: sessions sessions_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.sessions
    ADD CONSTRAINT sessions_pkey PRIMARY KEY (id);


--
-- Name: sessions sessions_token_hash_key; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.sessions
    ADD CONSTRAINT sessions_token_hash_key UNIQUE (token_hash);


--
-- Name: users users_email_key; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.users
    ADD CONSTRAINT users_email_key UNIQUE (email);


--
-- Name: users users_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.users
    ADD CONSTRAINT users_pkey PRIMARY KEY (id);


--
-- Name: users users_username_key; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.users
    ADD CONSTRAINT users_username_key UNIQUE (username);


--
-- Name: webauthn_challenges webauthn_challenges_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.webauthn_challenges
    ADD CONSTRAINT webauthn_challenges_pkey PRIMARY KEY (challenge);


--
-- Name: magic_links_email_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX magic_links_email_idx ON public.magic_links USING btree (email);


--
-- Name: magic_links_expires_at_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX magic_links_expires_at_idx ON public.magic_links USING btree (expires_at);


--
-- Name: passkeys_user_id_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX passkeys_user_id_idx ON public.passkeys USING btree (user_id);


--
-- Name: sessions_expires_at_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX sessions_expires_at_idx ON public.sessions USING btree (expires_at);


--
-- Name: sessions_user_id_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX sessions_user_id_idx ON public.sessions USING btree (user_id);


--
-- Name: webauthn_challenges_expires_at_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX webauthn_challenges_expires_at_idx ON public.webauthn_challenges USING btree (expires_at);


--
-- Name: webauthn_challenges_user_id_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX webauthn_challenges_user_id_idx ON public.webauthn_challenges USING btree (user_id);


--
-- Name: passkeys passkeys_user_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.passkeys
    ADD CONSTRAINT passkeys_user_id_fkey FOREIGN KEY (user_id) REFERENCES public.users(id) ON DELETE CASCADE;


--
-- Name: sessions sessions_user_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.sessions
    ADD CONSTRAINT sessions_user_id_fkey FOREIGN KEY (user_id) REFERENCES public.users(id) ON DELETE CASCADE;


--
-- Name: webauthn_challenges webauthn_challenges_user_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.webauthn_challenges
    ADD CONSTRAINT webauthn_challenges_user_id_fkey FOREIGN KEY (user_id) REFERENCES public.users(id) ON DELETE CASCADE;


--
-- Name: magic_links; Type: ROW SECURITY; Schema: public; Owner: -
--

ALTER TABLE public.magic_links ENABLE ROW LEVEL SECURITY;

--
-- Name: passkeys own_rows; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY own_rows ON public.passkeys USING ((user_id = ( SELECT (NULLIF(current_setting('app.user_id'::text, true), ''::text))::uuid AS "nullif"))) WITH CHECK ((user_id = ( SELECT (NULLIF(current_setting('app.user_id'::text, true), ''::text))::uuid AS "nullif")));


--
-- Name: sessions own_rows; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY own_rows ON public.sessions USING ((user_id = ( SELECT (NULLIF(current_setting('app.user_id'::text, true), ''::text))::uuid AS "nullif"))) WITH CHECK ((user_id = ( SELECT (NULLIF(current_setting('app.user_id'::text, true), ''::text))::uuid AS "nullif")));


--
-- Name: users own_rows; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY own_rows ON public.users USING ((id = ( SELECT (NULLIF(current_setting('app.user_id'::text, true), ''::text))::uuid AS "nullif"))) WITH CHECK ((id = ( SELECT (NULLIF(current_setting('app.user_id'::text, true), ''::text))::uuid AS "nullif")));


--
-- Name: webauthn_challenges own_rows; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY own_rows ON public.webauthn_challenges USING ((user_id = ( SELECT (NULLIF(current_setting('app.user_id'::text, true), ''::text))::uuid AS "nullif"))) WITH CHECK ((user_id = ( SELECT (NULLIF(current_setting('app.user_id'::text, true), ''::text))::uuid AS "nullif")));


--
-- Name: passkeys; Type: ROW SECURITY; Schema: public; Owner: -
--

ALTER TABLE public.passkeys ENABLE ROW LEVEL SECURITY;

--
-- Name: rate_limits; Type: ROW SECURITY; Schema: public; Owner: -
--

ALTER TABLE public.rate_limits ENABLE ROW LEVEL SECURITY;

--
-- Name: sessions; Type: ROW SECURITY; Schema: public; Owner: -
--

ALTER TABLE public.sessions ENABLE ROW LEVEL SECURITY;

--
-- Name: users; Type: ROW SECURITY; Schema: public; Owner: -
--

ALTER TABLE public.users ENABLE ROW LEVEL SECURITY;

--
-- Name: webauthn_challenges; Type: ROW SECURITY; Schema: public; Owner: -
--

ALTER TABLE public.webauthn_challenges ENABLE ROW LEVEL SECURITY;

--
-- Name: SCHEMA public; Type: ACL; Schema: -; Owner: -
--

GRANT USAGE ON SCHEMA public TO api;


--
-- Name: FUNCTION authenticate(p_token text); Type: ACL; Schema: public; Owner: -
--

REVOKE ALL ON FUNCTION public.authenticate(p_token text) FROM PUBLIC;
GRANT ALL ON FUNCTION public.authenticate(p_token text) TO api;


--
-- Name: FUNCTION delete_expired(); Type: ACL; Schema: public; Owner: -
--

REVOKE ALL ON FUNCTION public.delete_expired() FROM PUBLIC;
GRANT ALL ON FUNCTION public.delete_expired() TO api;


--
-- Name: FUNCTION rate_limit(p_key text, p_limit integer, p_window_seconds integer); Type: ACL; Schema: public; Owner: -
--

REVOKE ALL ON FUNCTION public.rate_limit(p_key text, p_limit integer, p_window_seconds integer) FROM PUBLIC;
GRANT ALL ON FUNCTION public.rate_limit(p_key text, p_limit integer, p_window_seconds integer) TO api;


--
-- Name: TABLE passkeys; Type: ACL; Schema: public; Owner: -
--

GRANT SELECT,INSERT,DELETE ON TABLE public.passkeys TO api;


--
-- Name: COLUMN passkeys.name; Type: ACL; Schema: public; Owner: -
--

GRANT UPDATE(name) ON TABLE public.passkeys TO api;


--
-- Name: TABLE sessions; Type: ACL; Schema: public; Owner: -
--

GRANT DELETE ON TABLE public.sessions TO api;


--
-- Name: COLUMN sessions.id; Type: ACL; Schema: public; Owner: -
--

GRANT SELECT(id) ON TABLE public.sessions TO api;


--
-- Name: COLUMN sessions.user_id; Type: ACL; Schema: public; Owner: -
--

GRANT SELECT(user_id) ON TABLE public.sessions TO api;


--
-- Name: COLUMN sessions.created_at; Type: ACL; Schema: public; Owner: -
--

GRANT SELECT(created_at) ON TABLE public.sessions TO api;


--
-- Name: COLUMN sessions.expires_at; Type: ACL; Schema: public; Owner: -
--

GRANT SELECT(expires_at) ON TABLE public.sessions TO api;


--
-- Name: TABLE users; Type: ACL; Schema: public; Owner: -
--

GRANT SELECT ON TABLE public.users TO api;


--
-- Name: TABLE webauthn_challenges; Type: ACL; Schema: public; Owner: -
--

GRANT SELECT,INSERT,DELETE ON TABLE public.webauthn_challenges TO api;


--
-- PostgreSQL database dump complete
--

\unrestrict schema

