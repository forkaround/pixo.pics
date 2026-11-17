--
-- PostgreSQL database dump
--

\restrict schema

-- Dumped from database version 18.7 (Postgres.app)
-- Dumped by pg_dump version 18.7 (Postgres.app)

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
-- Name: apply_paddle_subscription(text, uuid, text, text, text, timestamp with time zone, text, timestamp with time zone, timestamp with time zone); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.apply_paddle_subscription(p_subscription_id text, p_user_id uuid, p_customer_id text, p_status text, p_price_id text, p_period_ends_at timestamp with time zone, p_scheduled_change text, p_scheduled_change_at timestamp with time zone, p_paddle_updated_at timestamp with time zone) RETURNS text
    LANGUAGE plpgsql SECURITY DEFINER
    SET search_path TO 'public', 'pg_temp'
    AS $$
BEGIN
  UPDATE subscriptions AS s
  SET paddle_customer_id = p_customer_id,
      status = p_status,
      price_id = p_price_id,
      current_period_ends_at = p_period_ends_at,
      scheduled_change = p_scheduled_change,
      scheduled_change_at = p_scheduled_change_at,
      paddle_updated_at = p_paddle_updated_at,
      updated_at = now()
  WHERE s.paddle_subscription_id = p_subscription_id
    AND s.paddle_updated_at <= p_paddle_updated_at;

  IF FOUND THEN
    RETURN 'applied';
  END IF;

  IF EXISTS (SELECT 1 FROM subscriptions AS s WHERE s.paddle_subscription_id = p_subscription_id) THEN
    RETURN 'stale';
  END IF;

  INSERT INTO subscriptions (
    paddle_subscription_id, user_id, paddle_customer_id, status, price_id,
    current_period_ends_at, scheduled_change, scheduled_change_at, paddle_updated_at
  )
  SELECT p_subscription_id, u.id, p_customer_id, p_status, p_price_id,
    p_period_ends_at, p_scheduled_change, p_scheduled_change_at, p_paddle_updated_at
  FROM users AS u
  WHERE u.id = p_user_id
  ON CONFLICT (paddle_subscription_id) DO NOTHING;

  IF FOUND THEN
    RETURN 'applied';
  END IF;

  UPDATE subscriptions AS s
  SET paddle_customer_id = p_customer_id,
      status = p_status,
      price_id = p_price_id,
      current_period_ends_at = p_period_ends_at,
      scheduled_change = p_scheduled_change,
      scheduled_change_at = p_scheduled_change_at,
      paddle_updated_at = p_paddle_updated_at,
      updated_at = now()
  WHERE s.paddle_subscription_id = p_subscription_id
    AND s.paddle_updated_at <= p_paddle_updated_at;

  IF FOUND THEN
    RETURN 'applied';
  ELSIF EXISTS (SELECT 1 FROM subscriptions AS s WHERE s.paddle_subscription_id = p_subscription_id) THEN
    RETURN 'stale';
  END IF;

  RETURN 'unmatched';
END;
$$;


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
-- Name: consume_magic_link(text, integer); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.consume_magic_link(p_token text, p_session_seconds integer) RETURNS TABLE(outcome text, session_token text, email text, username text)
    LANGUAGE plpgsql SECURITY DEFINER
    SET search_path TO 'public', 'pg_temp'
    AS $$
DECLARE
  v_email text;
  v_username text;
  v_user_id uuid;
  v_account text;
BEGIN
  IF p_session_seconds IS NULL OR p_session_seconds < 1 THEN
    RAISE EXCEPTION 'consume_magic_link: session must last at least 1 second, got %', p_session_seconds
      USING ERRCODE = 'invalid_parameter_value';
  END IF;

  DELETE FROM magic_links AS m
  WHERE m.token_hash = sha256(convert_to(p_token, 'UTF8')) AND m.expires_at > now()
  RETURNING m.email, m.username INTO v_email, v_username;

  IF NOT FOUND THEN
    outcome := 'invalid';
    RETURN NEXT;
    RETURN;
  END IF;

  IF v_username IS NOT NULL THEN
    INSERT INTO users AS u (email, username)
    VALUES (v_email, v_username)
    ON CONFLICT DO NOTHING;
  END IF;

  SELECT u.id, u.username INTO v_user_id, v_account
  FROM users AS u
  WHERE u.email = v_email;

  IF NOT FOUND THEN
    IF v_username IS NULL THEN
      outcome := 'invalid';
    ELSE
      outcome := 'username-taken';
    END IF;
    RETURN NEXT;
    RETURN;
  END IF;

  outcome := 'verified';
  email := v_email;
  username := v_account;
  session_token := new_token();

  INSERT INTO sessions (token_hash, user_id, expires_at)
  VALUES (
    sha256(convert_to(session_token, 'UTF8')),
    v_user_id,
    now() + p_session_seconds * interval '1 second'
  );

  RETURN NEXT;
END;
$$;


--
-- Name: create_authentication_challenge(integer); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.create_authentication_challenge(p_ttl_seconds integer) RETURNS text
    LANGUAGE plpgsql SECURITY DEFINER
    SET search_path TO 'public', 'pg_temp'
    AS $$
DECLARE
  v_challenge text;
BEGIN
  IF p_ttl_seconds IS NULL OR p_ttl_seconds < 1 THEN
    RAISE EXCEPTION 'create_authentication_challenge: challenge must last at least 1 second, got %', p_ttl_seconds
      USING ERRCODE = 'invalid_parameter_value';
  END IF;

  INSERT INTO webauthn_challenges (challenge, ceremony, expires_at)
  VALUES (new_token(), 'authentication', now() + p_ttl_seconds * interval '1 second')
  RETURNING challenge INTO v_challenge;

  RETURN v_challenge;
END;
$$;


--
-- Name: create_sign_in_link(text, integer); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.create_sign_in_link(p_email text, p_ttl_seconds integer) RETURNS TABLE(outcome text, token text)
    LANGUAGE plpgsql SECURITY DEFINER
    SET search_path TO 'public', 'pg_temp'
    AS $$
DECLARE
  v_email text := lower(btrim(p_email));
BEGIN
  IF p_ttl_seconds IS NULL OR p_ttl_seconds < 1 THEN
    RAISE EXCEPTION 'create_sign_in_link: ttl must be at least 1 second, got %', p_ttl_seconds
      USING ERRCODE = 'invalid_parameter_value';
  END IF;

  IF NOT EXISTS (SELECT 1 FROM users AS u WHERE u.email = v_email) THEN
    outcome := 'no-account';
    token := NULL;
    RETURN NEXT;
    RETURN;
  END IF;

  outcome := 'sign-in';
  token := new_token();

  INSERT INTO magic_links (token_hash, email, expires_at)
  VALUES (
    sha256(convert_to(token, 'UTF8')),
    v_email,
    now() + p_ttl_seconds * interval '1 second'
  );

  RETURN NEXT;
END;
$$;


--
-- Name: create_sign_up_link(text, text, integer); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.create_sign_up_link(p_email text, p_username text, p_ttl_seconds integer) RETURNS TABLE(outcome text, token text)
    LANGUAGE plpgsql SECURITY DEFINER
    SET search_path TO 'public', 'pg_temp'
    AS $$
DECLARE
  v_email text := lower(btrim(p_email));
  v_username text := lower(btrim(p_username));
BEGIN
  IF v_email IS NULL OR v_email = '' THEN
    RAISE EXCEPTION 'create_sign_up_link: email must not be empty'
      USING ERRCODE = 'invalid_parameter_value';
  END IF;
  IF v_username IS NULL OR v_username = '' THEN
    RAISE EXCEPTION 'create_sign_up_link: username must not be empty'
      USING ERRCODE = 'invalid_parameter_value';
  END IF;
  IF p_ttl_seconds IS NULL OR p_ttl_seconds < 1 THEN
    RAISE EXCEPTION 'create_sign_up_link: ttl must be at least 1 second, got %', p_ttl_seconds
      USING ERRCODE = 'invalid_parameter_value';
  END IF;

  IF EXISTS (SELECT 1 FROM users AS u WHERE u.username = v_username) THEN
    outcome := 'username-taken';
    token := NULL;
    RETURN NEXT;
    RETURN;
  END IF;

  IF EXISTS (SELECT 1 FROM users AS u WHERE u.email = v_email) THEN
    outcome := 'sign-in';
    v_username := NULL;
  ELSE
    outcome := 'sign-up';
  END IF;

  token := new_token();

  INSERT INTO magic_links (token_hash, email, username, expires_at)
  VALUES (
    sha256(convert_to(token, 'UTF8')),
    v_email,
    v_username,
    now() + p_ttl_seconds * interval '1 second'
  );

  RETURN NEXT;
END;
$$;


--
-- Name: delete_account(text); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.delete_account(p_token text) RETURNS boolean
    LANGUAGE sql SECURITY DEFINER
    SET search_path TO 'public', 'pg_temp'
    AS $$
  WITH deleted AS (
    DELETE FROM users
    WHERE id = (
      SELECT user_id FROM sessions
      WHERE token_hash = sha256(convert_to(p_token, 'UTF8'))
        AND expires_at > now()
    )
    RETURNING email
  ), links AS (
    DELETE FROM magic_links
    WHERE email IN (SELECT email FROM deleted)
  )
  SELECT EXISTS (SELECT 1 FROM deleted);
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
$$;


--
-- Name: is_pro(uuid); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.is_pro(p_user_id uuid) RETURNS boolean
    LANGUAGE sql STABLE SECURITY DEFINER
    SET search_path TO 'public', 'pg_temp'
    AS $$
  SELECT EXISTS (
    SELECT 1
    FROM subscriptions AS s
    WHERE s.user_id = p_user_id
      AND s.status IN ('active', 'trialing', 'past_due')
  );
$$;


--
-- Name: magic_link(text); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.magic_link(p_token text) RETURNS TABLE(email text, username text, new_account boolean)
    LANGUAGE sql STABLE SECURITY DEFINER
    SET search_path TO 'public', 'pg_temp'
    AS $$
  SELECT m.email, COALESCE(u.username, m.username), u.id IS NULL
  FROM magic_links AS m
  LEFT JOIN users AS u ON u.email = m.email
  WHERE m.token_hash = sha256(convert_to(p_token, 'UTF8'))
    AND m.expires_at > now()
    AND (u.id IS NOT NULL OR m.username IS NOT NULL);
$$;


--
-- Name: new_token(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.new_token() RETURNS text
    LANGUAGE sql
    SET search_path TO 'public', 'pg_temp'
    AS $$
  SELECT translate(
    rtrim(encode(uuid_send(gen_random_uuid()) || uuid_send(gen_random_uuid()), 'base64'), '='),
    '+/',
    '-_'
  );
$$;


--
-- Name: passkey_public_key(text); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.passkey_public_key(p_credential_id text) RETURNS text
    LANGUAGE sql STABLE SECURITY DEFINER
    SET search_path TO 'public', 'pg_temp'
    AS $$
  SELECT p.public_key
  FROM passkeys AS p
  WHERE p.credential_id = p_credential_id;
$$;


--
-- Name: sign_in_with_passkey(text, text, bigint, boolean, integer); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.sign_in_with_passkey(p_challenge text, p_credential_id text, p_sign_count bigint, p_backed_up boolean, p_session_seconds integer) RETURNS TABLE(outcome text, session_token text, email text, username text)
    LANGUAGE plpgsql SECURITY DEFINER
    SET search_path TO 'public', 'pg_temp'
    AS $$
DECLARE
  v_user_id uuid;
BEGIN
  IF p_session_seconds IS NULL OR p_session_seconds < 1 THEN
    RAISE EXCEPTION 'sign_in_with_passkey: session must last at least 1 second, got %', p_session_seconds
      USING ERRCODE = 'invalid_parameter_value';
  END IF;

  DELETE FROM webauthn_challenges AS c
  WHERE c.challenge = p_challenge AND c.ceremony = 'authentication' AND c.expires_at > now();

  IF NOT FOUND THEN
    outcome := 'no-challenge';
    RETURN NEXT;
    RETURN;
  END IF;

  UPDATE passkeys AS p
  SET sign_count = p_sign_count,
      backed_up = p.backup_eligible AND p_backed_up,
      last_used_at = now()
  WHERE p.credential_id = p_credential_id
    AND (p_sign_count > p.sign_count OR (p_sign_count = 0 AND p.sign_count = 0))
  RETURNING p.user_id INTO v_user_id;

  IF NOT FOUND THEN
    outcome := 'no-passkey';
    RETURN NEXT;
    RETURN;
  END IF;

  SELECT u.email, u.username INTO email, username
  FROM users AS u
  WHERE u.id = v_user_id;

  outcome := 'signed-in';
  session_token := new_token();

  INSERT INTO sessions (token_hash, user_id, expires_at)
  VALUES (
    sha256(convert_to(session_token, 'UTF8')),
    v_user_id,
    now() + p_session_seconds * interval '1 second'
  );

  RETURN NEXT;
END;
$$;


--
-- Name: sign_out(text); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.sign_out(p_token text) RETURNS void
    LANGUAGE sql SECURITY DEFINER
    SET search_path TO 'public', 'pg_temp'
    AS $$
  DELETE FROM sessions AS s
  WHERE s.token_hash = sha256(convert_to(p_token, 'UTF8'));
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
-- Name: published_pictures; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.published_pictures (
    id uuid DEFAULT gen_random_uuid() NOT NULL,
    user_id uuid NOT NULL,
    prompt text NOT NULL,
    width integer NOT NULL,
    height integer NOT NULL,
    steps integer NOT NULL,
    seed bigint NOT NULL,
    model text NOT NULL,
    bytes integer NOT NULL,
    tags text[] DEFAULT '{}'::text[] NOT NULL,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    published_at timestamp with time zone,
    removed_at timestamp with time zone,
    CONSTRAINT published_pictures_bytes_check CHECK (((bytes >= 1) AND (bytes <= 2097152))),
    CONSTRAINT published_pictures_height_check CHECK ((((height >= 256) AND (height <= 1024)) AND ((height % 64) = 0))),
    CONSTRAINT published_pictures_model_check CHECK (((model <> ''::text) AND (char_length(model) <= 200))),
    CONSTRAINT published_pictures_prompt_check CHECK (((prompt = btrim(prompt)) AND (prompt <> ''::text) AND (char_length(prompt) <= 2000))),
    CONSTRAINT published_pictures_seed_check CHECK (((seed >= 0) AND (seed <= '4294967295'::bigint))),
    CONSTRAINT published_pictures_steps_check CHECK (((steps >= 1) AND (steps <= 50))),
    CONSTRAINT published_pictures_tags_check CHECK (((cardinality(tags) <= 8) AND (array_position(tags, NULL::text) IS NULL) AND (array_to_string(tags, ','::text) ~ '^([^,]{1,24}(,[^,]{1,24})*)?$'::text) AND (array_to_string(tags, ','::text) = lower(array_to_string(tags, ','::text))))),
    CONSTRAINT published_pictures_width_check CHECK ((((width >= 256) AND (width <= 1024)) AND ((width % 64) = 0)))
);

ALTER TABLE ONLY public.published_pictures FORCE ROW LEVEL SECURITY;


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
-- Name: subscriptions; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.subscriptions (
    paddle_subscription_id text NOT NULL,
    user_id uuid NOT NULL,
    paddle_customer_id text NOT NULL,
    status text NOT NULL,
    price_id text NOT NULL,
    current_period_ends_at timestamp with time zone,
    scheduled_change text,
    scheduled_change_at timestamp with time zone,
    paddle_updated_at timestamp with time zone NOT NULL,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL,
    CONSTRAINT subscriptions_check CHECK (((scheduled_change IS NULL) = (scheduled_change_at IS NULL))),
    CONSTRAINT subscriptions_paddle_customer_id_check CHECK ((paddle_customer_id ~ '^ctm_[a-z0-9]+$'::text)),
    CONSTRAINT subscriptions_paddle_subscription_id_check CHECK ((paddle_subscription_id ~ '^sub_[a-z0-9]+$'::text)),
    CONSTRAINT subscriptions_price_id_check CHECK ((price_id ~ '^pri_[a-z0-9]+$'::text)),
    CONSTRAINT subscriptions_scheduled_change_check CHECK ((scheduled_change = ANY (ARRAY['cancel'::text, 'pause'::text, 'resume'::text]))),
    CONSTRAINT subscriptions_status_check CHECK ((status = ANY (ARRAY['active'::text, 'canceled'::text, 'past_due'::text, 'paused'::text, 'trialing'::text])))
);

ALTER TABLE ONLY public.subscriptions FORCE ROW LEVEL SECURITY;


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
-- Name: published_pictures published_pictures_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.published_pictures
    ADD CONSTRAINT published_pictures_pkey PRIMARY KEY (id);


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
-- Name: subscriptions subscriptions_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.subscriptions
    ADD CONSTRAINT subscriptions_pkey PRIMARY KEY (paddle_subscription_id);


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
-- Name: published_pictures_feed_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX published_pictures_feed_idx ON public.published_pictures USING btree (published_at DESC, id DESC) WHERE ((published_at IS NOT NULL) AND (removed_at IS NULL));


--
-- Name: published_pictures_tags_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX published_pictures_tags_idx ON public.published_pictures USING gin (tags) WHERE ((published_at IS NOT NULL) AND (removed_at IS NULL));


--
-- Name: published_pictures_user_id_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX published_pictures_user_id_idx ON public.published_pictures USING btree (user_id);


--
-- Name: sessions_expires_at_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX sessions_expires_at_idx ON public.sessions USING btree (expires_at);


--
-- Name: sessions_user_id_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX sessions_user_id_idx ON public.sessions USING btree (user_id);


--
-- Name: subscriptions_user_id_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX subscriptions_user_id_idx ON public.subscriptions USING btree (user_id);


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
-- Name: published_pictures published_pictures_user_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.published_pictures
    ADD CONSTRAINT published_pictures_user_id_fkey FOREIGN KEY (user_id) REFERENCES public.users(id) ON DELETE CASCADE;


--
-- Name: sessions sessions_user_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.sessions
    ADD CONSTRAINT sessions_user_id_fkey FOREIGN KEY (user_id) REFERENCES public.users(id) ON DELETE CASCADE;


--
-- Name: subscriptions subscriptions_user_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.subscriptions
    ADD CONSTRAINT subscriptions_user_id_fkey FOREIGN KEY (user_id) REFERENCES public.users(id) ON DELETE CASCADE;


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
-- Name: published_pictures own_rows; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY own_rows ON public.published_pictures USING ((user_id = ( SELECT (NULLIF(current_setting('app.user_id'::text, true), ''::text))::uuid AS "nullif"))) WITH CHECK ((user_id = ( SELECT (NULLIF(current_setting('app.user_id'::text, true), ''::text))::uuid AS "nullif")));


--
-- Name: sessions own_rows; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY own_rows ON public.sessions USING ((user_id = ( SELECT (NULLIF(current_setting('app.user_id'::text, true), ''::text))::uuid AS "nullif"))) WITH CHECK ((user_id = ( SELECT (NULLIF(current_setting('app.user_id'::text, true), ''::text))::uuid AS "nullif")));


--
-- Name: subscriptions own_rows; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY own_rows ON public.subscriptions USING ((user_id = ( SELECT (NULLIF(current_setting('app.user_id'::text, true), ''::text))::uuid AS "nullif"))) WITH CHECK ((user_id = ( SELECT (NULLIF(current_setting('app.user_id'::text, true), ''::text))::uuid AS "nullif")));


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
-- Name: published_pictures; Type: ROW SECURITY; Schema: public; Owner: -
--

ALTER TABLE public.published_pictures ENABLE ROW LEVEL SECURITY;

--
-- Name: sessions; Type: ROW SECURITY; Schema: public; Owner: -
--

ALTER TABLE public.sessions ENABLE ROW LEVEL SECURITY;

--
-- Name: subscriptions; Type: ROW SECURITY; Schema: public; Owner: -
--

ALTER TABLE public.subscriptions ENABLE ROW LEVEL SECURITY;

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
-- Name: FUNCTION apply_paddle_subscription(p_subscription_id text, p_user_id uuid, p_customer_id text, p_status text, p_price_id text, p_period_ends_at timestamp with time zone, p_scheduled_change text, p_scheduled_change_at timestamp with time zone, p_paddle_updated_at timestamp with time zone); Type: ACL; Schema: public; Owner: -
--

REVOKE ALL ON FUNCTION public.apply_paddle_subscription(p_subscription_id text, p_user_id uuid, p_customer_id text, p_status text, p_price_id text, p_period_ends_at timestamp with time zone, p_scheduled_change text, p_scheduled_change_at timestamp with time zone, p_paddle_updated_at timestamp with time zone) FROM PUBLIC;
GRANT ALL ON FUNCTION public.apply_paddle_subscription(p_subscription_id text, p_user_id uuid, p_customer_id text, p_status text, p_price_id text, p_period_ends_at timestamp with time zone, p_scheduled_change text, p_scheduled_change_at timestamp with time zone, p_paddle_updated_at timestamp with time zone) TO api;


--
-- Name: FUNCTION authenticate(p_token text); Type: ACL; Schema: public; Owner: -
--

REVOKE ALL ON FUNCTION public.authenticate(p_token text) FROM PUBLIC;
GRANT ALL ON FUNCTION public.authenticate(p_token text) TO api;


--
-- Name: FUNCTION consume_magic_link(p_token text, p_session_seconds integer); Type: ACL; Schema: public; Owner: -
--

REVOKE ALL ON FUNCTION public.consume_magic_link(p_token text, p_session_seconds integer) FROM PUBLIC;
GRANT ALL ON FUNCTION public.consume_magic_link(p_token text, p_session_seconds integer) TO api;


--
-- Name: FUNCTION create_authentication_challenge(p_ttl_seconds integer); Type: ACL; Schema: public; Owner: -
--

REVOKE ALL ON FUNCTION public.create_authentication_challenge(p_ttl_seconds integer) FROM PUBLIC;
GRANT ALL ON FUNCTION public.create_authentication_challenge(p_ttl_seconds integer) TO api;


--
-- Name: FUNCTION create_sign_in_link(p_email text, p_ttl_seconds integer); Type: ACL; Schema: public; Owner: -
--

REVOKE ALL ON FUNCTION public.create_sign_in_link(p_email text, p_ttl_seconds integer) FROM PUBLIC;
GRANT ALL ON FUNCTION public.create_sign_in_link(p_email text, p_ttl_seconds integer) TO api;


--
-- Name: FUNCTION create_sign_up_link(p_email text, p_username text, p_ttl_seconds integer); Type: ACL; Schema: public; Owner: -
--

REVOKE ALL ON FUNCTION public.create_sign_up_link(p_email text, p_username text, p_ttl_seconds integer) FROM PUBLIC;
GRANT ALL ON FUNCTION public.create_sign_up_link(p_email text, p_username text, p_ttl_seconds integer) TO api;


--
-- Name: FUNCTION delete_account(p_token text); Type: ACL; Schema: public; Owner: -
--

REVOKE ALL ON FUNCTION public.delete_account(p_token text) FROM PUBLIC;
GRANT ALL ON FUNCTION public.delete_account(p_token text) TO api;


--
-- Name: FUNCTION delete_expired(); Type: ACL; Schema: public; Owner: -
--

REVOKE ALL ON FUNCTION public.delete_expired() FROM PUBLIC;
GRANT ALL ON FUNCTION public.delete_expired() TO api;


--
-- Name: FUNCTION is_pro(p_user_id uuid); Type: ACL; Schema: public; Owner: -
--

REVOKE ALL ON FUNCTION public.is_pro(p_user_id uuid) FROM PUBLIC;
GRANT ALL ON FUNCTION public.is_pro(p_user_id uuid) TO api;


--
-- Name: FUNCTION magic_link(p_token text); Type: ACL; Schema: public; Owner: -
--

REVOKE ALL ON FUNCTION public.magic_link(p_token text) FROM PUBLIC;
GRANT ALL ON FUNCTION public.magic_link(p_token text) TO api;


--
-- Name: FUNCTION new_token(); Type: ACL; Schema: public; Owner: -
--

REVOKE ALL ON FUNCTION public.new_token() FROM PUBLIC;


--
-- Name: FUNCTION passkey_public_key(p_credential_id text); Type: ACL; Schema: public; Owner: -
--

REVOKE ALL ON FUNCTION public.passkey_public_key(p_credential_id text) FROM PUBLIC;
GRANT ALL ON FUNCTION public.passkey_public_key(p_credential_id text) TO api;


--
-- Name: FUNCTION sign_in_with_passkey(p_challenge text, p_credential_id text, p_sign_count bigint, p_backed_up boolean, p_session_seconds integer); Type: ACL; Schema: public; Owner: -
--

REVOKE ALL ON FUNCTION public.sign_in_with_passkey(p_challenge text, p_credential_id text, p_sign_count bigint, p_backed_up boolean, p_session_seconds integer) FROM PUBLIC;
GRANT ALL ON FUNCTION public.sign_in_with_passkey(p_challenge text, p_credential_id text, p_sign_count bigint, p_backed_up boolean, p_session_seconds integer) TO api;


--
-- Name: FUNCTION sign_out(p_token text); Type: ACL; Schema: public; Owner: -
--

REVOKE ALL ON FUNCTION public.sign_out(p_token text) FROM PUBLIC;
GRANT ALL ON FUNCTION public.sign_out(p_token text) TO api;


--
-- Name: TABLE passkeys; Type: ACL; Schema: public; Owner: -
--

GRANT SELECT,INSERT,DELETE ON TABLE public.passkeys TO api;


--
-- Name: COLUMN passkeys.name; Type: ACL; Schema: public; Owner: -
--

GRANT UPDATE(name) ON TABLE public.passkeys TO api;


--
-- Name: TABLE published_pictures; Type: ACL; Schema: public; Owner: -
--

GRANT SELECT,DELETE ON TABLE public.published_pictures TO api;


--
-- Name: COLUMN published_pictures.user_id; Type: ACL; Schema: public; Owner: -
--

GRANT INSERT(user_id) ON TABLE public.published_pictures TO api;


--
-- Name: COLUMN published_pictures.prompt; Type: ACL; Schema: public; Owner: -
--

GRANT INSERT(prompt) ON TABLE public.published_pictures TO api;


--
-- Name: COLUMN published_pictures.width; Type: ACL; Schema: public; Owner: -
--

GRANT INSERT(width) ON TABLE public.published_pictures TO api;


--
-- Name: COLUMN published_pictures.height; Type: ACL; Schema: public; Owner: -
--

GRANT INSERT(height) ON TABLE public.published_pictures TO api;


--
-- Name: COLUMN published_pictures.steps; Type: ACL; Schema: public; Owner: -
--

GRANT INSERT(steps) ON TABLE public.published_pictures TO api;


--
-- Name: COLUMN published_pictures.seed; Type: ACL; Schema: public; Owner: -
--

GRANT INSERT(seed) ON TABLE public.published_pictures TO api;


--
-- Name: COLUMN published_pictures.model; Type: ACL; Schema: public; Owner: -
--

GRANT INSERT(model) ON TABLE public.published_pictures TO api;


--
-- Name: COLUMN published_pictures.bytes; Type: ACL; Schema: public; Owner: -
--

GRANT INSERT(bytes) ON TABLE public.published_pictures TO api;


--
-- Name: COLUMN published_pictures.tags; Type: ACL; Schema: public; Owner: -
--

GRANT INSERT(tags) ON TABLE public.published_pictures TO api;


--
-- Name: COLUMN published_pictures.published_at; Type: ACL; Schema: public; Owner: -
--

GRANT UPDATE(published_at) ON TABLE public.published_pictures TO api;


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
-- Name: TABLE subscriptions; Type: ACL; Schema: public; Owner: -
--

GRANT SELECT ON TABLE public.subscriptions TO api;


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

