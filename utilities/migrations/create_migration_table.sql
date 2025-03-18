--
-- PostgreSQL database dump
--

-- Dumped from database version 15.5 (Debian 15.5-1.pgdg120+1)
-- Dumped by pg_dump version 15.5 (Debian 15.5-1.pgdg120+1)

SET statement_timeout = 0;
SET lock_timeout = 0;
SET idle_in_transaction_session_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SELECT pg_catalog.set_config('search_path', '', false);
SET check_function_bodies = false;
SET xmloption = content;
SET client_min_messages = warning;
SET row_security = off;

SET default_tablespace = '';

SET default_table_access_method = heap;

--
-- Name: migrations; Type: TABLE; Schema: public; Owner: app_data
--

CREATE TABLE public.migrations (
    id integer NOT NULL,
    migration_id bigint NOT NULL,
    time_begun timestamp with time zone NOT NULL,
    time_complete timestamp with time zone NOT NULL,
    run_id text,
    message text,
    CONSTRAINT migrations_migration_id_check CHECK ((migration_id >= 0))
);


ALTER TABLE public.migrations OWNER TO app_data;

--
-- Name: migrations_id_seq; Type: SEQUENCE; Schema: public; Owner: app_data
--

CREATE SEQUENCE public.migrations_id_seq
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.migrations_id_seq OWNER TO app_data;

--
-- Name: migrations_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: app_data
--

ALTER SEQUENCE public.migrations_id_seq OWNED BY public.migrations.id;


--
-- Name: migrations id; Type: DEFAULT; Schema: public; Owner: app_data
--

ALTER TABLE ONLY public.migrations ALTER COLUMN id SET DEFAULT nextval('public.migrations_id_seq'::regclass);


--
-- Name: migrations migrations_pkey; Type: CONSTRAINT; Schema: public; Owner: app_data
--

ALTER TABLE ONLY public.migrations
    ADD CONSTRAINT migrations_pkey PRIMARY KEY (id);


--
-- Name: migrations unique_migration_id; Type: CONSTRAINT; Schema: public; Owner: app_data
--

ALTER TABLE ONLY public.migrations
    ADD CONSTRAINT unique_migration_id UNIQUE (migration_id);


--
-- Name: migrations_migration_id; Type: INDEX; Schema: public; Owner: app_data
--

CREATE INDEX migrations_migration_id ON public.migrations USING btree (migration_id);


--
-- PostgreSQL database dump complete
--
