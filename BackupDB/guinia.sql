--
-- PostgreSQL database dump
--

-- Dumped from database version 9.4.4
-- Dumped by pg_dump version 9.4.4
-- Started on 2016-05-31 11:01:53

SET statement_timeout = 0;
SET lock_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SET check_function_bodies = false;
SET client_min_messages = warning;

--
-- TOC entry 174 (class 3079 OID 11855)
-- Name: plpgsql; Type: EXTENSION; Schema: -; Owner: 
--

CREATE EXTENSION IF NOT EXISTS plpgsql WITH SCHEMA pg_catalog;


--
-- TOC entry 2006 (class 0 OID 0)
-- Dependencies: 174
-- Name: EXTENSION plpgsql; Type: COMMENT; Schema: -; Owner: 
--

COMMENT ON EXTENSION plpgsql IS 'PL/pgSQL procedural language';


SET search_path = public, pg_catalog;

SET default_tablespace = '';

SET default_with_oids = false;

--
-- TOC entry 173 (class 1259 OID 33113)
-- Name: data_set; Type: TABLE; Schema: public; Owner: postgres; Tablespace: 
--

CREATE TABLE data_set (
    id character varying(100) NOT NULL,
    name character varying(50),
    separator character varying(10),
    "decimal" character varying(10),
    quote character varying(10),
    na character varying(10)
);


ALTER TABLE data_set OWNER TO postgres;

--
-- TOC entry 172 (class 1259 OID 24886)
-- Name: user_guinia; Type: TABLE; Schema: public; Owner: postgres; Tablespace: 
--

CREATE TABLE user_guinia (
    user_name character varying(30) NOT NULL,
    name character varying(30),
    last_name character varying(30),
    email character varying(50),
    password character varying(50)
);


ALTER TABLE user_guinia OWNER TO postgres;

--
-- TOC entry 1998 (class 0 OID 33113)
-- Dependencies: 173
-- Data for Name: data_set; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY data_set (id, name, separator, "decimal", quote, na) FROM stdin;
1463440592_e02aa87e9e85e85c3e0fe8832a440a9a.csv	communities.csv	\N	\N	\N	\N
1463454493_af57afd9a634d2813c89a3ec37d6c3e1.csv	algae.csv	\N	\N	\N	\N
1463457735_ebe26891573a8590ae023524add84a20.csv	autosclean.csv	\N	\N	\N	\N
\.


--
-- TOC entry 1997 (class 0 OID 24886)
-- Dependencies: 172
-- Data for Name: user_guinia; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY user_guinia (user_name, name, last_name, email, password) FROM stdin;
 Lili	Liliana	Barros	liliana@barros.cl	1234 
 Nuevo	nuevo	nuevo	nuevo	123 
 sdsd	assd	asdsd	adsd	asd 
 admin 	\N	\N	\N	123
 user 	\N	\N	\N	1234
\.


--
-- TOC entry 1887 (class 2606 OID 33117)
-- Name: data_set_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres; Tablespace: 
--

ALTER TABLE ONLY data_set
    ADD CONSTRAINT data_set_pkey PRIMARY KEY (id);


--
-- TOC entry 1885 (class 2606 OID 33112)
-- Name: user_guinia_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres; Tablespace: 
--

ALTER TABLE ONLY user_guinia
    ADD CONSTRAINT user_guinia_pkey PRIMARY KEY (user_name);


--
-- TOC entry 2005 (class 0 OID 0)
-- Dependencies: 5
-- Name: public; Type: ACL; Schema: -; Owner: postgres
--

REVOKE ALL ON SCHEMA public FROM PUBLIC;
REVOKE ALL ON SCHEMA public FROM postgres;
GRANT ALL ON SCHEMA public TO postgres;
GRANT ALL ON SCHEMA public TO PUBLIC;


-- Completed on 2016-05-31 11:01:54

--
-- PostgreSQL database dump complete
--

