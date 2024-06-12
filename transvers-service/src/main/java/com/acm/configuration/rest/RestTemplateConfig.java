/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.configuration.rest;

import java.nio.charset.StandardCharsets;
import java.security.KeyManagementException;
import java.security.KeyStoreException;
import java.security.NoSuchAlgorithmException;
import java.security.cert.CertificateException;
import java.security.cert.X509Certificate;

import javax.net.ssl.SSLContext;

import org.apache.http.conn.ssl.NoopHostnameVerifier;
import org.apache.http.conn.ssl.SSLConnectionSocketFactory;
import org.apache.http.conn.ssl.TrustStrategy;
import org.apache.http.impl.client.CloseableHttpClient;
import org.apache.http.impl.client.HttpClients;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.http.client.HttpComponentsClientHttpRequestFactory;
import org.springframework.http.converter.StringHttpMessageConverter;
import org.springframework.web.client.RestTemplate;

/**
 * {@link RestTemplateConfig} class.
 *
 * @author HaythemBenizid
 * @since 0.9.0
 */
public class RestTemplateConfig implements java.io.Serializable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = -4654440055205699479L;

	/** Default Mode is INFO. */
	private static final Logger logger = LoggerFactory.getLogger(RestTemplateConfig.class);

	/**
	 * Init the rest template : DISABLE SSL CERTIFICATE VALIDATION IN RESTTEMPLATE.****************
	 * We will use RestTemplateBuilder to create a custom RestTemplate Bean that will trust all kind
	 * of bad SSL certificates. Please be aware that this should never be done for any production
	 * environment.
	 * 
	 * @author HaythemBenizid
	 * @return the rest template
	 * @throws KeyStoreException the key store exception
	 * @throws NoSuchAlgorithmException the no such algorithm exception
	 * @throws KeyManagementException the key management exception
	 */
	public static RestTemplate initRestTemplate()
			throws KeyStoreException, NoSuchAlgorithmException, KeyManagementException {

		TrustStrategy acceptingTrustStrategy = new TrustStrategy() {
			@Override
			public boolean isTrusted(X509Certificate[] x509Certificates, String s)
					throws CertificateException {

				return true;
			}
		};
		SSLContext sslContext = org.apache.http.ssl.SSLContexts.custom()
				.loadTrustMaterial(null, acceptingTrustStrategy).build();
		SSLConnectionSocketFactory csf =
				new SSLConnectionSocketFactory(sslContext, new NoopHostnameVerifier());
		CloseableHttpClient httpClient = HttpClients.custom().setSSLSocketFactory(csf).build();
		HttpComponentsClientHttpRequestFactory requestFactory =
				new HttpComponentsClientHttpRequestFactory();
		requestFactory.setHttpClient(httpClient);

		logger.debug("Init RestTemplate => (DISABLE SSL CERTIFICATE VALIDATION) :: DONE ");
		RestTemplate restTemplate = new RestTemplate(requestFactory);
		// setting StandardCharsets = UTF-8
		restTemplate.getMessageConverters().add(0,
				new StringHttpMessageConverter(StandardCharsets.UTF_8));
		return restTemplate;
	}
}
