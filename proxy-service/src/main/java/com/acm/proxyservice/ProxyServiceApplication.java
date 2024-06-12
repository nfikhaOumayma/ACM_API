/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.proxyservice;

import java.util.Arrays;

import javax.servlet.http.HttpServletRequest;

import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.boot.autoconfigure.security.oauth2.client.EnableOAuth2Sso;
import org.springframework.cloud.client.discovery.EnableDiscoveryClient;
import org.springframework.cloud.netflix.zuul.EnableZuulProxy;
import org.springframework.context.annotation.Bean;
import org.springframework.http.HttpMethod;
import org.springframework.web.multipart.MultipartResolver;
import org.springframework.web.multipart.support.StandardServletMultipartResolver;

import springfox.documentation.swagger.web.UiConfiguration;
import springfox.documentation.swagger2.annotations.EnableSwagger2;

/**
 * {@link ProxyServiceApplication} class.
 *
 * @author HaythemBenizid
 * @since 0.1.0
 */
@EnableSwagger2
@EnableDiscoveryClient
@EnableZuulProxy
@EnableOAuth2Sso
@SpringBootApplication
public class ProxyServiceApplication {

	/** The Constant SUPPORTED_MULTIPART_METHODS. */
	private static final HttpMethod[] SUPPORTED_MULTIPART_METHODS =
			{HttpMethod.POST, HttpMethod.PUT};

	/**
	 * The main method.
	 *
	 * @param args the arguments
	 */
	public static void main(String[] args) {

		SpringApplication.run(ProxyServiceApplication.class, args);
	}

	/**
	 * Ui config.
	 *
	 * @return the ui configuration
	 */
	@SuppressWarnings("deprecation")
	@Bean
	UiConfiguration uiConfig() {

		return new UiConfiguration("validatorUrl", "list", "alpha", "schema",
				UiConfiguration.Constants.DEFAULT_SUBMIT_METHODS, false, true, 60000L);
	}

	/**
	 * Multipart resolver.
	 *
	 * @return the multipart resolver
	 */
	@Bean
	public MultipartResolver multipartResolver() {

		return new StandardServletMultipartResolver() {
			@Override
			public boolean isMultipart(HttpServletRequest request) {

				boolean methodMatches = Arrays.stream(SUPPORTED_MULTIPART_METHODS)
						.anyMatch(method -> method.matches(request.getMethod()));
				String contentType = request.getContentType();
				return methodMatches && (contentType != null
						&& contentType.toLowerCase().startsWith("multipart/"));
			}
		};
	}

}
