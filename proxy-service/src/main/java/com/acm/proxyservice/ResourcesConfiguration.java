/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.proxyservice;

import java.io.IOException;
import java.util.Arrays;
import java.util.Collections;
import java.util.stream.Collectors;

import javax.servlet.Filter;
import javax.servlet.FilterChain;
import javax.servlet.FilterConfig;
import javax.servlet.ServletException;
import javax.servlet.ServletRequest;
import javax.servlet.ServletResponse;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.http.HttpMethod;
import org.springframework.security.config.annotation.web.builders.HttpSecurity;
import org.springframework.security.oauth2.config.annotation.web.configuration.EnableResourceServer;
import org.springframework.security.oauth2.config.annotation.web.configuration.ResourceServerConfigurerAdapter;
import org.springframework.web.cors.CorsConfiguration;
import org.springframework.web.cors.UrlBasedCorsConfigurationSource;
import org.springframework.web.filter.CorsFilter;

/**
 * {@link ResourcesConfiguration} class.
 * 
 * @author HaythemBenizid
 * @since 0.3.0
 */
@Configuration
@EnableResourceServer
public class ResourcesConfiguration extends ResourceServerConfigurerAdapter implements Filter {

	/*
	 * (non-Javadoc)
	 * @see org.springframework.security.oauth2.config.annotation.web.configuration.
	 * ResourceServerConfigurerAdapter#configure(org.springframework.security.config.annotation.web.
	 * builders.HttpSecurity)
	 */
	@Override
	public void configure(HttpSecurity http) throws Exception {

		/*
		 * swagger config
		 */
		http.authorizeRequests()
				.antMatchers("/actuator/**", "/v2/api-docs", "/configuration/ui",
						"/vneuron/searchWebhook", "/transvers-service/vneuron/searchWebhook",
						"/vneuron/customerStatus", "/transvers-service/vneuron/customerStatus",
						"/transvers-service/nafath-api/webhook", "/nafath-api/webhook",
						"/swagger-resources", "/configuration/security", "/webjars/**",
						"/swagger-resources/configuration/ui", "/swagger-ui.html",
						"/swagger-resources/configuration/security", "/ged-service/v2/api-docs",
						"/transvers-service/v2/api-docs", "/reporting-service/v2/api-docs",
						"/crm-service/v2/api-docs", "/credit-service/v2/api-docs",
						"/authentication-service/v2/api-docs", "/expenses-service/v2/api-docs",
						"/incentive-service/v2/api-docs", "/parametrage-service/v2/api-docs", "/dashboarding-service/v2/api-docs")
				.permitAll().anyRequest();
		http.authorizeRequests().anyRequest().fullyAuthenticated();
		/*
		 * Micro Service Parametrage Allow Acces
		 */
		http.authorizeRequests().antMatchers("/users/**").authenticated();

		/*
		 * Micro Service reporting Allow Acces
		 */
		http.authorizeRequests().antMatchers("/edition/**").permitAll().anyRequest();
		http.authorizeRequests().antMatchers("/mailsender/**").permitAll().anyRequest();
		http.authorizeRequests().antMatchers("/notifications/**").permitAll().anyRequest();

		/*
		 * Micro Service transvers Allow Acces
		 */
		// http.authorizeRequests().antMatchers("/transvers-service/**").authenticated();
		http.authorizeRequests().antMatchers("/load-data-abacus/**").authenticated();

		/*
		 * Micro Service credit Allow Acces
		 */
		http.authorizeRequests().antMatchers("/credit-service/**").authenticated();
		http.authorizeRequests().antMatchers("/loans/**").authenticated();
		http.authorizeRequests().antMatchers("/loans-documents/**").authenticated();
		http.authorizeRequests().antMatchers("/report-visits/**").authenticated();

		/*
		 * Micro Service crm Allow Acces
		 */
		http.authorizeRequests().antMatchers("/crm-service/**").authenticated();
		/*
		 * Dashboarding Service  Allow Acces
		 */
		http.authorizeRequests().antMatchers("/dashboarding-service/**").authenticated();

		/*
		 * Micro Service parametrage Allow Acces
		 */
		http.authorizeRequests().antMatchers("/habilitations/**").authenticated();
		http.cors();
	}

	/**
	 * Cors filter.
	 * 
	 * @return the filter registration bean
	 */
	@Bean
	public CorsFilter corsFilter() {

		final UrlBasedCorsConfigurationSource source = new UrlBasedCorsConfigurationSource();
		final CorsConfiguration config = new CorsConfiguration();
		config.setAllowCredentials(true);
		config.setAllowedOrigins(Collections.singletonList("*"));
		config.setAllowedHeaders(Collections.singletonList("*"));
		config.setAllowedMethods(Arrays.stream(HttpMethod.values()).map(HttpMethod::name)
				.collect(Collectors.toList()));
		source.registerCorsConfiguration("/**", config);
		return new CorsFilter(source);
	}

	/*
	 * (non-Javadoc)
	 * @see javax.servlet.Filter#doFilter(javax.servlet.ServletRequest,
	 * javax.servlet.ServletResponse, javax.servlet.FilterChain)
	 */
	@Override
	public void doFilter(ServletRequest request, ServletResponse response, FilterChain chain)
			throws IOException, ServletException {

		HttpServletRequest req = (HttpServletRequest) request;
		HttpServletResponse res = (HttpServletResponse) response;
		res.setHeader("Access-Control-Max-Age", "3600");
		res.setHeader("Access-Control-Allow-Origin", "*");
		res.setHeader("Access-Control-Allow-Credentials", "true");
		res.setHeader("Access-Control-Allow-Methods",
				"ACL, CANCELUPLOAD, CHECKIN, CHECKOUT, COPY, DELETE, GET, HEAD, LOCK, MKCALENDAR, MKCOL, MOVE, OPTIONS, POST, PROPFIND, PROPPATCH, PUT, REPORT, SEARCH, UNCHECKOUT, UNLOCK, UPDATE, VERSION-CONTROL");
		res.setHeader("Access-Control-Allow-Headers",
				"Authorization, Content-Type, Accept, x-requested-with, Cache-Control, Origin, X-Requested-With");
		if ("OPTIONS".equalsIgnoreCase(req.getMethod())) {
			res.setStatus(HttpServletResponse.SC_OK);
		}
		else {
			chain.doFilter(req, res);
		}
	}

	/*
	 * (non-Javadoc)
	 * @see javax.servlet.Filter#init(javax.servlet.FilterConfig)
	 */
	@Override
	public void init(FilterConfig filterConfig) throws ServletException {

		/*
		 * 
		 */
	}

	/*
	 * (non-Javadoc)
	 * @see javax.servlet.Filter#destroy()
	 */
	@Override
	public void destroy() {

		/*
		 * 
		 */
	}
}
