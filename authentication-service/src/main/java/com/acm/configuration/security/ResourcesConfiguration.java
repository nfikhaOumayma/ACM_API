/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.configuration.security;

import org.springframework.context.annotation.Configuration;
import org.springframework.security.config.annotation.web.builders.HttpSecurity;
import org.springframework.security.oauth2.config.annotation.web.configuration.EnableResourceServer;
import org.springframework.security.oauth2.config.annotation.web.configuration.ResourceServerConfigurerAdapter;
import org.springframework.security.oauth2.config.annotation.web.configurers.ResourceServerSecurityConfigurer;

import com.acm.configuration.oauth2server.AuthenticationServerConfig;

/**
 * {@link ResourcesConfiguration} class.
 *
 * @author HaythemBenizid
 * @since 0.1.0
 */
@Configuration
@EnableResourceServer
public class ResourcesConfiguration extends ResourceServerConfigurerAdapter {

	/*
	 * (non-Javadoc)
	 * @see org.springframework.security.oauth2.config.annotation.web.configuration.
	 * ResourceServerConfigurerAdapter#configure(org.springframework.security.config.annotation.web.
	 * builders.HttpSecurity)
	 */
	@Override
	public void configure(HttpSecurity http) throws Exception {

		http.authorizeRequests().antMatchers("/login/**", "/authentication-service/login/**",
				"/vneuron/searchWebhook", "/transvers-service/vneuron/searchWebhook",
				"/vneuron/customerStatus", "/transvers-service/vneuron/customerStatus",
				"/transvers-service/nafath-api/webhook", "/nafath-api/webhook", "/actuator/**",
				"/v2/api-docs", "/configuration/ui", "/swagger-resources",
				"/actuator/**", "/v2/api-docs", "/configuration/ui", "/swagger-resources",
				"/configuration/security", "/webjars/**", "/swagger-resources/configuration/ui",
				"/swagger-ui.html", "/swagger-resources/configuration/security",
				"/ged-service/v2/api-docs", "/transvers-service/v2/api-docs",
				"/reporting-service/v2/api-docs", "/crm-service/v2/api-docs",
				"/credit-service/v2/api-docs", "/authentication-service/v2/api-docs",
				"/expenses-service/v2/api-docs", "/incentive-service/v2/api-docs",
				"/parametrage-service/v2/api-docs", "/dashboarding-service/v2/api-docs" ,"/stomp").permitAll().anyRequest();
		http.authorizeRequests().anyRequest().fullyAuthenticated();
	}

	/*
	 * (non-Javadoc)
	 * @see org.springframework.security.oauth2.config.annotation.web.configuration.
	 * ResourceServerConfigurerAdapter#configure(org.springframework.security.oauth2.config.
	 * annotation.web.configurers.ResourceServerSecurityConfigurer)
	 */
	@Override
	public void configure(ResourceServerSecurityConfigurer resources) throws Exception {

		resources.resourceId(AuthenticationServerConfig.RESOURCE_ID);
	}
}
