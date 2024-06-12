/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.configuration.security;

import org.springframework.context.annotation.Configuration;
import org.springframework.security.config.annotation.web.builders.HttpSecurity;
import org.springframework.security.oauth2.config.annotation.web.configuration.EnableResourceServer;
import org.springframework.security.oauth2.config.annotation.web.configuration.ResourceServerConfigurerAdapter;

/**
 * {@link ResourcesConfiguration} class.
 * 
 * @author HaythemBenizid
 * @since 0.3.0
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

		/*
		 * swagger config
		 */
		http.authorizeRequests().antMatchers("/actuator/**", "/v2/api-docs", "/configuration/ui",
				"/swagger-resources", "/configuration/security", "/webjars/**",
				"/vneuron/searchWebhook", "/transvers-service/vneuron/searchWebhook",
				"/vneuron/customerStatus", "/transvers-service/vneuron/customerStatus",
				"/transvers-service/nafath-api/webhook", "/nafath-api/webhook",
				"/swagger-resources/configuration/ui", "/swagger-ui.html",
				"/transvers-service/vneuron/**", "/swagger-resources/configuration/security",
				"/ged-service/v2/api-docs", "/transvers-service/v2/api-docs",
				"/reporting-service/v2/api-docs", "/crm-service/v2/api-docs",
				"/credit-service/v2/api-docs", "/authentication-service/v2/api-docs",
				"/expenses-service/v2/api-docs", "/incentive-service/v2/api-docs",
				"/parametrage-service/v2/api-docs", "/dashboarding-service/v2/api-docs","/stomp/**").permitAll().anyRequest();
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
	}
}
