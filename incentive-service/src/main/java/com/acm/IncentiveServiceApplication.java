/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm;

import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.boot.autoconfigure.security.oauth2.client.EnableOAuth2Sso;
import org.springframework.cloud.client.discovery.EnableDiscoveryClient;
import org.springframework.cloud.openfeign.EnableFeignClients;
import org.springframework.context.annotation.PropertySource;

/**
 * The {@link IncentiveServiceApplication} Class.
 * 
 * @author HaythemBenizid
 * @since 1.0.8
 */
@SpringBootApplication
@EnableFeignClients
@EnableDiscoveryClient
@EnableOAuth2Sso
@PropertySource(value = "classpath:message.properties", encoding = "UTF-8")
public class IncentiveServiceApplication {
	/**
	 * The main method.
	 *
	 * @param args the arguments
	 */
	public static void main(String[] args) {

		SpringApplication.run(IncentiveServiceApplication.class, args);
	}
}
