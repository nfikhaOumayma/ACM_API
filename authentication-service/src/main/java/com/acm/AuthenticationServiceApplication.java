/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm;

import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.cloud.client.discovery.EnableDiscoveryClient;
import org.springframework.cloud.openfeign.EnableFeignClients;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.PropertySource;
import org.springframework.core.annotation.Order;
import org.springframework.web.context.request.RequestContextListener;

/**
 * The {@link AuthenticationServiceApplication} Class.
 * 
 * @author HaythemBenizid
 * @since 0.1.0
 */
@EnableFeignClients
@EnableDiscoveryClient
@SpringBootApplication
@PropertySource(value = "classpath:message.properties", encoding = "UTF-8")
public class AuthenticationServiceApplication {
	/**
	 * The main method.
	 *
	 * @param args the arguments
	 */
	public static void main(String[] args) {

		SpringApplication.run(AuthenticationServiceApplication.class, args);
	}

	/**
	 * Request context listener request context listener.
	 *
	 * @return the request context listener
	 */
	@Bean
	@Order(0)
	public RequestContextListener requestContextListener() {

		return new RequestContextListener();
	}
}
