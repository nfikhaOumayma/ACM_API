/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm;

import org.springframework.batch.core.configuration.annotation.EnableBatchProcessing;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.boot.autoconfigure.security.oauth2.client.EnableOAuth2Sso;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.cloud.client.discovery.EnableDiscoveryClient;
import org.springframework.cloud.openfeign.EnableFeignClients;
import org.springframework.context.annotation.PropertySource;

/**
 * The {@link ParametrageServiceApplication} Class.
 * 
 * @author HaythemBenizid
 * @since 0.6.0
 */
@EnableConfigurationProperties(ApplicationProperties.class)
@SpringBootApplication
@EnableFeignClients
@EnableBatchProcessing
@EnableDiscoveryClient
@EnableOAuth2Sso
@PropertySource(value = "classpath:message.properties", encoding = "UTF-8")
public class ParametrageServiceApplication {
	/**
	 * The main method.
	 *
	 * @param args the arguments
	 */
	public static void main(String[] args) {

		SpringApplication.run(ParametrageServiceApplication.class, args);
	}

}
