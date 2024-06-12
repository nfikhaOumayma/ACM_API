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
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Primary;
import org.springframework.context.annotation.PropertySource;
import org.springframework.core.task.TaskExecutor;
import org.springframework.scheduling.annotation.EnableAsync;
import org.springframework.scheduling.annotation.EnableScheduling;
import org.springframework.scheduling.concurrent.ThreadPoolTaskExecutor;

/**
 * The {@link CreditServiceApplication} Class.
 * 
 * @author HaythemBenizid
 * @since 0.1.0
 */
// @SpringBootApplication(exclude = org.activiti.spring.boot.SecurityAutoConfiguration.class)
@SpringBootApplication()
@EnableScheduling
@EnableFeignClients
@EnableDiscoveryClient
@EnableOAuth2Sso
@EnableAsync
@PropertySource(value = "classpath:message.properties", encoding = "UTF-8")
public class CreditServiceApplication {

	/**
	 * Primary task executor.
	 *
	 * @return the task executor
	 */
	@Primary
	@Bean
	public TaskExecutor primaryTaskExecutor() {

		// add necessary properties to the executor
		return new ThreadPoolTaskExecutor();
	}

	/**
	 * The main method.
	 *
	 * @param args the arguments
	 */
	public static void main(String[] args) {

		SpringApplication.run(CreditServiceApplication.class, args);
	}

}
