/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm;

import org.springframework.boot.ApplicationRunner;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.boot.autoconfigure.security.oauth2.client.EnableOAuth2Sso;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.cloud.client.discovery.EnableDiscoveryClient;
import org.springframework.cloud.openfeign.EnableFeignClients;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.PropertySource;
import org.springframework.scheduling.annotation.EnableAsync;

import com.acm.service.FileSystemStorageService;
import com.acm.service.ReportService;

/**
 * The {@link ReportingServiceApplication} Class.
 *
 * @author HaythemBenizid
 * @since 0.1.0
 */
@EnableConfigurationProperties(ApplicationProperties.class)
@EnableFeignClients
@EnableDiscoveryClient
@SpringBootApplication
@EnableAsync // enabling asynchronous processing with Java configuration
@PropertySource(value = "classpath:message.properties", encoding = "UTF-8")
@EnableOAuth2Sso
public class ReportingServiceApplication {

	/**
	 * The main method.
	 *
	 * @param args the arguments
	 */
	public static void main(String[] args) {

		SpringApplication.run(ReportingServiceApplication.class, args);
	}

	/**
	 * Inits : delete all .jasper file in temp folder.
	 *
	 * @param storageService the storage service
	 * @return the application runner
	 */
	@Bean
	ApplicationRunner init(FileSystemStorageService storageService) {

		return args -> {
			// storageService.deleteAll();
			// storageService.init();
		};
	}

	/**
	 * Inits : delete all generate file in temp folder.
	 *
	 * @param reportService the storage service
	 * @return the application runner
	 */
	@Bean
	ApplicationRunner inits(ReportService reportService) {

		return args -> {
			// reportService.deleteAll();
			// reportService.init();
		};
	}
}
