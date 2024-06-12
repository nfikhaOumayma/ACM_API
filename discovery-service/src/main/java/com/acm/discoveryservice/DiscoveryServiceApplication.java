/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.discoveryservice;

import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.cloud.netflix.eureka.server.EnableEurekaServer;

/**
 * {@link DiscoveryServiceApplication} class.
 *
 * @author HaythemBenizid
 * @since 0.1.0
 */
@EnableEurekaServer
@SpringBootApplication
public class DiscoveryServiceApplication {

	/**
	 * The main method.
	 *
	 * @param args the arguments
	 */
	public static void main(String[] args) {

		SpringApplication.run(DiscoveryServiceApplication.class, args);
	}
}
