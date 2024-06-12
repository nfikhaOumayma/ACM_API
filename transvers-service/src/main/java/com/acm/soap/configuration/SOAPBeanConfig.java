/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.soap.configuration;

import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.oxm.jaxb.Jaxb2Marshaller;

/**
 * {@link SOAPBeanConfig} class.
 *
 * @author HaythemBenizid
 * @since 0.1.0
 */
@Configuration
public class SOAPBeanConfig {

	/**
	 * Marshaller.
	 * 
	 * @author HaythemBenizid
	 * @return the jaxb 2 marshaller
	 */
	@Bean
	public Jaxb2Marshaller marshaller() {

		Jaxb2Marshaller jaxb2Marshaller = new Jaxb2Marshaller();
		jaxb2Marshaller.setPackagesToScan("com.acm.soap.model");
		return jaxb2Marshaller;
	}
}
