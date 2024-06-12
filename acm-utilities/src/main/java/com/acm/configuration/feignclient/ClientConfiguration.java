/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.configuration.feignclient;

import org.springframework.context.annotation.Bean;

import feign.Request;
import feign.codec.ErrorDecoder;

/**
 * {@link ClientConfiguration} class.
 *
 * @author HaythemBenizid
 * @since 0.1.0
 */
public class ClientConfiguration {

	/**
	 * Options.
	 *
	 * @return the request. options
	 */
	@Bean
	public Request.Options options() {

		return new Request.Options(100000, 1200000);
	}

	/**
	 * Error decoder.
	 * 
	 * @author hbeji
	 * @return the error decoder
	 */
	@Bean
	public ErrorDecoder errorDecoder() {

		return new CustomErrorDecoder();
	}
}
