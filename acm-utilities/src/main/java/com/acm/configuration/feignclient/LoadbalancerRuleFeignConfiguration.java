/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.configuration.feignclient;

import org.springframework.context.annotation.Bean;

import com.netflix.client.config.IClientConfig;
import com.netflix.loadbalancer.IRule;
import com.netflix.loadbalancer.WeightedResponseTimeRule;

/**
 * {@link LoadbalancerRuleFeignConfiguration} class.
 *
 * @author HaythemBenizid
 * @since 1.0.9
 */
public class LoadbalancerRuleFeignConfiguration {

	/**
	 * Ribbon rule.
	 * 
	 * @author HaythemBenizid
	 * @param config the config
	 * @return the i rule
	 */
	@Bean
	public IRule ribbonRule(IClientConfig config) {

		return new WeightedResponseTimeRule();
	}
}
