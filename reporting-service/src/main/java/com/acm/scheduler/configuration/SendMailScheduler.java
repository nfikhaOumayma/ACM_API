/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.scheduler.configuration;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.scheduling.annotation.EnableScheduling;

import com.acm.utils.repository.AcmEnvironnementRepository;

/**
 * {@link BatchScheduler} class.
 *
 * @author HaythemBenizid
 * @since 0.1.0
 */
@Configuration
@EnableScheduling
public class SendMailScheduler {

	/** The acmEnvironnement repository. */
	@Autowired
	private AcmEnvironnementRepository environnementRepository;

	/**
	 * Gets the cron value for sending mail licence.
	 *
	 * @return the cron value for sending mail licence
	 */
	@Bean
	public String getCronValueForSendingMailLicence() {

		return environnementRepository.findByKey("CRON_EXPRESSION_SENDING_MAIL_LICENCE").get(0)
				.getValue();
	}
}
