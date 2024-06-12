/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.configuration.mailing;

import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

import com.acm.utils.repository.AcmEnvironnementRepository;

/**
 * {@link MailingBeanConfiguration} class.
 *
 * @author HaythemBenizid
 * @since 1.0.10
 */
@Configuration
public class MailingBeanConfiguration {

	/**
	 * load the ACM default receiver mail : by key=DEFAULT_ACM_RECIEVER_EMAIL.
	 *
	 * @author HaythemBenizid
	 * @param environnementRepository the environnement repository
	 * @return the ACM default receiver mail
	 */
	@Bean
	public String defaultACMReceiverMail(AcmEnvironnementRepository environnementRepository) {

		return environnementRepository.findByKey("DEFAULT_ACM_RECEIVER_EMAIL").get(0).getValue();
	}
}
