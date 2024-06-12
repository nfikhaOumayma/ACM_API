/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.configuration.batch;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Configuration;
import org.springframework.scheduling.annotation.EnableScheduling;
import org.springframework.scheduling.annotation.Scheduled;

import com.acm.client.CreditClient;
import com.acm.constants.common.CommonFunctions;

/**
 * The Class BatchSanadLoanAuto.
 */
@Configuration
@EnableScheduling
public class BatchSanadLoanAuto {

	/** The token. */
	private String token = "NOT";

	/** The url serveur authentification. */
	@Value("${url.serveur.authentification}")
	private String urlServeurAuthentification;

	/** The credit client. */
	@Autowired
	private CreditClient creditClient;

	/**
	 * Job loans sanad.
	 */
	@Scheduled(cron = "${cron.expression.loansOfSanad.batch}")
	public void jobLoansSanad() {

		token = "Bearer " + CommonFunctions.generateToken(urlServeurAuthentification);
		creditClient.jobLoansSanad(token);
	}

}
