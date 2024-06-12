/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.configuration.batch;

import org.springframework.batch.core.JobExecutionListener;
import org.springframework.batch.core.launch.support.SimpleJobLauncher;
import org.springframework.batch.core.repository.JobRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.scheduling.annotation.EnableScheduling;

import com.acm.service.batch.JobCompletionListener;
import com.acm.utils.repository.AcmEnvironnementRepository;

/**
 * {@link BatchScheduler} class.
 *
 * @author HaythemBenizid
 * @since 0.1.0
 */
@Configuration
@EnableScheduling
public class BatchScheduler {

	/** The acmEnvironnement repository. */
	@Autowired
	private AcmEnvironnementRepository environnementRepository;

	/**
	 * Job launcher.
	 * 
	 * @author HaythemBenizid
	 * @param jobRepository the job repository
	 * @return the simple job launcher
	 */
	@Bean
	public SimpleJobLauncher simpleJobLauncher(JobRepository jobRepository) {

		SimpleJobLauncher launcher = new SimpleJobLauncher();
		launcher.setJobRepository(jobRepository);
		return launcher;
	}

	/**
	 * Listener.
	 * 
	 * @author HaythemBenizid
	 * @return the job execution listener
	 */
	@Bean
	public JobExecutionListener listener() {

		return new JobCompletionListener();
	}

	/**
	 * Gets the loans cron value by key=CRON_EXPRESSION_LOANS .
	 * 
	 * @author HaythemBenizid
	 * @return the loans cron value
	 */
	@Bean
	public String getLoansCronValue() {

		return environnementRepository.findByKey("CRON_EXPRESSION_LOANS").get(0).getValue();
	}

	/**
	 * Gets the users cron value by key=CRON_EXPRESSION_USERS.
	 * 
	 * @author HaythemBenizid
	 * @return the users cron value
	 */
	@Bean
	public String getUsersCronValue() {

		return environnementRepository.findByKey("CRON_EXPRESSION_USERS").get(0).getValue();
	}

	/**
	 * Gets the products cron value by key=CRON_EXPRESSION_PRODUCTS .
	 * 
	 * @author HaythemBenizid
	 * @return the products cron value
	 */
	@Bean
	public String getProductCronValue() {

		return environnementRepository.findByKey("CRON_EXPRESSION_PRODUCTS").get(0).getValue();
	}

	/**
	 * Gets the Address customer cron value by key=CRON_EXPRESSION_ADDRESS .
	 * 
	 * @author HaythemBenizid
	 * @return the Address customer cron value
	 */
	@Bean
	public String getAddressCustomerCronValue() {

		return environnementRepository.findByKey("CRON_EXPRESSION_ADDRESS").get(0).getValue();
	}

	/**
	 * Gets the Loan issued cron value by key=CRON_EXPRESSION_DISBURSEMENT_CASE_CLOSURE .
	 *
	 * @author HaythemBenizid
	 * @return the loan issued cron value
	 */
	@Bean
	public String getLoanIssuedCronValue() {

		return environnementRepository.findByKey("CRON_EXPRESSION_DISBURSEMENT_CASE_CLOSURE").get(0)
				.getValue();
	}

	/**
	 * Gets the loan canceled cron value.
	 *
	 * @author YesserSomai
	 * @return the loan canceled cron value
	 */
	@Bean
	public String getLoanCanceledCronValue() {

		return environnementRepository.findByKey("CRON_EXPRESSION_CANCEL_LOAN").get(0).getValue();
	}

	/**
	 * Gets the collections cron value.
	 * 
	 * @author idridi
	 * @return the collections cron value
	 */
	@Bean
	public String getCollectionCronValue() {

		return environnementRepository.findByKey("CRON_EXPRESSION_COLLECTION").get(0).getValue();
	}

}
