package com.acm.service.impl;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.scheduling.annotation.EnableScheduling;

import com.acm.utils.repository.AcmEnvironnementRepository;

// TODO: Auto-generated Javadoc
/**
 * The Class LoanBackStepServiceImpl.
 */
@Configuration
@EnableScheduling
public class LoanBackStepServiceImpl {

	/** The environnement repository. */
	@Autowired
	private AcmEnvironnementRepository environnementRepository;

	/**
	 * Gets the period value.
	 *
	 * @return the period value
	 */
	@Bean
	public String getPeriodValue() {

		Integer hours = Integer.parseInt(environnementRepository
				.findByKey("PERIOD_BACK_LOAN_TO_FIRST_STEP_SIGN_CONTRACT").get(0).getValue());
		if (hours <= 0) {
			throw new IllegalArgumentException("Hours must be greater than 0");
		}
		if (hours <= 24) {
			// If hours are less than or equal to 24, create an hourly cron expression
			return "0 0 */" + hours + " * * ?";
		}
		else {
			// If hours are greater than 24, convert to days and create a daily cron expression
			int days = hours / 24;
			int remainingHours = hours % 24;
			return "0 0 " + remainingHours + " */" + days + " * ?";
		}

	}

}
