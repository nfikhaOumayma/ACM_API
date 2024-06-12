/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.service.impl;

import java.math.BigDecimal;
import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.acm.repository.SimahClassDetailsRepository;
import com.acm.service.SimahClassDetailsService;
import com.acm.utils.models.SimahClassDetails;
import com.acm.utils.models.SimahClassType;

/**
 * The Class SimahClassDetailsServiceImpl.
 */
@Service
public class SimahClassDetailsServiceImpl implements SimahClassDetailsService {

	/** The simah class details repo. */
	@Autowired
	private SimahClassDetailsRepository classDetailsRepo;

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.SimahClassDetailsService#calculateMaxInstallment(java.math.BigDecimal)
	 */
	@Override
	public BigDecimal calculateMaxInstallment(BigDecimal salary) {

		SimahClassType classType = new SimahClassType();
		/** Set ID Class_Simah_Type as 1 ==> Default Class Chosen Class1 */
		classType.setId(1L);
		List<SimahClassDetails> simahClassDetails =
				classDetailsRepo.findBySimahClassType(classType);

		Integer rate = 0;

		for (SimahClassDetails simahClassDetail : simahClassDetails) {
			BigDecimal salaryFrom = simahClassDetail.getSalaryFrom();
			BigDecimal salaryTo = simahClassDetail.getSalaryTo();
			Integer rateValue = simahClassDetail.getRate();

			if (salaryTo.compareTo(BigDecimal.ZERO) == 0) {
				if (salary.compareTo(salaryFrom) >= 0) {
					rate = rateValue;
					break;
				}
			}
			else {
				if (salary.compareTo(salaryTo) <= 0) {
					rate = rateValue;
					break;
				}
			}
		}

		// Calculate the final amount based on rate and salary
		BigDecimal amount = (salary.multiply(new BigDecimal(rate))).divide(new BigDecimal(100));

		return amount;

	}

}
