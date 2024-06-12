/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.service;

import java.util.List;

/**
 * {@link IncentiveRepaymentRunService} interface.
 *
 * @author HaythemBenizid
 * @since 1.0.8
 */
public interface IncentiveRepaymentRunService {

	/**
	 * Run incentive calculate : execute SQL Procedure.
	 * 
	 * @author HaythemBenizid
	 */
	void runIncentiveCalculate();

	/**
	 * Generate incentive report : EXCEL.
	 *
	 * @author HaythemBenizid
	 * @param year the year
	 * @param month the month
	 * @return the byte[]
	 */
	byte[] generateIncentiveReport(Integer year, Integer month);

	/**
	 * Gets the run year.
	 * 
	 * @author HaythemBenizid
	 * @return the run year
	 */
	List<Integer> getRunYear();

	/**
	 * Gets the run month.
	 * 
	 * @author HaythemBenizid
	 * @return the run month
	 */
	List<Integer> getRunMonth();
}
