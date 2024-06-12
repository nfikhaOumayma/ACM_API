/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.service;

import java.math.BigDecimal;

/**
 * The Interface ClassDetailsService.
 */
public interface SimahClassDetailsService {

	/**
	 * Calculate max installment.
	 *
	 * @param salary the salary
	 * @return the big decimal
	 */
	BigDecimal calculateMaxInstallment(BigDecimal salary);

}
