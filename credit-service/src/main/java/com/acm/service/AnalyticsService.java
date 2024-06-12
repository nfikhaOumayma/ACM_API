/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.service;

import com.acm.utils.dtos.LoanAnalyticsDTO;

/**
 * {@link AnalyticsService} interface.
 *
 * @author HaythemBenizid
 * @since 1.1.3
 */
public interface AnalyticsService {

	/**
	 * Count TOTAL OF APPLIED LOANS by user connected branch OR Access Branch.
	 *
	 * @author HaythemBenizid
	 * @return the loan analytics DTO
	 */
	LoanAnalyticsDTO totalAppliedLoans();

	/**
	 * Count TOTAL OF APPROVED LOANS by user connected branch OR Access Branch.
	 *
	 * @author HaythemBenizid
	 * @return the loan analytics DTO
	 */
	LoanAnalyticsDTO totalApprovedLoans();

	/**
	 * Count TOTAL OF CANCELED/REJECTED LOANS by user connected branch OR Access Branch.
	 *
	 * @author HaythemBenizid
	 * @return the loan analytics DTO
	 */
	LoanAnalyticsDTO totalCanceledRejectedLoans();

	/**
	 * Loans by products Stats used in Donut Chart.
	 * 
	 * @author HaythemBenizid
	 * @return the loan analytics DTO
	 */
	LoanAnalyticsDTO loansByProducts();

	/**
	 * Loans stat by months.
	 * 
	 * @author HaythemBenizid
	 * @return the loan analytics DTO
	 */
	LoanAnalyticsDTO loansStatByMonths();

	/**
	 * Total loans amount.
	 *
	 * @author HaythemBenizid
	 * @return the loan analytics DTO
	 */
	LoanAnalyticsDTO totalLoansAmount();

	/**
	 * Total customers.
	 *
	 * @author HaythemBenizid
	 * @return the loan analytics DTO
	 */
	LoanAnalyticsDTO totalCustomers();

	/**
	 * Total active customers.
	 *
	 * @author HaythemBenizid
	 * @return the loan analytics DTO
	 */
	LoanAnalyticsDTO totalActiveCustomers();

	/**
	 * Loans amount stat by months.
	 * 
	 * @author HaythemBenizid
	 * @return the loan analytics DTO
	 */
	LoanAnalyticsDTO loansAmountStatByMonths();

	/**
	 * Customers stat by months.
	 * 
	 * @author HaythemBenizid
	 * @return the loan analytics DTO
	 */
	LoanAnalyticsDTO customersStatByMonths();
}
