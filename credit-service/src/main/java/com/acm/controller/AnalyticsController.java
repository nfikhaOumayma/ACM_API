/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.controller;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import com.acm.service.AnalyticsService;
import com.acm.utils.dtos.LoanAnalyticsDTO;

/**
 * This class @{link AnalyticsController} used for Analytics DATA.
 *
 * @author HaythemBenizid
 * @since 1.1.3
 */
@RestController
@RequestMapping("/analytics")
public class AnalyticsController {

	/** The analytics service. */
	@Autowired
	private AnalyticsService analyticsService;

	/**
	 * Count TOTAL OF APPLIED LOANS by user connected branch OR Access Branch.
	 *
	 * @author HaythemBenizid
	 * @return the loan analytics DTO
	 */
	@GetMapping("/total-applied-loans")
	public LoanAnalyticsDTO totalAppliedLoans() {

		return analyticsService.totalAppliedLoans();
	}

	/**
	 * Count TOTAL OF APPROVED LOANS by user connected branch OR Access Branch.
	 *
	 * @author HaythemBenizid
	 * @return the loan analytics DTO
	 */
	@GetMapping("/total-approved-loans")
	public LoanAnalyticsDTO totalApprovedLoans() {

		return analyticsService.totalApprovedLoans();
	}

	/**
	 * Count TOTAL OF LOANS AMOUNT by user connected branch OR Access Branch.
	 *
	 * @author HaythemBenizid
	 * @return the loan analytics DTO
	 */
	@GetMapping("/total-loans-amount")
	public LoanAnalyticsDTO totalLoansAmount() {

		return analyticsService.totalLoansAmount();
	}

	/**
	 * Count TOTAL OF CANCELED/REJECTED LOANS by user connected branch OR Access Branch.
	 *
	 * @author HaythemBenizid
	 * @return the loan analytics DTO
	 */
	@GetMapping("/total-canceled-rejected-loans")
	public LoanAnalyticsDTO totalCanceledRejectedLoans() {

		return analyticsService.totalCanceledRejectedLoans();
	}

	/**
	 * Count loans By Products for donut chart.
	 *
	 * @author HaythemBenizid
	 * @return the loan analytics DTO
	 */
	@GetMapping("/count-loans-products")
	public LoanAnalyticsDTO countLoansByProducts() {

		return analyticsService.loansByProducts();
	}

	/**
	 * Stats loans By Months (6 - CURRENT + 2) for Bar chart.
	 *
	 * @author HaythemBenizid
	 * @return the loan analytics DTO
	 */
	@GetMapping("/loans-stat-months")
	public LoanAnalyticsDTO loansStatByMonths() {

		return analyticsService.loansStatByMonths();
	}

	/**
	 * Count TOTAL OF CUSTOMERS by user connected branch OR Access Branch.
	 *
	 * @author HaythemBenizid
	 * @return the loan analytics DTO
	 */
	@GetMapping("/total-customers")
	public LoanAnalyticsDTO totalCustomers() {

		return analyticsService.totalCustomers();
	}

	/**
	 * Count TOTAL ACTIVES CUSTOMERS by user connected branch OR Access Branch.
	 *
	 * @author HaythemBenizid
	 * @return the loan analytics DTO
	 */
	@GetMapping("/total-active-customers")
	public LoanAnalyticsDTO totalActiveCustomers() {

		return analyticsService.totalActiveCustomers();
	}

	/**
	 * Loans amount stat by months.
	 * 
	 * @author HaythemBenizid
	 * @return the loan analytics DTO
	 */
	@GetMapping("/loans-amount-stat-months")
	public LoanAnalyticsDTO loansAmountStatByMonths() {

		return analyticsService.loansAmountStatByMonths();
	}

	/**
	 * Customers stat by months.
	 *
	 * @author HaythemBenizid
	 * @return the loan analytics DTO
	 */
	@GetMapping("/customers-stat-months")
	public LoanAnalyticsDTO customersStatByMonths() {

		return analyticsService.customersStatByMonths();
	}
}
