package com.acm.api_dakhli.service;

import org.springframework.http.ResponseEntity;

import com.acm.utils.dtos.ResponseIncomeDakhliApiDTO;

// TODO: Auto-generated Javadoc
/**
 * The Interface DakhliApiService.
 */
public interface DakhliApiService {

	/**
	 * Gets the income.
	 *
	 * @param nationalId the national id
	 * @param loanId the loan id
	 * @return the income
	 */
	ResponseEntity<ResponseIncomeDakhliApiDTO> getIncome(String nationalId, Long loanId);

	/**
	 * Gets the employment status rowad.
	 *
	 * @param customerId the customer id
	 * @return the employment status rowad
	 */
	ResponseEntity<?> getIncomeRowad(String customerId);

	/**
	 * Gets the government payslip.
	 *
	 * @param id the id
	 * @param birthDate the birth date
	 * @return the government payslip
	 */
	ResponseEntity<?> getGovernmentPayslip(String id, String birthDate);

	/**
	 * Gets the masdr deposited salary.
	 *
	 * @param customerId the customer id
	 * @param periodInMonths the period in months
	 * @return the masdr deposited salary
	 */
	ResponseEntity<?> getMasdrDepositedSalary(String customerId, String periodInMonths);

	/**
	 * Gets the masdr akeed.
	 *
	 * @param establishmentNumber the establishment number
	 * @param customerId the customer id
	 * @return the masdr akeed
	 */
	ResponseEntity<?> getMasdrAkeed(String establishmentNumber, String customerId);

	/**
	 * Gets the government contract.
	 *
	 * @param idType the id type
	 * @param idValue the id value
	 * @param contractNumber the contract number
	 * @return the government contract
	 */
	ResponseEntity<?> getGovernmentContract(String idType, String idValue, String contractNumber);

}
