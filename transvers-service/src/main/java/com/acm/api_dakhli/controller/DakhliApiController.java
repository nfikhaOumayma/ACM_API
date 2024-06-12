package com.acm.api_dakhli.controller;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import com.acm.api_dakhli.service.DakhliApiService;
import com.acm.utils.dtos.ResponseIncomeDakhliApiDTO;

// TODO: Auto-generated Javadoc
/**
 * The Class DakhliApiController.
 */
@RestController
@RequestMapping("/dakhli-api")
public class DakhliApiController {

	/** The dakhli api service. */
	@Autowired
	private DakhliApiService dakhliApiService;

	/**
	 * Gets the employment status.
	 *
	 * @param nationalId the national id
	 * @param loanId the loan id
	 * @return the employment status
	 */
	@GetMapping("/gosi/income/{nationalId}/{loanId}")
	public ResponseEntity<ResponseIncomeDakhliApiDTO> getEmploymentStatus(
			@PathVariable String nationalId, @PathVariable("loanId") Long loanId) {

		return dakhliApiService.getIncome(nationalId, loanId);
	}

	/**
	 * Gets the employment status rowad.
	 *
	 * @param customerId the customer id
	 * @return the employment status rowad
	 */
	@GetMapping("rowad/gosi/income/{customerId}")
	public ResponseEntity<?> getEmploymentStatusRowad(@PathVariable String customerId) {

		return dakhliApiService.getIncomeRowad(customerId);
	}

	/**
	 * Government payslip.
	 *
	 * @param id the id
	 * @param birthDate the birth date
	 * @return the response entity
	 */
	@GetMapping("goverment/payslip")
	public ResponseEntity<?> governmentPayslip(@RequestParam("id") String id,
			@RequestParam("birthDate") String birthDate) {

		return dakhliApiService.getGovernmentPayslip(id, birthDate);
	}

	/**
	 * Gets the masdr deposited salary.
	 *
	 * @param customerId the customer id
	 * @param periodInMonths the period in months
	 * @return the masdr deposited salary
	 */
	@GetMapping("masdr/deposited-salary/{customerId}/{periodInMonths}")
	public ResponseEntity<?> getMasdrDepositedSalary(@PathVariable String customerId,
			@PathVariable String periodInMonths) {

		return dakhliApiService.getMasdrDepositedSalary(customerId, periodInMonths);
	}

	/**
	 * Gets the masdr akeed.
	 *
	 * @param establishmentNumber the establishment number
	 * @param customerId the customer id
	 * @return the masdr akeed
	 */
	@GetMapping("gosi/akeed/engagement-status/{establishmentNumber}/Relation/{customerId}")
	public ResponseEntity<?> getMasdrAkeed(@PathVariable String establishmentNumber,
			@PathVariable String customerId) {

		return dakhliApiService.getMasdrAkeed(establishmentNumber, customerId);
	}

	/**
	 * Gets the masdr akeed.
	 *
	 * @param idType the id type
	 * @param idValue the id value
	 * @param contractNumber the contract number
	 * @return the masdr akeed
	 */
	@GetMapping("goverment/contracts")
	public ResponseEntity<?> getMasdrAkeed(@RequestParam("idType") String idType,
			@RequestParam("idValue") String idValue,
			@RequestParam("contractNumber") String contractNumber) {

		return dakhliApiService.getGovernmentContract(idType, idValue, contractNumber);
	}
}
